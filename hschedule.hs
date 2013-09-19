import Data.List
import Data.List.Split hiding (oneOf, sepBy)
import Data.Ord
import Data.String.Utils
import Data.Time
import Data.Time.LocalTime
import System.Environment
import System.IO
import Text.ParserCombinators.Parsec

--                startTime  endTime    style   arguments
type Subject   = (TimeOfDay, TimeOfDay, String, [String])
--                header  subjects
type DayColumn = (String, [Subject])

main ::  IO ()
main = do
  args <- getArgs
  case args of
    [] -> hPutStrLn stderr "usage: schedule.hs <filename>"
    _  -> do
      res  <- parseFromFile scheduleFile (args !! 0)
      case res of
        Left  x -> hPutStrLn stderr $ "Parse Error: " ++ (show x)
        Right x -> outputLaTeX stdout x

----------------------
--      Output      --
----------------------

outputLaTeX :: Handle-> [DayColumn] -> IO ()
outputLaTeX h xs = do 
    cat "header.tex"
    out $ generateLaTeX xs
    out $ timeline 1 xs
    cat "footer.tex"
  where out   = hPutStrLn h
        cat s = readFile s >>= out

timeline :: Int -> [DayColumn] -> String
timeline n xs = unlines $ map fmt $ to2Tuples $ takeWhile (<= end) steps
  where times = concat [[s,e] | (_,ys) <- xs, (s,e,_,_) <- ys]
        ma    = maximum times
        mi    = minimum times
        step  = TimeOfDay 0 (div 60 n) 0
        steps = start : map (onTimeOfDay (+) step) steps
        start = mi { todMin = 0 }
        end   = if todMin ma == 0 then ma else ma { todHour = (todHour ma) +1, todMin = 0}
        fmt (x,y) = let inner = command "timeentry" $ map formatTimeOfDay [x,y]
                    in  node "timestyle" [] 1 (metricTime x) inner

generateLaTeX :: [DayColumn] -> String
generateLaTeX xs = unlines' $ zipWith dayColumnToLaTeX [1..] xs

dayColumnToLaTeX :: Float -> DayColumn -> String
dayColumnToLaTeX d (header, xs) = unlines' $ (map (subjectsToLaTeX d) xs') ++ [t]
  where t = node "headerstyle" [] (d+0.5) 7 (command "headerentry" [header])
        xs' = groupOverlapping $ sortBy (comparing startTime) xs
        groupOverlapping = groupBy (\(s1, e1,_,_) (s2, e2, _,_) -> overlap s1 e1 s2 e2)


subjectsToLaTeX :: Float -> [Subject] -> String
subjectsToLaTeX d xs = unlines' $ zipWith f [0..] xs
  where l         = fromIntegral $ length xs
        f n entry = subjectToLaTeX entry l (d + (n/l))
        

subjectToLaTeX :: (Show a) => Subject -> a -> a -> String
subjectToLaTeX (s,e,style,sub) num d = node style' [dur,num'] d (metricTime s) sub'
  where num' = show num
        dur  = metricString $ onTimeOfDay (-) e s
        style' = "entrystyle" ++ style
        cargs = [formatTimeOfDay s, formatTimeOfDay e] ++ sub
        cmd  = concat ["entry", romans !! (length cargs)]
        sub' = command cmd cargs

node :: (Show a, Show b) => String -> [String] -> a -> b -> String -> String
node style sargs x y s = concat ["\\node[", style, "=", styleargs, "]", " at (", x', ",", y', ") {", s, "};"]
  where styleargs = concatMap (enclose "{" "}") sargs
        x'        = show x
        y'        = show y

command ::  String -> [String] -> String
command name cargs = concat ["\\", name, concatMap (enclose "{" "}") cargs]

----------------------
--    Formatting    --
----------------------

romans ::  [String]
romans = ["", "I", "II", "III", "IV", "V", "VI", "VII", "VIII", "IX"]

unlines' :: [String] -> String
unlines' = intercalate "\n"

enclose ::  [a] -> [a] -> [a] -> [a]
enclose a b x = a ++ x ++ b

metricString ::  TimeOfDay -> String
metricString = show . metricTime

formatTimeOfDay ::  TimeOfDay -> String
formatTimeOfDay = take 5 . show

----------------------
--  Time Handling   --
----------------------

overlap ::  Ord a => a -> a -> a -> a -> Bool
overlap s1 e1 s2 e2 =  (s1 >= s2 && e1 <= e2)
                    || (s1 <= s2 && e1 >= e2)
                    || (s1 <= s2 && s2 <= e1)
                    || (s1 <= e2 && e2 <= e1)

metricTime ::  Fractional a => TimeOfDay -> a
metricTime t = ((fromIntegral .todHour) t) + ((fromIntegral . todMin) t / 60)

onTimeOfDay :: (DiffTime -> DiffTime -> DiffTime)-> TimeOfDay -> TimeOfDay -> TimeOfDay
onTimeOfDay f x y = timeToTimeOfDay $ onTimeOfDay' f x y

onTimeOfDay' :: (DiffTime -> DiffTime -> t) -> TimeOfDay -> TimeOfDay -> t
onTimeOfDay' f x y = f (timeOfDayToTime x) (timeOfDayToTime y) 

startTime :: Subject -> TimeOfDay
startTime (x,_,_,_) = x

----------------------
--     Helpers      --
----------------------

to2Tuples ::  [t] -> [(t, t)]
to2Tuples []        = []
to2Tuples [_]       = []
to2Tuples (x:y:xs) = (x,y) : to2Tuples (y:xs)
 
----------------------
--      Parser      --
----------------------

scheduleFile :: GenParser Char st [DayColumn]
scheduleFile = 
    do result <- many day
       eof
       return result

day :: GenParser Char st DayColumn
day = do
  h <- header
  l <- block `sepEndBy` (many eol)
  return (h, l)
  

block :: GenParser Char st Subject
block = do
  s <- dayTime
  spaces
  e <- dayTime
  spaces
  st <- option "" style
  spaces
  l <- (many1 (noneOf "\n")) `sepEndBy` eol
  return (s, e, st, l)
  
header :: GenParser Char st String
header = do
  char '#'
  l <- line
  many eol
  return $ strip l

style :: GenParser Char st String
style = do
  char '.'
  many alphaNum

line :: GenParser Char st String
line = do
  l <- many (noneOf "\n")
  eol
  return l

noneDashLine :: GenParser Char st String
noneDashLine = do
  x <- noneOf "#"
  l <- line
  return $ x:l

dayTime :: GenParser Char st TimeOfDay
dayTime = do
  h <- hour
  char ':'
  m <- minute
  return $ TimeOfDay h m 0

hour :: GenParser Char st Int
hour = restrictedTwoDigitInt "012"

minute :: GenParser Char st Int
minute = restrictedTwoDigitInt "012345"

restrictedTwoDigitInt :: String -> GenParser Char st Int
restrictedTwoDigitInt s = do
  h1 <- option '0' (oneOf s)
  h2 <- option '0' digit
  return $ read [h1,h2]

eol :: GenParser Char st Char
eol = char '\n'
