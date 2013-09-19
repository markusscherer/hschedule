hschedule
=========

hschedule is a small haskell program to generate schedules for university
courses and such.

Requirements
------------

These will probably already be installed:

 * [split](http://hackage.haskell.org/packages/archive/split/latest/doc/html/Data-List-Split.html)
 * [parsec](http://hackage.haskell.org/package/parsec-3.1.3)
 * [missingh](http://hackage.haskell.org/package/MissingH-1.1.0.3)

Running
-------

    runhaskell hschedule.hs schedule.txt > schedule.tex && pdflatex schedule.tex

A `header.tex` and a `footer.tex` file (see examples) has to be present in the
current directory.

Syntax
------

```
# This will be the header of a day

08:00 10:00 .Style1
line one
line two

08:00 10:00 .Style2
line one
line two
```

The the optional style tag translates into a `entrystyle&lt;Stylename&gt;` style
specification used for the generated nodes (which has to be specified in
`header.tex`).

Every block tanslates to a `\entry*` command, where * is a roman literal from II
to IX describing the number of lines below below the block header (the line with
the start and end time) + 2 (which has to be specified in `header.tex`).

Just look at the example, it's less complicated than it sounds.
