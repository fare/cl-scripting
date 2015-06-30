cl-scripting, utilities for scripting in Common Lisp
====================================================

cl-scripting is a collection of utilities for scripting in Common Lisp.

Don't write shell scripts, write Common Lisp scripts instead! See my article
[Common Lisp as a Scripting Language, 2015 edition](http://fare.livejournal.com/184127.html).

For examples on how I use these utilities, see
[fare-scripts](http://github.com/fare/fare-scripts).

Contents:

* [cl-scripting.asd](cl-scripting.asd): the .asd file,
  which trivially uses package-inferred-system.

* [failure.lisp](failure.lisp): recording failure of subtasks,
  so you can report in the end which things went wrong in a script.

* [commands.lisp](commands.lisp): running and registering functions
  that may be called from the shell command line.

* [cl-scripting.lisp](cl-scripting.lisp): one package to rule them all.
  Importing and reexporting functionality from the above files.
