============
Design Notes
============

This section tells reasons behind various design choices
of Ahven. Since Ahven is meant to be buildable with multiple
different Ada compilers on multiple different operating systems,
we have made certain amount of compromises.

Timeout Feature
===============

Starting from version 2.0, tests can be interrupted after
a given time. This feature is implemented using tasks.

Ahven spawns a new local task for each test routine (procedure),
and kills it with "abort" statement of Ada. The task and
the test routine is aborted if the runtime system of the
used compiler can abort the task. The test routine
should include at least one "abort completion point"
to make the abortion successful.

However, some environments allow the task abortion even
without abort completion points in some cases. FSF GCC
on OpenBSD with user space threads is one such environment.

