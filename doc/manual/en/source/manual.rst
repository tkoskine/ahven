

==================
Ahven User's Guide
==================

Tero Koskinen <tero.koskinen@iki.fi>

Overview
########

Introduction
============

Ahven is a unit test library. It is modeled after
JUnit framework for Java, but some ideas are also
taken from another Ada unit test library, AUnit.

The purpose of Ahven is to be a small and portable
unit test library, which works with multiple
different Ada 95 compilers. Ahven has no
external dependencies and therefore it is easy
to build on various platforms.

Ahven tries to be compatible with utilities related
to unit testing. For example, it uses same
XML format for test results as Java tools.
This allows easy integration to CruiseControl, Ant,
and other similar programs.

License
=======

Ahven is distributed under permissive ISC license (shown below).

::

    --
    -- Copyright (c) 2007-2016 Tero Koskinen <tero.koskinen@iki.fi>
    --
    -- Permission to use, copy, modify, and distribute this software for any
    -- purpose with or without fee is hereby granted, provided that the above
    -- copyright notice and this permission notice appear in all copies.
    --
    -- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
    -- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
    -- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
    -- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
    -- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
    -- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
    -- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
    --

You are allowed to embed Ahven into a proprietary commercial application.
Only requirement is to keep the copyright notice and the permission notice
in the source code files. You do not need to distribute Ahven's source code
if you distribute Ahven or some parts of Ahven in the binary form.

Building and Installing Ahven
#############################

To build and install Ahven source code, you need an Ada 95 compiler.
At the moment, Ahven is tested with three different
compiler families: GNAT, Irvine ICCAda, and Janus/Ada.

GNAT GPL series and FSF GCC
===========================

For GNAT there are two sets of project files provided:

* gnat
* gnat_linux

GNAT project files under *gnat* directory are generic and can be used
on any system, but *gnat_linux* depends on GNU Make utility and
expects Unix-like environment (Linux, \*BSD, Cygwin or MinGW on Windows).

Using GNAT project files from gnat directory
--------------------------------------------

There are two project files under *gnat* directory:
ahven.gpr and ahven_tests.gpr.

Project file ahven.gpr will build the library itself
and ahven_tests.gpr will build the unit tests for the library.

Example on Windows:

::

   gnatmake -P gnat\ahven
   gnatmake -P gnat\ahven_tests

Once the source files and tests are compiled, there are two executables
in *gnat* directory: tester(.exe) and tap_tester(.exe).

You can run them and if there are no errors, the library can be expected
to be working correctly.

Project files in *gnat* directory do not provide separate installation
step for the library. When you wish to use the library, you can simply
reference ahven.gpr in your project file::

   -- my_project.gpr
   with "/path/to/ahven/gnat/ahven.gpr";

   project My_Project is
      -- ...
   end My_Project;


Alternative build system for GNAT on Linux
------------------------------------------

People using Linux and GNAT, especially Fedora Linux and
FSF GNAT, can use an alternative build system based on
comfignat. This build system integrates better into existing
Ada library infrastructure provided by the used Linux distribution.

To build and install Ahven using comfignat-based system, run:

::

    $ cd gnat_linux
    $ make
    $ sudo make install

Note: You need to have *python-sphinx* and *python-sphinxcontrib-adadomain*
packages installed to generate the documentation for Ahven.

If you want to change the installation directory, you can give *make*
command prefix parameter:

::

   $ cd gnat_linux
   $ make prefix=$HOME/tmp/ahven-install-dir
   $ make install

Irvine ICCAda
=============

Easiest way to build Ahven with ICCAda is to use *icm* utility::

    C:\ahven-2.7>cd src
    C:\ahven-2.7\src>icm new
    C:\ahven-2.7\src>icm scan *.ad? windows\*.ad?
    C:\ahven-2.7\src>icm make libmain
    C:\ahven-2.7\src>cd ..\test
    C:\ahven-2.7\test>icm new -search=..\src
    C:\ahven-2.7\test>icm scan *.ad?
    C:\ahven-2.7\test>icm make tester
  
There is no installation step. If you want to use Ahven
from your program, run *icm new* with -search parameter:

::

    icm new -search=c:\ahven-2.7\src

Janus/Ada
=========

Directory *janusada* contains project file creation scripts for Janus/Ada.
By default, the scripts assume Janus/Ada to be installed to directory
*C:\\Janus312\\*.  If that is not the case, change the path from
file *prepare.bat*.

::

    C:\ahven-2.7>janusada\prepare.bat

Before compiling the library, you need to run
the preparation script *janusada\\prepare.bat*.
Then, scan the sources and create compilation script
by running *janusada\\update.bat*.

::

    C:\ahven-2.7>janusada\update.bat

Now you are ready to compile the project.
This happens by running
*compile.bat* script.

::

    C:\ahven-2.7>janusada\compile.bat

After a while, you should have compiled library files
in the *lib_obj* directory and an executable called *tap_test.exe*
in the *test_obj* directory.

The executable is Ahven's test suite and if it reports
no errors, everything is working as expected.

At the time of writing (Ahven 2.7), every test, which is not skipped,
should pass with the latest version of Janus/Ada.

However, with earlier versions of Janus/Ada some tests will fail.
The failing tests are worked around in Ahven's source code, but
the test exists so that one can verify when the Janus/Ada bug
causing the failure is fixed.


Using Ahven
###########

The heart of Ahven is an abstract type called ``Test``.

It presents an entity which can be run by *a test runner*.
Types ``Test_Case`` and ``Test_Suite`` are derived from the
``Test`` type. The ``Test_Case`` type is the base type
for unit tests and the ``Test_Suite`` type is a container,
which can hold other ``Test`` objects.

Writing a Test Case
===================

To create a new test case you need to create a new package
and a new type, which is derived from
``Ahven.Framework.Test_Case``.
There are no required functions or procedures to
be implemented, but to make the test case do something
you need to override the ``Initialize`` procedure
and create at least one procedure which tests something::

    -- my_tests.ads
    with Ahven.Framework;
    package My_Tests is
       type Test is new Ahven.Framework.Test_Case with null record;
       procedure Initialize (T : in out Test);
    private
       procedure Test_Addition;
    end My_Tests;

To add tests to the test case you need to
call procedure ``Ahven.Framework.Add_Test_Routine``
during the test case initialization (in other words, in the
``Initialize`` procedure).
:ref:`testcase_a_body` shows how the
``Test_Addition`` is added to the test case.
It also shows how to set a name for the test case with
the ``Set_Name`` procedure.


.. _testcase_a_body:

A test case package body
------------------------

::

    -- my_tests.adb
    package body My_Tests is
       procedure Initialize (T : in out Test) is
       begin
          Set_Name (T, "My tests");
          Ahven.Framework.Add_Test_Routine
            (T, Test_Addition'Access, "Addition");
       end Initialize;

       procedure Test_Addition is
       begin
          null;
       end Test_Addition;
    end My_Tests;

Calling Assertion Procedures
============================

To test whether a condition is true or false,
Ahven offers you three procedures. The first
procedure is :ref:`Ahven.Assert <ahven-assert>`.
It takes a boolean value and a message string as its parameters.
If the boolean value is false the ``Assert``
raises an ``Assertion_Error`` exception
with the given string. The exception is catched by the framework.
and when the test results are shown the error is also shown
with the given message.

Another assertion procedure is a generic
:ref:`Ahven.Assert_Equal <ahven-assert_equal>` procedure.
It is meant for comparing two objects of same type.
If the objects are not equal
the ``Assertion_Error`` exception
with the given message string is raised.

The third assertion procedure is simple
:ref:`Ahven.Fail <ahven-fail>` which always raises
the ``Assertion_Error`` exception.
It is handy for situations where the execution should not
reach a certain place (see :ref:`fail_example`).

.. _fail_example:

Fail in action
--------------

::

    package body My_Tests is
       ...
       procedure Test_My_Proc is
       begin
          begin
             My_Proc (-1); -- should raise Custom_Error
             Fail ("Custom_Error expected");
          exception
             when Custom_Error =>
                null; -- expected
                -- Note: the exception block should not
                -- catch Assertion_Error. Otherwise
                -- the assertion failure will not be noticed.
          end;
       end Test_My_Proc;
    end My_Tests;

Composing Test Hierarchies With Test Suites
===========================================

The ``Test_Suite`` type is used to group related tests together.
You can also add other test suites to the suite and create
a hierarchy of tests.

The tests are added to the test suite using either procedure
``Add_Static_Test`` or ``Add_Test``.
The former procedure is meant for statically created tests and
it places a copy of the given test to the test suite.
The ``Add_Test`` procedure is used with dynamically created tests
and test objects of type Test_Class_Access.

At the moment, the dynamically added tests are executed first in
the order they have been added (first in, first out - FIFO)
and after them the statically added tests, also in FIFO order.

:ref:`suite_example` shows how to put test cases in a test suite.

.. _suite_example:

Suite Example
-------------

::

    package body My_Tests is
       ...
       function Get_Test_Suite return Ahven.Framework.Test_Suite is
          S : Framework.Test_Suite := Framework.Create_Suite ("All");
          Hello_World_Test : Hello_World.Test;
          Listener_Test    : Basic_Listener_Tests.Test;
       begin
          Framework.Add_Static_Test (S, Hello_World_Test);
          Framework.Add_Static_Test (S, Listener_Test);
          return S;
       end Get_Test_Suite;
    end My_Tests;

Running Tests
=============

The tests are run by test runners.  These runners are procedures which take
either test cases or test suites as their parameters.

Currently, there exists three test runners. Ahven.Runner is the basic
runner, which prints the test results as a hierarchy. Ahven.XML_Runner
on the other hand writes the test results to an XML file, which is
understood by continuous integration systems like CruiseControl and Hudson.
The third runner is Ahven.Tap_Runner. It produces the results in
Test-Anything-Protocol (TAP) format.

The recommended way to use these test runners is to call them from
the main program:

::

    with Ahven.Text_Runner;
    with Ahven.Framework;
    with Simple_Tests;
    procedure Tester is
       S : Ahven.Framework.Test_Suite := Ahven.Framework.Create_Suite ("All");
    begin
        Ahven.Framework.Add_Test (S, new Simple_Tests.Test);
        Ahven.Text_Runner.Run (S);
    end Tester;


Parameters
----------

Ahven.Text_Runner recognizes following parameters:

.. program:: tester

.. cmdoption:: -d

    directory for test results

.. cmdoption:: -x 

    output in XML format

.. cmdoption:: -c

    capture and report test outputs

.. cmdoption:: -s

    Specify test name suffix to be used in XML files

.. cmdoption:: -t

    specify timeout value for tests (value 0 means infinite timeout)

.. cmdoption:: -q 

    quiet results

.. cmdoption:: -v

    verbose results (default)

.. cmdoption:: -i 

    ignore remaining parameters - for passing parameters to the test cases

