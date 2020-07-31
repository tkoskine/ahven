.. index:: ! Ahven.XML_Runner (package)

:mod:`Ahven.XML_Runner` -- Package
==================================

.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

.. versionadded:: 1.2

Package Ahven.XML_Runner provides procedures to run
the test suites and write the results in XML file format.

In normal case, Ahven.Text_Runner uses Ahven.XML_Runner
automatically, if user has given a parameter to write the results
in XML format.

The target directory is parsed by Ahven.Runner from the program
arguments and given as a parameter to Report_Results procedure.

------------------------
Procedures and Functions
------------------------


Run
'''

::

   procedure Run (Suite : in out Framework.Test_Suite'Class);

Run the suite and write the results to a file in XML format.

Run
'''

::

   procedure Run (Suite : Framework.Test_Suite_Access);

Run the suite and write the results to a file. The routine is
identical to the Run (Suite : in out Framework.Test_Suite'Class) procedure,
but takes an access parameter to a test suite.

Report_Results
''''''''''''''

::

   procedure Report_Results (Result : Results.Result_Collection;
                             Dir    : String);

Write the test results to the given directory. This is called
automatically during the execution of either of Run procedures.

