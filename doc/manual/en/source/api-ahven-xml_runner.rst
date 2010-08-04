:mod:`Ahven.XML_Runner` -- Package
==================================

.. module:: Ahven.XML_Runner
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>


------------------------
Procedures and Functions
------------------------


Run
'''

::

   procedure Run (Suite : in out Framework.Test_Suite'Class);

Run the suite and print the results.

Run
'''

::

   procedure Run (Suite : Framework.Test_Suite_Access);

Run the suite and print the results. The routine is
identical to the above routine, but takes an access
parameter to a test suite.

Report_Results
''''''''''''''

::

   procedure Report_Results (Result : Results.Result_Collection;
                             Dir    : String);

Report results to the given directory.

