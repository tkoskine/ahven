:mod:`Ahven.Runner` -- Package
==============================

.. module:: Ahven.Runner
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----

Report_Proc
'''''''''''

::

   type Report_Proc is access procedure
     (Test_Results : Results.Result_Collection;
      Args         : Parameters.Parameter_Info);


-------------------------
Procedures and Funcstions
-------------------------

Run_Suite
'''''''''

::

   procedure Run_Suite (Suite    : in out Framework.Test'Class;
                        Reporter :        Report_Proc);

Run the given test (case/suite) and pass the results and
the command line argument info to the reporter procedure.

