:mod:`Ahven.Parameters` -- Package
==================================

.. module:: Ahven.Parameters
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----

Parameter_Info
''''''''''''''

::

   type Parameter_Info is private;


Parameter_Mode
''''''''''''''

::

   type Parameter_Mode is (NORMAL_PARAMETERS, TAP_PARAMETERS);


------------------------
Procedures and Functions
------------------------

   procedure Parse_Parameters (Mode :     Parameter_Mode;
                               Info : out Parameter_Info);

Parse Ada.Command_Line parameters and put the results
to the Info parameter. Raises Invalid_Parameter if
some parameter is invalid.

Parse_Parameters
''''''''''''''''

::

   procedure Parse_Parameters (Mode :     Parameter_Mode;
                               Info : out Parameter_Info);

Parse Ada.Command_Line parameters and put the results
to the Info parameter. Raises Invalid_Parameter if
some parameter is invalid.

Usage
'''''

::

   procedure Usage (Mode : Parameter_Mode := NORMAL_PARAMETERS);

Print usage.

Capture
'''''''

::

   function Capture (Info : Parameter_Info) return Boolean;

Capture Ada.Text_IO output?

Verbose
'''''''

::

   function Verbose (Info : Parameter_Info) return Boolean;

Use verbose mode?

XML_Results
'''''''''''

::

   function XML_Results (Info : Parameter_Info) return Boolean;

Output XML?

Single_Test
'''''''''''

::

   function Single_Test (Info : Parameter_Info) return Boolean;

Run a single test (case/suite/routine) only?

Test_Name
'''''''''

::

   function Test_Name (Info : Parameter_Info) return String;

Return the name of the test passed as a parameter.

Result_Dir
''''''''''

::

   function Result_Dir (Info : Parameter_Info) return String;

Return the directory for XML results.

