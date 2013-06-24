:mod:`Ahven.Tap_Runner` -- Package
==================================

.. ada:module:: Ahven.Tap_Runner
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>


========================
Procedures and Functions
========================

Run
'''

::

   procedure Run (Suite : in out Framework.Test'Class);

Run the suite and print the results in Test-Anything-Protocol (TAP)
format. Skipped tests are reported as *ok* with *# SKIP* text in description.

Add_Pass
''''''''

::

   procedure Add_Pass (Listener : in out Tap_Listener;
                       Info     :        Context);

Add_Failure
'''''''''''

::

   procedure Add_Failure (Listener : in out Tap_Listener;
                          Info     :        Context);

Add_Error
'''''''''

::

   procedure Add_Error (Listener : in out Tap_Listener;
                        Info     :        Context);


Start_Test
''''''''''

::

   procedure Start_Test (Listener : in out Tap_Listener;
                         Info     :        Context);

End_Test
''''''''

::

   procedure End_Test (Listener : in out Tap_Listener;
                       Info     :        Context);


