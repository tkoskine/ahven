:mod:`Ahven.Temporary_Output` -- Package
========================================

.. ada:module:: Ahven.Temporary_Output
.. moduleauthor:: Tero Koskinen <tero.koskinen@iki.fi>

-----
Types
-----

Temporary_File
''''''''''''''

::

   type Temporary_File is limited private;

A type which represents a temporary file.

------------------------
Procedures and Functions
------------------------

Create_Temp
'''''''''''

::

   procedure Create_Temp (File : out Temporary_File);

Create a new temporary file. Exception Temporary_File_Error
is raised if the procedure cannot create a new temp file.

The name of the file is automatically generated and
follows form ahven_123 where "ahven\_" is a constant prefix
and "123" increases by one after every Create_Temp call.
The file is created to the current working directory.

For every created temporary file, you need to call either
Remove_Temp or Close_Temp when the file is no longer needed.
Otherwise, there will be a memory leak.


Get_Name
''''''''

::

   function Get_Name (File : Temporary_File) return String;

Return the name of the file. You need to create a new temporary
file first using Create_Temp procedure before calling this
function.

Example::

      ...
      Temp : Temporary_Output.Temporary_File;
   begin
      Temporary_Output.Create_Temp (Temp);
      Ada.Text_IO.Put_Line (Temporary_Output.Get_Name (Temp));

Redirect_Output
'''''''''''''''

::

   procedure Redirect_Output (To_File : in out Temporary_File);

Redirect the standard output to the file.
To_File must be opened using Create_Temp.

Restore_Output
''''''''''''''

::

   procedure Restore_Output;

Restore the standard output to its default settings.

Remove_Temp
'''''''''''

::

   procedure Remove_Temp (File : in out Temporary_File);

Remove the temporary file. File can be either open or closed.

Close_Temp
''''''''''

::

   procedure Close_Temp (File : in out Temporary_File);

Close the temporary file. If you want to remove the temporary
file from the file system, you need to call Remove_Temp
procedure.

