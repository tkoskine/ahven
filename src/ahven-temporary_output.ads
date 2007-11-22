with Ada.Strings.Unbounded;
with Ada.Text_IO;

package Ahven.Temporary_Output is
   Temporary_File_Error : exception;

   type Temporary_File is limited record
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Handle  : Ada.Text_IO.File_Type;
   end record;

   procedure Create_Temp (File : out Temporary_File);
   -- Create a new temporary file. Exception Temporary_File_Error
   -- is raised if the procedure cannot create a new temp file.

   procedure Redirect_OutPut (To_File : in out Temporary_File);

   procedure Restore_Output;

   procedure Remove_Temp (File : in out Temporary_File);

   procedure Close_Temp (File : in out Temporary_File);

end Ahven.Temporary_Output;
