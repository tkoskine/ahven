--
-- Copyright (c) 2007, 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Strings.Unbounded;
with Ada.Text_IO;

use Ada.Strings.Unbounded;

package body Ahven.Listeners.Output_Capture is

   function Create return Basic.Basic_Listener_Class_Access is
   begin
      return new Output_Capture_Listener;
   end Create;

   procedure Start_Test (Listener : in out Output_Capture_Listener;
                         Info  : Result_Info) is
   begin
      -- Empty routine name means a test suite or test case
      if Get_Routine_Name (Info) /= Null_Unbounded_String then
         -- A test routine? Let's create a temporary file
         -- and direct Ada.Text_IO output there.
         Temporary_Output.Create_Temp (Listener.Output_File);
         Temporary_Output.Redirect_Output (Listener.Output_File);
      end if;
      Basic.Start_Test (Basic.Basic_Listener (Listener), Info);
   end Start_Test;

   procedure End_Test (Listener : in out Output_Capture_Listener;
                       Info : Result_Info) is
      use type Basic.Result_Type;
      My_Info : Result_Info := Info;
   begin
      -- Sanity check: if we have existing Result_Collection then...
      if Listener.Current_Result /= null then
         if Listener.Last_Test_Result /= Basic.NO_RESULT then
            -- End of the test routine, so we can restore
            -- the normal output now and close the temporary file.
            Temporary_Output.Restore_Output;
            Temporary_Output.Close_Temp (Listener.Output_File);

            -- Saving the name of the temporary file to the test result,
            -- so the file can be deleted later
            Set_Output_File
              (My_Info, Temporary_Output.Get_Name (Listener.Output_File));
         end if;
      end if;
      Basic.End_Test (Basic.Basic_Listener (Listener), My_Info);
   end End_Test;

   procedure Remove_File (Name : String) is
      Handle : Ada.Text_IO.File_Type;
   begin
      Ada.Text_IO.Open (Handle, Ada.Text_IO.Out_File, Name);
      Ada.Text_IO.Delete (Handle);
   exception
      when others =>
         null; -- For now we can safely ignore errors (like missing file)
   end Remove_File;

   procedure Remove_Files (Collection : in out Result_Collection) is
      procedure Remove_Loop (First_Item : Result_Info_Iterator);
      procedure Remove (Name : Unbounded_String);

      procedure Remove (Name : Unbounded_String) is
      begin
         if Length (Name) > 0 then
            Remove_File (To_String (Name));
         end if;
      end Remove;

      procedure Remove_Loop (First_Item : Result_Info_Iterator) is
         Loop_Iter : Result_Info_Iterator := First_Item;
      begin
         loop
            exit when not Is_Valid (Loop_Iter);
            Remove (Get_Output_File (Data (Loop_Iter)));
            Loop_Iter := Next (Loop_Iter);
         end loop;
      end Remove_Loop;

      Child_Iter : Result_Collection_Iterator;
   begin
      Remove_Loop (First_Pass (Collection));
      Remove_Loop (First_Failure (Collection));
      Remove_Loop (First_Error (Collection));

      Child_Iter := First_Child (Collection);
      Child_Loop:
      loop
         exit Child_Loop when not Is_Valid (Child_Iter);
         Remove_Files (Data (Child_Iter).all);
         Child_Iter := Next (Child_Iter);
      end loop Child_Loop;
   end Remove_Files;

   procedure Finalize (Listener : in out Output_Capture_Listener) is
   begin
      Remove_Files (Listener.Main_Result);
   end Finalize;

end Ahven.Listeners.Output_Capture;
