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

with Ada.Text_IO;

package body Ahven.Listeners.Basic is

   -- Because of Ada.Text_IO output capturing, the result
   -- recording is happening in the End_Test procedure.
   --
   -- Add_{Pass,Failure,Error} procedures delegate result
   -- saving to the Set_Last_Test_Info procedure, which
   -- records the latest result to the listener.

   procedure Set_Last_Test_Info (Listener : in out Basic_Listener;
                                 Info     : Result_Info;
                                 Result   : Result_Type) is
   begin
      Listener.Last_Test_Result := Result;
      Listener.Last_Test_Message := To_Unbounded_String (Get_Message (Info));
      Listener.Last_Test_Long_Message :=
        To_Unbounded_String (Get_Long_Message (Info));
   end Set_Last_Test_Info;

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Info  : Result_Info) is
   begin
      Set_Last_Test_Info (Listener, Info, PASS_RESULT);
   end Add_Pass;

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Info  : Result_Info) is
   begin
      Set_Last_Test_Info (Listener, Info, FAILURE_RESULT);
   end Add_Failure;

   procedure Add_Error (Listener : in out Basic_Listener;
                        Info  : Result_Info) is
   begin
      Set_Last_Test_Info (Listener, Info, ERROR_RESULT);
   end Add_Error;

   procedure Start_Test (Listener : in out Basic_Listener;
                         Info  : Result_Info) is
      R : Result_Collection_Access := null;
   begin
      -- Empty routine name means a test suite or test case
      if Get_Routine_Name (Info) = Null_Unbounded_String then
         R := new Result_Collection;
         Set_Name (R.all, Get_Test_Name (Info));
         Set_Parent (R.all, Listener.Current_Result);

         if Listener.Current_Result = null then
            Add_Child (Listener.Main_Result, R);
         else
            Add_Child (Listener.Current_Result.all, R);
         end if;
         Listener.Current_Result := R;
      elsif Listener.Capture_Output then
         -- A test routine? Let's create a temporary file
         -- and direct Ada.Text_IO output there (if requested).
         Temporary_Output.Create_Temp (Listener.Output_File);
         Temporary_Output.Redirect_Output (Listener.Output_File);
      end if;
   end Start_Test;

   procedure End_Test (Listener : in out Basic_Listener;
                       Info  : Result_Info) is
      procedure Add_Result (Collection : in out Result_Collection);

      procedure Add_Result (Collection : in out Result_Collection) is
         My_Info : Result_Info := Info;
      begin
         -- It is possible that only Start_Test and End_Test
         -- are called (e.g. for Test_Suite), so the latest
         -- test result can be unset (set to NO_RESULT)
         --
         -- In that case, we simply jump to parent collection.
         -- Otherwise, we record the result.
         if Listener.Last_Test_Result /= NO_RESULT then
            if Listener.Capture_Output then
               -- End of the test routine, so we can restore
               -- the normal output now and close the temporary file.
               Temporary_Output.Restore_Output;
               Temporary_Output.Close_Temp (Listener.Output_File);

               -- Saving the name of the temporary file to the test result,
               -- so the file can be deleted later
               Set_Output_File
                 (My_Info, Temporary_Output.Get_Name (Listener.Output_File));
            end if;

            Set_Message (My_Info, Listener.Last_Test_Message);
            Set_Long_Message (My_Info, Listener.Last_Test_Long_Message);
            case Listener.Last_Test_Result is
               when PASS_RESULT =>
                  Add_Pass (Collection, My_Info);
               when FAILURE_RESULT =>
                  Add_Failure (Collection, My_Info);
               when ERROR_RESULT | NO_RESULT =>
                  Add_Error (Collection, My_Info);
            end case;
            Listener.Last_Test_Result := NO_RESULT;
         else
            Listener.Current_Result :=
              Get_Parent (Listener.Current_Result.all);
         end if;
      end Add_Result;
   begin
      if Listener.Current_Result /= null then
         Add_Result (Listener.Current_Result.all);
      else
         Add_Result (Listener.Main_Result);
      end if;
   end End_Test;

   procedure Set_Output_Capture (Listener : in out Basic_Listener;
                                 Capture  : Boolean) is
   begin
      Listener.Capture_Output := Capture;
   end Set_Output_Capture;

   function Get_Output_Capture (Listener : Basic_Listener)
     return Boolean is
   begin
      return Listener.Capture_Output;
   end Get_Output_Capture;

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

   procedure Finalize (Listener : in out Basic_Listener) is
   begin
      Remove_Files (Listener.Main_Result);
      Results.Release (Listener.Main_Result);
   end Finalize;
end Ahven.Listeners.Basic;
