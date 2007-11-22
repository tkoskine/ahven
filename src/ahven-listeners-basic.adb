--
-- Copyright (c) 2007 Tero Koskinen <tero.koskinen@iki.fi>
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

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Place : Result_Info) is
   begin
      Listener.Last_Test_Result := PASS_RESULT;
      Listener.Last_Test_Message := Message (Place);
   end Add_Pass;

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Place : Result_Info) is
   begin
      Listener.Last_Test_Result := FAILURE_RESULT;
      Listener.Last_Test_Message := Message (Place);
   end Add_Failure;

   procedure Add_Error (Listener : in out Basic_Listener;
                        Place : Result_Info) is
   begin
      Listener.Last_Test_Result := ERROR_RESULT;
      Listener.Last_Test_Message := Message (Place);
   end Add_Error;

   procedure Start_Test (Listener : in out Basic_Listener;
                         Place : Result_Info) is
      R : Result_Collection_Access := null;
   begin
      if Routine_Name (Place) = Null_Unbounded_String then
         R := new Result_Collection;
         Set_Name (R.all, Test_Name (Place));
         Set_Parent (R.all, Listener.Current_Result);

         if Listener.Current_Result = null then
            Add_Child (Listener.Main_Result, R);
         else
            Add_Child (Listener.Current_Result.all, R);
         end if;
         Listener.Current_Result := R;
      else
         Temporary_Output.Create_Temp (Listener.Output_File);
         Temporary_Output.Redirect_Output (Listener.Output_File);
      end if;
   end Start_Test;

   procedure End_Test (Listener : in out Basic_Listener;
                       Place : Result_Info) is
      My_Place : Result_Info := Place;
   begin
      if Listener.Current_Result /= null then
         if Listener.Last_Test_Result /= NO_RESULT then
            Set_Message (My_Place, Listener.Last_Test_Message);
            Temporary_Output.Restore_Output;
            Temporary_Output.Close_Temp (Listener.Output_File);
            Set_Output_File (My_Place, Listener.Output_File.Name);

            case Listener.Last_Test_Result is
               when PASS_RESULT =>
                  Add_Pass (Listener.Current_Result.all, My_Place);
               when FAILURE_RESULT =>
                  Add_Failure (Listener.Current_Result.all, My_Place);
               when ERROR_RESULT | NO_RESULT =>
                  Add_Error (Listener.Current_Result.all, My_Place);
            end case;
            Listener.Last_Test_Result := NO_RESULT;
         end if;

         if Routine_Name (Place) = Null_Unbounded_String then
            Listener.Current_Result := Parent (Listener.Current_Result.all);
         end if;
      end if;
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
      Collection_End : Boolean := False;
      Info : Result_Info;
      Child : Result_Collection_Access := null;
   begin
      Pass_File_Loop:
      loop
         Next_Pass (Collection, Info, Collection_End);
         exit Pass_File_Loop when Collection_End;
         If Length (Output_File (Info)) > 0 then
            Remove_File (To_String (Output_File (Info)));
         end if;
      end loop Pass_File_Loop;

      Failure_File_Loop:
      loop
         Next_Failure (Collection, Info, Collection_End);
         exit Failure_File_Loop when Collection_End;
         If Length (Output_File (Info)) > 0 then
            Remove_File (To_String (Output_File (Info)));
         end if;
      end loop Failure_File_Loop;

      Error_File_Loop:
      loop
         Next_Error (Collection, Info, Collection_End);
         exit Error_File_Loop when Collection_End;
         If Length (Output_File (Info)) > 0 then
            Remove_File (To_String (Output_File (Info)));
         end if;
      end loop Error_File_Loop;

      Child_Loop:
      loop
         Next_Child (Collection, Child, Collection_End);
         exit Child_Loop when Collection_End;
         Remove_Files (Child.all);
      end loop Child_Loop;
   end Remove_Files;

   procedure Finalize (Listener : in out Basic_Listener) is
   begin
      Remove_Files (Listener.Main_Result);
   end Finalize;

end Ahven.Listeners.Basic;
