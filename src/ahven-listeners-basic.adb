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

package body Ahven.Listeners.Basic is

   function Create return Result_Listener_Class_Access is
   begin
      return new Basic_Listener;
   end Create;

   procedure Add_Pass (Listener : in out Basic_Listener;
                       Info  : Result_Info) is
   begin
      Listener.Last_Test_Result := PASS_RESULT;
      Listener.Last_Test_Message := Message (Info);
      Listener.Last_Test_Long_Message := Long_Message (Info);
   end Add_Pass;

   procedure Add_Failure (Listener : in out Basic_Listener;
                          Info  : Result_Info) is
   begin
      Listener.Last_Test_Result := FAILURE_RESULT;
      Listener.Last_Test_Message := Message (Info);
      Listener.Last_Test_Long_Message := Long_Message (Info);
   end Add_Failure;

   procedure Add_Error (Listener : in out Basic_Listener;
                        Info  : Result_Info) is
   begin
      Listener.Last_Test_Result := ERROR_RESULT;
      Listener.Last_Test_Message := Message (Info);
      Listener.Last_Test_Long_Message := Long_Message (Info);
   end Add_Error;

   procedure Start_Test (Listener : in out Basic_Listener;
                         Info  : Result_Info) is
      R : Result_Collection_Access := null;
   begin
      if Routine_Name (Info) = Null_Unbounded_String then
         R := new Result_Collection;
         Set_Name (R.all, Test_Name (Info));
         Set_Parent (R.all, Listener.Current_Result);

         if Listener.Current_Result = null then
            Add_Child (Listener.Main_Result, R);
         else
            Add_Child (Listener.Current_Result.all, R);
         end if;
         Listener.Current_Result := R;
      end if;
   end Start_Test;

   procedure End_Test (Listener : in out Basic_Listener;
                       Info  : Result_Info) is
      My_Info  : Result_Info := Info;
   begin
      if Listener.Current_Result /= null then
         if Listener.Last_Test_Result /= NO_RESULT then
            Set_Message (My_Info, Listener.Last_Test_Message);
            Set_Long_Message (My_Info, Listener.Last_Test_Long_Message);
            case Listener.Last_Test_Result is
               when PASS_RESULT =>
                  Add_Pass (Listener.Current_Result.all, My_Info);
               when FAILURE_RESULT =>
                  Add_Failure (Listener.Current_Result.all, My_Info);
               when ERROR_RESULT | NO_RESULT =>
                  Add_Error (Listener.Current_Result.all, My_Info);
            end case;
            Listener.Last_Test_Result := NO_RESULT;
         end if;

         if Routine_Name (Info) = Null_Unbounded_String then
            Listener.Current_Result := Parent (Listener.Current_Result.all);
         end if;
      end if;
   end End_Test;

end Ahven.Listeners.Basic;
