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

with Ada.Unchecked_Deallocation;

package body Ahven.Results is
   use Ahven.Results.Result_List;
   use Ahven.Results.Result_Info_List;

   -- Local procedures
   procedure Next_In_List (List : in out Result_Info_List.List;
                           Iter : in out Result_Info_List.Iterator;
                           Info : out Result_Info;
                           End_Of_List : out Boolean);

   -- Bunch of setters and getters.
   -- The implementation is straightforward.
   procedure Set_Test_Name (Info : in out Result_Info;
                            Name : Unbounded_String) is
   begin
      Info.Test_Name := Name;
   end Set_Test_Name;

   procedure Set_Routine_Name (Info : in out Result_Info;
                               Name : Unbounded_String) is
   begin
      Info.Routine_Name := Name;
   end Set_Routine_Name;

   procedure Set_Message (Info : in out Result_Info;
                          Message : Unbounded_String) is
   begin
      Info.Message := Message;
   end Set_Message;

   procedure Set_Test_Name (Info : in out Result_Info; Name : String) is
   begin
      Set_Test_Name (Info, To_Unbounded_String (Name));
   end Set_Test_Name;

   procedure Set_Routine_Name (Info : in out Result_Info; Name : String) is
   begin
      Set_Routine_Name (Info, To_Unbounded_String (Name));
   end Set_Routine_Name;

   procedure Set_Message (Info : in out Result_Info; Message : String) is
   begin
      Set_Message (Info, To_Unbounded_String (Message));
   end Set_Message;

   procedure Set_Execution_Time (Info : in out Result_Info;
                                 Elapsed_Time : Duration) is
   begin
      Info.Execution_Time := Elapsed_Time;
   end Set_Execution_Time;

   procedure Set_Output_File (Info : in out Result_Info;
                              Filename : Unbounded_String) is
   begin
      Info.Output_File := Filename;
   end Set_Output_File;

   function Test_Name (Info : Result_Info) return Unbounded_String is
   begin
      return Info.Test_Name;
   end Test_Name;

   function Routine_Name (Info : Result_Info) return Unbounded_String is
   begin
      return Info.Routine_Name;
   end Routine_Name;

   function Message (Info : Result_Info) return Unbounded_String is
   begin
      return Info.Message;
   end Message;

   function Execution_Time (Info : Result_Info) return Duration is
   begin
      return Info.Execution_Time;
   end Execution_Time;

   function Output_File (Info : Result_Info) return Unbounded_String is
   begin
      return Info.Output_File;
   end Output_File;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child : Result_Collection_Access) is
   begin
      Append (Collection.Children, Child);
   end Add_Child;

   procedure Add_Error (Collection : in out Result_Collection;
                        Info : Result_Info) is
   begin
      Append (Collection.Errors, Info);
   end Add_Error;

   procedure Add_Failure (Collection : in out Result_Collection;
                          Info : Result_Info) is
   begin
      Append (Collection.Failures, Info);
   end Add_Failure;

   procedure Add_Pass (Collection : in out Result_Collection;
                       Info : Result_Info) is
   begin
      Append (Collection.Passes, Info);
   end Add_Pass;

   -- When Result_Collection is finalized, it recursively releases
   -- its all children.
   procedure Finalize (Collection : in out Result_Collection) is
      procedure Free is
        new Ada.Unchecked_Deallocation
          (Result_Collection, Result_Collection_Access);

      Iter : Result_List.Iterator := First (Collection.Children);
      Ptr  : Result_Collection_Access := null;
   begin
      loop
         exit when Iter = null;

         Ptr := Data (Iter);
         Free (Ptr);

         Iter := Next (Iter);
      end loop;
      Remove_All (Collection.Children);

      -- No need to call Free for these three since
      -- they are stored as plain objects instead of pointers.
      Remove_All (Collection.Errors);
      Remove_All (Collection.Failures);
      Remove_All (Collection.Passes);
   end Finalize;

   procedure Set_Name (Collection : in out Result_Collection;
                       Name : Unbounded_String) is
   begin
      Collection.Test_Name := Name;
   end Set_Name;

   procedure Set_Parent (Collection : in out Result_Collection;
                         Parent : Result_Collection_Access) is
   begin
      Collection.Parent := Parent;
   end Set_Parent;

   function Test_Count (Collection : Result_Collection) return Natural is
      Count : Natural := Size (Collection.Errors) +
                         Size (Collection.Failures) +
                         Size (Collection.Passes);
      Iter : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Test_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Test_Count;

   function Pass_Count (Collection : Result_Collection) return Natural is
      Count : Natural              := Size (Collection.Passes);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Pass_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Pass_Count;

   function Error_Count (Collection : Result_Collection) return Natural is
      Count : Natural              := Size (Collection.Errors);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Error_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Error_Count;

   function Failure_Count (Collection : Result_Collection) return Natural is
      Count : Natural              := Size (Collection.Failures);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Failure_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Failure_Count;

   function Test_Name (Collection : Result_Collection)
     return Unbounded_String is
   begin
      return Collection.Test_Name;
   end Test_Name;

   function Parent (Collection : Result_Collection)
     return Result_Collection_Access is
   begin
      return Collection.Parent;
   end Parent;

   procedure Next_In_List (List : in out Result_Info_List.List;
                           Iter : in out Result_Info_List.Iterator;
                           Info : out Result_Info;
                           End_Of_List : out Boolean) is
   begin
      if Iter = null then
         Iter := First (List);
      else
         Iter := Next (Iter);
      end if;

      if Iter = null then
         End_Of_List := True;
         Info := (Null_Unbounded_String,
                   Null_Unbounded_String,
                   Null_Unbounded_String,
                   0.0,
                   Null_Unbounded_String);
      else
         End_of_List := False;
         Info := Data (Iter);
      end if;
   end Next_In_List;

   procedure Next_Error (Collection : in out Result_Collection;
                         Info : out Result_Info;
                         End_Of_Errors : out Boolean) is
   begin
      Next_In_list (Collection.Errors,
                    Collection.Error_Iter,
                    Info,
                    End_Of_Errors);
   end Next_Error;

   procedure Next_Failure (Collection : in out Result_Collection;
                           Info : out Result_Info;
                           End_Of_Failures : out Boolean) is
   begin
      Next_In_list (Collection.Failures,
                    Collection.Failure_Iter,
                    Info,
                    End_Of_Failures);
   end Next_Failure;

   procedure Next_Pass (Collection : in out Result_Collection;
                        Info : out Result_Info;
                        End_Of_Passes : out Boolean) is
   begin
      Next_In_list (Collection.Passes,
                    Collection.Pass_Iter,
                    Info,
                    End_Of_Passes);
   end Next_Pass;

   procedure Next_Child (Collection : in out Result_Collection;
                         Child : out Result_Collection_Access;
                         End_Of_Children : out Boolean) is
   begin
      if Collection.Child_Iter = null then
         Collection.Child_Iter := First (Collection.Children);
      else
         Collection.Child_Iter := Next (Collection.Child_Iter);
      end if;
      if Collection.Child_Iter = null then
         End_Of_Children := True;
         Child := null;
      else
         End_of_Children := False;
         Child := Data (Collection.Child_Iter);
      end if;
   end Next_Child;

end Ahven.Results;
