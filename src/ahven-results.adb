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

with Ada.Unchecked_Deallocation;

package body Ahven.Results is
   use Ahven.Results.Result_List;
   use Ahven.Results.Result_Place_List;

   procedure Set_Test_Name (Place : in out Result_Place;
                            Name : Unbounded_String) is
   begin
      Place.Test_Name := Name;
   end Set_Test_Name;

   procedure Set_Routine_Name (Place : in out Result_Place;
                               Name : Unbounded_String) is
   begin
      Place.Routine_Name := Name;
   end Set_Routine_Name;

   procedure Set_Test_Name (Place : in out Result_Place; Name : String) is
   begin
      Set_Test_Name (Place, To_Unbounded_String (Name));
   end Set_Test_Name;

   procedure Set_Routine_Name (Place : in out Result_Place; Name : String) is
   begin
      Set_Routine_Name (Place, To_Unbounded_String (Name));
   end Set_Routine_Name;

   function Test_Name (Place : Result_Place) return Unbounded_String is
   begin
      return Place.Test_Name;
   end Test_Name;

   function Routine_Name (Place : Result_Place) return Unbounded_String is
   begin
      return Place.Routine_Name;
   end Routine_Name;

   procedure Add_Child (Res : in out Result; Child : Result_Access) is
   begin
      Append (Res.Children, Child);
   end Add_Child;

   procedure Add_Error (Res : in out Result; Place : Result_Place) is
   begin
      Append (Res.Errors, Place);
   end Add_Error;

   procedure Add_Failure (Res : in out Result; Place : Result_Place) is
   begin
      Append (Res.Failures, Place);
   end Add_Failure;

   procedure Add_Pass (Res : in out Result; Place : Result_Place) is
   begin
      Append (Res.Passes, Place);
   end Add_Pass;

   procedure Finalize (Res : in out Result) is
      use Result_List;

      procedure Free is new Ada.Unchecked_Deallocation (Result, Result_Access);

      Iter : Result_List.Iterator := First (Res.Children);
      Ptr  : Result_Access := null;
   begin
      loop
         exit when Iter = null;

         Ptr := Data (Iter);
         Free (Ptr);

         Iter := Next (Iter);
      end loop;
      Remove_All (Res.Children);
      Remove_All (Res.Errors);
      Remove_All (Res.Failures);
      Remove_All (Res.Passes);
   end Finalize;

   procedure Set_Name (Res : in out Result; Name : Unbounded_String) is
   begin
      Res.Test_Name := Name;
   end Set_Name;

   procedure Set_Parent (Res : in out Result; Parent : Result_Access) is
   begin
      Res.Parent := Parent;
   end Set_Parent;

   function Test_Count (Res : Result) return Natural is
      Count : Natural := Size (Res.Errors) +
                         Size (Res.Failures) +
                         Size (Res.Passes);
      Iter : Result_List.Iterator := First (Res.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Test_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Test_Count;

   function Pass_Count (Res : Result) return Natural is
      Count : Natural              := Size (Res.Passes);
      Iter  : Result_List.Iterator := First (Res.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Pass_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Pass_Count;

   function Error_Count (Res : Result) return Natural is
      Count : Natural              := Size (Res.Errors);
      Iter  : Result_List.Iterator := First (Res.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Error_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Error_Count;

   function Failure_Count (Res : Result) return Natural is
      Count : Natural              := Size (Res.Failures);
      Iter  : Result_List.Iterator := First (Res.Children);
   begin
      loop
         exit when Iter = null;

         Count := Count + Failure_Count (Data (Iter).all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Failure_Count;

   function Test_Name (Res : Result) return Unbounded_String is
   begin
      return Res.Test_Name;
   end Test_Name;

   function Parent (Res : Result) return Result_Access is
   begin
      return Res.Parent;
   end Parent;

   procedure Next_In_List (List : in out Result_Place_List.List;
                           Iter : in out Result_Place_List.Iterator;
                           Place : out Result_Place;
                           End_Of_List : out Boolean) is
   begin
      if Iter = null then
         Iter := First (List);
      else
         Iter := Next (Iter);
      end if;

      if Iter = null then
         End_Of_List := True;
         Place := (Null_Unbounded_String, Null_Unbounded_String);
      else
         End_of_List := False;
         Place := Data (Iter);
      end if;
   end Next_In_List;

   procedure Next_Error (Res : in out Result;
                         Place : out Result_Place;
                         End_Of_Errors : out Boolean) is
   begin
      Next_In_list (Res.Errors, Res.Error_Iter, Place, End_Of_Errors);
   end Next_Error;

   procedure Next_Failure (Res : in out Result;
                           Place : out Result_Place;
                           End_Of_Failures : out Boolean) is
   begin
      Next_In_list (Res.Failures, Res.Failure_Iter, Place, End_Of_Failures);
   end Next_Failure;

   procedure Next_Pass (Res : in out Result;
                        Place : out Result_Place;
                        End_Of_Passes : out Boolean) is
   begin
      Next_In_list (Res.Passes, Res.Pass_Iter, Place, End_Of_Passes);
   end Next_Pass;

   procedure Next_Child (Res : in out Result;
                         Child : out Result_Access;
                         End_Of_Children : out Boolean) is
   begin
      if Res.Child_Iter = null then
         Res.Child_Iter := First (Res.Children);
      else
         Res.Child_Iter := Next (Res.Child_Iter);
      end if;
      if Res.Child_Iter = null then
         End_Of_Children := True;
         Child := null;
      else
         End_of_Children := False;
         Child := Data (Res.Child_Iter);
      end if;
   end Next_Child;

end Ahven.Results;
