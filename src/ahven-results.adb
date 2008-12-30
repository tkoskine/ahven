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

   -- Bunch of setters and getters.
   -- The implementation is straightforward.
   procedure Set_Test_Name (Info : in out Result_Info;
                            Name :        VString) is
   begin
      Info.Test_Name := Name;
   end Set_Test_Name;

   procedure Set_Routine_Name (Info : in out Result_Info;
                               Name :        VString) is
   begin
      Info.Routine_Name := Name;
   end Set_Routine_Name;

   procedure Set_Message (Info    : in out Result_Info;
                          Message :        VString) is
   begin
      Info.Message := Message;
   end Set_Message;

   procedure Set_Test_Name (Info : in out Result_Info; Name : String) is
   begin
      Set_Test_Name (Info, +Name);
   end Set_Test_Name;

   procedure Set_Routine_Name (Info : in out Result_Info; Name : String) is
   begin
      Set_Routine_Name (Info, +Name);
   end Set_Routine_Name;

   procedure Set_Message (Info : in out Result_Info; Message : String) is
   begin
      Set_Message (Info, +Message);
   end Set_Message;

   procedure Set_Long_Message (Info : in out Result_Info;
                               Message : VString) is
   begin
      Info.Long_Message := Message;
   end Set_Long_Message;

   procedure Set_Long_Message (Info : in out Result_Info; Message : String) is
   begin
      Set_Long_Message (Info, +Message);
   end Set_Long_Message;

   procedure Set_Execution_Time (Info         : in out Result_Info;
                                 Elapsed_Time :        Duration) is
   begin
      Info.Execution_Time := Elapsed_Time;
   end Set_Execution_Time;

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        VString) is
   begin
      Info.Output_File := Filename;
   end Set_Output_File;

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        String) is
   begin
      Set_Output_File (Info, +Filename);
   end Set_Output_File;

   function Get_Test_Name (Info : Result_Info) return String is
   begin
      return To_String (Info.Test_Name);
   end Get_Test_Name;

   function Get_Routine_Name (Info : Result_Info) return String is
   begin
      return To_String (Info.Routine_Name);
   end Get_Routine_Name;

   function Get_Message (Info : Result_Info) return String is
   begin
      return To_String (Info.Message);
   end Get_Message;

   function Get_Long_Message (Info : Result_Info) return String is
   begin
      return To_String (Info.Long_Message);
   end Get_Long_Message;

   function Get_Execution_Time (Info : Result_Info) return Duration is
   begin
      return Info.Execution_Time;
   end Get_Execution_Time;

   function Get_Output_File (Info : Result_Info) return VString is
   begin
      return Info.Output_File;
   end Get_Output_File;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child : Result_Collection_Access) is
   begin
      Append (Collection.Children, Result_Collection_Wrapper'(Ptr => Child));
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

   -- When Result_Collection is released, it recursively releases
   -- its all children.
   procedure Release (Collection : in out Result_Collection) is
      procedure Free is
        new Ada.Unchecked_Deallocation (Object => Result_Collection,
                                        Name   => Result_Collection_Access);

      Iter : Result_List.Iterator := First (Collection.Children);
      Ptr  : Result_Collection_Access := null;
   begin
      loop
         exit when not Is_Valid (Iter);

         Ptr := Data (Iter).Ptr;
         Release (Ptr.all);
         Free (Ptr);

         Iter := Next (Iter);
      end loop;
      Remove_All (Collection.Children);

      -- No need to call Free for these three since
      -- they are stored as plain objects instead of pointers.
      Remove_All (Collection.Errors);
      Remove_All (Collection.Failures);
      Remove_All (Collection.Passes);
   end Release;

   procedure Set_Name (Collection : in out Result_Collection;
                       Name : VString) is
   begin
      Collection.Test_Name := Name;
   end Set_Name;

   procedure Set_Parent (Collection : in out Result_Collection;
                         Parent     :        Result_Collection_Access) is
   begin
      Collection.Parent := Parent;
   end Set_Parent;

   function Test_Count (Collection : Result_Collection) return Natural is
      Count : Natural := Result_Info_List.Length (Collection.Errors) +
                         Result_Info_List.Length (Collection.Failures) +
                         Result_Info_List.Length (Collection.Passes);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Iter);

         Count := Count + Test_Count (Data (Iter).Ptr.all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Test_Count;

   function Direct_Test_Count (Collection : Result_Collection)
     return Natural
   is
   begin
      return Length (Collection.Errors) +
             Length (Collection.Failures) +
             Length (Collection.Passes);
   end Direct_Test_Count;

   function Pass_Count (Collection : Result_Collection) return Natural is
      Count : Natural              := Length (Collection.Passes);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Iter);

         Count := Count + Pass_Count (Data (Iter).Ptr.all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Pass_Count;

   function Error_Count (Collection : Result_Collection) return Natural is
      Count : Natural              := Length (Collection.Errors);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Iter);

         Count := Count + Error_Count (Data (Iter).Ptr.all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Error_Count;

   function Failure_Count (Collection : Result_Collection) return Natural is
      Count : Natural              := Length (Collection.Failures);
      Iter  : Result_List.Iterator := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Iter);

         Count := Count + Failure_Count (Data (Iter).Ptr.all);
         Iter := Next (Iter);
      end loop;
      return Count;
   end Failure_Count;

   function Get_Test_Name (Collection : Result_Collection)
     return VString is
   begin
      return Collection.Test_Name;
   end Get_Test_Name;

   function Get_Parent (Collection : Result_Collection)
     return Result_Collection_Access is
   begin
      return Collection.Parent;
   end Get_Parent;

   function Get_Execution_Time (Collection : Result_Collection)
     return Duration
   is
      Iter       : Result_Info_List.Iterator;
      Total_Time : Duration := 0.0;
      Child_Iter : Result_List.Iterator;
   begin
      Iter := First (Collection.Passes);
      Pass_Loop:
      loop
         exit Pass_Loop when not Is_Valid (Iter);
         Total_Time := Total_Time + Get_Execution_Time (Data (Iter));
         Iter := Next (Iter);
      end loop Pass_Loop;

      Iter := First (Collection.Failures);
      Failure_Loop:
      loop
         exit Failure_Loop when not Is_Valid (Iter);
         Total_Time := Total_Time + Get_Execution_Time (Data (Iter));
         Iter := Next (Iter);
      end loop Failure_Loop;

      Iter := First (Collection.Errors);
      Error_Loop:
      loop
         exit Error_Loop when not Is_Valid (Iter);
         Total_Time := Total_Time + Get_Execution_Time (Data (Iter));
         Iter := Next (Iter);
      end loop Error_Loop;

      Child_Loop:
      loop
         exit Child_Loop when not Result_List.Is_Valid (Child_Iter);
         Total_Time := Total_Time +
                       Get_Execution_Time
                         (Result_List.Data (Child_Iter).Ptr.all);
         Child_Iter := Result_List.Next (Child_Iter);
      end loop Child_Loop;

      return Total_Time;
   end Get_Execution_Time;

   function First_Pass (Collection : Result_Collection)
     return Result_Info_Iterator is
   begin
      return First (Collection.Passes);
   end First_Pass;

   function First_Failure (Collection : Result_Collection)
     return Result_Info_Iterator is
   begin
      return First (Collection.Failures);
   end First_Failure;

   function First_Error (Collection : Result_Collection)
     return Result_Info_Iterator is
   begin
      return First (Collection.Errors);
   end First_Error;

   function Next (Iter : Result_Info_Iterator) return Result_Info_Iterator is
   begin
      return Result_Info_Iterator
        (Result_Info_List.Next (Result_Info_List.Iterator (Iter)));
   end Next;

   function Data (Iter : Result_Info_Iterator) return Result_Info is
   begin
      return Result_Info_List.Data (Result_Info_List.Iterator (Iter));
   end Data;

   function Is_Valid (Iter : Result_Info_Iterator) return Boolean is
   begin
      return Result_Info_List.Is_Valid (Result_Info_List.Iterator (Iter));
   end Is_Valid;

   function First_Child (Collection : in Result_Collection)
     return Result_Collection_Iterator is
   begin
      return First (Collection.Children);
   end First_Child;

   function Next (Iter : Result_Collection_Iterator)
     return Result_Collection_Iterator is
   begin
      return Result_Collection_Iterator
        (Result_List.Next (Result_List.Iterator (Iter)));
   end Next;

   function Is_Valid (Iter : Result_Collection_Iterator) return Boolean is
   begin
      return Result_List.Is_Valid (Result_List.Iterator (Iter));
   end Is_Valid;

   function Data (Iter : Result_Collection_Iterator)
     return Result_Collection_Access is
   begin
      return Result_List.Data (Result_List.Iterator (Iter)).Ptr;
   end Data;

   function Child_Depth (Collection : Result_Collection) return Natural
   is
      function Child_Depth_Impl (Coll  : Result_Collection;
                                 Level : Natural) return Natural;

      function Child_Depth_Impl (Coll  : Result_Collection;
                                 Level : Natural)
        return Natural
      is
         Max     : Natural := 0;
         Current : Natural := 0;
         Iter    : Result_List.Iterator := Result_List.First (Coll.Children);
      begin
         loop
            exit when not Is_Valid (Iter);
            Current := Child_Depth_Impl (Data (Iter).Ptr.all, Level + 1);
            if Max < Current then
               Max := Current;
            end if;
            Iter := Result_List.Next (Iter);
         end loop;
         return Level + Max;
      end Child_Depth_Impl;
   begin
      return Child_Depth_Impl (Collection, 0);
   end Child_Depth;

end Ahven.Results;
