--
-- Copyright (c) 2007-2009 Tero Koskinen <tero.koskinen@iki.fi>
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

   procedure Init_Collection (Collection : in out Result_Collection)
   is
   begin
      Init_List (Collection.Children);
      Collection.Passes := Result_Info_List.Empty_List;
      Collection.Failures := Result_Info_List.Empty_List;
      Collection.Errors := Result_Info_List.Empty_List;
      Collection.Parent := null;
      Collection.Test_Name := Empty_VString;
   end Init_Collection;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child      :        Result_Collection_Access) is
   begin
      Append (Collection.Children, (Ptr => Child));
   end Add_Child;

   procedure Add_Error (Collection : in out Result_Collection;
                        Info       :        Result_Info) is
   begin
      Append (Collection.Errors, Info);
   end Add_Error;

   procedure Add_Failure (Collection : in out Result_Collection;
                          Info       :        Result_Info) is
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

      Position: Result_List.Cursor := First (Collection.Children);
      Ptr  : Result_Collection_Access := null;
   begin
      loop
         exit when not Is_Valid (Position);

         Ptr := Data (Position).Ptr;
         Release (Ptr.all);
         Free (Ptr);

         Position:= Next (Position);
      end loop;
      Clear (Collection.Children);

      -- No need to call Free for these three since
      -- they are stored as plain objects instead of pointers.
      Clear (Collection.Errors);
      Clear (Collection.Failures);
      Clear (Collection.Passes);
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
      Position : Result_List.Cursor := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Position);

         Count := Count + Test_Count (Data (Position).Ptr.all);
         Position:= Next (Position);
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
      Count    : Natural            := Length (Collection.Passes);
      Position : Result_List.Cursor := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Position);

         Count := Count + Pass_Count (Data (Position).Ptr.all);
         Position:= Next (Position);
      end loop;
      return Count;
   end Pass_Count;

   function Error_Count (Collection : Result_Collection) return Natural is
      Count    : Natural            := Length (Collection.Errors);
      Position : Result_List.Cursor := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Position);

         Count := Count + Error_Count (Data (Position).Ptr.all);
         Position:= Next (Position);
      end loop;
      return Count;
   end Error_Count;

   function Failure_Count (Collection : Result_Collection) return Natural is
      Count    : Natural            := Length (Collection.Failures);
      Position : Result_List.Cursor := First (Collection.Children);
   begin
      loop
         exit when not Is_Valid (Position);

         Count := Count + Failure_Count (Data (Position).Ptr.all);
         Position:= Next (Position);
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
      Position      : Result_Info_List.Cursor;
      Total_Time : Duration := 0.0;
      Child_Position: Result_List.Cursor;
   begin
      Position:= First (Collection.Passes);
      Pass_Loop:
      loop
         exit Pass_Loop when not Is_Valid (Position);
         Total_Time := Total_Time + Get_Execution_Time (Data (Position));
         Position:= Next (Position);
      end loop Pass_Loop;

      Position:= First (Collection.Failures);
      Failure_Loop:
      loop
         exit Failure_Loop when not Is_Valid (Position);
         Total_Time := Total_Time + Get_Execution_Time (Data (Position));
         Position:= Next (Position);
      end loop Failure_Loop;

      Position:= First (Collection.Errors);
      Error_Loop:
      loop
         exit Error_Loop when not Is_Valid (Position);
         Total_Time := Total_Time + Get_Execution_Time (Data (Position));
         Position:= Next (Position);
      end loop Error_Loop;

      Child_Loop:
      loop
         exit Child_Loop when not Result_List.Is_Valid (Child_Position);
         Total_Time := Total_Time +
                       Get_Execution_Time
                         (Result_List.Data (Child_Position).Ptr.all);
         Child_Position:= Result_List.Next (Child_Position);
      end loop Child_Loop;

      return Total_Time;
   end Get_Execution_Time;

   function First_Pass (Collection : Result_Collection)
     return Result_Info_Cursor is
   begin
      return First (Collection.Passes);
   end First_Pass;

   function First_Failure (Collection : Result_Collection)
     return Result_Info_Cursor is
   begin
      return First (Collection.Failures);
   end First_Failure;

   function First_Error (Collection : Result_Collection)
     return Result_Info_Cursor is
   begin
      return First (Collection.Errors);
   end First_Error;

   function Next (Position: Result_Info_Cursor) return Result_Info_Cursor is
   begin
      return Result_Info_Cursor
        (Result_Info_List.Next (Result_Info_List.Cursor (Position)));
   end Next;

   function Data (Position: Result_Info_Cursor) return Result_Info is
   begin
      return Result_Info_List.Data (Result_Info_List.Cursor (Position));
   end Data;

   function Is_Valid (Position: Result_Info_Cursor) return Boolean is
   begin
      return Result_Info_List.Is_Valid (Result_Info_List.Cursor (Position));
   end Is_Valid;

   function First_Child (Collection : in Result_Collection)
     return Result_Collection_Cursor is
   begin
      return First (Collection.Children);
   end First_Child;

   function Next (Position: Result_Collection_Cursor)
     return Result_Collection_Cursor is
   begin
      return Result_Collection_Cursor
        (Result_List.Next (Result_List.Cursor (Position)));
   end Next;

   function Is_Valid (Position: Result_Collection_Cursor) return Boolean is
   begin
      return Result_List.Is_Valid (Result_List.Cursor (Position));
   end Is_Valid;

   function Data (Position: Result_Collection_Cursor)
     return Result_Collection_Access is
   begin
      return Result_List.Data (Result_List.Cursor (Position)).Ptr;
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
         Position   : Result_List.Cursor := Result_List.First (Coll.Children);
      begin
         loop
            exit when not Is_Valid (Position);
            Current := Child_Depth_Impl (Data (Position).Ptr.all, Level + 1);
            if Max < Current then
               Max := Current;
            end if;
            Position:= Result_List.Next (Position);
         end loop;
         return Level + Max;
      end Child_Depth_Impl;
   begin
      return Child_Depth_Impl (Collection, 0);
   end Child_Depth;

   package body Result_List is
      procedure Init_List (Target : in out List) is
      begin
         Target.First := null;
         Target.Last := null;
         Target.Size := 0;
      end Init_List;

      procedure Remove (Ptr : Node_Access) is
         procedure Free is
           new Ada.Unchecked_Deallocation (Object => Node,
                                           Name   => Node_Access);
         My_Ptr : Node_Access := Ptr;
      begin
         Ptr.Next := null;
         Free (My_Ptr);
      end Remove;

      procedure Append (Target    : in out List;
                        Node_Data :        Result_Collection_Wrapper) is
         New_Node : Node_Access  := null;
      begin
         if Target.Size = Count_Type'Last then
            raise List_Full;
         end if;

         New_Node := new Node'(Data => Node_Data, Next => null);

         if Target.Last = null then
            Target.First := New_Node;
         else
            Target.Last.Next := New_Node;
         end if;
         Target.Last := New_Node;

         Target.Size := Target.Size + 1;
      end Append;

      procedure Clear (Target : in out List) is
         Current_Node : Node_Access := Target.First;
         Next_Node : Node_Access := null;
      begin
         while Current_Node /= null loop
            Next_Node := Current_Node.Next;
            Remove (Current_Node);
            Current_Node := Next_Node;
         end loop;

         Target.First := null;
         Target.Last := null;
         Target.Size := 0;
      end Clear;

      function First (Target : List) return Cursor is
      begin
         return Cursor (Target.First);
      end First;

      function Next (Position : Cursor) return Cursor is
      begin
         if Position = null then
            raise Invalid_Cursor;
         end if;
         return Cursor (Position.Next);
      end Next;

      function Data (Position : Cursor) return Result_Collection_Wrapper is
      begin
         if Position = null then
            raise Invalid_Cursor;
         end if;

         return Position.Data;
      end Data;

      function Is_Valid (Position : Cursor) return Boolean is
      begin
         return Position /= null;
      end Is_Valid;

      function Length (Target : List) return Count_Type is
      begin
         return Target.Size;
      end Length;
   end Result_List;

end Ahven.Results;
