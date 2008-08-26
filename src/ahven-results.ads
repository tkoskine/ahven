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

with Ada.Finalization;
with Ada.Strings.Unbounded;

use Ada.Strings.Unbounded;

-- Like the name implies, the Results package is used for
-- storing the test results.
--
-- Result_Info holds one invidual result and
-- Result_Collection holds multiple Result_Infos.
--
package Ahven.Results is
   type Result_Info is private;

   Empty_Result_Info : constant Result_Info;
   -- Result_Info object which holds no result. It can be used
   -- to initialize a new Result_Info object.

   procedure Set_Test_Name (Info : in out Result_Info;
                            Name : Unbounded_String);
   -- Set a test name for the result place.

   procedure Set_Routine_Name (Info : in out Result_Info;
                               Name : Unbounded_String);
   -- Set a routine name for the result place.

   procedure Set_Message (Info : in out Result_Info;
                          Message : Unbounded_String);
   -- Set a message for the result place.

   procedure Set_Test_Name (Info : in out Result_Info; Name : String);
   -- A helper function, which calls Set_Test_Name (.. ; Unbounded_String)

   procedure Set_Routine_Name (Info : in out Result_Info; Name : String);
   -- A helper function, which calls Set_Routine_Name (.. ; Unbounded_String)

   procedure Set_Message (Info : in out Result_Info; Message : String);
   -- A helper function, which calls Set_Message (.. ; Unbounded_String)

   procedure Set_Long_Message (Info : in out Result_Info;
                               Message : Unbounded_String);
   -- A helper function, which calls Set_Message (.. ; Unbounded_String)

   procedure Set_Long_Message (Info : in out Result_Info; Message : String);
   -- A helper function, which calls Set_Long_Message (.. ; Unbounded_String)

   procedure Set_Execution_Time (Info : in out Result_Info;
                                 Elapsed_Time : Duration);
   -- Set the execution time of the result info (test).

   procedure Set_Output_File (Info : in out Result_Info;
                              Filename : Unbounded_String);
   -- Set the name of the test output file.

   procedure Set_Output_File (Info : in out Result_Info;
                              Filename : String);
   -- Set the name of the test output file.

   function Get_Test_Name (Info : Result_Info) return Unbounded_String;
   -- Return the test name of the result info.

   function Get_Routine_Name (Info : Result_Info) return Unbounded_String;
   -- Return the routine name of the result info.

   function Get_Message (Info : Result_Info) return Unbounded_String;
   -- Return the message of the result info.

   function Get_Long_Message (Info : Result_Info) return Unbounded_String;
   -- Return the long message of the result info.

   function Get_Execution_Time (Info : Result_Info) return Duration;
   -- Return the execution time of the result info.

   function Get_Output_File (Info : Result_Info) return Unbounded_String;
   -- Return the name of the output file.
   -- Empty string is returned in case there is no output file.

   type Result_Collection is
     new Ada.Finalization.Limited_Controlled with private;
   -- A collection of Result_Info objects.
   -- Contains also child collections.

   type Result_Collection_Access is access Result_Collection;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child : Result_Collection_Access);
   -- Add a child collection to the collection.

   procedure Add_Error (Collection : in out Result_Collection;
                        Info : Result_Info);
   -- Add a test error to the collection.

   procedure Add_Failure (Collection : in out Result_Collection;
                          Info : Result_Info);
   -- Add a test failure to the collection.

   procedure Add_Pass (Collection : in out Result_Collection;
                       Info : Result_Info);
   -- Add a passed test to the collection

   procedure Finalize (Collection : in out Result_Collection);
   -- Finalize procedure for the collection. Frees also
   -- all children added via Add_Child.

   procedure Set_Name (Collection : in out Result_Collection;
                       Name : Unbounded_String);
   -- Set a test name for the collection.

   procedure Set_Parent (Collection: in out Result_Collection;
                         Parent : Result_Collection_Access);
   -- Set a parent collection to the collection.

   function Test_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of tests in the collection.
   -- Tests in child collections are included.

   function Direct_Test_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of tests in the collection.
   -- The tests in the child collections are NOT included.

   function Pass_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of passed tests in the collection.
   -- Tests in child collections are included.

   function Error_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of test errors in the collection.
   -- Tests in child collections are included.

   function Failure_Count (Collection : Result_Collection) return Natural;
   -- Return the amount of test errors in the collection.
   -- Tests in child collections are included.

   function Get_Test_Name (Collection : Result_Collection)
     return Unbounded_String;
   -- Return the name of the collection's test.

   function Get_Parent (Collection : Result_Collection)
     return Result_Collection_Access;
   -- Return the parent of the collection.

   function Get_Execution_Time (Collection : Result_Collection)
     return Duration;
   -- Return the execution time of the whole collection.

   type Result_Info_Iterator is private;
   -- An iterator type for Pass, Failure and Error results.

   function First_Pass (Collection : Result_Collection)
     return Result_Info_Iterator;
   -- Get the first pass from the collection.

   function First_Failure (Collection : Result_Collection)
     return Result_Info_Iterator;
   -- Get the first failure from the collection.

   function First_Error (Collection : Result_Collection)
     return Result_Info_Iterator;
   -- Get the first error from the collection.

   function Next (Iter : Result_Info_Iterator) return Result_Info_Iterator;
   -- Get the next pass/failure/error.

   function Data (Iter : Result_Info_Iterator) return Result_Info;
   -- Get the data behind the iterator.

   function Is_Valid (Iter : Result_Info_Iterator) return Boolean;
   -- Is the iterator still valid?

   type Result_Collection_Iterator is private;
   -- Iterator for iterating over a set of Result_Collection access objects.

   function First_Child (Collection : in Result_Collection)
     return Result_Collection_Iterator;
   -- Get the first child of the collection.

   function Next (Iter : Result_Collection_Iterator)
     return Result_Collection_Iterator;
   -- Get the next child.

   function Is_Valid (Iter : Result_Collection_Iterator) return Boolean;
   -- Is the iterator still valid?

   function Data (Iter : Result_Collection_Iterator)
     return Result_Collection_Access;
   -- Get the data (Result_Collection_Access) behind the iterator.

   function Child_Depth (Collection : in Result_Collection) return Natural;
   -- Return the maximum depth of children. (a child of a child, etc.)

private
   type Result_Info is record
      Test_Name      : Unbounded_String := Null_Unbounded_String;
      Output_File    : Unbounded_String := Null_Unbounded_String;
      Routine_Name   : Unbounded_String := Null_Unbounded_String;
      Execution_Time : Duration         := 0.0;
      Message        : Unbounded_String := Null_Unbounded_String;
      Long_Message   : Unbounded_String := Null_Unbounded_String;
   end record;

   Empty_Result_Info : constant Result_Info :=
     (Test_Name      => Null_Unbounded_String,
      Routine_Name   => Null_Unbounded_String,
      Message        => Null_Unbounded_String,
      Long_Message   => Null_Unbounded_String,
      Execution_Time => 0.0,
      Output_File    => Null_Unbounded_String);

   package Result_Info_List is
      type List is new Ada.Finalization.Controlled with private;
      type Iterator is private;
      Invalid_Iterator : exception;

      Empty_List : constant List;

      procedure Append (Target : in out List; Node_Data : Result_Info);
      -- Append an element at the end of the list.

      procedure Remove_All (Target : in out List);
      -- Remove all elements from the list.

      function Empty (Target : List) return Boolean;
      -- Is the list empty?

      function First (Target : List) return Iterator;
      -- Return an iterator to the first element of the list.

      function Last (Target : List) return Iterator;
      -- Return an iterator to the last element of the list.

      function Next (Iter : Iterator) return Iterator;
      -- Move the iterator to point to the next element on the list.

      function Prev (Iter : Iterator) return Iterator;
      -- Move the iterator to point to the previous element on the list.

      function Data (Iter : Iterator) return Result_Info;
      -- Return element pointed by the iterator.

      function Is_Valid (Iter : Iterator) return Boolean;

      function Size (Target : List) return Natural;
   private
      type Node;
      type Node_Access is access Node;
      type Iterator is new Node_Access;

      procedure Remove (Ptr : Node_Access);
      -- A procedure to release memory pointed by Ptr.

      type Node is record
         Next : Node_Access := null;
         Prev : Node_Access := null;
         Data : Result_Info;
      end record;

      type List is new Ada.Finalization.Controlled with record
         First : Node_Access := null;
         Last  : Node_Access := null;
         Size  : Natural := 0;
      end record;

      procedure Initialize (Target : in out List);
      procedure Finalize   (Target : in out List);
      procedure Adjust     (Target : in out List);

      Empty_List : constant List :=
        (Ada.Finalization.Controlled with First => null,
                                          Last  => null,
                                          Size  => 0);
   end Result_Info_List;

   package Result_List is
      type List is new Ada.Finalization.Controlled with private;
      type Iterator is private;
      Invalid_Iterator : exception;

      Empty_List : constant List;

      procedure Append (Target : in out List;
                        Node_Data : Result_Collection_Access);
      -- Append an element at the end of the list.

      procedure Remove_All (Target : in out List);
      -- Remove all elements from the list.

      function Empty (Target : List) return Boolean;
      -- Is the list empty?

      function First (Target : List) return Iterator;
      -- Return an iterator to the first element of the list.

      function Last (Target : List) return Iterator;
      -- Return an iterator to the last element of the list.

      function Next (Iter : Iterator) return Iterator;
      -- Move the iterator to point to the next element on the list.

      function Prev (Iter : Iterator) return Iterator;
      -- Move the iterator to point to the previous element on the list.

      function Data (Iter : Iterator) return Result_Collection_Access;
      -- Return element pointed by the iterator.

      function Is_Valid (Iter : Iterator) return Boolean;

   private
      type Node;
      type Node_Access is access Node;
      type Iterator is new Node_Access;

      procedure Remove (Ptr : Node_Access);
      -- A procedure to release memory pointed by Ptr.

      type Node is record
         Next : Node_Access := null;
         Prev : Node_Access := null;
         Data : Result_Collection_Access;
      end record;

      type List is new Ada.Finalization.Controlled with record
         First : Node_Access := null;
         Last  : Node_Access := null;
         Size  : Natural := 0;
      end record;

      procedure Initialize (Target : in out List);
      procedure Finalize   (Target : in out List);
      procedure Adjust     (Target : in out List);

      Empty_List : constant List :=
        (Ada.Finalization.Controlled with First => null,
                                          Last  => null,
                                          Size  => 0);
   end Result_List;

   type Result_Info_Iterator is new Result_Info_List.Iterator;

   type Result_Collection_Iterator is new Result_List.Iterator;

   type Result_Collection is
     new Ada.Finalization.Limited_Controlled with record
      Test_Name : Unbounded_String         := Null_Unbounded_String;
      Passes    : Result_Info_List.List    := Result_Info_List.Empty_List;
      Failures  : Result_Info_List.List    := Result_Info_List.Empty_List;
      Errors    : Result_Info_List.List    := Result_Info_List.Empty_List;
      Children  : Result_List.List         := Result_List.Empty_List;
      Parent    : Result_Collection_Access := null;
   end record;
end Ahven.Results;
