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
with Ahven.Doubly_Linked_List;

use Ada.Strings.Unbounded;

-- Like the name implies, Results package is used for
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

   type Result_Collection is new Ada.Finalization.Controlled with private;
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

   procedure Next_Error (Collection : in out Result_Collection;
                         Info : out Result_Info;
                         End_Of_Errors : out Boolean);
   -- Return the next error in the collection.
   -- If there are no more errors, End_Of_Errors is set to True.
   -- Calling the procedure again after End_Of_Error is set to True
   -- starts the iteration from the beginning.

   procedure Next_Failure (Collection : in out Result_Collection;
                           Info : out Result_Info;
                           End_Of_Failures: out Boolean);
   -- Return the next failure in the collection.
   -- If there are no more failures, End_Of_Failures is set to True.
   -- Calling the procedure again after End_Of_Error is set to True
   -- starts the iteration from the beginning.

   procedure Next_Pass (Collection : in out Result_Collection;
                        Info : out Result_Info;
                        End_Of_Passes : out Boolean);
   -- Return the next pass in the collection.
   -- If there are no more passes, End_Of_Passes is set to True.
   -- Calling the procedure again after End_Of_Error is set to True
   -- starts the iteration from the beginning.

   procedure Next_Child (Collection : in out Result_Collection;
                         Child : out Result_Collection_Access;
                         End_Of_Children : out Boolean);
   -- Return the next child collection.
   -- If there are no more children, End_Of_Children is set to True.
   -- Calling the procedure again after End_Of_Error is set to True
   -- starts the iteration from the beginning.

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
     new Ahven.Doubly_Linked_List (Data_Type => Result_Info);

   package Result_List is
     new Ahven.Doubly_Linked_List (Data_Type => Result_Collection_Access);

   type Result_Collection is new Ada.Finalization.Controlled with record
      Test_Name : Unbounded_String := Null_Unbounded_String;
      Passes    : Result_Info_List.List    := Result_Info_List.Empty_List;
      Failures  : Result_Info_List.List    := Result_Info_List.Empty_List;
      Errors    : Result_Info_List.List    := Result_Info_List.Empty_List;
      Children  : Result_List.List         := Result_List.Empty_List;
      Parent    : Result_Collection_Access := null;

      Pass_Iter    : Result_Info_List.Iterator;
      Failure_Iter : Result_Info_List.Iterator;
      Error_Iter   : Result_Info_List.Iterator;
      Child_Iter   : Result_List.Iterator;
   end record;
end Ahven.Results;
