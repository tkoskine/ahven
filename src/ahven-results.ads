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

with Ahven.SList;
with Ahven.VStrings;

pragma Elaborate_All (Ahven.SList);

-- Like the name implies, the Results package is used for
-- storing the test results.
--
-- Result_Info holds one invidual result and
-- Result_Collection holds multiple Result_Infos.
--
package Ahven.Results is
   use Ahven.VStrings;

   type Result_Info is private;

   Empty_Result_Info : constant Result_Info;
   -- Result_Info object which holds no result. It can be used
   -- to initialize a new Result_Info object.

   procedure Set_Test_Name (Info : in out Result_Info;
                            Name :        VString);
   -- Set a test name for the result.

   procedure Set_Routine_Name (Info : in out Result_Info;
                               Name :        VString);
   -- Set a routine name for the result.

   procedure Set_Message (Info : in out Result_Info;
                          Message : VString);
   -- Set a message for the result.

   procedure Set_Test_Name (Info : in out Result_Info; Name : String);
   -- A helper function, which calls Set_Test_Name (.. ; VString)

   procedure Set_Routine_Name (Info : in out Result_Info; Name : String);
   -- A helper function, which calls Set_Routine_Name (.. ; VString)

   procedure Set_Message (Info : in out Result_Info; Message : String);
   -- A helper function, which calls Set_Message (.. ; VString)

   procedure Set_Long_Message (Info    : in out Result_Info;
                               Message :        VString);
   -- Set a long message for the result

   procedure Set_Long_Message (Info : in out Result_Info; Message : String);
   -- A helper function, which calls Set_Long_Message (.. ; VString)

   procedure Set_Execution_Time (Info         : in out Result_Info;
                                 Elapsed_Time :        Duration);
   -- Set the execution time of the result info (test).

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        VString);
   -- Set the name of the test output file.

   procedure Set_Output_File (Info     : in out Result_Info;
                              Filename :        String);
   -- Set the name of the test output file.

   function Get_Test_Name (Info : Result_Info) return String;
   -- Return the test name of the result info.

   function Get_Routine_Name (Info : Result_Info) return String;
   -- Return the routine name of the result info.

   function Get_Message (Info : Result_Info) return String;
   -- Return the message of the result info.

   function Get_Long_Message (Info : Result_Info) return String;
   -- Return the long message of the result info.

   function Get_Execution_Time (Info : Result_Info) return Duration;
   -- Return the execution time of the result info.

   function Get_Output_File (Info : Result_Info) return VString;
   -- Return the name of the output file.
   -- Empty string is returned in case there is no output file.

   type Result_Collection is limited private;
   -- A collection of Result_Info objects.
   -- Contains also child collections.

   type Result_Collection_Access is access all Result_Collection;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child      :        Result_Collection_Access);
   -- Add a child collection to the collection.

   procedure Add_Error (Collection : in out Result_Collection;
                        Info       :        Result_Info);
   -- Add a test error to the collection.

   procedure Add_Failure (Collection : in out Result_Collection;
                          Info       :        Result_Info);
   -- Add a test failure to the collection.

   procedure Add_Pass (Collection : in out Result_Collection;
                       Info       :        Result_Info);
   -- Add a passed test to the collection

   procedure Release (Collection : in out Result_Collection);
   -- Release resourced held by the collection.
   -- Frees also all children added via Add_Child.

   procedure Set_Name (Collection : in out Result_Collection;
                       Name       :        VString);
   -- Set a test name for the collection.

   procedure Set_Parent (Collection : in out Result_Collection;
                         Parent     :        Result_Collection_Access);
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
     return VString;
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

   function Child_Depth (Collection : Result_Collection) return Natural;
   -- Return the maximum depth of children. (a child of a child, etc.)

private
   type Result_Info is record
      Test_Name      : VString  := Empty_VString;
      Output_File    : VString  := Empty_VString;
      Routine_Name   : VString  := Empty_VString;
      Execution_Time : Duration := 0.0;
      Message        : VString  := Empty_VString;
      Long_Message   : VString  := Empty_VString;
   end record;

   Empty_Result_Info : constant Result_Info :=
     (Test_Name      => Empty_VString,
      Routine_Name   => Empty_VString,
      Message        => Empty_VString,
      Long_Message   => Empty_VString,
      Execution_Time => 0.0,
      Output_File    => Empty_VString);

   package Result_Info_List is
     new Ahven.SList (Element_Type => Result_Info);

   type Result_Collection_Wrapper is record
      Ptr : Result_Collection_Access;
   end record;
   -- Work around for Janus/Ada 3.1.1d/3.1.2beta generic bug.

   package Result_List is
    new Ahven.SList (Element_Type => Result_Collection_Wrapper);

   type Result_Info_Iterator is new Result_Info_List.Iterator;

   type Result_Collection_Iterator is new Result_List.Iterator;

   type Result_Collection is limited record
      Test_Name : VString := Empty_VString;
      Passes    : Result_Info_List.List;
      Failures  : Result_Info_List.List;
      Errors    : Result_Info_List.List;
      Children  : Result_List.List;
      Parent    : Result_Collection_Access := null;
   end record;
end Ahven.Results;
