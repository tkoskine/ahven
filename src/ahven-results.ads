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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ahven.Double_Linked_List;

use Ada.Strings.Unbounded;

package Ahven.Results is
   type Result_Place is private;

   procedure Set_Test_Name (Place : in out Result_Place;
                            Name : Unbounded_String);
   -- Set a test name for the result place.

   procedure Set_Routine_Name (Place : in out Result_Place;
                               Name : Unbounded_String);
   -- Set a routine name for the result place.

   procedure Set_Message (Place : in out Result_Place;
                          Message : Unbounded_String);
   -- Set a message for the result place.

   procedure Set_Test_Name (Place : in out Result_Place; Name : String);
   -- A helper function, which calls Set_Test_Name (.. ; Unbounded_String)

   procedure Set_Routine_Name (Place : in out Result_Place; Name : String);
   -- A helper function, which calls Set_Routine_Name (.. ; Unbounded_String)

   procedure Set_Message (Place : in out Result_Place; Message : String);
   -- A helper function, which calls Set_Message (.. ; Unbounded_String)

   function Test_Name (Place : Result_Place) return Unbounded_String;
   -- Return the test name of the result place.

   function Routine_Name (Place : Result_Place) return Unbounded_String;
   -- Return the routine name of the result place.

   function Message (Place : Result_Place) return Unbounded_String;
   -- Return the message of the result place.

   type Result_Collection is new Ada.Finalization.Controlled with private;
   type Result_Collection_Access is access Result_Collection;

   procedure Add_Child (Collection : in out Result_Collection;
                        Child : Result_Collection_Access);
   -- Add a child collection to the collection.

   procedure Add_Error (Collection : in out Result_Collection;
                        Place : Result_Place);
   -- Add a test error to the collection.

   procedure Add_Failure (Collection : in out Result_Collection;
                          Place : Result_Place);
   -- Add a test failure to the collection.

   procedure Add_Pass (Collection : in out Result_Collection;
                       Place : Result_Place);
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

   function Test_Name (Collection : Result_Collection) return Unbounded_String;
   -- Return the name of the collection's test.

   function Parent (Collection : Result_Collection)
     return Result_Collection_Access;
   -- Return the parent of the collection.

   procedure Next_Error (Collection : in out Result_Collection;
                         Place : out Result_Place;
                         End_Of_Errors : out Boolean);
   -- Return the next error in the collection.
   -- If there are no more errors, End_Of_Errors is set to True.

   procedure Next_Failure (Collection : in out Result_Collection;
                           Place : out Result_Place;
                           End_Of_Failures: out Boolean);
   -- Return the next failure in the collection.
   -- If there are no more failures, End_Of_Failures is set to True.

   procedure Next_Pass (Collection : in out Result_Collection;
                        Place : out Result_Place;
                        End_Of_Passes : out Boolean);
   -- Return the next pass in the collection.
   -- If there are no more passes, End_Of_Passes is set to True.

   procedure Next_Child (Collection : in out Result_Collection;
                         Child : out Result_Collection_Access;
                         End_Of_Children : out Boolean);
   -- Return the next child collection.
   -- If there are no more children, End_Of_Children is set to True.

private
   type Result_Place is record
      Test_Name    : Unbounded_String := Null_Unbounded_String;
      Routine_Name : Unbounded_String := Null_Unbounded_String;
      Message      : Unbounded_String := Null_Unbounded_String;
   end record;

   package Result_Place_List is
     new Ahven.Double_Linked_List (Result_Place);

   package Result_List is
     new Ahven.Double_Linked_List (Result_Collection_Access);

   type Result_Collection is new Ada.Finalization.Controlled with record
      Test_Name : Unbounded_String := Null_Unbounded_String;
      Passes    : Result_Place_List.List := Result_Place_List.Empty_List;
      Failures  : Result_Place_List.List := Result_Place_List.Empty_List;
      Errors    : Result_Place_List.List := Result_Place_List.Empty_List;
      Children  : Result_List.List       := Result_List.Empty_List;
      Parent    : Result_Collection_Access := null;

      Pass_Iter    : Result_Place_List.Iterator := null;
      Failure_Iter : Result_Place_List.Iterator := null;
      Error_Iter   : Result_Place_List.Iterator := null;
      Child_Iter   : Result_List.Iterator       := null;
   end record;

end Ahven.Results;
