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

   procedure Set_Test_Name (Place : in out Result_Place; Name : String);
   -- A helper function, which calls Set_Test_Name (.. ; Unbounded_String)

   procedure Set_Routine_Name (Place : in out Result_Place; Name : String);
   -- A helper function, which calls Set_Routine_Name (.. ; Unbounded_String)

   function Test_Name (Place : Result_Place) return Unbounded_String;
   -- Return the test name of the result place.

   function Routine_Name (Place : Result_Place) return Unbounded_String;
   -- Return the routine name of the result place.

   type Result is new Ada.Finalization.Controlled with private;
   type Result_Access is access Result;

   procedure Add_Child (Res : in out Result; Child : Result_Access);

   procedure Add_Error (Res : in out Result; Place : Result_Place);

   procedure Add_Failure (Res : in out Result; Place : Result_Place);

   procedure Add_Pass (Res : in out Result; Place : Result_Place);

   procedure Finalize (Res : in out Result);

   procedure Set_Name (Res : in out Result; Name : Unbounded_String);

   procedure Set_Parent (Res : in out Result; Parent : Result_Access);

   function Test_Count (Res : Result) return Natural;

   function Pass_Count (Res : Result) return Natural;

   function Error_Count (Res : Result) return Natural;

   function Failure_Count (Res : Result) return Natural;

   function Test_Name (Res : Result) return Unbounded_String;

   function Parent (Res : Result) return Result_Access;

   procedure Next_Error (Res : in out Result;
                         Place : out Result_Place;
                         End_Of_Errors : out Boolean);

   procedure Next_Failure (Res : in out Result;
                           Place : out Result_Place;
                           End_Of_Failures : out Boolean);

   procedure Next_Pass (Res : in out Result;
                        Place : out Result_Place;
                        End_Of_Passes : out Boolean);

   procedure Next_Child (Res : in out Result;
                         Child : out Result_Access;
                         End_Of_Children : out Boolean);
private
   type Result_Place is record
      Test_Name    : Unbounded_String := Null_Unbounded_String;
      Routine_Name : Unbounded_String := Null_Unbounded_String;
   end record;

   package Result_Place_List is
     new Ahven.Double_Linked_List (Result_Place);

   package Result_List is
     new Ahven.Double_Linked_List (Result_Access);

   type Result is new Ada.Finalization.Controlled with record
      Test_Name : Unbounded_String := Null_Unbounded_String;
      Passes    : Result_Place_List.List := Result_Place_List.Empty_List;
      Failures  : Result_Place_List.List := Result_Place_List.Empty_List;
      Errors    : Result_Place_List.List := Result_Place_List.Empty_List;
      Children  : Result_List.List       := Result_List.Empty_List;
      Parent    : Result_Access := null;

      Pass_Iter    : Result_Place_List.Iterator := null;
      Failure_Iter : Result_Place_List.Iterator := null;
      Error_Iter   : Result_Place_List.Iterator := null;
      Child_Iter   : Result_List.Iterator       := null;
   end record;

end Ahven.Results;
