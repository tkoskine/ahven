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
   type Result_Place is record
      Test_Name : Unbounded_String;
      Routine_Name : Unbounded_String;
   end record;

   package Result_Place_List is
     new Ahven.Double_Linked_List (Result_Place);

   type Result;
   type Result_Access is access Result;

   package Result_List is
     new Ahven.Double_Linked_List (Result_Access);

   type Result is new Ada.Finalization.Controlled with record
      Test_Name : Unbounded_String;
      Passes    : Result_Place_List.List;
      Failures  : Result_Place_List.List;
      Errors    : Result_Place_List.List;
      Children  : Result_List.List;
      Parent    : Result_Access;
   end record;

   procedure Add_Children (Res : in out Result; Child : Result_Access);

   procedure Add_Error (Res : in out Result; Place : Result_Place);

   procedure Add_Failure (Res : in out Result; Place : Result_Place);

   procedure Add_Pass (Res : in out Result; Place : Result_Place);

   procedure Finalize (Res : in out Result);

   procedure Set_Name (Res : in out Result; Name : Unbounded_String);

   function Test_Count (Res : Result) return Natural;

   function Pass_Count (Res : Result) return Natural;

   function Error_Count (Res : Result) return Natural;

   function Failure_Count (Res : Result) return Natural;

end Ahven.Results;
