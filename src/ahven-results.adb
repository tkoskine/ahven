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

   procedure Add_Children (Res : in out Result; Child : Result_Access) is
   begin
      Append (Res.Children, Child);
   end Add_Children;

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

end Ahven.Results;
