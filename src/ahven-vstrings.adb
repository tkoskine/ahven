--
-- Copyright (c) 2008 Tero Koskinen <tero.koskinen@iki.fi>
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

package body Ahven.VStrings is
   function "+"(Source : String) return VString is
   begin
      if Source'Length in VString_Size'Range then
         return VString'(Len => Source'Length, Data => Source);
      else
         raise Constraint_Error;
      end if;
   end "+";

   function Truncate (Source : String) return VString is
   begin
      if Source'Length in VString_Size'Range then
         return VString'(Len => Source'Length, Data => Source);
      else
         return VString'(Len  => VString_Size'Last,
                         Data => Source (Source'First .. Source'First
                                         + (VString_Size'Last - 1)));
      end if;
   end Truncate;

   function To_String (Source : VString) return String is
   begin
      return Source.Data;
   end To_String;

   function Length (Source : VString) return VString_Size is
   begin
      return Source.Len;
   end Length;
end Ahven.VStrings;
