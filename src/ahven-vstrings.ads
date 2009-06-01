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

package Ahven.VStrings is
   VString_Max_Size : constant := 160;

   subtype VString_Size is Integer range 0 .. VString_Max_Size;

   type VString (Len : VString_Size := VString_Size'Last) is private;
   -- A variable length string. The size of the string
   -- must be something between 0 and VString_Size'Last.
   --
   -- By default the maximum size String filled with spaces is created.

   function "+"(Source : String) return VString;
   -- Convert String into VString.
   -- Raises Constraint_Error if the string is longer than VString_Max_Size.

   function Truncate (Source : String) return VString;
   -- Convert String into VString and truncate oversized strings
   -- to VString_Max_Size.

   function To_String (Source : VString) return String;
   -- Convert VString into String.

   function Length (Source : VString) return VString_Size;
   -- Return the length of the VString.

   Empty_VString : constant VString;
   -- Empty String (len = 0).

private
   type VString (Len : VString_Size := VString_Size'Last) is record
      Data : String (1 .. Len) := (others => ' ');
   end record;

   Empty_VString : constant VString := (Len => 0, Data => (others => ' '));
end Ahven.VStrings;
