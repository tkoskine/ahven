--
-- Copyright (c) 2012 Tero Koskinen <tero.koskinen@iki.fi>
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

with Ada.Strings.Unbounded;

package Ahven.Long_AStrings is
  subtype Long_String is Ada.Strings.Unbounded.Unbounded_String;
  Null_Long_String : constant Long_String
   := Ada.Strings.Unbounded.Null_Unbounded_String;
  function To_Long_String (Str : String) return Long_String renames
    Ada.Strings.Unbounded.To_Unbounded_String;
  function To_String (U : Long_String) return String renames
    Ada.Strings.Unbounded.To_String;
  function Length (U : Long_String) return Natural renames
    Ada.Strings.Unbounded.Length;
end Ahven.Long_AStrings;
