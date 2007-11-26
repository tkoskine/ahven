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
with Ada.Strings.Unbounded;
with Ada.Text_IO;

package Ahven.Temporary_Output is
   Temporary_File_Error : exception;

   type Temporary_File is limited record
      Name    : Ada.Strings.Unbounded.Unbounded_String;
      Handle  : Ada.Text_IO.File_Type;
   end record;

   procedure Create_Temp (File : out Temporary_File);
   -- Create a new temporary file. Exception Temporary_File_Error
   -- is raised if the procedure cannot create a new temp file.

   procedure Redirect_OutPut (To_File : in out Temporary_File);

   procedure Restore_Output;

   procedure Remove_Temp (File : in out Temporary_File);

   procedure Close_Temp (File : in out Temporary_File);

end Ahven.Temporary_Output;
