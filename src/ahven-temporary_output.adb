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
use Ada.Strings.Unbounded;

package body Ahven.Temporary_Output is
   Temp_Counter : Natural := 0;

   procedure Create_Temp (File : out Temporary_File) is
      Filename : Unbounded_String := Null_Unbounded_String;
   begin
      Filename := To_Unbounded_String ("ahven_temp_") &
        Trim (To_Unbounded_String(Integer'Image (Temp_Counter)),
              Ada.Strings.Both);
      if Temp_Counter < Natural'Last then
         Temp_Counter := Temp_Counter + 1;
      else
         raise Temporary_File_Error;
      end if;

      File.Name := Filename;

      Ada.Text_IO.Create (File.Handle, Ada.Text_IO.Out_File,
                          To_String (File.Name));
   end Create_Temp;

   procedure Redirect_OutPut (To_File : in out Temporary_File) is

   begin
      Ada.Text_IO.Flush;

      Ada.Text_IO.Set_Output (To_File.Handle);
   end Redirect_Output;

   procedure Restore_Output is
   begin
      Ada.Text_IO.Flush;
      Ada.Text_IO.Set_Output (Ada.Text_IO.Standard_Output);
   end Restore_Output;

   procedure Remove_Temp (File : in out Temporary_File) is
   begin
      if not Ada.Text_IO.Is_Open (File.Handle) then
         Ada.Text_IO.Open (File.Handle, Ada.Text_IO.Out_File,
                           To_String (File.Name));
      end if;
      Ada.Text_IO.Delete (File.Handle);
   end Remove_Temp;

   procedure Close_Temp (File : in out Temporary_File) is
   begin
      Ada.Text_IO.Close (File.Handle);
   end Close_Temp;

end Ahven.Temporary_Output;
