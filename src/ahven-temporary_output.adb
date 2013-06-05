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
with Ada.Characters.Latin_1;

package body Ahven.Temporary_Output is
   procedure Create_Temp (File : out Temporary_File) is
   begin
      Ada.Text_IO.Create (File.Handle, Ada.Text_IO.Out_File, "");
   end Create_Temp;

   procedure Redirect_Output (To_File : in out Temporary_File) is
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
      if Ada.Text_IO.Is_Open (File.Handle) then
         Ada.Text_IO.Delete (File.Handle);
      end if;
   end Remove_Temp;

   procedure Close_Temp (File : in out Temporary_File) is
   begin
      Ada.Text_IO.Close (File.Handle);
   end Close_Temp;

   procedure Read_Contents
     (File     : in out Temporary_File;
      Contents :    out Ahven.Long_AStrings.Bounded_String)
   is
      Ch : Character;
   begin
      Restore_Output;
      Contents := Long_AStrings.Null_Bounded_String;
      if not Ada.Text_IO.Is_Open (File.Handle) then
         return;
      end if;

      Ada.Text_IO.Flush (File.Handle);
      Ada.Text_IO.Reset (File.Handle, Ada.Text_IO.In_File);

      loop
         exit when Ada.Text_IO.End_Of_File (File.Handle);

         Ada.Text_IO.Get (File.Handle, Ch);
         Long_AStrings.Append (Contents, Ch);
         if Ada.Text_IO.End_Of_Line (File.Handle) then
            Long_AStrings.Append (Contents, Ada.Characters.Latin_1.LF);
         end if;
      end loop;
   end Read_Contents;

end Ahven.Temporary_Output;
