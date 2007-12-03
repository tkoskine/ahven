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

with Ahven.Temporary_Output;
with Ahven.Listeners.Basic;

package Ahven.Listeners.Output_Capture is
   type Output_Capture_Listener is new Basic.Basic_Listener with record
      Output_File : Temporary_Output.Temporary_File;
   end record;

   type Output_Capture_Listener_Access is access all Output_Capture_Listener;

   procedure Start_Test (Listener : in out Output_Capture_Listener;
                         Info     :        Result_Info);

   procedure End_Test (Listener : in out Output_Capture_Listener;
                       Info     :        Result_Info);

   procedure Remove_File (Name : String);
   procedure Remove_Files (Collection : in out Result_Collection);

   procedure Finalize (Listener : in out Output_Capture_Listener);
private

end Ahven.Listeners.Output_Capture;
