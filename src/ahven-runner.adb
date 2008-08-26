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

with Ada.Command_Line;
with Ada.Unchecked_Deallocation;

with Ahven.Listeners;
with Ahven.Listeners.Basic;
with Ahven.Listeners.Output_Capture;

package body Ahven.Runner is
   use Ahven.Results;

   procedure Run_Suite (Suite : in out Framework.Test_Suite'Class;
                        Reporter : Report_Proc) is
      procedure Free is new Ada.Unchecked_Deallocation
        (Object => Listeners.Basic.Basic_Listener'Class,
         Name   => Listeners.Basic.Basic_Listener_Class_Access);

      use Ahven.Listeners.Basic;

      Result   : Framework.Test_Result;
      Listener : Listeners.Basic.Basic_Listener_Class_Access;
      Params   : Parameters.Parameter_Info;
   begin
      Parameters.Parse_Parameters (Params);
      if Parameters.Capture (Params) then
         Listener := Listeners.Output_Capture.Create;
      else
         Listener := Listeners.Basic.Create;
      end if;

      Framework.Add_Listener
        (Result, Listeners.Result_Listener_Class_Access (Listener));
      if Parameters.Single_Test (Params) then
         Framework.Execute
           (Suite, Parameters.Test_Name (Params), Listener.all);
      else
         Framework.Execute (Suite, Listener.all);
      end if;

      Reporter (Listener.Main_Result, Params);
      if (Error_Count (Listener.Main_Result) > 0) or
         (Failure_Count (Listener.Main_Result) > 0) then
         Ada.Command_Line.Set_Exit_Status (Ada.Command_Line.Failure);
      end if;
      Free (Listener);
   exception
      when Parameters.Invalid_Parameter =>
         Parameters.Usage;
   end Run_Suite;

end Ahven.Runner;
