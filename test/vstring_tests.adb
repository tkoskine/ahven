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
with Ahven;
with Ahven.VStrings;

package body VString_Tests is
   use Ahven;

   procedure Initialize (T : in out Test) is
   begin
      Set_Name (T, "Ahven.VStrings");
      Framework.Add_Test_Routine (T, Test_Overlong_String'Access,
                                  "Test Overlong String");
      Framework.Add_Test_Routine (T, Test_VString_Create'Access,
                                  "Test VString Create");
      Framework.Add_Test_Routine (T, Test_Max_Size_Create'Access,
                                  "Test Maximum Size Create");
      Framework.Add_Test_Routine (T, Test_Overlong_Truncate'Access,
                                  "Test Overlong Truncate");
      Framework.Add_Test_Routine (T, Test_Max_Size_Truncate'Access,
                                  "Test Maximum Size Truncate");
   end Initialize;

   procedure Test_Overlong_String is
      use Ahven.VStrings;
      Too_Big_Length : constant := 200;

      Long_String : constant String (1 .. Too_Big_Length) := (others => ' ');
   begin
      declare
         Target : constant Ahven.VStrings.VString := +Long_String;
      begin
         Fail ("No exception raised");

         -- This is here to avoid warning
         --  ''warning: constant "Target" is not referenced''
         Assert (To_String (Target) = Long_String, "Target /= Long_String");
      end;
   exception
      when Constraint_Error =>
         null; -- ok, this was expected
   end Test_Overlong_String;

   procedure Test_Overlong_Truncate is
      use Ahven.VStrings;
      Too_Big_Length : constant := VString_Max_Size + 1;
      Way_Too_Big_Length : constant := VString_Max_Size + 100;

      Long_String   : constant String (1 .. Too_Big_Length) := (others => ' ');
      Long_String_2 : constant String (1 .. Way_Too_Big_Length)
        := (others => ' ');
      Target       : VString;
      Target_2     : VString;
   begin
      Target := Truncate (Source => Long_String);

      Assert (Condition => Length (Target) = VString_Max_Size,
              Message   => "Length was invalid");

      Target_2 := Truncate (Source => Long_String_2);
      Assert (Condition => Length (Target_2) = VString_Max_Size,
              Message   => "Length was invalid");
   end Test_Overlong_Truncate;

   procedure Test_VString_Create is
      use Ahven.VStrings;

      Source : constant String := "test";
   begin
      declare
         Target : constant VString := +Source;
      begin
         Assert (Length (Target) = Source'Last, "Length was invalid");
         Assert (To_String (Target) = Source, "Target /= Source");
      end;
   end Test_VString_Create;

   procedure Test_Max_Size_Create is
      use Ahven.VStrings;

      Source : constant String (1 .. VString_Max_Size) := (others => ' ');
      Target : constant VString := +Source;
   begin
      Assert (Length (Target) = Source'Last, "Length was invalid");
      Assert (To_String (Target) = Source, "Target /= Source");
   end Test_Max_Size_Create;

   procedure Test_Max_Size_Truncate is
      use Ahven.VStrings;

      Max_String : constant String (1 .. VString_Max_Size) := (others => ' ');
      Target     : VString;
   begin
      Target := Truncate (Source => Max_String);

      Assert (Condition => Length (Target) = VString_Max_Size,
              Message   => "Length was invalid");
   end Test_Max_Size_Truncate;
end VString_Tests;
