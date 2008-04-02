with Ahven;
with Ahven.Results;

use Ahven;

package body Results_Tests is

   procedure Initialize (T : in out Test) is
      use Ahven.Framework;
   begin
      Set_Name (T, "Ahven.Results");
      Add_Test_Routine (T, Test_Count_Children'Access, "Test Count Children");
   end Initialize;

   procedure Test_Count_Children is
      use Ahven.Results;

      Coll     : Result_Collection;
      Coll_Dyn : Result_Collection_Access;
      Info     : Result_Info := Empty_Result_Info;
   begin
      Coll_Dyn := new Result_Collection;
      Add_Error (Coll, Info);
      Add_Pass (Coll_Dyn.all, Info);

      Add_Child (Coll, Coll_Dyn);
      Assert (2 = Test_Count (Coll), "Invalid test count");
   end Test_Count_Children;
end Results_Tests;
