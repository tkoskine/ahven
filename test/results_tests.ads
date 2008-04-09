with Ahven.Framework;

package Results_Tests is

   type Test is new Ahven.Framework.Test_Case with null record;

   procedure Initialize (T : in out Test);
private
   procedure Test_Count_Children;
   procedure Test_Direct_Count;

end Results_Tests;
