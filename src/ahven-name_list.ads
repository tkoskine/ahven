
with Ahven.SList;
with Ahven.AStrings;

pragma Elaborate_All (Ahven.SList);

package Ahven.Name_List is new
  Ahven.SList (Ahven.AStrings.Bounded_String);
