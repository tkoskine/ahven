with Ada.Strings.Bounded;

package Ahven.AStrings is
  new Ada.Strings.Bounded.Generic_Bounded_Length (Max => 160);
