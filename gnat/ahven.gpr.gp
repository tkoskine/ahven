#if Directories_GPR'Defined then
with $Directories_GPR;
#end if;

library project Ahven is
   for Library_Name     use "ahven";
   for Library_Kind     use $Library_Type;
   for Source_Dirs      use ($Includedir & "/ahven");
   for Library_Dir      use $Libdir;
   for Library_ALI_Dir  use $Alidir & "/ahven";
   for Externally_Built use "true";
end Ahven;
