cd simple1
set januspath=C:\JanusAda\rts
del /q ..\simple1_obj\*.*
mkdir ..\simple1_obj
jmanager Add_Project (..\simple1_obj\,Simple1)
jmanager Add_Link (..\simple1_obj\,Simple1,%januspath%, JNT_RTS)
jmanager Add_Link (..\simple1_obj\,Simple1,..\..\..\lib_obj\, AhvenLib)
cd ..
