set januspath=C:\JanusAda\rts

cd simple1
del /q ..\simple1_obj\*.*
del /q ..\fail1_obj\*.*
mkdir ..\simple1_obj
jmanager Add_Project (..\simple1_obj\,Simple1)
jmanager Add_Link (..\simple1_obj\,Simple1,%januspath%, JNT_RTS)
jmanager Add_Link (..\simple1_obj\,Simple1,..\..\..\lib_obj\, AhvenLib)
cd ..

cd fail1
del /q ..\fail1_obj\*.*
mkdir ..\fail1_obj
jmanager Add_Project (..\fail1_obj\,fail1)
jmanager Add_Link (..\fail1_obj\,fail1,%januspath%, JNT_RTS)
jmanager Add_Link (..\fail1_obj\,fail1,..\..\..\lib_obj\, AhvenLib)
cd ..
