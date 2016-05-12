cd src
set januspath=C:\JanusAda\rts
del /q ..\lib_obj\*.*
del /q ..\com_obj\*.*
mkdir ..\lib_obj
mkdir ..\com_obj
copy /y ..\janusada\compat.adb ..\src\windows
cd windows
jmanager Add_Project (..\..\com_obj\,AhvenCompat)
jmanager Add_Link (..\..\com_obj\,AhvenCompat,%januspath%, JNT_RTS)
cd ..
jmanager Add_Project (..\lib_obj\,AhvenLib)
jmanager Add_Link (..\lib_obj\,AhvenLib,%januspath%, JNT_RTS)
jmanager Add_Link (..\lib_obj\,AhvenLib,..\com_obj\, AhvenCompat)

cd ..\test
del /q ..\test_obj\*.*
mkdir ..\test_obj
jmanager Add_Project (..\test_obj\,AhvenTst)
jmanager Add_Link (..\test_obj\,AhvenTst,..\lib_obj, AhvenLib)
jmanager Add_Project (..\test_obj\,AhvenTstTAP)
jmanager Add_Link (..\test_obj\,AhvenTstTAP,..\lib_obj, AhvenLib)

cd ..
