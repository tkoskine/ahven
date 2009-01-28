cd src
set januspath=C:\Janus312\rts\console
del /q ..\objects\*.*
del /q ..\objects2\*.*
mkdir ..\objects
mkdir ..\objects2
copy /y ..\janusada\libmain.adb ..\src
cd windows
jmanager Add_Project (..\..\Objects2\,AhvenCompat)
jmanager Add_Link (..\..\Objects2\,AhvenCompat,%januspath%, JNT_RTS_CONSOLE)
cd ..
jmanager Add_Project (..\Objects\,AhvenLib)
jmanager Add_Link (..\Objects\,AhvenLib,%januspath%, JNT_RTS_CONSOLE)
jmanager Add_Link (..\Objects\,AhvenLib,..\Objects2\, AhvenCompat)

rem corder libmain /pAhvenLib/l'ads'/n'adb'/t/w/k255/js'jbind'/jb'/t/l'/b'ctst.bat'/r..\objects
cd ..\test
del /q ..\test_objects\*.*
mkdir ..\test_objects
jmanager Add_Project (..\Test_Objects\,AhvenTst)
jmanager Add_Link (..\Test_Objects\,AhvenTst,..\Objects, AhvenLib)

rem corder tap_test /pAhvenTst/l'ads'/n'adb'/js'jbind'/jb'/t/l'/t/z/w/k255/b'ctst.bat'/r..\test_objects

cd ..