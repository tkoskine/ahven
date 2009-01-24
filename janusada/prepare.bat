cd src
set januspath=C:\Janus312\rts\console
del ..\objects\*.*
mkdir ..\objects
copy ..\janusada\libmain.adb ..\src
jmanager Add_Project (..\Objects\,AhvenLib)
jmanager Add_Link (..\Objects\,AhvenLib,%januspath%, JNT_RTS_CONSOLE)

rem corder libmain /pAhvenLib/l'ads'/n'adb'/t/w/k255/js'jbind'/jb'/t/l'/b'ctst.bat'/r..\objects
cd ..\test
del ..\test_objects\*.*
mkdir ..\test_objects
jmanager Add_Project (..\Test_Objects\,AhvenTst)
jmanager Add_Link (..\Test_Objects\,AhvenTst,..\Objects, AhvenLib)

rem corder tap_test /pAhvenTst/l'ads'/n'adb'/js'jbind'/jb'/t/l'/t/z/w/k255/b'ctst.bat'/r..\test_objects

cd ..