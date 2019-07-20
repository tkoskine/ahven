setlocal
if [%JAWSII%] == [] (
  set januspath=C:\JanusAda\rts\console
) else (
  if exist %JAWSII% (set januspath=%JAWSII%rts\console) else (
    set januspath=C:\JanusAda\rts\console
  )
)

echo JANUSPATH: %januspath%

if exist lib_obj rmdir /q /s lib_obj
if exist com_obj rmdir /q /s com_obj
mkdir lib_obj
mkdir com_obj
copy /y janusada\compat.adb src\windows
jmanager Add_Project(com_obj,AhvenCompat)
jmanager Add_Link(com_obj,AhvenCompat,%januspath%, JWIN_RTS_CONSOLE)
jmanager Add_Project (lib_obj,AhvenLib)
jmanager Add_Link(lib_obj,AhvenLib,%januspath%, JWIN_RTS_CONSOLE)
jmanager Add_Link(lib_obj,AhvenLib,com_obj, AhvenCompat)

if exist test_obj rmdir /q /s test_obj
mkdir test_obj
jmanager Add_Project(test_obj,AhvenTst)
jmanager Add_Link(test_obj,AhvenTst,lib_obj, AhvenLib)
jmanager Add_Project(test_obj,AhvenTstTAP)
jmanager Add_Link(test_obj,AhvenTstTAP,lib_obj, AhvenLib)
cd ..
endlocal
