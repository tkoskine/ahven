cd src\windows
call ctst.bat
if ErrorLevel 2 goto Abort
cd ..
call ctst.bat
if ErrorLevel 2 goto Abort
cd ..\test
call ctst.bat
if ErrorLevel 2 goto Abort
call ctsttap.bat
if ErrorLevel 2 goto Abort
cd ..\test_obj
call lkc tap_test
call lkc tester
cd ..
:abort