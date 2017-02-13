call com_obj\ctst.bat
if ErrorLevel 2 goto Abort
call lib_obj\ctst.bat
if ErrorLevel 2 goto Abort
call test_obj\ctst.bat
if ErrorLevel 2 goto Abort
call test_obj\ctsttap.bat
if ErrorLevel 2 goto Abort
cd test_obj
link  -subsystem:console -entry:mainCRTStartup -out:tap_test.exe tap_test.obj libcmt.lib kernel32.lib user32.lib -map:tap_test.map
link  -subsystem:console -entry:mainCRTStartup -out:tester.exe tester.obj libcmt.lib kernel32.lib user32.lib -map:tap_test.map
cd ..
:abort