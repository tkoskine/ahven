setlocal

set JBIND_OPTIONS=/t/l/YLLIBCMT/q
set COMP_OPTIONS=/q/b
set TOPDIR=%CD%
set SUFFIX=/l'ads'/n'adb'

corder compat /e/pAhvenCompat/osrc\windows%SUFFIX%/t/w/k255/js'jbind'/jb'%JBIND_OPTIONS%'/c'%COMP_OPTIONS%'/b'com_obj\ctst.bat'/rcom_obj

if %ERRORLEVEL NEQ 0 goto errorhappened

call com_obj\ctst.bat

if %ERRORLEVEL NEQ 0 goto errorhappened

corder libmain /e/pAhvenLib/osrc%SUFFIX%/t/w/k255/js'jbind'/jb'%JBIND_OPTIONS%'/c'%COMP_OPTIONS%'/b'lib_obj\ctst.bat'/rlib_obj

if %ERRORLEVEL NEQ 0 goto errorhappened

call lib_obj\ctst.bat

if %ERRORLEVEL NEQ 0 goto errorhappened

corder tap_tester /e/pAhvenTstTAP/otest%SUFFIX%/js'jbind'/jb'%JBIND_OPTIONS%'/c'%COMP_OPTIONS%'/t/w/k255/b'test_obj\ctsttap.bat'/rtest_obj

call test_obj\ctsttap.bat

if %ERRORLEVEL NEQ 0 goto errorhappened

corder tester /e/pAhvenTst/otest%SUFFIX%/js'jbind'/jb'%JBIND_OPTIONS%'/c'%COMP_OPTIONS%'/t/w/k255/b'test_obj\ctst.bat'/rtest_obj

call test_obj\ctst.bat

if %ERRORLEVEL NEQ 0 goto errorhappened

cd test_obj
call lkc tester

if %ERRORLEVEL NEQ 0 goto errorhappened

call lkc tap_test

if %ERRORLEVEL NEQ 0 goto errorhappened

:allok:
cd %TOPDIR%
exit /b 0

:errorhappened:
cd %TOPDIR%
exit /b 1
