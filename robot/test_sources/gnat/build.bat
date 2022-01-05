if not exist bin ( mkdir bin )
gprbuild -p -P gnat\tests
if %ERRORLEVEL NEQ 0 goto errorhappened
copy gnat\simple1r.exe bin
copy gnat\fail1r.exe bin

:allok:
exit /b 0

:errorhappened:
exit /b 1
