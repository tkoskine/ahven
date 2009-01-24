set januspath=C:\Janus312\rts\console
del ..\objects\*.*
mkdir ..\objects
copy ..\janusada\libmain.adb ..\src
jmanager Add_Project (..\Objects\,AhvenLib)
jmanager Add_Link (..\Objects\,AhvenLib,%januspath%, JNT_RTS_CONSOLE)

