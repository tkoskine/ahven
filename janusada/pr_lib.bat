set januspath=C:\Janus312\rts\console
del ..\objects\*.*
jmanager Add_Project (..\Objects\,AhvenLib)
jmanager Add_Link (..\Objects\,AhvenLib,%januspath%, JNT_RTS_CONSOLE)

