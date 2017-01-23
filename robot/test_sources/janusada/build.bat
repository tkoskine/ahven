cd simple1
corder simple1r /pSimple1/l'ads'/n'adb'/t/w/k255/js'jbind'/jb'/t/l/YLLIBCMT'/b'..\simple1_obj\ctst.bat'/r..\simple1_obj
cd ..\simple1_obj
call ctst.bat
call lkc simple1r
move simple1r.exe ..
cd ..

cd fail1
corder fail1r /pfail1/l'ads'/n'adb'/t/w/k255/js'jbind'/jb'/t/l/YLLIBCMT'/b'..\fail1_obj\ctst.bat'/r..\fail1_obj
cd ..\fail1_obj
call ctst.bat
call lkc fail1r
move fail1r.exe ..
cd ..
