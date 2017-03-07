del objects\*.adt
del objects\*.ali
mkdir objects
cd objects
adactl -p ..\gnat\ahven.gpr -f ..\rules\ahven.aru libmain Ahven Ahven.Framework Ahven.Listeners Ahven.Listeners.Basic Ahven.Parameters Ahven.Results Ahven.Runner Ahven.Slist Ahven.Tap_Runner Ahven.Text_Runner Ahven.Xml_Runner Ahven.Astrings Ahven.Long_AStrings Ahven.Name_List Ahven_Compat Ahven.Temporary_Output
if ErrorLevel 0 goto :end
exit /b 1
:end
cd ..
