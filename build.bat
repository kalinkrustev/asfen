@finddbg alarmservice.exe
@if errorlevel 1 goto a1
makejcldbg -E alarmservice.map
rem @upx alarmservice.exe
:a1

@finddbg alarmgui.exe
@if errorlevel 1 goto a2
makejcldbg -E alarmgui.map
rem @upx alarmgui.exe
:a2

"h:\Program Files\7-Zip\7z.exe" u install.zip alarmservice.exe alarmgui.exe monitor\monitor.html monitor\monitor.js
pause