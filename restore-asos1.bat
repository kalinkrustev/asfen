@echo off
echo proceed with data restore?
pause
osql -U sa -b -P 123 -n -d master -S (local) -Q "sp_addlogin 'Alarm'"
osql -U sa -b -P 123 -n -d master -S (local) -Q "RESTORE DATABASE [asos1] FROM  DISK = N'i:\work\sip\asos1.bak' WITH MOVE N'alarm_D' TO N'I:\work\database\asos1.Mdf', MOVE N'alarm_L' TO N'I:\work\database\asos1.LDF'"
osql -U sa -b -P 123 -n -d asos1  -S (local) -Q "exec sp_change_users_login 'Update_One', 'Alarm', 'Alarm'"
pause