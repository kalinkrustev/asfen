@echo off
echo proceed with data restore?
pause
osql -U sa -b -P 123 -n -d master -S (local) -Q "sp_addlogin 'Alarm'"
osql -U sa -b -P 123 -n -d master -S (local) -Q "RESTORE DATABASE [asosdb] FROM  DISK = N'i:\work\sip\asosdb.bak' WITH MOVE N'alarm_D' TO N'I:\work\database\asosdb.Mdf', MOVE N'alarm_L' TO N'I:\work\database\asosdb.LDF'"
osql -U sa -b -P 123 -n -d asosdb  -S (local) -Q "exec sp_change_users_login 'Update_One', 'Alarm', 'Alarm'"
pause