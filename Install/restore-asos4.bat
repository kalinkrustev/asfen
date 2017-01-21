@echo off
echo proceed with restore of c:\asos\asos4.bak (ctr+c to abort)?
pause
md data
osql -U sa -b -n -d master -S (local) -Q "sp_addlogin 'Alarm', 'par4evi453'"
osql -U sa -b -n -d master -S (local) -Q "RESTORE DATABASE [asos4] FROM  DISK = N'c:\asos\asos4.bak' WITH MOVE N'alarm_D' TO N'c:\asos\data\asos4.Mdf', MOVE N'alarm_L' TO N'c:\asos\data\asos4.LDF'"
osql -U sa -b -n -d asos4  -S (local) -Q "exec sp_change_users_login 'Update_One', 'Alarm', 'Alarm'"
pause