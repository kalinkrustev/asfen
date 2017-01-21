@echo off
echo proceed with restore of c:\asos\asos1.bak (ctr+c to abort)?
pause
md data
osql -U sa -b -n -d master -S (local) -Q "sp_addlogin 'Alarm', 'par4evi453'"
osql -U sa -b -n -d master -S (local) -Q "RESTORE DATABASE [asos1] FROM  DISK = N'c:\asos\asos1.bak' WITH MOVE N'alarm_D' TO N'c:\asos\data\asos1.Mdf', MOVE N'alarm_L' TO N'c:\asos\data\asos1.LDF'"
osql -U sa -b -n -d asos1  -S (local) -Q "exec sp_change_users_login 'Update_One', 'Alarm', 'Alarm'"
pause