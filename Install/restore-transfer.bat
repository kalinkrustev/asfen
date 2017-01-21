@echo off
echo proceed with restore of c:\asos\transfer.bak (ctr+c to abort)?
pause
md data
osql -U sa -b -n -d master -S (local) -Q "sp_addlogin 'Alarm', 'par4evi453'"
osql -U sa -b -n -d master -S (local) -Q "RESTORE DATABASE [transfer] FROM  DISK = N'c:\asos\transfer.bak' WITH MOVE N'transfer_D' TO N'c:\asos\data\transfer.Mdf', MOVE N'transfer_L' TO N'c:\asos\data\transfer.LDF'"
osql -U sa -b -n -d transfer  -S (local) -Q "exec sp_change_users_login 'Update_One', 'Alarm', 'Alarm'"
pause