@echo off
echo proceed with data backup to c:\asos\asos2.bak (ctr+c to abort)?
pause
osql -U sa -b -n -d master -S (local) -Q "backup database [asos2] TO DISK = N'c:\asos\asos2.bak' with init"
pause