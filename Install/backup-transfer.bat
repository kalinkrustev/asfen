@echo off
echo proceed with data backup to c:\asos\transfer.bak (ctr+c to abort)?
pause
osql -U sa -b -n -d master -S (local) -Q "backup database [transfer] TO DISK = N'c:\asos\transfer.bak' with init"
pause