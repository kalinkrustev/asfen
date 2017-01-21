@echo off
echo proceed with data backup to c:\asos\asosdb.bak ?
pause
osql -U sa -b -P 710511par4evi453 -n -d master -S (local) -Q "backup database [asosdb] TO DISK = N'c:\asos\asosdb.bak' with init"
pause