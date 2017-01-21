@echo off
echo proceed with deletion of asos3 database (ctr+c to abort)?
pause
osql -U sa -b -n -d master -S (local) -Q "drop database asos3"
pause