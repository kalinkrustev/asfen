@echo off
echo proceed with deletion of asos1 database (ctr+c to abort)?
pause
osql -U sa -b -n -d master -S (local) -Q "drop database asos1"
pause