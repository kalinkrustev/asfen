@echo off
echo proceed with deletion of asos4 database (ctr+c to abort)?
pause
osql -U sa -b -n -d master -S (local) -Q "drop database asos4"
pause