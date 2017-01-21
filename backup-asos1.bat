@echo off
echo proceed with data backup to i:\work\sip\asos1.bak ?
pause
osql -U sa -b -P 123 -n -d master -S (local) -Q "backup database [asos1] TO DISK = N'i:\work\sip\asos1.bak' with init"
pause