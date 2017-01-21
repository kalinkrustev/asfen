del ModelSupport\*.* /Q
rd ModelSupport

md ..\CoreLib\lib
copy rtcCoreFpc.lpk ..\CoreLib
copy lib\*.* ..\CoreLib\lib\*.*
copy Help\*.chm ..\CoreLib

del *.local
del *.identcache
del *.dcpil
del *.pdb
del *.bpl
del *.dcp
del *.rsp
del *.dcu
del *.~*
del *.cfg
del *.dof
del *.bak
del *.ppu
del *.o
del rtccorefpc.pas
del lib\*.* /Q
rd lib