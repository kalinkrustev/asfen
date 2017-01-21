osql -U sa -b -P 123 -n -d asos1 -S (local) -Q "create table tblCrisis(id varchar(10) not null,name varchar(150) not null,LastUser int,LastChange datetime, primary key (id))"
pause