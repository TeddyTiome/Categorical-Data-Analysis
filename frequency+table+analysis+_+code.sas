libname project 'G:\project';
/*transform satisfaction into binomial*/
data project.database1_clear;
set project.database1_clear;
if satis=1 then satiselect=0;
else satiselect=1;
run;

/*freqency analysis with overall satisfaction and dinner_importance*/
proc sql;
create table project.freqbase1 as
select count(*) as count,satiselect,dinner_impor from project.database1_clear
group by satiselect,dinner_impor;
run; 
proc freq data=project.freqbase1;
weight count;
tables satiselect*dinner_impor/nopct nocol norow chisq expected measures riskdiff;
run;

/*freqency analysis with overall satisfaction and house_area*/
proc sql;
create table project.freqbase2 as
select count(*) as count,satiselect,house_area from project.database1_clear
group by satiselect,house_area;
run; 
proc freq data=project.freqbase2;
weight count;
tables satiselect*house_area/nopct nocol norow chisq expected measures riskdiff;
run;
