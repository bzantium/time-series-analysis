symbol1 i=join v=none l=1 c=black;
symbol2 i=join v=none l=3 c=blue;

*example 12.1;
title1 'Intervention Analysis for Ozone Concentration';
title2 '(Box and Tia, JASA 1975 P.70)';

data air;
infile '../timedata/ozone.txt';
input ozone @@;
label ozone = 'Ozone Concentration'
	  x1 = 'Intervention for post 1960 period'
	  summer = 'Summer Months Intervention'
	  winter = 'Winter Months Intervention';

date = intnx('month', '31dec1954'd, _n_);
format date monyy.;
month = month(date);
year = year(date);

x1 = year >= 1960;
summer = (5<month<11) * (year>1965);
winter = (year>1965) - summer;
run;

proc gplot data=air;
plot ozone*date=1; run;

proc arima data=air;
identify var=ozone nlag=24;
where year<1960; run;
identify var=ozone nlag=24; run;
estimate q=(1)(12) noconstant;
forecast lead=12 id=date interval=month out=res; run;

proc arima;
identify var=residual nlag=24; run;
data res; set res; time=_n_; run;

proc gplot;
plot residual*time=1 / frame vref=0; run;

proc arima data=air;
identify var=ozone(12) crosscorr=(x1(12) summer winter) nlag=36; run;
estimate q=(1)(12) input=(x1 summer winter) noconstant method=ml 
noint noprint; run;
forecast lead=12 id=date interval=month out=res; run;
data res; set res; time=_n_; run;

proc gplot;
plot residual*time=1 / frame vref=0; run;
proc arima;
identify var=residual nlag=36; run;

*example 12.2;
title1 'Intervention Analysis for Construction of 2million houses';
data const;
infile '../timedata/const.txt';
input area@@;
month = intnx('month', '01jan1980'd, _n_-1);
if('01feb1989'd<month<'01aug1991'd) then phase=1; else phase=0;
format month monyy.; run;

proc gplot data=const;
plot area*month=1; run;
data const1; set const;
larea=log(area); run;
proc gplot data=const1;
plot larea*month=1; run;

proc arima data=const1;
identify var=larea(1,12);
where month<='1jan1989'd; run;
estimate q=(1)(12); run;

proc arima data=const1;
identify var=larea(1,12) crosscorr=(phase(1,12)); run;
estimate q=(1)(12) input=(1$ phase) noconstant plot; run;
forecast lead=1 interval=month id=month out=const1; run;

data const2;
set const1;
expfor=exp(forecast);
area=exp(larea); run;
proc print data=const2; run;
proc gplot data=const1;
plot residual*month / vref=0; run; quit;
proc gplot data=const2;
plot expfor*month=2 area*month=1 / overlay legend; run; quit;

*example 12.4;
title1 'Outlier detection for the Ozone concentration';
data air;
infile '../timedata/ozone.txt';
input ozone@@;
date = intnx('month', '31dec1954'd, _n_);
format date monyy.;
run;

proc arima data=air;
identify var=ozone(12) noprint;
estimate q=(1)(12) noint method=ml;
outlier maxnum=3 alpha=0.01;
run;
