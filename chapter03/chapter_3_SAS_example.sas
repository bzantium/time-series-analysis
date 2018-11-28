symbol1 l=1 i=join v=none c=black;
symbol2 l=3 i=join v=none c=blue;
symbol3 l=1 i=join v=point c=black;
symbol4 l=1 i=join v=point c=blue;

*example 3.1: Simple Exponential Smoothing;
data mindex;
infile '../timedata/mindex.txt';
input mindex@@;
date = intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

data sse;
input w sse @@;
cards;
0.1		1460.371	0.2		1012.6626	0.3		779.00814	0.4		648.75022
0.5		570.4980	0.6		521.570		0.7		491.440		0.8		475.1534
0.81	474.1885	0.82	473.3390	0.83	472.6044	0.84	471.9842
0.85	471.478		0.86	471.0859	0.87	470.8079	0.88	470.6749
0.89	470.5947	0.90	470.661		0.91	470.842		0.92	471.1406
0.93	471.5567	0.94	472.0919	0.95	472.7475	0.96	473.525
0.97	474.4263	0.98	475.4532	0.99	476.6078
; run;

proc gplot data=sse;
plot sse*w=1 / frame
haxis=0.7 to 1.0 by 0.05
vaxis=470 to 500 by 5;
run;

proc print; var w sse; run;

proc forecast data=mindex
interval=month method=expo out=out1 outest=est1
weight=0.89 trend=1 lead=6 outfull outresid;
id date; var mindex; run;

proc print data=est1; run;

data out11;
set out1;
if _type_='RESIDUAL';
error=mindex;
run;

data out1; set out1;
if _type_='RESIDUAL' then delete; run;

proc gplot data=out1;
plot mindex*date=_type_ / frame
haxis='1jan86'd to '1jan95'd by year
vaxis=0 to 30 by 5; run;

proc print data=out1; run;

proc gplot data=out11; plot error*date=1 / frame vref=0; run;
proc arima data=out11; identify var=error; quit; run;
proc univariate data=out11; var error; run;

proc forecast data=mindex
interval=month method=expo out=out2 outest=est2
weight=0.2 trend=1 lead=6 outfull outresid;
id date; var mindex; run;

proc print data=est2; run;
proc print data=out2; run;
data out22; set out2;
if _type_='RESIDUAL'; error=mindex; run;

data out2; set out2;
if _type_='RESIDUAL' then delete; run;

proc gplot data=out2;
plot mindex*date=_type_ / frame
haxis='1jan86'd to '1jan95'd by year
vaxis=0 to 30 by 5; run;

proc gplot data=out22; 
plot error*date=1 / frame vref=0; run;

proc arima data=out22; identify var=error; run;
proc univariate data=out22; var error; run;

*example 3.2: Double Exponential Smoothing;
data stock;
infile '../timedata/stock.txt';
input stock@@;
date=intnx('month', '1jan84'd, _n_-1);
format date monyy.; run;

proc forecast data=stock
interval=month method=expo out=out3 outest=est3
weight=0.6 trend=2 lead=6 outfull outresid;
id date; var stock; run;

proc print data=est3; run;
proc print data=out3; run;

data out33;
set out3;
if _type_='RESIDUAL';
error=stock; run;

data out3;
set out3;
if _type_='RESIDUAL' then delete; run;

proc gplot data=out3;
plot stock*date=_type_ / frame
haxis='1jan86'd to '1dec93'd by year; run;

proc gplot data=out33;
plot error*date=1 / frame
vref=0; run;

proc arima data=out33; identify var=error; run;
proc univariate data=out33; var error; run;

*example 3.3: Winters Seasonal Exponential Smoothing;
data pass;
infile '../timedata/koreapass.txt';
input pass@@;
date=intnx('month', '1jan81'd, _n_-1);
format date monyy.; run;

proc forecast data=pass
interval=month method=addwinters out=out4 outest=est4
weight=0.4 weight=0.1 weight=0.7 lead=12 seasons=12
outfull outresid;
id date; var pass; run;

proc print data=est4; run;
proc print data=out4; run;
data out44;
set out4;
if _type_='RESIDUAL';
error=pass; run;

data out4;
set out4;
if _type_='RESIDUAL' then delete; run;

proc gplot data=out4;
plot pass*date=_type_ / frame
haxis='1jan81'd to '1dec92'd by year; run; quit;

proc gplot data=out44;
plot error*date=1 / frame vref=0; run;
proc arima data=out44; identify var=error; run;
proc univariate data=out44; var error; run;

proc forecast data=pass
interval=month method=winters out=out5 outest=est5
weight=0.5 weight=0.1 weight=0.4 lead=12 seasons=12
outfull outresid;
id date; var pass; run;

proc print data=est5; run;
proc print data=out5; run;
data out55; 
set out5; 
if _type_='RESIDUAL'; error=pass; run;

data out5;
set out5;
if _type_='RESIDUAL' then delete; run;

proc gplot data=out5;
plot pass*date=_type_ / frame
haxis='1jan81'd to '1dec92'd by year; run;

proc gplot data=out55;
plot error*date=1 / frame vref=0; run;
proc arima data=out55; identify var=error; run;
proc univariate data=out55; var error; run;
