symbol1 v=none i=join h=2 l=1 c=black;

*example 8.6;
data gas;
infile '../timedata/gas.txt';
input gas rate @@; time+1; run;

proc gplot data=gas;
plot gas*time=1 / frame; run;
proc arima;
identify var=gas nlag=24;
estimate p=3 method=cls;
estimate p=3 method=ml noint plot;
forecast lead=0 out=res; run;
data res; set res; time=_n_; run;

proc gplot;
plot residual*time=1 / frame vref=0; run;
proc arima;
identify var=residual nlag=24; run;

*example 8.7;
data ex8_7;
infile '../timedata/ex8_7.txt';
input z @@; time+1; run;

proc gplot;
plot z*time=1 / frame haxis=0 to 100 by 10;
run; quit;

proc arima;
identify var=z nlag=24; run;
estimate p=1 plot; run;
forecast lead=0 out=res;
run; quit;

proc print data=res; run;
data res; set res; time=_n_; run;

proc gplot;
plot residual*time=1 / frame vref=0; run; quit;

proc univariate data=res plot;
var residual; run;

proc arima data=ex8_7;
identify var=z nlag=12 noprint; run;
estimate p=2; run;
estimate p=1 q=1; run; quit;

*example 8.8;
data stock;
infile '../timedata/elecstock.txt';
input stock @@;
week+1; difstock=dif(stock); run;
proc gplot data=stock;
plot stock*week=1 / frame; run;
plot difstock*week=1 / frame vref=0; run; quit;

proc arima data=stock;
identify var=stock nlags=24; run;

identify var=stock stationarity=(adf dlag=1) nlag=24;
identify var=stock(1) nlags=24; run;

*example 8.9;
data female;
infile '../timedata/female.txt';
input female @@;
date=intnx('month', '1dec82'd, _n_-1);
format date monyy.;
dfemale = dif(female); run;

proc gplot data=female;
plot female*date=1 / frame; run;
plot dfemale*date=1 / frame vref=0; run;
proc arima data=female;
identify var=female stationarity=(adf dlag=1) nlag=24; run;
identify var=female(1) nlag=24; run;
estimate method=uls; run;
