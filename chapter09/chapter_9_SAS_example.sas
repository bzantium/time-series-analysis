symbol1 i=join v=none l=1 c=black;
symbol2 i=join v=none l=3 c=blue;

*example 9.1;
data eg9_1;
infile '../timedata/ex8_7.txt';
input z @@; time+1; run;

proc arima;
identify var=z nlag=24 noprint; run;
estimate p=1 noprint; run;

forecast lead=25 out=fore; run; quit;
proc print data=fore; run;
data fore; set fore; time=_n_; if z ne . then forecast=.; run;

proc gplot;
plot z*time=1 forecast*time=2 / frame legend overlay haxis=0 to 125 by 25;
run; quit;

*example 9.5;
data ex9_5;
infile '../timedata/ex9_5.txt';
input z@@; time+1;
run;

proc arima;
identify var=z(1) nlag=15 noprint; run;
estimate q=1 method=ml noint noprint; run;
forecast lead=50 out=fore; run; quit;

proc print data=fore; run;
data fore; set fore; time=_n_; if z ne . then forecast=.; run;

proc gplot;
plot z*time=1 forecast*time=2 / frame legend overlay haxis=0 to 350 by 50;
run; quit;
