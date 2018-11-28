symbol1 i=join v=none l=1 c=black;
symbol2 i=join v=none l=3 c=blue;

data pass;
infile '../timedata/tourist.txt';
input pass@@;
date=intnx('month', '1dec80'd, _n_); format date monyy.;
lpass=log(pass); lpass1=dif(lpass);
lpass12=dif12(lpass); lpass121=dif(lpass12); run;

proc gplot data=pass;
plot pass*date=1 / frame haxis='01jan81'd to '01jan92'd by year; run;
plot lpass*date=1 / frame haxis='01jan81'd to '01jan92'd by year; run;
plot lpass12*date=1 / frame haxis='01jan81'd to '01jan92'd by year; run;
plot lpass121*date=1 / frame vref=0 haxis='01jan81'd to '01jan92'd by year; 
run; quit;

proc arima data=pass;
identify var=lpass(12) nlags=36; run;
identify var=lpass(1) nlags=36; run;
identify var=lpass(1,12) nlags=36; run;
estimate q=(1)(12) plot noint; run;
forecast lead=0 noprint out=res; run; quit;
proc print data=res; run;
data res; set res; n=_n_; run;

proc gplot data=res;
plot residual*n=1 / frame vref=0; run; quit;
