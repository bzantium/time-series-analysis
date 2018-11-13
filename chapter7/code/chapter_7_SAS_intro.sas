symbol1 v=none i=join h=2 l=1 c=black;

*example 7.1;
data depart;
infile '../timedata/depart.txt'; input z@@;
logz=log(z); dif1=dif1(logz);
dif1_12=dif12(dif1);
date=intnx('month', '1JAN84'd, _n_-1);
format date Monyy.; run;

proc print data=depart; run;

proc gplot;
label z='depart';
label logz='lnZ';
label dif1='d_lnZ';
label dif1_12='dd12_lnZ';
plot z*date=1 / frame haxis='1JAN84'd to '1JAN89'd by year; run;
plot logz*date=1 / frame haxis='1JAN84'd to '1JAN89'd by year; run;
plot dif1*date=1 / frame vref=0
				   haxis='1JAN84'd to '1JAN89'd by year
				   vaxis=-1.5 to 1.5 by 0.5; run;
plot dif1_12*date=1 / frame vref=0
					  haxis='1JAN85'd to '1JAN89'd by year
					  vaxis=-0.10 to 0.10 by 0.05; run; quit;

*figure 7.3;
data fig7_3;
infile '../timedata/interest.txt';
input interest@@;
date=intnx('month', '1APR82'd, _n_-1); 
format date Monyy.; run;

proc gplot data=fig7_3;
plot interest*date=1 / href='01DEC91'd; run; quit;

*figure 7.4;
data fig7_4;
z1=0;
do t=1 to 300;
a=rannor(63739);
z=z1+a; dif1=dif1(z);
output;
z1=z;
end; run;

proc gplot data=fig7_4;
label dif1='d_Z';
label t='time';
plot z*t=1;
plot dif1*t=1; run; quit;

*figure 7.7;
data fig7_7;
z1=0; z2=0; a1=0;
do t=1 to 300;
a=rannor(36975); z=1.8*z1-0.8*z2+a-0.5*a1;
dif1=dif1(z); output; z2=z1; z1=z; a1=a;
end; run;

symbol1 i=join v=none h=2 l=1;
proc gplot;
label t='time';
plot z*t=1 / frame; run; quit;

proc arima;
identify var=z nlag=24; run;
proc gplot;
label dif1='d_Z';
plot dif1*t=1 / frame vref=0.0 haxis=2 100 200 300;
run; quit;

proc arima;
identify var=dif1 nlag=24;
run; quit;
