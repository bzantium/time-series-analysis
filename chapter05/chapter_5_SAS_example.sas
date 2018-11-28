symbol1 i=join v=none h=2 l=1 c=black;
symbol2 i=none v=star h=1 l=1 c=black;

*example 5.1;
data ex5_1;
z1=15;
do t=1 to 100;
a=rannor(139672);
z=17-0.7*z1+a;
z2=lag2(z);
output;
z1=z;
end; run;

proc gplot;
plot z*t=1/frame vref=10 haxis=0 to 100 by 10; run; quit;

proc gplot;
plot z1*z=2/frame vref=10 href=10; run; quit;

proc gplot;
plot z2*z=2/frame vref=10 href=10; run; quit;

proc arima;
identify var=z nlag=10; run;

*figure 5.2;
data fig5_2;
do t=1 to 300;
a=rannor(23456);
z=a;
output;
end; run;

proc gplot;
plot z*t=1/frame vref=0.0 haxis=0 100 200 300; run; quit;

*figure 5.3;
data fig5_3;
z1=0;
do t=1 to 300;
a=rannor(63739);
z=z1+a;
output;
z1=z;
end; run;

proc gplot data=fig5_3;
plot z*t=1/frame haxis=0 100 200 300; run; quit;

*figure 5.4;
data fig5_4;
z1=0;
do t=1 to 300;
a=rannor(52517);
z=0.2+z1+a;
output;
z1=z;
end; run;

proc gplot;
plot z*t=1/frame haxis=0 100 200 300; run; quit;


*example 5.2;
data ex5_2;
a1=0;
do t=1 to 100;
a=rannor(45678);
z=a+0.8*a1;
z1=lag1(z); z2=lag2(z);
output;
a1=a;
z4=z-0.4991*z1;
z3=z2-0.4991*z1;
end; run;

proc gplot;
plot z*t=1/frame vref=0.0 haxis=0 20 40 60 80 100; run; quit;

proc gplot;
plot z4*z3=2/frame vref=0.0 href=0.0; run; quit;

proc arima;
identify var=z nlag=24; run;

