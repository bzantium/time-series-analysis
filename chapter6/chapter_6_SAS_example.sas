symbol1 i=join v=none h=2 l=1 c=black;
symbol2 i=none v=star h=1 l=1 c=black;
symbol3 interpol=needle h=2 w=4;

*figure 6.1;

data fig6_1;
z1=0.0; y1=0.0;
do t=1 to 100;
a=rannor(1234);
z=-0.5*z1+a;
y=0.5*y1+a;
output;
z1=z; y1=y;
end; run;

proc gplot;
plot y*t=1 / frame vref=0.0 haxis=0 to 100 by 20; run;
plot z*t=1 / frame vref=0.0 haxis=0 to 100 by 20; run;
plot y1*y=2 / frame href=0 vref=0; run; quit;

*figure 6.6;
data fig6_6;
a1=0.0;
do t=1 to 100;
a=rannor(1234);
z=a-0.6*a1;
y=a+0.6+a1;
output;
a1=a;
end; run;

proc gplot;
plot z*t=1 / frame vref=0.0 haxis=0 to 100 by 20; run; 
plot y*t=1 / frame vref=0.0 haxis=0 to 100 by 20; run; quit;

*figure 6.4;
data fig6_4;
input phi;
do k=0 to 10;
acf=phi**k;
pacf=0;
if k=1 then do;
pacf=phi;
end; output;
end;

cards;
0.6
-0.6
; run;

proc sort data=fig6_4; by phi; run;

proc gplot; by phi;
plot acf*k=3 / haxis=0 to 10 by 2
			   vaxis=-1.0 to 1.0 by 0.2 frame; run;
plot pacf*k=3 / haxis=0 to 10 by 2
			 	vaxis=-1.0 to 1.0 by 0.2 frame; run; quit;

* figure 6.5;
data fig6_5;
input phi1 phi2;
acf1=0; acf2=0;
do k=0 to 10;
if k=0 then do;
acf=1;
pacf=0;
end;

if k=1 then do;
acf=phi1/(1-phi2);
pacf=acf;
end;

if k=2 then do;
acf=(phi1*phi1+phi2-phi2*phi2)/(1-phi2);
pacf=phi2;
end;

if k>=3 then do;
acf=phi1*acf1+phi2*acf2;
pacf=0;
end;
output;
acf2=acf1; acf1=acf;
end;

cards;
-0.8 -0.7
-0.5 0.2
0.7 0.2
1.3 -0.5
; run;

proc sort data=fig6_5; by phi1 phi2; run;
proc gplot; by phi1 phi2;
plot acf*k=3 / haxis=0 to 10 by 2
			   vaxis=-1.0 to 1.0 by 0.2 frame; run;
plot pacf*k=3 / haxis=0 to 10 by 2
			    vaxis=-1.0 to 1.0 by 0.2 frame; run; quit;

*figure 6.8;
data fig6_8;
input theta;
do k=0 to 10;
kk=2*(k+1);

if k=0 then do;
acf=1; pacf=0;
end;

if k=1 then do;
acf=-theta/(1+theta*theta);
pacf=-(theta**k)*(1-theta*theta)/(1-theta**k);
end;

if k>=2 then do;
acf=0;
pacf=-(theta**k)*(1-theta*theta)/(1-theta**kk);
end; output;
end;

cards;
0.9
-0.6
; run;

proc sort data=fig6_8; by theta; run;

proc gplot; by theta;
plot acf*k=3 / haxis=0 to 10 by 2
			   vaxis=-1.0 to 1.0 by 0.2 frame; run;
plot pacf*k=3 / haxis=0 to 10 by 2
			    vaxis=-1.0 to 1.0 by 0.2 frame; run; quit;

*figure 6.9;
data fig6_9;
input theta1 theta2;
do k=0 to 10;
if k=0 then acf=1;
if k=1 then acf=-theta1*(1-theta2)/(1+theta1*theta1+theta2*theta2);
if k=2 then acf=-theta2/(1+theta1*theta1+theta2*theta2);
if k>=3 then acf=0;
output;
end;

cards;
-1.6 -0.7
0.8 -0.7
; run;

proc sort data=fig6_9; by theta1 theta2; run;
proc gplot data=fig6_9; by theta1 theta2;
plot acf*k=3 / haxis=0 to 10 by 2
			   vaxis=-1.0 to 1.0 by 0.2 frame; run; quit;

*figure 6.10;
data fig6_10;
input phi theta;
acf1=0;
do k=0 to 10;
if k=0 then acf=1;
if k=1 then acf=(phi-theta)*(1-phi*theta)/(1+theta*theta-2*phi*theta);
if k>=2 then acf=phi*acf1;
output;
acf1 = acf;
end;

cards;
-0.9 -0.5
0.8 -0.7
; run;

proc gplot data=fig6_10; by phi theta;
plot acf*k=3 / haxis=0 to 10 by 2
			   vaxis=-1.0 to 1.0 by 0.2 frame; run; quit;
