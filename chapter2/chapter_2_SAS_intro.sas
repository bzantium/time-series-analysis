symbol1 i=join v=plus h=1 l=1 c=black;
symbol2 i=join v=star h=1 l=1 c=black;

*example 2.1;
data pop;
infile '.../timedata/population.txt';
input pop@@;
pop=round(pop/10000);
lnpop=log(pop);
t+1; t2=t*t; year=1959+t; run;

proc gplot data=pop;
plot pop*year=2; run;

proc reg data=pop;
model pop=t / dw;
output out=out1 p=pred1 r=residual1; run;

proc gplot data=out1;
plot residual1*year=1 / vref=0; run;

proc reg data=pop;
model pop=t t2 / dw;
output out=out2 p=pred2 r=residual2; run;

proc gplot data=out2;
legend1 across=2 position=(top left inside) value=('pop' 'forecast');
plot pop*year=2 pred2*year=1 / overlay legend=legend1; run;

proc gplot data=out2;
plot residual2*year=1 / vref=0; run;

proc reg data=pop;
model lnpop=t t2 / dw;
output out=out3 r=residual3; run;

proc gplot data=out3;
plot residual3*year=1 / frame vref=0; run;

*figure 2.8;
data fig2_8;
do t=1 to 100;
a1=-0.8; a2=1.4; pi=3.141592654; phi1=pi/8; phi2=3*pi/4;
first=a1*sin(pi*t/6+phi1);
second=a2*sin(pi*t/3+phi2);
z=first + second;
output; end; run;

symbol1 i=join v=dot h=0.5 c=black;
symbol2 i=join v=circle h=0.5 c=black;
symbol3 i=join v=star h=0.5 c=black;

proc gplot;
plot z*t=1 first*t=2 second*t=3 / overlay frame legend; run;

* example2.2;
data dept;
infile '.../timedata/depart.txt';
input dept@@;
lndept=log(dept); t+1;
date=intnx('month', '1JAN84'd, _n_-1);
format date monyy.;
mon=month(date);
if mon=1 then i1=1; else i1=0;
if mon=2 then i2=1; else i2=0;
if mon=3 then i3=1; else i3=0;
if mon=4 then i4=1; else i4=0;
if mon=5 then i5=1; else i5=0;
if mon=6 then i6=1; else i6=0;
if mon=7 then i7=1; else i7=0;
if mon=8 then i8=1; else i8=0;
if mon=9 then i9=1; else i9=0;
if mon=10 then i10=1; else i10=0;
if mon=11 then i11=1; else i11=0;
if mon=12 then i12=1; else i12=0; run;

symbol3 i=join v=point h=0.5 l=1 c=black;
proc gplot;
plot dept*date=3 / frame; run;

proc gplot;
plot lndept*date=3 / frame; run;

proc reg;
model lndept=t i1-i2/ noint dw;
output out=deptout r=residual; run;

proc gplot data=deptout;
plot residual*date=3 / frame vref=0; run;

proc arima data=deptout; identify var=residual; run;
proc univariate data=deptout; var residual; run;

*figure 2.12;
data exp;
b0=.2; b1=-12;
do t=1 to 60;
z1=exp(b0+b1/t); output;
end; run;

data gom;
b0=10; b1=.15; k=1;
do t=1 to 60;
z2=k*exp(-b0*exp(-b1*t)); output;
end; run;

data von;
b0=.95; b1=.09;
do t=1 to 60;
z3=(1-b0*exp((-b1)*t))**3; output;
end; run;

data pearl;
b0=5; b1=-0.2; k=1;
do t=1 to 60;
z4=k/(1+exp(b0+b1*t)); output;
end; run;

data fig2_12;
merge exp gom von pearl; run;

symbol1 i=join v=star h=0.5 c=black;
symbol2 i=join v=circle h=0.5 c=black;
symbol3 i=join v=dash h=0.5 c=black;
symbol4 i=join v=dot h=0.5 c=black;
legend1 across=1 position=(top left inside) value=('von' 'gom' 'pearl' 'exp');

proc gplot;
plot z3*t=1 z2*t=2 z4*t=3 z1*t=4 / overlay frame legend=legend1; run;

*example 2.3;
data catv;
infile '.../timedata/catv.txt';
input catv@@;
t+1; year=1969+t; k=70000000;
lncatv=log(catv); run;

symbol1 i=none v=plus h=1 c=black;
symbol2 i=none v='p' h=1 c=black;

proc gplot data=catv;
plot catv*year=1 / frame haxis=1970 to 1995 by 5; run;

proc gplot data=catv;
plot lncatv*year=1 / frame haxis=1970 to 1995 by 5; run;

proc reg data=catv;
model lncatv=year / dw;
output out=out1 pred=p; run;

data out1; set out1;
p1=k/(exp(p)+1);
residual=catv-p1; run;

legend2 across=2 position=(top left inside) value=('catv' 'forecast');

proc gplot data=out1;
plot catv*year=1 p1*year=2 / frame overlay legend=legend2
haxis=1970 to 1995 by 5; run;

proc gplot data=out1;
plot residual*year=1 / frame vref=0 haxis=1970 to 1995 by 5; run;

proc nlin data=catv method=gauss noitprint;
parms k=70000000 b0=2 b1=0;
temp=exp(b0+b1*t);
model catv=k/(1+temp);
output out=tvout p=pred r=residual; run;

*example 2.4;
proc autoreg data=dept;
model lndept=t i1-i2 / noint backstep nlag=13 dwprob;
output out=out1 r=residual; run;

symbol3 i=join v=none h=0.3 l=1 c=black;

proc gplot data=out1;
plot residual*date=3 / frame vref=0; run;

*trend model prediction;
data trendata;
input z@@; t+1;
cards;
23 25 27 34 38 47 49 39 57 59 63 64 69 78 73 89 83 84 86 92
. . . . . . . . . . . .
; run;

symbol1 i=join v=dot h=0.5 l=1 c=black;
symbol2 i=join v='p' h=0.5 l=1 c=black;
symbol3 i=join v='L' h=0.5 l=1 c=black;
symbol4 i=join v='U' h=0.5 l=1 c=black;

proc reg;
model z=t / dw;
output out=out1 p=zhat r=ehat l95=lci95 u95=uci95; 
run;

proc print; run;

proc gplot data=out1;
plot z*t=1 zhat*t=2 lci95*t=3 uci95*t=4/ href=21 overlay frame legend;
run;  
