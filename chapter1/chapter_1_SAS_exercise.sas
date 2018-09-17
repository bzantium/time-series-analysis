symbol1 i=join v=star h=1 l=1 c=black;
symbol2 i=join v=star h=1 l=1 c=red;
symbol3 i=join v=star h=1 l=1 c=blue;
symbol4 i=join v=star h=1 l=1 c=green;
symbol5 i=join v=none h=1 l=1 c=red;

*exercise 1.5;
data ex1_5;
do t=1 to 100;
z1=100+rannor(1234);
z2=500+rannor(1234);
z3=100+10*rannor(1234);
z4=100+t*rannor(1234);
output;
end; run;

legend1 across=2 position=(top left inside) value=('a' 'b' 'c' 'd');

proc gplot;
plot z1*t=1 z2*t=2 z3*t=3 z4*t=4 / overlay frame legend=legend1;
run;

legend2 across=2 position=(top left inside) value=('Z' 'E[Z]');

*exercise 1.6.1;
data ex1_6_1;
do t=1 to 100;
e=100;
z=100+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.2;
data ex1_6_2;
do t=1 to 100;
e=100+t;
z=100+t+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.3;
data ex1_6_3;
do t=1 to 100;
e=100+t+2*t**2;
z=100+t+2*t**2+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.4;
data ex1_6_4;
pi=3.14159265;
do t=1 to 100;
e=100+sin(2*pi*t/12)+cos(2*pi*t/12);
z=100+sin(2*pi*t/12)+cos(2*pi*t/12)+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.5;
data ex1_6_5;
pi=3.14159265;
do t=1 to 100;
e=100+sin(2*pi*t/4)+cos(2*pi*t/4);
z=100+sin(2*pi*t/4)+cos(2*pi*t/4)+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.6;
data ex1_6_6;
pi=3.14159265;
do t=1 to 100;
e=100+0.3*t+sin(2*pi*t/12)+cos(2*pi*t/12);
z=100+0.3*t+sin(2*pi*t/12)+cos(2*pi*t/12)+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.7;
data ex1_6_7;
pi=3.14159265;
do t=1 to 100;
e=100+sin(2*pi*t/12)+cos(2*pi*t/12)+0.8*sin(2*pi*t/6)+0.7*cos(2*pi*t/6);
z=100+sin(2*pi*t/12)+cos(2*pi*t/12)+0.8*sin(2*pi*t/6)+0.7*cos(2*pi*t/6)+rannor(1234);
output;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.8;
data ex1_6_8;
z0=250;
do t=1 to 100;
e=250;
z=100+0.6*z0+rannor(1234);
output;
z0=z;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.9;
data ex1_6_9;
z0=50+rannor(1234);
z1=100+0.5*z0+rannor(1234);
do t=1 to 100;
z=100+0.5*z1-0.7*z0+rannor(1234);
e=100/1.2;
output;
z0=z1;
z1=z;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.10;
data ex1_6_10;
eps0=rannor(1234);
do t=1 to 100;
eps=rannor(1234);
z=100+eps+0.8*eps0;
output;
eps0=eps;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.6.11;
data ex1_6_11;
eps0=rannor(1234);
do t=1 to 100;
eps=rannor(1234);
z=100+eps-0.8*eps0;
output;
eps0=eps;
end; run;

proc gplot;
plot z*t=1 e*t=5 / overlay frame legend=legend2;
run;

*exercise 1.7_1;
data ex1_7_1;
    infile './timedata/female.txt';
    input z@@;
    run;

data ex1_7_1;
    set ex1_7_1;
    do t=1 to _n_;
    end; run;

proc sgplot data=ex1_7_1;
    series x=t y=z;
    xaxis label='months';
    yaxis label='female workers(unit: 100thou)';
run;

*exercise 1.7_2;
data ex1_7_2;
    infile './timedata/build.txt';
    input z@@;
    run;

data ex1_7_2;
    set ex1_7_2;
    do t=1 to _n_;
    end; run;
    
proc sgplot data=ex1_7_2;
    series x=t y=z;
    xaxis label='months';
    yaxis label='permission';
run;

*exercise 1.7.3;
data ex1_7_3;
    infile './timedata/export.txt';
    input z@@;
    run;

data ex1_7_3;
    set ex1_7_3;
    do t=1 to _n_;
    end; run;
    
proc sgplot data=ex1_7_3;
    series x=t y=z;
    xaxis label='months';
    yaxis label='sales(unit: $100m)';
run;

*exercise 1.7.4;
data ex1_7_4;
    infile './timedata/usapass.txt';
    input z@@;
    run;

data ex1_7_4;
    set ex1_7_4;
    do t=1 to _n_;
    end; run;
    
proc sgplot data=ex1_7_4;
    series x=t y=z;
    xaxis label='months';
    yaxis label='flight customers(unit: 1thou)';
run;
