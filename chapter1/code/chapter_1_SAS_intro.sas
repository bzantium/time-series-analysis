symbol1 i=join v=none h=2 l=1 c=black;
symbol2 i=none v=star h=1 l=1 c=black;

*figure 1.1;
data fig1_1;
do t=1 to 100;
z=5000+20*rannor(1234);
output;
end; run;

data fig1_1;
set fig1_1;
date=intnx('month', '1jan80'd, _n_-1);
format date monyy.; run;

proc gplot;
plot z*date=1 / vref=5000; run;

*figure 1.2;
data fig1_2;
do t=1 to 100;
x=0.5*t;
z=0.5*t + rannor(1234);
output;
end; run;

data fig1_2; 
set fig1_2;
date = intnx('month', '1jan80'd, _n_-1);
format date monyy.; run;

proc gplot;
plot z*date=1 x*date=1 / overlay; run;

*figure 1.3;
data fig1_3;
do t=1 to 120;
a = rannor(2483);
z = 10 + 3 * sin((2*3.14*t)/12) + 0.8 * a;
output;
end; run;

data fig1_3;
set fig1_3;
date = intnx('month', '1jan85'd, _n_-1);
format date monyy.; run;

proc gplot;
plot z*date=1 / vref=10; run;

*figure 1.4;
data fig1_4;
infile '../timedata/depart.txt'; 
input z@@;
logz=log(z);
date=intnx('month','1jan84'd,_n_-1);
format date monyy.;
x=2.701573+0.000409*date;  
run; 

proc gplot;
plot logz*date=1 x*date=1 / overlay;  run; 

*figure 1.5;
data fig1_5;
infile '../timedata/koreapass.txt'; 
input z@@;
date = intnx('month', '1jan81'd, _n_-1);
format date monyy.; run;

proc gplot;
plot z*date=1 / haxis='01jan81'd to '01jan90'd by year;
run;

*figure 1.6;
data fig1_6;
do t=1 to 120;
a = rannor(4321);
if t le 60 then x=0.5*t;
else x = 2 * (t-46);
z = x + a;
output;
end; run;

data fig1_6;
set fig1_6;
date = intnx('month', '1jan85'd, _n_-1);
format date monyy.; run;

proc gplot;
plot z*date=1 / href='1jan90'd;
run;

