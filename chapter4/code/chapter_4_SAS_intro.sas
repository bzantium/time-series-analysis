symbol1 l=1 i=join v=none c=black;
symbol2 l=1 i=join v=none c=red;
symbol3 l=1 i=join v=none c=blue;
symbol4 l=1 i=join v=none c=green;
symbol5 l=1 i=join v=none c=orange;
symbol6 l=1 i=join v=none c=purple;
symbol7 l=1 i=join v=none c=pink;

symbol2 h=0.5 i=join v=none c=red;
symbol3 l=3 i=join v=none c=blue;

*example 4.1;
data food;
infile '../timedata/food.txt';
input food@@;
date=intnx('month', '1jan80'd, _n_-1);
format date monyy.; t+1;
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
if mon=12 then i12=1; else i12=0;

proc reg data=food;
model food=t/dw;
output out=trendata p=trend;
id date; run;

data adtrdata;
set trendata;
adjtrend=food/trend; run;

proc print data=adtrdata;
run;

proc autoreg data=adtrdata;
model adjtrend=i1-i12/noint nlag=13 dwprob backstep;
output out=seasdata p=seasonal; run;

data all;
set seasdata;
keep date food trend seasonal irregula fitted;
irregula=adjtrend/seasonal;
fitted=trend*seasonal; run;

proc print data=all;
var date food trend seasonal irregula fitted; run;

proc arima data=all;
identify var=irregula nlag=12; run;

proc univariate data=all;
var irregula; run;

proc gplot data=all;
plot food*date=1 trend*date=2/frame overlay legend
haxis='1jan80'd to '1jan92'd by year; run;

plot seasonal*date=1/frame; run;
plot irregula*date=1/frame vref=1.0; run;
plot food*date=1 fitted*date=2/frame overlay legend; run;

*example 4.2;
data index;
infile '../timedata/mindex.txt';
input index@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; t+1; run;

proc expand data=index out=index1;
convert index=m3/transformout=(cmovave 3 trim 1);
convert index=m7/transformout=(cmovave 7 trim 3);

proc gplot data=index1;
plot index*date=1 m3*date=3/frame overlay legend
haxis='1jan86'd to '1apr94'd by year; run;
plot index*date=1 m7*date=3/frame overlay legend; run;

*example 4.3;
data food;
infile '../timedata/food.txt';
input food@@;
date=intnx('month', '1jan80'd, _n_-1);
format date monyy.; t+1;
mon=month(date); run;

proc expand data=food out=food2;
convert food=tc/transformout=(cd_tc 12);
convert food=s/transformout=(cda_s 12);
convert food=i/transformout=(cda_i 12);
convert food=sa/transformout=(cda_sa 12);
run;

proc gplot data=food2;
plot food*date=1 tc*date=3/frame overlay legend
haxis='1jan80'd to '1jan92'd by year; run;
plot food*date=1 s*date=3/frame overlay legend; run;
plot food*date=1 i*date=3/frame overlay legend; run;
plot food*date=1 sa*date=3/frame overlay legend; run;

data raw;
input z@@;
t+1;
cards;
10 12 8 12 7 5 8 7 9 10 3 6 8 4 9 12 8 12 13 9
;
run;

data ma; set raw;
L1=lag1(z); L2=lag2(z); L3=lag3(z); L4=lag4(z); L5=lag5(z); L6=lag6(z);
L7=lag7(z); L8=lag8(z); L9=lag9(z); L10=lag10(z); L11=lag11(z);
ma3=(z+L1+L2)/3;
LL1=lag1(ma3); LL2=lag2(ma3); LL3=lag3(ma3); LL4=lag4(ma3);
ma33=(ma3+LL1+LL2)/3;
ma35=(ma3+LL1+LL2+LL3+LL4)/5;
ma12=(z+L1+L2+L3+L4+L5+L6+L7+L8+L9+L10+L11)/12;
LLL1=lag1(ma12);
ma122=(ma12+LLL1)/2;
hendersn=-0.073*L4+0.294*L3+0.558*L2+0.294*L1-0.073*z; run;

data ma3; set ma; t=_n_-1; keep t ma3; run;
data ma33; set ma; t=_n_-2; keep t ma33; run;
data ma35; set ma; t=_n_-3; keep t ma35; run;
data ma12; set ma; t=_n_-6; keep t ma12; run;
data ma122; set ma; t=_n_-6; keep t ma122; run;
data hendersn; set ma; t=_n_-2; keep t hendersn; run;
data all; merge raw ma3 ma33 ma35 ma12 ma122 hendersn; by t; if t>0; run;
proc print; var t z ma3 ma33 ma35 ma12 ma122 hendersn; run;
proc gplot data=all; 
plot z*t=1 ma3*t=2 ma33*t=3 ma35*t=4 ma12*t=5 ma122*t=6 hendersn*t=7/frame overlay legend; run;

*example 4.4;
data food;
infile '../timedata/food.txt';
input food@@;infile '../timedata/food.txt';
input food@@;
date=intnx('month', '1jan80'd, _n_-1);
format date monyy.; t+1;
mon=month(date);
run;

proc X12 data=food seasons=12 start=jan1980;
var food;
x11;
output out=foodout a1 d10 d11 d12 d13; run;

proc print data=foodout;
run;

proc arima data=foodout;
identify var=food_d13 nlag=24;
run;

data foodout; set foodout(rename=(_date_=date food_a1=food food_d10=d10 food_d11=d11 food_d12=d12 food_d13=d13));
label food="" d10="" d11="" d12="" d13="";
run;

proc gplot data=foodout;
title "Final seasonal factors";
plot d10*date=1/frame; run;
title "Original time series vs Sasonally adjusted series";
plot food*date=1 d11*date=2/frame overlay; run;
title "Original time series vs Trend cycle";
plot food*date=1 d12*date=2/frame overlay; run;
title "Irregular component";
plot d13*date=1/frame vref=1.0; run;
