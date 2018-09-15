*exercise 2.2;
data ex2_2;
input z@@; t+1;
cards;
303 298 303 314 303 314 310 324 317 326 323 324 331 330 332
. . . . .
; run;

proc reg;
model z=t / dw;
output out=out1 p=zhat r=ehat l95=lci95 u95=uci95;
run;

proc print;
run;

symbol1 i=join v=dot h=0.5 l=1 c=black;
symbol2 i=join v='p' h=0.5 l=1 c=black;
symbol3 i=join v='L' h=0.5 l=1 c=black;
symbol4 i=join v='U' h=0.5 l=1 c=black;

proc gplot data=out1;
plot z*t=1 zhat*t=2 lci95*t=3 uci95*t=4/ href=21 overlay frame legend;
run;

*exercise 2.9;
data ex2_9_1;
infile '../timedata/book.txt';
input z@@; t+1;
run;

data ex2_9_2;
input z t;
cards;
. 42
;
run;

data ex2_9;
set ex2_9_1 ex2_9_2;
run;

proc reg;
model z=t / dw;
output out=out2 p=zhat r=ehat l95=lci95 u95=uci95;
run;

proc print;
run;

proc gplot data=out2;
plot z*t=1 zhat*t=2 lci95*t=3 uci95*t=4/ href=21 overlay frame legend
haxis=0 to 43;
run;

