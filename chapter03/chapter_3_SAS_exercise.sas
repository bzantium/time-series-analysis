*excercise 3.5;
symbol1 l=1 i=join v=none c=black;

*(a)female;
data female;
infile '../timedata/female.txt';
input female@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=female
interval=month method=expo out=outa outest=esta
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var female; run;
run;

data outaa;
set outa;
if _type_='RESIDUAL'; error=female; run;

data outa;
set outa;
if _type_='RESIDUAL' then delete; run;

proc gplot data=outa;
plot female*date=_type_ / frame; run;

*(b)build;
data build;
infile '../timedata/build.txt';
input build@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=build
interval=month method=expo out=outb outest=estb
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var build; run;

data outbb;
set outb;
if _type_='RESIDUAL'; error=build; run;

data outb;
set outb;
if _type_='RESIDUAL' then delete; run;

proc gplot data=outb;
plot build*date=_type_ / frame; run;

*(c)export;
data export;
infile '../timedata/export.txt';
input export@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=export
interval=month method=expo out=outc outest=estc
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var export; run;

data outcc;
set outc;
if _type_='RESIDUAL'; error=export; run;

data outc;
set outc;
if _type_='RESIDUAL' then delete; run;

proc gplot data=outc;
plot export*date=_type_ / frame; run;

*(d)usapass;
data usapass;
infile '../timedata/usapass.txt';
input usapass@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=usapass
interval=month method=expo out=outd outest=estd
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var usapass; run;

data outdd;
set outd;
if _type_='RESIDUAL'; error=usapass; run;

data outd;
set outd;
if _type_='RESIDUAL' then delete; run;

proc gplot data=outd;
plot usapass*date=_type_ / frame; run;

*(e)depart;
data depart;
infile '../timedata/depart.txt';
input depart@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=depart
interval=month method=expo out=oute outest=este
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var depart; run;

data outee;
set oute;
if _type_='RESIDUAL'; error=depart; run;

data oute;
set oute;
if _type_='RESIDUAL' then delete; run;

proc gplot data=oute;
plot depart*date=_type_ / frame; run;

*(f)stationery;
data stationery;
infile '../timedata/stationery.txt';
input stationery@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=stationery
interval=month method=expo out=outf outest=estf
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var stationery; run;

data outff;
set outf;
if _type_='RESIDUAL'; error=stationery; run;

data outf;
set outf;
if _type_='RESIDUAL' then delete; run;

proc gplot data=outf;
plot stationery*date=_type_ / frame; run;


*(g)book;
data book;
infile '../timedata/book.txt';
input book@@;
date=intnx('month', '1jan86'd, _n_-1);
format date monyy.; run;

proc forecast data=book
interval=month method=expo out=outg outest=estg
weight=0.89 trend=2 lead=6 outfull outresid;
id date; var book; run;

data outgg;
set outg;
if _type_='RESIDUAL'; error=book; run;

data outg;
set outG;
if _type_='RESIDUAL' then delete; run;

proc gplot data=outg;
plot book*date=_type_ / frame; run;
