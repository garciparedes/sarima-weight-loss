proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate q=(1)(12) noint method=ml;
   forecast id=month interval=month lead=0 out=arimaout_1;
run;

proc export data=arimaout_1
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-1.csv'
            dbms=csv replace;
run;


proc arima data = weightloss;
	identify var=weight_loss(1, 12);
   	estimate q=(1)(12)(17) noint method=ml;
   	forecast id=month interval=month lead=0 out=arimaout_2;
run;

proc export data=arimaout_2
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-2.csv'
            dbms=csv replace;
run;


proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate p=(1) q=(1)(12) noint method=ml;
   forecast id=month interval=month lead=0 out=arimaout_3;
run;

proc export data=arimaout_3
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-3.csv'
            dbms=csv replace;
run;


proc arima data = weightloss;
	identify var=weight_loss(1, 12);
   	estimate p=(1) q=(1)(12)(17) noint method=ml;
   	forecast id=month interval=month lead=0 out=arimaout_4;
run;

proc export data=arimaout_4
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-4.csv'
            dbms=csv replace;
run;
