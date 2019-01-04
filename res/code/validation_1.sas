proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate q=(1)(12) noint method=ml;
   forecast id=month interval=month lead=0 out=arimaout_1;
run;

proc export data=arimaout_1
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-1.csv'
            dbms=csv replace;
run;

proc univariate data = arimaout_1;
	var residual;
run;
