proc arima data = weightloss;
	identify var=weight_loss(1, 12);
   	estimate q=(1)(12)(17) noint method=ml;
   	forecast id=month interval=month lead=0 out=arimaout_2;
run;

proc export data=arimaout_2
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-2.csv'
            dbms=csv replace;
run;
