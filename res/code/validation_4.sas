proc arima data = weightloss;
	identify var=weight_loss(1, 12);
   	estimate p=(1) q=(1)(12)(17) noint method=ml;
   	forecast id=month interval=month lead=0 out=arimaout_4;
run;

proc export data=arimaout_4
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-4.csv'
            dbms=csv replace;
run;
