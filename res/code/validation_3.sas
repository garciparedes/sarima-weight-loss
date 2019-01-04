proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate p=(1) q=(1)(12) noint method=ml;
   forecast id=month interval=month lead=0 out=arimaout_3;
run;

proc export data=arimaout_3
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/validation-3.csv'
            dbms=csv replace;
run;
