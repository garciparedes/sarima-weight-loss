proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate q=(1)(12) noint method=ml;
   forecast id=month interval=month lead=12 out=comparison_1 back = 12;
run;

proc export data=comparison_1
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/comparison-1.csv'
            dbms=csv replace;
run;


proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate q=(1)(12)(17) noint method=ml;
   forecast id=month interval=month lead=12 out=comparison_2 back = 12;
run;

proc export data=comparison_2
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/comparison-2.csv'
            dbms=csv replace;
run;
