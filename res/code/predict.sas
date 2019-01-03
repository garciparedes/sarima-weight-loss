proc arima data = weightloss;
   identify var=weight_loss(1, 12);
   estimate q=(1)(12)(17) noint method=ml;
   forecast id=month interval=month lead=24 out=predict;
run;

proc export data=predict
            outfile='/folders/myshortcuts/sarima-weight-loss/res/data/predict.csv'
            dbms=csv replace;
run;
