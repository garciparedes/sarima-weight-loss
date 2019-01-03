FILENAME REFFILE '/folders/myshortcuts/sarima-weight-loss/res/data/weight-loss.csv';

PROC IMPORT DATAFILE=REFFILE
	DBMS=CSV
	OUT=WORK.weightloss;
	GETNAMES=YES;
RUN;
