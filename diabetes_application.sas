PROC IMPORT DATAFILE="diabetes.csv"
            OUT=diabetes
            DBMS=CSV REPLACE;
RUN;


proc means data=diabetes N Mean Median Std Min Max Skewness Kurtosis;
  var _ALL_;
  output out=summary_stats n= mean= median= std= min= max= skewness= kurtosis=;
run;


proc corr data=diabetes pearson;
   var _ALL_;
   title 'Pearson correlation matrix';
run;


/*** LAR_PRESS_SBC ***/
proc glmselect data=diabetes outdesign=model1;
   model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 / selection=LAR (choose=PRESS stop=SBC lscoeffs);
   title 'Variables selection with LAR';
run;


/*** ElasticNet_CP_CV ***/
proc glmselect data=diabetes outdesign=model1 seed=7;
   model Y = AGE SEX BMI BP S1 S2 S3 S4 S5 S6 / selection=ElasticNet (choose=CP stop=CV);
   title 'Variables selection with ElasticNet';
run;


