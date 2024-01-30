proc iml;

/* parameters */
N = 100;  /* number of observations */
P = 50;   /* number of explanatory variables */
beta = {1.5, 0.9, 1, 0.1, -0.5}; /* betas of our model */

/* null mean vector : 1 row of 50 zeros */
meanVec = j(1, P, 0);

/* variance-covariance matrix, for DGP1 we use the identity matrix */
covMatrix = I(P);

*call randseed(123); /* for reproductible results */

/* set the counters */
success=0;
overfitting=0;
overfitting_1=0;
underfitting=0;
underfitting_1=0;

do i = 1 to 1000  ; /* 1000 databases*/
	X = RandNormal(N, meanVec, covMatrix);

	/*generate the first equation*/
	X5 = X[,1:5]; /*only use the first 5 explanatory variables*/
	epsilon = RandNormal(N, 0, 0.1); /* errors iid normal distribution */

	/* equation */
	Y = X5 * beta + epsilon;

	/*creation of the final data set*/
	dataset1 = Y||X;
	cname="y"||("X1":"X50");

	create DGP1 from dataset1[colname=cname];
	   append from dataset1;
	   close DGP1;
	


	/* ALGORITHM TEST */
	submit;
	
	proc glmselect data=DGP1 outdesign=tab1 noprint;
	model y = X1-X50 /selection=forward (CHOOSE=AIC);
	run;
	
		proc glmselect data=DGP1 outdesign=tab1 noprint;
	model y = X1-X50 /selection=backward (CHOOSE=AIC);
	run;


	proc glmselect data=DGP1 outdesign=tab1 noprint;
		model y = X1-X50 /selection=stepwise (CHOOSE=BIC);
	run;

	proc transpose data=tab1 out=tab1;
	run;
	endsubmit;

	use tab1;
	read all;
	close tab1;

	/*creating the sets*/
	selected_variables=_NAME_[1:nrow(_name_)-1]; /*selected variables*/
	*print selected_variables;

	true_variables={"Intercept","X1","X2","X3","X4","X5"}; /*variables that need to be selected*/

	inter=Xsect(selected_variables,true_variables); /*select the variables that our algorithm predicted right*/
	*print inter; 

	if ncol(inter)=6 & nrow(selected_variables)=6 then do;
		*print "right selection";
		success = success +1 ;
		end;

	else if ncol(inter)=6 & nrow(selected_variables)>7 then do;
		overfitting = overfitting+1;
		*print "overfitting";
		end;
	
	else if ncol(inter)=6 & nrow(selected_variables)>6 then do;
		overfitting_1 = overfitting_1+1;
		*print "overfitting_1";
		end;

	else if ncol(inter) ^= 6 & nrow(selected_variables)<=6 then do;
		underfitting = underfitting+1;
		*print "underfitting";
		end;

	else if ncol(inter) ^= 6 & nrow(selected_variables)>=6 then do;
		underfitting_1= underfitting_1+1;
		*print "underfitting_1";
		end;

end;

/*calculate metrics after the loop, must be equal to 1000*/
total=success+overfitting+overfitting_1+underfitting+underfitting_1;
*print total;

/* percentage */
pct_overfitting = 100*overfitting/total;
pct_overfitting_1= 100*overfitting_1/total;
pct_underfitting = 100*underfitting/total;
pct_underfitting_1= 100*underfitting_1/total;
pct_success = 100*success/total;
*print pct_success pct_overfitting pct_overfitting_1 pct_underfitting pct_underfitting_1;

print "selection=stepwise and choose=BIC";
print pct_success pct_overfitting pct_underfitting;

/* graphiques */

submit;

endsubmit;

quit;



