proc iml;

/* parameters */
N = 100;  /* number of observations */
P = 50;   /* number of explanatory variables */
beta = {1.5, 0.9, 1, 0.1, -0.5}; /* betas of our model */

/* null mean vector */
meanVec_X5 = j(1, 5, 0); /* first 5 explanatory variables */
meanVec_X45 = j(1, 45, 0); /* the rest */

/* variance-covariance matrix X1-X5 : we use toeplitz matrix for the correlation matrix */
/*positively and symmetrically correlated variables (X1 to X5) */
P5 = 5;
h = 1/P5;
v = do(1, h, -h);
corrMatrix_X5 = toeplitz(v);

/* calculate standard deviation (ecart type) of X1-X5*/
stdDeviation_X5 = sqrt(diag(corrMatrix_X5));
covMatrix_X5 = diag(stdDeviation_X5)*corrMatrix_X5*diag(stdDeviation_X5);

/* variance-covariance matrix X6-X50 : identity matrix */
covMatrix_X45 = I(45);

*call randseed(123); /* for reproductible results */

/* set the counters */
success=0;
overfitting=0;
overfitting_1=0;
underfitting=0;
underfitting_1=0;

do i = 1 to 1000 ; /* 1000 databases*/
	X5 = RandNormal(N, meanVec_X5, covMatrix_X5);
	X45 = RandNormal(N, meanVec_X45, covMatrix_X45);


	/*generate the first equation*/
	epsilon = RandNormal(N, 0, 0.1); /* errors iid normal distribution */

	/* equation */
	Y = X5 * beta + epsilon;

	/*creation of the final data set*/
	dataset2 = Y||X5||X45;
	cname="y"||("X1":"X5")||("X6":"X50");

	create DGP2 from dataset2[colname=cname];
	   append from dataset2;
	   close DGP2;



	/* ALGORITHM TEST */
	submit;
	

	proc glmselect data=DGP2 outdesign=tab2 noprint;
		model y = X1-X50 /selection=forward (choose=PRESS);
	run;


	proc transpose data=tab2 out=tab2;
	run;
	endsubmit;

	use tab2;
	read all;
	close tab2;

	/*creating the sets*/
	selected_variables=_NAME_[1:nrow(_name_)-1]; /*selected variables*/
	*print selected_variables;

	true_variables={"Intercept","X1","X2","X3","X4","X5"}; /*variables that need to be selected*/

	inter=Xsect(selected_variables,true_variables);
	*print inter; /*select the variable that our model predicted right*/

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

print "selection=forward and choose=PRESS";
print pct_success pct_overfitting pct_underfitting;

quit;

