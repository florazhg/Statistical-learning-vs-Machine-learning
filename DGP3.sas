proc iml;

/* parameters */
N = 100;  /* number of observations */
P = 50;   /* number of explanatory variables */
beta = {1.5, 0.9, 1, 0.1, -0.5}; /* betas of our model */

/* null mean vector : 1 row of 50 zeros */
meanVec = j(1, P, 0);

/* variance-covariance matrix */
covMatrix = I(P);
 
call randseed(123); /* for reproductible results */

/* set the counters */
success=0;
overfitting=0;
overfitting_1=0;
underfitting=0;
underfitting_1=0;

do i = 1 to 1000 ; /* 1000 databases*/
	X = RandNormal(N, meanVec, covMatrix);
	
	/* add a few unusual points (outliers) to the last 10 obs of the first 2 columns*/
	pi = constant('pi');
	t = T(90:N) * pi / 6; /* evenly spaced points in [0, 2p] */
	outliers = 5#cos(t) || 5#sin(t); /* evenly spaced on circle r=5 */
	
	X[90:N,1:2] = outliers; /* concatenate MVN data and outliers */
	
	
	/* visualize the outliers */
	call histogram (X);
	
	/*generate the equation*/
	X5 = X[,1:5]; /*only use the first 5 explanatory variables*/
	epsilon = RandNormal(N, 0, 0.1); /* errors iid normal distribution */

	/* equation */
	Y = X5 * beta + epsilon;

	/*creation of the final data set*/
	dataset3 = Y||X;
	cname="y"||("X1":"X50");

	create DGP3 from dataset3[colname=cname];
	   append from dataset3;
	   close DGP3;
	
	/* verify the outliers with box plot | to run without the 1000 loop */
	submit;
	proc sgplot data=DGP3;
	hbox X1 / extreme;
	hbox X2;
	hbox X3;
	run;
	endsubmit;
	
	
	/* ALGORITHM TEST */
	submit;
	

	proc glmselect data=DGP3 outdesign=tab3 noprint;
		model y = X1-X50 /selection=forward (choose=AIC);
	run;


	proc transpose data=tab3 out=tab3;
	run;
	endsubmit;

	use tab3;
	read all;
	close tab3;

	/*creating the sets*/
	selected_variables=_NAME_[1:nrow(_name_)-1]; /*selected variables*/
	print selected_variables;

	true_variables={"Intercept","X1","X2","X3","X4","X5"}; /*variables that need to be selected*/

	inter=Xsect(selected_variables,true_variables);
	print inter; /*select the variable that our model predicted right*/

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


quit;

