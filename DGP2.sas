/********************************************** DGP2 : MULTICOLLINEARITY *********************************************/
*OPTIONS NONOTES;
proc iml;

/**************************************** parameters ****************************************/
N = 100;  /* number of observations */
P = 50;   /* number of explanatory variables */
beta = {1.5, 0.9, 1, 1.8, -0.5}; /* betas of our model */

/* null mean vector */
meanVec_X5 = j(1, 5, 0); /* first 5 explanatory variables */
meanVec_X45 = j(1, 45, 0); /* the rest */

/* variance-covariance matrix X1-X5 : we use toeplitz matrix for the correlation matrix */
/*positively and symmetrically correlated variables (X1 to X5) */
P5 = 5;
h = 1/P5;
v = do(1, h, -h);
corrMatrix_X5 = toeplitz(v);

/* variance-covariance matrix X6-X50 : identity matrix */
covMatrix_X45 = I(45);

/**************************************** counters ****************************************/
success=0; /* all 5 true variables are selected */
overfitting=0; /* all 5 true variables are selected + other variables */
underfitting=0; /* less than 5 true variables are selected */
fail=0; /* less than 5 true variables are selected + other variables */

/* change the algo and criteria here */
%Let algo=ElasticNet;
%Let criteria=sbc;
%Let criteria2=cv;

/**************************************** 1000 data sets loop ****************************************/
do i = 1 to 1000 ;
	X5 = RandNormal(N, meanVec_X5, corrMatrix_X5);
	X45 = RandNormal(N, meanVec_X45, covMatrix_X45);

	/*generate the first equation*/
	epsilon = RandNormal(N, 0, 0.01); /* errors iid normal distribution */

	/* equation */
	Y = X5 * beta + epsilon;

	/*creation of the final data set*/
	dataset2 = Y||X5||X45;
	cname="y"||("X1":"X50");

	create DGP2 from dataset2[colname=cname];
	   append from dataset2;
	   close DGP2;



	/********** ALGORITHM TEST ***********/
	submit;
	

	proc glmselect data=DGP2 outdesign=tab2 noprint;
	model y = X1-X50 /selection=&algo (CHOOSE=&criteria stop=&criteria2);
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

	inter=Xsect(selected_variables,true_variables); /*select the variable that our model predicted right*/
	*print inter;

	if ncol(inter)=6 & nrow(selected_variables)=6 then 
		success = success +1 ;
	else q=1;

	if ncol(inter)=6 & nrow(selected_variables)>6 then 
		overfitting = overfitting+1;
	else q=1;
	
	if ncol(inter) < 6 & nrow(selected_variables)=ncol(inter) then 
		underfitting = underfitting+1;
	else q=1;

	if ncol(inter) < 6 & nrow(selected_variables)>=6 then 
		fail= fail+1;
	else q=1;

end;

/* total must be equal to 1000 */
total=success+overfitting+underfitting+fail;
*print total;

/* percentage metrics */
pct_success = success/total;
pct_overfitting = overfitting/total;
pct_underfitting = underfitting/total;
pct_fail = fail/total;

/* create a matrix containing the percentages */
results=pct_success//pct_overfitting//pct_underfitting//pct_fail;
print results;

create forward_AIC from results[colname={'Success' 'Overfitting' 'Underfitting' 'Fail'}];
	append from results;
close forward_AIC;


quit;
