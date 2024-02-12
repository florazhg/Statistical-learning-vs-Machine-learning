/******************************************* DGP1 : LINEAR INDEPENDANCE ***********************************************/
*OPTIONS NONOTES;
proc iml;

/**************************************** parameters ****************************************/
N = 100;  /* number of observations */
P = 50;   /* number of explanatory variables */
beta = {1.5, 0.9, 1, 1.8, -0.5}; /* betas of our model */

/* null mean vector : 1 row of 50 zeros */
meanVec = j(1, P, 0);

/* variance-covariance matrix, for DGP1 we use the identity matrix */
covMatrix = I(P);


/**************************************** counters ****************************************/
success=0; /* all 5 true variables are selected */
overfitting=0; /* all 5 true variables are selected + other variables */
underfitting=0; /* less than 5 true variables are selected */
fail=0; /* less than 5 true variables are selected + other variables */

/* change the algo and criteria here */
%Let algo=elasticnet;
%Let criteria=cp;
%Let criteria2=cv;

/**************************************** 1000 data sets loop ****************************************/
do i = 1 to 1000 ;
	X = RandNormal(N, meanVec, covMatrix);

	/*generate the first equation*/
	X5 = X[,1:5]; /*only use the first 5 explanatory variables*/
	epsilon = RandNormal(N, 0, 0.01); /* errors iid normal distribution */

	/* equation */
	Y = X5 * beta + epsilon;

	/*creation of the final data set*/
	dataset1 = Y||X;
	cname="y"||("X1":"X50");

	create DGP1 from dataset1[colname=cname];
	   append from dataset1;
	   close DGP1;
	


	/********** ALGORITHM TEST ***********/
	submit;
	
	proc glmselect data=DGP1 outdesign=DGP1_results noprint;
	model y = X1-X50 /selection=&algo (CHOOSE=&criteria stop=&criteria2);
	run;


	proc transpose data=DGP1_results out=DGP1_results;
	run;
	endsubmit;

	use DGP1_results;
	read all;
	close DGP1_results;

	/*creating the sets*/
	selected_variables=_NAME_[1:nrow(_name_)-1]; /*selected variables*/
	*print selected_variables;

	true_variables={"Intercept","X1","X2","X3","X4","X5"}; /*variables that need to be selected*/

	inter=Xsect(selected_variables,true_variables); /*select the variables that our algorithm predicted right*/
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

create resu from results[colname={'Probability'}];
	append from results;
close resu;


quit;
