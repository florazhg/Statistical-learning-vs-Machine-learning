/************************************************* DGP3 : OUTLIERS *************************************************/
*OPTIONS NONOTES;
proc iml;

/**************************************** parameters ****************************************/
N = 100;  /* number of observations */
P = 50;   /* number of explanatory variables */
beta = {1.5, 0.9, 1, 1.8, -0.5}; /* betas of our model */

/* null mean vector : 1 row of 50 zeros */
meanVec = j(1, P, 0);

/* variance-covariance matrix */
covMatrix = I(P);

/**************************************** counters ****************************************/
success=0; /* all 5 true variables are selected */
overfitting=0; /* all 5 true variables are selected + other variables */
underfitting=0; /* less than 5 true variables are selected */
fail=0; /* less than 5 true variables are selected + other variables */

/* change the algo and criteria here */
%Let algo=forward;
%Let criteria=CV;
%Let criteria2=CV;

/**************************************** 1000 data sets loop ****************************************/
do i = 1 to 1000 ;
	X = RandNormal(N, meanVec, covMatrix);
	
	do t=1 to N;
		do j=1 to 10; /*outliers in the first 10 variables*/
			u=uniform(0);
			if u<0.9 then X[t,j]=normal(0);
			else X[t,j]=normal(0)+5;
		end;
	end;
	
	
	*call histogram(X[,2]);
	
	/*generate the equation*/
	X5 = X[,1:5]; /*only use the first 5 explanatory variables*/
	epsilon = RandNormal(N, 0, 0.01); /* errors iid normal distribution */

	/* equation */
	Y = X5 * beta + epsilon;

	/*creation of the final data set*/
	dataset3 = Y||X;
	cname="y"||("X1":"X50");

	create DGP3 from dataset3[colname=cname];
	   append from dataset3;
	   close DGP3;
	
	
	/********** ALGORITHM TEST ***********/
	submit;
	

	proc glmselect data=DGP3 outdesign=tab3 noprint;
	model y = X1-X50 /selection=&algo (CHOOSE=&criteria stop=&criteria2);
	run;


	proc transpose data=tab3 out=tab3;
	run;
	endsubmit;

	use tab3;
	read all;
	close tab3;

	/*creating the sets*/
	selected_variables=_NAME_[1:nrow(_name_)-1]; /*selected variables*/
	*print selected_variables;

	true_variables={"Intercept","X1","X2","X3","X4","X5"}; /*variables that need to be selected*/

	inter=Xsect(selected_variables,true_variables);
	*print inter; /*select the variable that our model predicted right*/

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
