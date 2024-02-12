/************************************ DGP4 : MULTICOLLINEARITY + OUTLIERS ********************************************/
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
%Let algo=lar;
%Let criteria=cp;
%Let criteria2=sbc;


/**************************************** 1000 data sets loop ****************************************/
do i = 1 to 1000 ;
	X5 = RandNormal(N, meanVec_X5, corrMatrix_X5);
	X45 = RandNormal(N, meanVec_X45, covMatrix_X45);
	X=X5||X45;

	/* add outliers */
	do t=1 to N;
		do j=1 to 10; /*outliers in the first 10 variables*/
			u=uniform(0);
			if u<0.9 then X[t,j]=normal(0);
			else X[t,j]=normal(0)+5;
		end;
	end;
	*call histogram(X[,2]);
	
	X5=X[,1:5];
	
	/* Iman Conover : the do loop (X5, corrMatrix_X5) => W */
	S = J(N, P5);
	/* T1: Create normal scores of each column */
	do k = 1 to P5;
		ranks = ranktie(X5[,k], "mean");          /* tied ranks */
		S[,k] = quantile("Normal", ranks/(N+1)); /* van der Waerden scores */
	end;
	/* T2: apply two linear transformations to the scores */
	CS = corr(S);        /* correlation of scores */
	Q = root(CS);        /* Cholesky root of correlation of scores */
	P = root(corrMatrix_X5);         /* Cholesky root of target correlation */
	T = solve(Q,P);      /* same as  T = inv(Q) * P; */
	Y = S*T;             /* transform scores: Y has rank corr close to target C */
 
	/* T3: Permute or reorder data in the columns of X to have the same ranks as Y */
	W = X5;
	do k = 1 to ncol(Y);
		rank = rank(Y[,k]);          /* use ranks as subscripts, so no tied ranks */
		tmp = W[,k]; 
		call sort(tmp); /* sort column by ranks */
		W[,k] = tmp[rank];           /* reorder the column of X by the ranks of M */
	end;

	/*generate the equation*/
	
	epsilon = RandNormal(N, 0, 0.01); /* errors iid normal distribution */

	/* equation */
	Y = W * beta + epsilon;
	
	X45=X[,6:50];

	/*creation of the final data set*/
	dataset4 = Y||W||X45;
	cname="y"||("X1":"X50");

	create DGP4 from dataset4[colname=cname];
	   append from dataset4;
	   close DGP4;
	
	
	/********** ALGORITHM TEST ***********/
	submit;
	

	proc glmselect data=DGP4 outdesign=tab4 noprint;
	model y = X1-X50 /selection=&algo (CHOOSE=&criteria stop=&criteria2);
	run;


	proc transpose data=tab4 out=tab4;
	run;
	endsubmit;

	use tab4;
	read all;
	close tab4;

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

create DGP4_results from results[colname={'Probability'}];
	append from results;
close DGP4_results;


quit;
