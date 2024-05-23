* project: solar stove - intermediate outcomes
* created on: February 2021
* created by: lem
* edited by:
* Stata v.15.1 (mac)

* does
	* inputs cleaned dietary data
	* outputs results and table code for intermediate and final outcomes 

* assumes
	* table numbers correspond to PAP Checklist document as of 8 June 2021

* to do:
	* check notes for each table and delete template language
	* double-check each title
	* double-check stats included for each estimator

***********************************************************************
* 0 - setup
***********************************************************************

* define
	global				ans		=	"$data"
	global				output	=	"$data/analysis/tables"
	global				figure	=	"$data/analysis/figures"
	global				logout	=	"$data/logs"

* open log
	cap log 			close 
	log using			"$logout/outcomes", append
	
* load data
	use					"$ans/refined/dietary_cleaned.dta", clear	
	
* set up global list of control variables, including village dummies
	global 				x_cov age gender educ hh_size tli cc 	
	
***********************************************************************
* 2 - final outcomes: LATE
***********************************************************************

***********************************************************************
* 2 (a) - household dietary diversity score
***********************************************************************

* Q: Do households with solar stoves change the composition of their diet?

* (1) Estimate LATE effect of being randomly assigned a solar stove on HDDS
	* (i) Regress hdds ct for a dish on treatment assignment using use as instrument
	* final hdds (ct) dish outcomes with and without controls using Poisson
	ivpoisson gmm 			HDDS_ct_d (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
	summarize 				HDDS_ct_d if treat_assign == 0		
	estadd scalar			dep_mean = r(mean)	
	estadd scalar			dep_sd = r(sd)	
	estadd local 			cov "No", replace	
	estadd local 			fe "Yes", replace		
	est						store dHDDSIVp	
	eststo 					clear
	
	ivpoisson gmm 			HDDS_ct_d (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
	summarize 				HDDS_ct_d if treat_assign == 0		
	estadd scalar			dep_mean = r(mean)
	estadd scalar			dep_sd = r(sd)	
	estadd local 			cov "Yes", replace	
	estadd local 			fe "Yes", replace			
	est						store dHDDSIVpc	
	eststo 					clear
	
* (ii) Regress hdds for a meal on treatment assignment
	* final hdds (ct) meal outcomes with and without controls using Poisson
	preserve
		duplicates drop		hhid week day meal, force	
		ivpoisson gmm 		HDDS_ct_m (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
		summarize 			HDDS_ct_m if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store mHDDSIVp
		eststo 				clear
		
		ivpoisson gmm		HDDS_ct_m (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
		summarize 			HDDS_ct_m if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace		
		est					store mHDDSIVpc	
		eststo 				clear
		
* final hdds (avg) meal outcomes with and without controls using OLS	
		ivreg2 				HDDS_avg_m (ss_use = treat_assign) i.group i.village, cluster(hhid)
		summarize 			HDDS_avg_m if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace		
		est					store mHDDSIVo	
		eststo 				clear
		
		ivreg2				HDDS_avg_m (ss_use = treat_assign) $x_cov i.group i.village, cluster(hhid)
		summarize 			HDDS_avg_m if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store mHDDSIVoc	
		eststo 				clear
	restore
	
	* final hdds (ct) day outcomes with and without controls using Poisson
	preserve
		duplicates drop		hhid week day, force	
		ivpoisson gmm 		HDDS_ct_da (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
		summarize 			HDDS_ct_da if  treat_assign == 0
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store daHDDSIVp	
		eststo 				clear
		
		ivpoisson gmm 		HDDS_ct_da (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
		summarize 			HDDS_ct_da if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store daHDDSIVpc	
		eststo 				clear
	
	* final hdds (avg) day outcomes with and without controls using OLS
		ivreg2				HDDS_avg_da (ss_use = treat_assign) i.group i.village, cluster(hhid)
		summarize 			HDDS_avg_da if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store daHDDSIVo	
		eststo 				clear
		
		ivreg2				HDDS_avg_da (ss_use = treat_assign) $x_cov i.group i.village, cluster(hhid)
		summarize 			HDDS_avg_da if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store daHDDSIVoc	
		eststo 				clear
	restore
	
* (iv) Regress hhds for a week on treatment assignment
	* final hdds (ct) week outcomes with and without controls using Poisson
	preserve
		duplicates drop		hhid week, force	
		ivpoisson gmm 		HDDS_ct_w (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)	
		summarize 			HDDS_ct_w if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wHDDSIVp	
		eststo 				clear
		
		
		ivpoisson gmm 		HDDS_ct_w (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)
		summarize 			HDDS_ct_w if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store wHDDSIVpc	
		eststo 				clear
		
	* final hdds (avg) week outcomes with and without controls using OLS		
		ivreg2				HDDS_avg_w (ss_use = treat_assign) i.group i.village, cluster(hhid)
		summarize 			HDDS_avg_w if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store wHDDSIVo
		eststo 				clear
		
		ivreg2				HDDS_avg_w (ss_use = treat_assign) $x_cov i.group i.village, cluster(hhid) 
		summarize 			HDDS_avg_w if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store wHDDSIVoc
		eststo 				clear	
	restore 

* (v) Regress hhds for overall (6 week) on treatment assignment
	* final hdds (ct) overall outcomes with and without controls using Poisson
	preserve
		duplicates drop		hhid, force	
		ivpoisson gmm 		HDDS_ct_t (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs)
		
		/*start bootstrap program
		program boot_cf, eclass
		capture drop res_hddsw
		* regress endogenous var on instrument using OLS
		reg 	 ss_use treat_assign i.group i.village
	* predict residuals and call them res_hddsw
		predict	res_hddsw, res
	* regress outcome var on instrument + residuals using POISSON
		poisson	HDDS_ct_w ss_use res_hddsw	
		end
		bootstrap:boot_cf
		
	
	* example of bootstrapping -- still can't tell what "small" means/how it affects things
	    ivregress 2sls rent pcturban (hsngval = faminc i.region), small
        program boot_cf, eclass
        capture drop resid
        reg hsngval  faminc i.region pcturban
        predict resid, res
        reg rent pcturban  hsngval resid
        end
        bootstrap:boot_cf*/
			
		summarize 			HDDS_ct_t if  treat_assign == 0		
		estadd	scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store tHDDSIVp 
		eststo 				clear	

		ivpoisson gmm 		HDDS_ct_t (ss_use = treat_assign) $x_cov i.group i.village,vce(robust) multiplicative tech(bfgs)
		summarize 			HDDS_ct_t if  treat_assign == 0		
		estadd	scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store tHDDSIVpc
		eststo 				clear	

* final hdds (avg) overall outcomes with and without controls using OLS

		ivreg2				HDDS_avg_t (ss_use = treat_assign) i.group i.village, robust
		summarize 			HDDS_avg_t if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store tHDDSIVo	
		eststo 				clear
		
		ivreg2				HDDS_avg_t (ss_use = treat_assign) $x_cov i.group i.village, robust
		summarize 			HDDS_avg_t if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace			
		estadd local 		fe "Yes", replace		
		est					store tHDDSIVoc	
		eststo 				clear	
	restore
	
* table fo_1: IV HDDS poisson
	esttab dHDDSIVp dHDDSIVpc mHDDSIVp mHDDSIVpc daHDDSIVp daHDDSIVpc wHDDSIVp wHDDSIVpc tHDDSIVp tHDDSIVpc using "$output/late_outcomes/l.fo_hddsivpoisson.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(ss_use) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe r2_p, fmt(3 3 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"'   `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"'  `"Pseudo \(R^{2}\)"'))
	
* table fo_2: IV HDDS ols
	esttab mHDDSIVo mHDDSIVoc daHDDSIVo daHDDSIVoc wHDDSIVo wHDDSIVoc tHDDSIVo tHDDSIVoc using "$output/late_outcomes/l.fo_hddsivols.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(ss_use) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe r2_p, fmt(3 3 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"'  `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"'  `"Pseudo \(R^{2}\)"'))

************************************************************************
* 2 (b) - dietary species richness
************************************************************************	

* Estimate ITT effect of being randomly assigned a solar stove on DSR
* (i) Regress dsr for a dish on treatment assignment

* final dsr (ct) dish outcomes with and without controls using Poisson
	ivpoisson gmm 		DSR_dish (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs) 	
	summarize 			DSR_dish if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd scalar		dep_sd = r(sd)		
	estadd local 		cov "No", replace		
	estadd local 		fe "Yes", replace			
	est					store dDSRIVp	
	eststo 				clear	
	
	ivpoisson gmm 		DSR_dish (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs) 	
	summarize 			DSR_dish if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd scalar		dep_sd = r(sd)		
	estadd local 		cov "Yes", replace	
	estadd local 		fe "Yes", replace			
	est					store dDSRIVpc	
	eststo 				clear	
	
* (ii) Regress dsr for a meal on treatment assignment
	
* final dsr (ct) meal outcomes with and without controls using Poisson
	preserve
		duplicates drop		hhid week day meal, force	
		ivpoisson gmm 		DSR_meal (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs) 
		summarize 			DSR_meal if treat_assign == 0			
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store mDSRIVp		
		eststo 				clear	
		
		ivpoisson gmm 		DSR_meal (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs) 
		summarize 			DSR_meal if treat_assign == 0			
		estadd scalar		dep_mean = r(mean)			
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store mDSRIVpc	
		eststo 				clear	
	restore
		
* (iii) Regress dsr for a day on treatment assignment
		
* final dsr (ct) day outcomes with and without controls using Poisson
	preserve
	duplicates drop		hhid week day, force	
		ivpoisson gmm 		DSR_day (ss_use = treat_assign) i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)	
		summarize 			DSR_day if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store daDSRIVp	
		eststo 				clear	
			
		ivpoisson gmm 		DSR_day (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs) 
		summarize 			DSR_day if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace		
		est					store daDSRIVpc	
		eststo 				clear	
	restore
		
* (iv) Regress dsr for a week on treatment assignment
						
* final dsr (ct) week outcomes with and without controls using Poisson
	preserve
	duplicates drop		hhid week, force	
		ivpoisson gmm 		DSR_week (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs) 	
		summarize 			DSR_week if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store wDSRIVp		
		eststo 				clear	
		
		ivpoisson gmm 		DSR_week (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			DSR_week if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store wDSRIVpc	
		eststo 				clear	
	restore	
		
* (v) Regress dsr for overall (6 week) on treatment assignment
	
* final dsr (ct) total outcomes with and without controls using Poisson
	preserve
	duplicates drop		hhid, force	
		ivpoisson gmm 		DSR_total (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			DSR_total if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store tDSRIVp		
		eststo 				clear	
		
		ivpoisson gmm 		DSR_total (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			DSR_total if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store tDSRIVpc	
		eststo 				clear	
	restore   
	
* table fo_3: DSR poisson 
	esttab dDSRIVp dDSRIVpc mDSRIVp mDSRIVpc daDSRIVp daDSRIVpc wDSRIVp wDSRIVpc tDSRIVp tDSRIVpc using "$output/late_outcomes/l.fo_DSRpoisson.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(ss_use) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2, fmt(3 3 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"'  `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"'  `"\(R^{2}"'))

		power twomeans 2.255, power(0.7 0.8 0.9) n(25500 27804 29000  31000) ///
			sd(1.065) a(.10) graph(y(delta))		
			
		graph export "$output\dsr_mealpower.png", as(png) replace
			
	
************************************************************************
* 4 - final outcomes: # of dishes prepared
************************************************************************

* (i) final number of all dishes prepared using solar stoves with and without controls
	* using ols regression
 	preserve
		duplicates drop		hhid, force
		ivreg2 				avg_dish (ss_use = treat_assign) i.group i.village, cluster(hhid) 
		summarize 			avg_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace	
		est					store ltnump			
		
		ivreg2 				avg_dish treat_assign $x_cov i.group i.village, cluster(hhid) 
		summarize 			avg_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace			
		estadd local 		fe "Yes", replace			
		est					store ltnumpc			
	restore
	
* (ii) final number of breakfast dishes prepared using solar stoves with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force
		ivreg2				avg_brdish (ss_use = treat_assign) i.group i.village, cluster(hhid) 
		summarize 			avg_brdish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace	
		est					store lbnump		
	
		ivreg2 				avg_brdish (ss_use = treat_assign) $x_cov i.group i.village, cluster(hhid) 
		summarize 			avg_brdish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace	
		est					store lbnumpc	
	restore						
	
* (iii) final number of lunch dishes prepared using solar stoves with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force
		ivreg2 				avg_lundish (ss_use = treat_assign) i.group i.village, cluster(hhid) 
		summarize 			avg_lundish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace	
		est					store llnump	
		
		ivreg2 				avg_lundish (ss_use = treat_assign) $x_cov i.group i.village, cluster(hhid)
		summarize 			avg_lundish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)	
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace		
		est					store llnumpc	
	restore	
						   	
* (iv) final number of dinner dishes prepared using solar stoves with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force
		ivreg2 				avg_dindish (ss_use = treat_assign) i.group i.village, cluster(hhid) 
		summarize 			avg_dindish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)			
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace		
		est					store ldnump			
		
		ivreg2 				avg_dindish (ss_use = treat_assign) $x_cov i.group i.village, cluster(hhid) 
		summarize 			avg_dindish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace		
		est					store ldnumpc		
	restore
		
* table 10: number of dishes
	esttab ltnump ltnumpc lbnump lbnumpc llnump llnumpc ldnump ldnumpc using "$output/late_outcomes/l.fo_avgnumdish.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(ss_use) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe r2, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"\(R^{2}\)"'))

		power twomeans 2.112966, power(0.7 0.8 0.9) n(13500 14500 15468  16500) ///
			sd(.6130476) a(.10) graph(y(delta))		
			
		graph export "$output\hddsavg_mealpower.png", as(png) replace		
		
************************************************************************
* 5 - final outcomes: # of meals skipped
************************************************************************			

* (4) Estimate LATE effect of being randomly assigned a solar stove on # meals skipped		
* (i) final number of meals skipped (total) with and without controls
	* using poisson regression			
	preserve
		duplicates drop		hhid, force	
		ivpoisson 			gmm	hhtot_skipped (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhtot_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store ltmskipp	
		eststo 				clear	
		
		ivpoisson 	gmm		hhtot_skipped (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhtot_skipped if treat_assign == 0
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)	
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store ltmskippc	
		eststo 				clear	
	restore
	
* (ii) final number of breakfast meals skipped (total) with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		ivpoisson 	gmm		hhbr_skipped (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhbr_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)	
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store lbmskipp	
		eststo 				clear	
		
		ivpoisson 	gmm		hhbr_skipped (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhbr_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store lbmskippc	
		eststo 				clear	
	restore
						
* (iii) final number of lunch meals skipped (total) with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		ivpoisson 	gmm		hhlun_skipped (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhlun_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store llmskipp	
		eststo 				clear	
		
		ivpoisson gmm		hhlun_skipped (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhlun_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store llmskippc
		eststo 				clear	
	restore					
	
* (iv) final number of dinner meals skipped (total) with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		ivpoisson 	gmm		hhdin_skipped (ss_use = treat_assign) i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhdin_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
 		est					store ldmskipp	
		eststo 				clear	
			
		ivpoisson 	gmm		hhdin_skipped (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)
		summarize 			hhdin_skipped if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd scalar		dep_sd = r(sd)
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store ldmskippc	
		eststo 				clear	
	restore			

* table 11: meals skipped
	esttab ltmskipp ltmskippc lbmskipp lbmskippc llmskipp llmskippc ldmskippc	ldmskippc using "$output/late_outcomes/l.fo_mealsskipped.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(ss_use) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2_p, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"Pseudo \(R^{2}\)"'))
				
************************************************************************
* 6 - Cooking of legumes
************************************************************************		
		
* Q: Do households with solar stoves prepare more legumes?

* Legumes are measured as a count. Estimate using Poisson regression with 
	* and without controls.
	
* (i) number of times legumes hh cooked in a day with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid week day, force	
		ivpoisson gmm 		legume_ct_day (ss_use = treat_assign), vce(cluster hhid) multiplicative tech(bfgs)	
		summarize 			legume_ct_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store dIVlegP
		eststo 				clear	
		
		ivpoisson gmm 		legume_ct_day (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)		
		summarize 			legume_ct_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store dIVlegPc	
		eststo 				clear	
	restore
				
* (ii) number of times legumes hh cooked in a week with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid week, force	
		ivpoisson gmm 		legume_ct_week (ss_use = treat_assign), vce(cluster hhid) multiplicative tech(bfgs)			
		summarize 			legume_ct_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store wIVlegP	
		eststo 				clear	
		
		ivpoisson gmm 		legume_ct_week (ss_use = treat_assign) $x_cov i.group i.village, vce(cluster hhid) multiplicative tech(bfgs)	
		summarize 			legume_ct_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store wIVlegPc
		eststo 				clear	
	restore					
	
* (iii) number of times legumes hh cooked over all 6 weeks with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		ivpoisson gmm 		legume_ct_tot (ss_use = treat_assign), vce(robust) multiplicative tech(bfgs)			
		summarize 			legume_ct_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
 		est					store tIVlegP	
		eststo 				clear	

		ivpoisson gmm 		legume_ct_tot (ss_use = treat_assign) $x_cov i.group i.village, vce(robust) multiplicative tech(bfgs)			
		summarize 			legume_ct_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store tIVlegPc	
		eststo 				clear	
	restore			

* table 12: legumes cooked
	esttab dIVlegP dIVlegPc wIVlegP wIVlegPc tIVlegP tIVlegPc using "$output/late_outcomes/l.fo_legumescooked.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(ss_use) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2_p, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"Pseudo \(R^{2}\)"'))		


	save 			"$root/analysis/dietary_outcomes.dta", replace	
	
* close the log
	log				close
	
************************************************************************
* END
************************************************************************
