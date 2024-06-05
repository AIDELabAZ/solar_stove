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
	global				ans		=	"$data/refined"
	global				output	=	"$data/analysis/tables"
	global				figure	=	"$data/analysis/figures"
	global				logout	=	"$data/logs"

* open log
	cap log 			close 
	log using			"$logout/outcomes", append

************************************************************************
* 1 - intermediate outcomes: average treatment effect
************************************************************************

* load data
	use					"$ans/dietary_cleaned.dta", clear	
	
	merge 				m:1 hhid week using "$ans/fuel_cleaned.dta"
	*** 28,643 obs matched, 1901 in dietary data not matched, 32 obs in fuel data not matched
	
	order _merge
	sort _merge hhid
	
	
								
* set up global list of control variables, including village dummies
	global 				x_cov age gender educ hh_size tli cc 	
	
************************************************************************
* 1a - take-up measured by ss use indicator
************************************************************************

* Q: Do households assigned solar stoves use solar stoves?

* (i) Regress hh use of solar stove at meal on treatment assignment
	
* dish-level use with and without controls using LPM	
	reg 				ss_use treat_assign i.group i.village, vce(cluster hhid)	
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store dLPM
	eststo 				clear
	
	reg 				ss_use treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store dLPMc		
	eststo 				clear
	
* dish-level use with and without controls using Probit	
	probit 				ss_use treat_assign i.group i.village, vce(cluster hhid)
	estadd local 		cov "No", replace	
	estadd local 		fe "Yes", replace	
	est					store dPro_lo	
	
	probit				ss_use treat_assign $x_cov i.group i.village, vce(cluster hhid)	
	estadd local 		cov "Yes", replace	
	estadd local 		fe "Yes", replace		
	est					store dProc_lo
			
* table 4a: dish use ss probit, lpm
	esttab dLPM dPro_lo dLPMc dProc_lo using "$output/itt_outcomes/io_dishlo.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)   ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(N cov fe r2 r2_p ll, fmt(0 0 0 3 3 3) layout( ///
		"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
		"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}" ///
		"\multicolumn{1}{c}{@}" "\multicolumn{1}{c}{@}") ///
		labels( `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"\(R^{2}\)"' `"Pseudo-\(R^{2}\)"' `"Log Likelihood"'))
			
* dish-level use with and without controls using Probit	
	probit 				ss_use treat_assign i.group i.village, vce(cluster hhid)
	eststo margin1:     margins, dydx(*) post	
	
	probit				ss_use treat_assign $x_cov i.group i.village, vce(cluster hhid)		
	eststo margin2:     margins, dydx(*) post			
					
* table 4b: dish use -- ONLY MARGINAL EFFECTS AND SEs (for now, just paste in AME and SE)
	esttab margin1 margin2 using "$output/itt_outcomes/io_dishme.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(margins se, fmt(3 3))
	

** NOTE: table 4b is manually combined into 4a in Overleaf
		
* dish-level use with and without controls using LPM

/* run regression, save coeff
	reg 				ss_use treat_assign i.group i.village, vce(cluster hhid)
	scalar 				b = _b[treat_assign]
	
* use coeff to calculate t stat and bootstrap t stat [tstat = pivotal statistic]
	bootstrap 			t=abs((_b[treat_assign]-b)/_se[treat_assign]), ///
							reps(5000): reg ss_use treat_assign i.group ///
							i.village, vce(cluster hhid) level(95)							
	estat 				bootstrap, percentile*/
	
* bootstrap standard errors
	*reg 				ss_use treat_assign i.group i.village, vce(bootstrap, cluster(hhid) reps(5000))			
************************************************************************
* 1b - take-up measured by ss use share
************************************************************************
	
* Q: What share (percentage) of dishes are prepared on solar stoves?
	
* (ii) Regress hh share of solar stove use for prep for each meal, day, week, 
* total level on treatment assignment
								
* meal-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid week day meal, force	
		reg 				share_meal treat_assign i.group i.village, vce(cluster hhid)
		summarize 			share_meal if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store mshare
		eststo 				clear
			
		reg 				share_meal treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			share_meal if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store msharec	
		eststo 				clear
	restore					 
	
* day-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid week day, force	
		reg 				share_day treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			share_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)		
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace	
		eststo 				clear
		est					store dshare	
		
		reg 				share_day treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			share_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace	
		eststo 				clear		
		est					store dsharec	
	restore
	
* week-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid week, force	
		reg 				share_week treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			share_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)				
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace	
		eststo 				clear
		est					store wshare	
					
		reg 				share_week treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			share_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)	
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace		
		est					store wsharec	
		eststo 				clear
	restore
	
* overall (6 week)-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid, force	
		reg 				share_tot treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			share_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)					
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace	
		eststo			 	clear
		est					store tshare	
		
		reg 				share_tot treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			share_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store tsharec	
		eststo 				clear
	restore				
	
* table 5: share ss used (ols)	
	esttab mshare msharec dshare dsharec wshare wsharec tshare tsharec using "$output/itt_outcomes/io_share.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(N dep_mean cov fe fe r2, fmt(0 3 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}") ///
		labels(`"Observations"' `"Mean of Control Group"'  ///
		`"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"\(R^{2}\)"'))
		
		

	
************************************************************************
* 2 - final outcomes: intent-to-treat effect
************************************************************************

************************************************************************
/* 2a - fuel use
************************************************************************
* Q: Do households with solar stoves reduce the amount of fuel used to
	* prepare meals?
	
* (i) Estimate ITT effect of being randomly assigned a solar stove on
	* weekly fuel collected, with and without controls using OLS
	
	reg 				wk_allfuel_collect treat_assign i.group i.village, vce(cluster hhid)		
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store fcoll
	eststo 				clear
	
	reg 				wk_allfuel_collect treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store fcollc		
	eststo 				clear
	
* (ii) Estimate ITT effect of being randomly assigned a solar stove on
	* weekly fuel sales, with and without controls using OLS
	reg 				wk_allfuel_sales treat_assign i.group i.village, vce(cluster hhid)		
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store fsale
	eststo 				clear
	
	reg 				wk_allfuel_sales treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store fsalec		
	eststo 				clear
	
* (iii) Estimate ITT effect of being randomly assigned a solar stove on
	* weekly fuel expenditures, with and without controls using OLS
	reg 				wk_allfuel_expend treat_assign i.group i.village, vce(cluster hhid)		
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store fexpend
	eststo 				clear
	
	reg 				wk_allfuel_expend treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store fexpendc		
	eststo 				clear
	
	
* table 6: fuels (all) used ss ols
	esttab store fcoll store fcollc fsale fsalec fexpend fexpendc using "$output/intermediate_outcomes/io_dish.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(margins N cov fe r2, fmt(3 0 0 0 3 3 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{c}{@}" ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels( `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"\(R^{2}\)"' ))*/
	

************************************************************************
* 2b - composition of diet
************************************************************************

************************************************************************
* 2bi - household dietary diversity score
************************************************************************
* Q: Do households with solar stoves change the composition of their diet?

* (1) Estimate ITT effect of being randomly assigned a solar stove on HDDS	
	* (i) Regress hhds ct for a dish on treatment assignment
	* final hdds (ct) dish outcomes with and without controls using Poisson
		poisson 				HDDS_ct_d treat_assign i.group i.village, vce(cluster hhid) 
		summarize 				HDDS_ct_d if treat_assign == 0	
		estadd scalar			dep_mean = r(mean)		
		estadd scalar	     	dep_sd = r(sd)
		estadd local 			cov "Yes", replace	
		estadd local 			fe "Yes", replace			
		est						store dHDDSp	
		eststo 					clear
		
		poisson					HDDS_ct_d treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 				HDDS_ct_d if treat_assign == 0		
		estadd scalar			dep_mean = r(mean)		
		estadd scalar			dep_sd = r(sd)		
		estadd local 			cov "Yes", replace	
		estadd local 			fe "Yes", replace			
		est						store dHDDSpc	
		eststo 					clear
	
	* (ii) Regress hhds for a meal on treatment assignment
	* final hdds (ct) meal outcomes with and without controls using Poisson
		preserve
			duplicates drop		hhid week day meal, force	
			poisson 			HDDS_ct_m treat_assign i.group i.village, vce(cluster hhid)
			summarize 			HDDS_ct_m if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "No", replace		
			estadd local 		fe "Yes", replace			
			est					store mHDDSp
			eststo 				clear
			
			poisson				HDDS_ct_m treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			HDDS_ct_m if treat_assign == 0		
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace		
			estadd local 		fe "Yes", replace		
			est					store mHDDSpc	
			eststo 				clear
			
	* final hdds (avg) meal outcomes with and without controls using OLS
			reg 				HDDS_avg_m treat_assign i.group i.village, vce(cluster hhid) 
			summarize 			HDDS_avg_m if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "No", replace		
			estadd local 		fe "Yes", replace		
			est					store mHDDSo	
			eststo 				clear
			
			reg					HDDS_avg_m treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			HDDS_avg_m if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace	
			estadd local 		fe "Yes", replace			
			est					store mHDDSoc	
			eststo 				clear
		restore
	
	* (iii) Regress hhds for a day on treatment assignment
	* final hdds (ct) day outcomes with and without controls using Poisson
		preserve
			duplicates drop		hhid week day, force	
			poisson 			HDDS_ct_da treat_assign i.group i.village, vce(cluster hhid)  
			summarize 			HDDS_ct_da if treat_assign == 0
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)
			estadd local 		cov "No", replace
			estadd local 		fe "Yes", replace			
			est					store daHDDSp	
			eststo 				clear
			
			poisson 			HDDS_ct_da treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			HDDS_ct_da if treat_assign == 0		
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace	
			estadd local 		fe "Yes", replace			
			est					store daHDDSpc	
			eststo 				clear
		
	* final hdds (avg) day outcomes with and without controls using OLS
			reg					HDDS_avg_da treat_assign i.group i.village, vce(cluster hhid)  	
			summarize 			HDDS_avg_da if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "No", replace	
			estadd local 		fe "Yes", replace			
			est					store daHDDSo	
			eststo 				clear
			
			reg					HDDS_avg_da treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			HDDS_avg_da if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace	
			estadd local 		fe "Yes", replace			
			est					store daHDDSoc	
			eststo 				clear
		restore
	
	* (iv) Regress hhds for a week on treatment assignment			
	* final hdds (ct) week outcomes with and without controls using Poisson
		preserve
			duplicates drop		hhid week, force	
			poisson 			HDDS_ct_w treat_assign i.group i.village, vce(cluster hhid) 
			summarize 			HDDS_ct_w if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar		dep_sd = r(sd)					
			estadd local 		cov "No", replace
			estadd local 		fe "Yes", replace			
			est					store wHDDSp	
			eststo 				clear
			
			poisson 			HDDS_ct_w treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			HDDS_ct_w if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace		
			estadd local 		fe "Yes", replace			
			est					store wHDDSpc	
			eststo 				clear
			
	* final hdds (avg) week outcomes with and without controls using OLS
			reg					HDDS_avg_w treat_assign i.group i.village, vce(cluster hhid) 
			summarize 			HDDS_avg_w if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "No", replace	
			estadd local 		fe "Yes", replace			
			est					store wHDDSo
			eststo 				clear
			
			reg					HDDS_avg_w treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			HDDS_avg_w if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace	
			estadd local 		fe "Yes", replace			
			est					store wHDDSoc
			eststo 				clear	
		restore 

* (v) Regress hhds for overall (6 week) on treatment assignment
	* final hdds (ct) overall outcomes with and without controls using Poisson
		preserve
			duplicates drop		hhid, force	
			poisson 			HDDS_ct_t treat_assign i.group i.village, vce(robust)
			summarize 			HDDS_ct_t if treat_assign == 0	
			estadd	scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "No", replace	
			estadd local 		fe "Yes", replace			
			est					store tHDDSp 
			eststo 				clear	

			poisson 			HDDS_ct_t treat_assign $x_cov i.group i.village, vce(robust)
			summarize 			HDDS_ct_t if treat_assign == 0	
			estadd	scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace		
			estadd local 		fe "Yes", replace			
			est					store tHDDSpc
			eststo 				clear	

	* final hdds (avg) overall outcomes with and without controls using OLS
			reg					HDDS_avg_t treat_assign i.group i.village, vce(robust)  
			summarize 			HDDS_avg_t if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "No", replace		
			estadd local 		fe "Yes", replace			
			est					store tHDDSo	
			eststo 				clear
			
			reg					HDDS_avg_t treat_assign $x_cov i.group i.village, vce(robust)
			summarize 			HDDS_avg_t if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)	
			estadd scalar		dep_sd = r(sd)			
			estadd local 		cov "Yes", replace			
			estadd local 		fe "Yes", replace		
			est					store tHDDSoc	
			eststo 				clear	
		restore

* table 7: fo_hddspoisson
	esttab dHDDSp dHDDSpc mHDDSp mHDDSpc daHDDSp daHDDSpc wHDDSp wHDDSpc tHDDSp tHDDSpc using "$output/itt_outcomes/fo_hddspoisson.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(c)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2_p, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"Pseudo \(R^{2}\)"'))
	
* table 8: fo_hddsols
	esttab mHDDSo mHDDSoc daHDDSo daHDDSoc wHDDSo wHDDSoc tHDDSo tHDDSoc using "$output/itt_outcomes/fo_hddsols.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(c)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"\(R^{2}\)"'))

		
* power calculations for avg hdds: meal
		power twomeans 2.213, power(0.7 0.8 0.9) n(12500 14541 1500  16500) ///
			sd(.619) a(.10) graph(y(delta))		
		
		graph export "$figure/power_calc/hddsavg_mealpower.png", as(png) replace		

* power calculations for avg hdds: total
		*power twomeans .2332, power(0.7 0.8 0.9) n(20 89 150  300) ///
			*sd(.03070577) a(.10) graph(y(delta))		
			
		*graph export "$output\hddsavg_mealpower.png", as(png) replace		

************************************************************************
* 2bii - dietary species richness
************************************************************************	
* Q: Do households with solar stoves change the composition of their diet?
			     		
* (2) Estimate ITT effect of being randomly assigned a solar stove on DSR	
	* (i) Regress dsr for a dish on treatment assignment
	* final dsr (ct) dish outcomes with and without controls using Poisson
		poisson 			DSR_dish treat_assign i.group i.village, vce(cluster hhid)
		summarize 			DSR_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)				
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store dDSRp	
		eststo 				clear	
		
		poisson 			DSR_dish treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			DSR_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)		
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store dDSRpc	
		eststo 				clear	
	
	* (ii) Regress dsr for a meal on treatment assignment	
	* final dsr (ct) meal outcomes with and without controls using Poisson
		preserve
			duplicates drop		hhid week day meal, force	
			poisson 			DSR_meal treat_assign i.group i.village, vce(cluster hhid) 
			summarize 			DSR_meal if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)			
			estadd local 		cov "No", replace		
			estadd local 		fe "Yes", replace			
			est					store mDSRp		
			eststo 				clear	
			
			poisson 			DSR_meal treat_assign $x_cov i.group i.village, vce(cluster hhid) 
			summarize 			DSR_meal if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)					
			estadd local 		cov "Yes", replace		
			estadd local 		fe "Yes", replace			
			est					store mDSRpc	
			eststo 				clear	
		restore
		
	* (iii) Regress dsr for a day on treatment assignment		
	* final dsr (ct) day outcomes with and without controls using Poisson
		preserve
		duplicates drop		hhid week day, force	
			poisson 			DSR_day treat_assign i.group i.village, vce(cluster hhid)
			summarize 			DSR_day if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)								
			estadd local 		cov "No", replace
			estadd local 		fe "Yes", replace			
			est					store daDSRp	
			eststo 				clear	
				
			poisson 			DSR_day treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			DSR_day if treat_assign == 0				
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)					
			estadd local 		cov "Yes", replace	
			estadd local 		fe "Yes", replace		
			est					store daDSRpc	
			eststo 				clear	
		restore
		
	* (iv) Regress dsr for a week on treatment assignment					
	* final dsr (ct) week outcomes with and without controls using Poisson
		preserve
		duplicates drop		hhid week, force	
			poisson 			DSR_week treat_assign i.group i.village, vce(cluster hhid)  
			summarize 			DSR_week if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)							
			estadd local 		cov "No", replace	
			estadd local 		fe "Yes", replace			
			est					store wDSRp		
			eststo 				clear	
			
			poisson 			DSR_week treat_assign $x_cov i.group i.village, vce(cluster hhid)
			summarize 			DSR_week if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)							
			estadd local 		cov "Yes", replace		
			estadd local 		fe "Yes", replace			
			est					store wDSRpc	
			eststo 				clear	
		restore	
		
	* (v) Regress dsr for overall (6 week) on treatment assignment	
	* final dsr (ct) total outcomes with and without controls using Poisson
		preserve
		duplicates drop		hhid, force	
			poisson 			DSR_total treat_assign i.group i.village, vce(robust)
			summarize 			DSR_total if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)						
			estadd local 		cov "No", replace		
			estadd local 		fe "Yes", replace			
			est					store tDSRp		
			eststo 				clear	
			
			poisson 			DSR_total treat_assign $x_cov i.group i.village, vce(robust)
			summarize 			DSR_total if treat_assign == 0	
			estadd scalar		dep_mean = r(mean)		
			estadd scalar	    dep_sd = r(sd)										
			estadd local 		cov "Yes", replace		
			estadd local 		fe "Yes", replace			
			est					store tDSRpc	
			eststo 				clear	
		restore   
	
* table 9: DSR poisson 
	esttab dDSRp dDSRpc mDSRp mDSRpc daDSRp daDSRpc wDSRp wDSRpc tDSRp tDSRpc using "$output/itt_outcomes/fo_dsrpoisson.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2_p, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"Pseudo \(R^{2}\)"'))

		
* power calculations for count dsr: dish
		power twomeans  2.255, power(0.7 0.8 0.9) n(22000 25000 27804  29000) ///
			sd(1.065) a(.10) graph(y(delta))		
			
		graph export "$figure/power_calc/dsr_dishpower.png", as(png) replace		
		
		
************************************************************************
* 3 - final outcomes: # of dishes prepared
************************************************************************		

* NOTE: The PAP defines this outcome variable as the "average number of ///
	* dishes in breakfast over the six weeks" but prescribes a Poisson
	* regression in the analysis section. We are estimating the regressions
	* below using OLS, as the variable is an average and not a count. 

* Estimate ITT effect of being randomly assigned a solar stove on avg # dishes
		
* (i) final number of all dishes prepared for a hh over the time period with 
	* and without controls, using ols regression
 	preserve
		duplicates drop		hhid, force	
		reg 				avg_dish treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			avg_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store tnump			
		
		reg 				avg_dish treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			avg_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store tnumpc			
	restore
	
* (ii) final number of breakfast dishes prepared using solar stoves with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force
		reg					avg_brdish treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			avg_brdish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store bnump		
	
		reg 				avg_brdish treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			avg_brdish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store bnumpc	
	restore				
	
* NOTE: hhid 226000 recorded all cooking methods associated with lunch and 
	* dinner as "Unspecified," therefore the lunch and dinner obs. for that hh were 
	* dropped in  raw_cleaning.do. Thus, the obs for total & breakfast meals include
	* 143 households but the lunch and dinner obs only include 142 hh. 
	
* (iii) final number of lunch dishes prepared using solar stoves with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force
		reg					avg_lundish treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			avg_lundish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store lnump	
		
		reg					avg_lundish treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			avg_lundish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store lnumpc	
	restore	
						   	
* (iv) final number of dinner dishes prepared using solar stoves with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force
		reg					avg_dindish treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			avg_dindish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store dnump			
		
		reg					avg_dindish  treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			avg_dindish  if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store dnumpc		
	restore
		
* table 10: number of dishes
	esttab tnump tnumpc bnump bnumpc lnump lnumpc dnump dnumpc using "$output/itt_outcomes/fo_avgnumdish.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"\(R^{2}\)"'))

************************************************************************
* 4 - final outcomes: # of meals skipped
************************************************************************			
		
* (4) Estimate ITT effect of being randomly assigned a solar stove on total 
	* number of meals skipped over all six weeks.	
	
* (i) final number of meals skipped (total) with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		poisson 			hhtot_skipped treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			hhtot_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace		
		estadd local 		fe "Yes", replace			
		est					store tmskipp	
		eststo 				clear	
		
		poisson 			hhtot_skipped treat_assign $x_cov i.group i.village, vce(cluster hhid)	
		summarize 			hhtot_skipped if treat_assign == 0
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store tmskippc	
		eststo 				clear	
	restore
	
* (ii) total number of hh breakfast meals skipped  with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		poisson 			hhbr_skipped treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			hhbr_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store bmskipp	
		eststo 				clear	
		
		poisson 			hhbr_skipped treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			hhbr_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store bmskippc	
		eststo 				clear	
	restore
						
* (iii) total number of hh lunch meals skipped with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		poisson 			hhlun_skipped treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			hhlun_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store lmskipp	
		eststo 				clear	
		
		poisson 			hhlun_skipped treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			hhlun_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store lmskippc
		eststo 				clear	
	restore					
	
* (iv) total number of hh dinner meals skipped with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		poisson 			hhdin_skipped treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			hhdin_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
 		est					store dmskipp	
		eststo 				clear	
			
		poisson 			hhdin_skipped treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			hhdin_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store dmskippc	
		eststo 				clear	
	restore			

* table 11: meals skipped
	esttab tmskipp tmskippc bmskipp bmskippc lmskipp lmskippc dmskippc dmskippc using "$output/itt_outcomes/fo_mealsskipped.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2_p, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"Pseudo \(R^{2}\)"'))
		
		
************************************************************************
* 5 - Cooking of legumes
************************************************************************

* Q: Do households with solar stoves prepare more legumes?

* Legumes are measured as a count. Estimate using Poisson regression with 
	* and without controls.
	
* (i) number of times legumes hh cooked in a day with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid week day, force	
		poisson 			legume_ct_day treat_assign i.group i.village, vce(cluster hhid)  
		summarize 			legume_ct_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store dlegP
		eststo 				clear	
		
		poisson 			legume_ct_day treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			legume_ct_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace	
		estadd local 		fe "Yes", replace			
		est					store dlegPc	
		eststo 				clear	
	restore
				
* (ii) number of times legumes hh cooked in a week with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid week, force	
		poisson 			legume_ct_week treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			legume_ct_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
		est					store wlegP	
		eststo 				clear	
		
		poisson 			legume_ct_week treat_assign $x_cov i.group i.village, vce(cluster hhid)
		summarize 			legume_ct_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store wlegPc
		eststo 				clear	
	restore					
	
* (iii) number of times legumes hh cooked over all 6 weeks with and without controls
	* using poisson regression
 	preserve
		duplicates drop		hhid, force	
		poisson 			legume_ct_tot treat_assign i.group i.village, vce(robust) 
		summarize 			legume_ct_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "No", replace	
		estadd local 		fe "Yes", replace			
 		est					store tlegP	
		eststo 				clear	
			
		poisson 			legume_ct_tot treat_assign $x_cov i.group i.village, vce(robust)
		summarize 			legume_ct_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd scalar	    dep_sd = r(sd)			
		estadd local 		cov "Yes", replace		
		estadd local 		fe "Yes", replace			
		est					store tlegPc	
		eststo 				clear	
	restore			

* table 12: legumes cooked
	esttab dlegP dlegPc wlegP wlegPc tlegP tlegPc using "$output/itt_outcomes/fo_legumescooked.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(c)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(dep_mean dep_sd N cov fe fe r2_p, fmt(3 3 0 0 0 0 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}"  ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels(`"Mean of Control Group"' `"Standard Dev. of Control Group"' `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"Group Fixed Effects"' `"Pseudo \(R^{2}\)"'))

***********************************************************************
* estimation of ITT for dietary outcomes complete!
***********************************************************************	


* save	
	save 			"$data/analysis/dietary_outcomes.dta", replace	
		

***********************************************************************
* estimation of ITT for fuel outcomes - time
***********************************************************************	

week, total
time on treatment 
money on treatment 


* (iv) Regress fuel use for a week on treatment assignment			
	* final fuel use week outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week, force	
		reg					time_collection_min treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			time_collection_min if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wftimeo	
		eststo 				clear
			
		reg					time_collection_min treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			time_collection_min if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wftimeoc	
		eststo 				clear
	restore	
	
	* (v) Regress fuel use for a week on treatment assignment			
	* final fuel use week outcomes with and without controls using OLS	
	preserve
		duplicates drop		hhid, force	
		reg					time_collection_min treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			time_collection_min if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store tftimeo	
		eststo 				clear
			
		reg					time_collection_min treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			time_collection_min if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store tftimeoc	
		eststo 				clear			
	restore	
		
		
	
	* (iv) Regress fuel use for a week on treatment assignment			
	* final fuel use week outcomes with and without controls using OLS
	
	gen time = time_collection_min*times_collected_week
	preserve
		duplicates drop		hhid week, force	
		reg					time treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			time if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wftimeo	
		eststo 				clear
			
		reg					time treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			time if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wftimeinoc	
		eststo 				clear
	restore	
	
	* (v) Regress fuel use for a week on treatment assignment			
	* final fuel use week outcomes with and without controls using OLS	
	preserve
		duplicates drop		hhid, force	
		reg					time treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			time if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store tftimeino	
		eststo 				clear
			
		reg					time treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			time if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store tftimeoc	
		eststo 				clear			
	restore		
		
		

***********************************************************************
* estimation of ITT for fuel outcomes -money
***********************************************************************	

* interact times bought and price bought
	gen price = time_bought_week*price_bought_kwacha

* (i) weekly cost (kwacha) of fuel on treatment assignment with and without 
	* controls using OLS regression
	preserve
		duplicates drop		hhid week, force	
		reg					price treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			price if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wfmoneyo
		eststo 				clear
			
		reg					price treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			price if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store wfmoneyoc	
		eststo 				clear
	restore 

* (ii) total (six-week) cost (kwacha) of fuel on treatment assignment with and 
	* without controls using OLS regression	 
	preserve
		duplicates drop		hhid, force	
		reg					price treat_assign i.group i.village, vce(cluster hhid) 
		summarize 			price if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store tfmoneyo
		eststo 				clear
			
		reg					price treat_assign $x_cov i.group i.village, vce(cluster hhid) 
		summarize 			price if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd scalar		dep_sd = r(sd)					
		estadd local 		cov "No", replace
		estadd local 		fe "Yes", replace			
		est					store tfmoneyoc	
		eststo 				clear
	restore 

* interact times collected/bought and price 





week, total 
time*times collected 
money*timesbought 







* close the log
	log				close
	
***END
