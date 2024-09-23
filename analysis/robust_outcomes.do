* project: solar stove - robustness
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 19 Sep 2024
* stata v.18.5

* does
	* inputs cleaned dietary data
	* outputs results and table code for appendix
	* outcomes limited to JDE short paper (not full PAP)

* assumes
	* access to cleaned dietary data
	* estout

* to do:
	* everything

***********************************************************************
**# 0 - setup
***********************************************************************

* define
	global				ans		=	"$data/refined"
	global				output	=	"$data/analysis/tables"
	global				figure	=	"$data/analysis/figures"
	global				logout	=	"$data/logs"

* open log
	cap log 			close 
	log using			"$logout/robust_outcomes", append
	
	
************************************************************************
**# 1 - load and prepare data
************************************************************************

* load data
	use					"$ans/dietary_cleaned.dta", clear	
						
* set up global list of control variables, including village dummies
	global 				x_cov hh_size ai tli sex age edu cc 	

	lab var				treat_assign "Solar Stove"
	lab var				ss_use "Solar Stove Use"
	
* generate day counter
	gen					day_count = day if week == 1
	replace				day_count = day + 7 if week == 2
	replace				day_count = day + 14 if week == 3
	replace				day_count = day + 21 if week == 4
	replace				day_count = day + 28 if week == 5
	replace				day_count = day + 35 if week == 6
	order				day_count, after(day)
	lab var				day_count "Day in the Experiment"
	
	
	
************************************************************************
**# 2 - fatigue
************************************************************************

* histogram of responses per day by treatment		
	twoway			(histogram day_count if treat_assign == 1, color(teal%50) discrete ) ///
						(histogram day_count if treat_assign == 0, color(sienna%50) discrete ), ///
						xlabel(1 7 14 21 28 35 42) ///
						graphregion(fcolor(white)) xtitle("Day in Study") ///
						legend(pos(6) cols(2) label(1 "Treatment") label(2 "Control"))

* graph save
	graph export 	"$figure/response.pdf", replace as(pdf)			

	reghdfe 		ingred_dish ib(42).day_count if treat_assign == 1, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_t

	reghdfe 		ingred_dish ib(42).day_count if treat_assign == 0, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_c
	
	coefplot 		(event_dish_t, lc(teal) lpattern(solid) ciopts(recast(rarea) color(teal%20)) ) ///
					(event_dish_c, lc(sienna) lpattern(dash) ciopts(recast(rarea) color(sienna%20)) ), ///
					graphregion(fcolor(white)) xtitle("Day in Study") ///
						vertical omitted yline(0, lc(black) lw(vthin)) recast(connected) msize(vtiny) ///
						xlabel(1 7 14 21 28 35 42, angle(0) nogrid) drop(_cons) ytitle("Coefficient Size") ///
						legend(pos(6) cols(2) ) p1(label("Treatment")) p2(label("Control"))

* graph save
	graph export 	"$figure/event.pdf", replace as(pdf)		
	
	
reg ingred_dish day_count, vce(cluster hhid)

reg ingred_dish day_count $$x_cov, vce(cluster hhid)

reg ingred_meal day_count, vce(cluster hhid)

reg ingred_meal day_count $$x_cov, vce(cluster hhid)

reg ingred_day day_count, vce(cluster hhid)

reg ingred_day day_count $$x_cov, vce(cluster hhid)



************************************************************************
**# 3 - LATE
************************************************************************


* Q: Do households with solar stoves change the composition of their diet?

* (1) Estimate LATE effect of being randomly assigned a solar stove on HDDS
	* (i) Regress hdds ct for a dish on treatment assignment using use as instrument
	* final hdds dish outcomes with and without controls using OLS

		ivreg2 				hdds_dish (ss_use = treat_assign) i.aas i.village, cluster (hhid)
		summarize 			hdds_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		eststo				dHDDSIVo	
		
		ivreg2 				hdds_dish (ss_use = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			hdds_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		eststo				dHDDSIVoc	
	
	
* (ii) Regress hdds for a meal on treatment assignment
	* final hdds (avg) meal outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week day meal, force	
		
		ivreg2 				hdds_meal (share_meal = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			hdds_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		eststo mHDDSIVo	
		
		ivreg2				hdds_meal (share_meal = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			hdds_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		eststo mHDDSIVoc	
	restore
	
	* final hdds (avg) day outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week day, force	
		ivreg2				hdds_day (share_day = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			hdds_day if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "No", replace			
		eststo daHDDSIVo	
		
		ivreg2				hdds_day (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			hdds_day if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)			
		estadd local 		cov "Yes", replace			
		eststo daHDDSIVoc	
	restore
	
* (iv) Regress hhds for a week on treatment assignment
	* final hdds (avg) week outcomes with and without controls using OLS	
	preserve
		duplicates drop		hhid week, force	
		ivreg2				hdds_week (share_week = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			hdds_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		eststo wHDDSIVo
		
		ivreg2				hdds_week (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)  
		summarize 			hdds_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		eststo wHDDSIVoc	
	restore 

* (v) Regress hhds for overall (6 week) on treatment assignment
	* final hdds (avg) overall outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid, force	
		ivreg2				hdds_total (share_total = treat_assign) i.aas i.village, robust
		summarize 			hdds_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace				
		eststo tHDDSIVo	
		
		ivreg2				hdds_total (share_total = treat_assign) $x_cov i.aas i.village, robust
		summarize 			hdds_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace				
		eststo tHDDSIVoc	
	restore
	
* table fo_2: IV HDDS ols
	esttab dHDDSIVo dHDDSIVoc mHDDSIVo mHDDSIVoc daHDDSIVo daHDDSIVoc wHDDSIVo wHDDSIVoc tHDDSIVo tHDDSIVoc ///
							using "$output/late_out.tex", b(3) se(3) replace ///
							prehead("\begin{tabular}{l*{10}{c}} \\[-1.8ex]\hline \hline \\[-1.8ex] " ///
							"& \multicolumn{2}{c}{Dish} & \multicolumn{2}{c}{Meal} & \multicolumn{2}{c}{Day} " ///
							"& \multicolumn{2}{c}{Week} & \multicolumn{2}{c}{Overall} \\ \cline{2-3} " ///
							"\cline{4-5} \cline{6-7} \cline{8-9} \cline{10-11} \\[-1.8ex] " ///	                   
							"& \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} " ///
							"& \multicolumn{1}{c}{(4)} &\multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} " ///
							"& \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} & \multicolumn{1}{c}{(9)} " ///
							"& \multicolumn{1}{c}{(10)} \\ \midrule " ///
							"\multicolumn{11}{l}{\emph{Panel A: Dietary Diversity Score}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f))



************************************************************************
**# 4 - averages
************************************************************************


* close the log
	log				close
	
***END
