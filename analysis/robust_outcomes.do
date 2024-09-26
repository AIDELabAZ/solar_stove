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
**# 3 - LATE outcomes: food diversity
************************************************************************

* Q: Do households with solar stoves change the composition of their diet?
		
************************************************************************
**## 3.1 - household dietary diversity score
************************************************************************

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
	
* table: panel a IV HDDS ols
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
**## 3.2 - LATE: DSR
************************************************************************


************************************************************************
**## 3.3 - LATE: legumes
************************************************************************

* (i) Regress sr for a dish on treatment assignment
	* LATE sr dish outcomes with and without controls using OLS
		ivreg2 				sr_dish (ss_use = treat_assign) i.aas i.village, cluster (hhid)
		summarize 			sr_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		eststo				dsrIVo	
		
		ivreg2 				sr_dish (ss_use = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			sr_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		eststo				dsrIVoc	
	
	
* (ii) Regress sr for a meal on treatment assignment
	* LATE sr meal outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week day meal, force	
		
		ivreg2 				sr_meal (share_meal = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			sr_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		eststo msrIVo	
		
		ivreg2				sr_meal (share_meal = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			sr_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		eststo msrIVoc	
	restore
	
	* LATE sr day outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week day, force	
		ivreg2				sr_day (share_day = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			sr_day if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "No", replace			
		eststo dasrIVo	
		
		ivreg2				sr_day (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			sr_day if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)			
		estadd local 		cov "Yes", replace			
		eststo dasrIVoc	
	restore
	
* (iv) Regress hhds for a week on treatment assignment
	* LATE sr week outcomes with and without controls using OLS	
	preserve
		duplicates drop		hhid week, force	
		ivreg2				sr_week (share_week = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			sr_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		eststo wsrIVo
		
		ivreg2				sr_week (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)  
		summarize 			sr_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		eststo wsrIVoc	
	restore 

* (v) Regress hhds for overall (6 week) on treatment assignment
	* LATE sr overall outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid, force	
		ivreg2				sr_total (share_total = treat_assign) i.aas i.village, robust
		summarize 			sr_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace				
		eststo tsrIVo	
		
		ivreg2				sr_total (share_total = treat_assign) $x_cov i.aas i.village, robust
		summarize 			sr_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace				
		eststo tsrIVoc	
	restore
								
							
* table 2, Panel B: Solar stove assignment on SR
	esttab 			dsrIVo dsrIVoc msrIVo msrIVoc dasrIVo dasrIVoc wsrIVo wsrIVoc tsrIVo tsrIVoc ///
						using "$output/late_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{11}{l}{\emph{Panel B: Species Richness}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) 


************************************************************************
**## 3.3 - frequency of legumes
************************************************************************	

* LATE legumes dish outcomes with and without controls using OLS
		ivreg2 				p_dish (ss_use = treat_assign) i.aas i.village, cluster (hhid)
		summarize 			p_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		est					store dPIV
		
		ivreg2				p_dish (ss_use = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			p_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dPIVc
	
* LATE legumes meal outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day meal, force
			
			ivreg2			p_meal (share_meal = treat_assign) i.aas i.village, cluster (hhid)
			summarize 		p_meal if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store mPIV
			
			ivreg2			p_meal (share_meal = treat_assign) $x_cov i.aas i.village, cluster (hhid)
			summarize 		p_meal if treat_assign == 0		
			estadd scalar	dep_mean = r(mean)			
			estadd local 	cov "Yes", replace		
			est				store mPIVc
		restore
			
* LATE legumes day outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day, force
			
			ivreg2			p_day (share_day = treat_assign) i.aas i.village, cluster (hhid)
			summarize 		p_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "No", replace		
			est				store daPIV	
			
			ivreg2			p_day (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid)
			summarize 		p_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store daPIVc
		restore
	
* LATE legumes week outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week, force	
			
			ivreg2			p_week (share_week = treat_assign) i.aas i.village, cluster (hhid)
			summarize 		p_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store wPIV
			
			ivreg2			p_week (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
			summarize 		p_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store wPIVc
		restore 

* LATE legumes overall outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid, force	
	
			ivreg2			p_total (share_total = treat_assign) i.aas i.village, robust
			summarize 		p_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace			
			est				store tPIV
			
			ivreg2			p_total (share_total = treat_assign) $x_cov i.aas i.village, robust
			summarize 		p_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "Yes", replace		
			est				store tPIVc	
		restore


* Table 2, Panel C: Solar stove assignment on legumes
	esttab 			dPIV dPIVc mPIV mPIVc daPIV daPIVc wPIV wPIVc tPIV tPIVc ///
						using "$output/late_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{11}{l}{\emph{Panel C: Count of Legume Consumption}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///							
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) ///
							postfoot("\hline \hline \\[-1.8ex] \multicolumn{11}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: Dependent variables are different measure of " ///
							" household dietary composition. In Panel A, we use dietary diversity " ///
							"score. In Panel B, we use species richness. In Panel C, we calculate " ///
							"the number of times legumes are eaten. All regressions include two levels of strata " ///
							"fixed effects: village and Agricultural and Aquatic Systems (AAS) group. " ///
							"For regressions with more than one observation per houhold (columns 1-8), " ///
							"we calculate Liang-Zeger cluster-robust standard errors since the unit " ///
							"of randomization is the household. For regressions with only one " ///
							"observation per household (columns 9-10), we calculate Eicker-Huber-White " ///
							"(EHW) robust standard errors. Standard errors are presented in " ///
							"parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 

							
							
							
							
							

************************************************************************
**# 4 - averages
************************************************************************



************************************************************************
**# 5 - # of dishes prepared by meal
************************************************************************		
	
* number of breakfast dishes prepared for a hh in a day using OLS
 	preserve
		duplicates drop		hhid week day, force
		
		reg					brdish_day treat_assign i.aas i.village, vce(robust) 
		summarize 			brdish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				brdish_day treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			brdish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				

* number of breakfast dishes prepared for a hh in a week using OLS
 	preserve
		duplicates drop		hhid week, force
		
		reg					brdish_week treat_assign i.aas i.village, vce(robust) 
		summarize 			brdish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				brdish_week treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			brdish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				
		
* number of breakfast dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					brdish_tot treat_assign i.aas i.village, vce(robust) 
		summarize 			brdish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				brdish_tot treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			brdish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				

* number of lunch dishes prepared for a hh in a day using OLS
 	preserve
		duplicates drop		hhid week day, force
		
		reg					lundish_day treat_assign i.aas i.village, vce(robust) 
		summarize 			lundish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				lundish_day treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			lundish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				

* number of lunch dishes prepared for a hh in a week using OLS
 	preserve
		duplicates drop		hhid week, force
		
		reg					lundish_week treat_assign i.aas i.village, vce(robust) 
		summarize 			lundish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				lundish_week treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			lundish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				
		
* number of lunch dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					lundish_tot treat_assign i.aas i.village, vce(robust) 
		summarize 			lundish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				lundish_tot treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			lundish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore			

* number of dinner dishes prepared for a hh in a day using OLS
 	preserve
		duplicates drop		hhid week day, force
		
		reg					dindish_day treat_assign i.aas i.village, vce(robust) 
		summarize 			dindish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				dindish_day treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			dindish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				

* number of dinner dishes prepared for a hh in a week using OLS
 	preserve
		duplicates drop		hhid week, force
		
		reg					dindish_week treat_assign i.aas i.village, vce(robust) 
		summarize 			dindish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				dindish_week treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			dindish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				
		
* number of dinner dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					dindish_tot treat_assign i.aas i.village, vce(robust) 
		summarize 			dindish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				dindish_tot treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			dindish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore		

				
************************************************************************
**# 6 - # of meals skipped
************************************************************************		

* number of breakfast skipped for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					hhbr_skipped treat_assign i.aas i.village, vce(robust) 
		summarize 			hhbr_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bskip		
	
		reg 				hhbr_skipped treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhbr_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bskipc	
	restore				
	
* number of lunch skipped for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					hhlun_skipped treat_assign i.aas i.village, vce(robust) 
		summarize 			hhlun_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace		
		est					store lskip	
		
		reg					hhlun_skipped treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhlun_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		est					store lskipc	
	restore	
						   	
* number of dinner skipped for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					hhdin_skipped treat_assign i.aas i.village, vce(robust)  
		summarize 			hhdin_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace		
		est					store dskip			
		
		reg					hhdin_skipped  treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhdin_skipped  if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dskipc		
	restore

* number of all meals skipped for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force	
		
		reg 				hhtot_skipped treat_assign i.aas i.village, vce(robust) 
		summarize 			hhtot_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store tskip		
		
		reg 				hhtot_skipped treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhtot_skipped if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "Yes", replace			
		est					store tskipc			
	restore		


* close the log
	log				close
	
***END
