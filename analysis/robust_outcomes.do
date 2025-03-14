* project: solar stove - robustness
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 13 Mar 25
* stata v.18.5

* does
	* inputs cleaned dietary data
	* outputs results and table code for appendix
	* outcomes limited to JDE short paper (not full PAP)

* assumes
	* access to cleaned dietary data
	* estout

* to do:
	* done

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
	
************************************************************************
**# 2 - fatigue
************************************************************************
	
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
**## 2.1 - histogram of responses per day by treatment	
************************************************************************

	twoway			(histogram day_count if treat_assign == 1, color(teal%50) discrete ) ///
						(histogram day_count if treat_assign == 0, color(sienna%50) discrete ), ///
						xlabel(1 7 14 21 28 35 42) xtitle("Day in Study") ///
						graphregion(fcolor(white)) ytitle("Density") ///
						title("A: Distribution of the Number of Responses per Day") ///
						legend(pos(6) cols(2) label(1 "Treatment") label(2 "Control"))

* graph save
	graph export 	"$figure/response.pdf", replace as(pdf)			
	

************************************************************************
**## 2.2 - total number of entries by treatment
************************************************************************

* generate total entries by household
	tostring		hhid, replace
	encode			hhid, gen(hh)

	duplicates tag 	hh, generate(hh_ent)
	replace			hh_ent = hh_ent + 1
	gen				tot_ent = 351
	
* generate number of entries per day by household
	duplicates tag 	hh day_count, generate(day_ent)
	replace			day_ent = day_ent + 1

* collapse to household and day
	duplicates drop	hh day_count, force
	
* create entries by treatment
	sort			treat_assign day_count
	egen			trt_ent = total(day_ent) if treat_assign == 1, by(day_count)
	egen			trt_tot= total(day_ent) if treat_assign == 1
		
	egen			cnt_ent = total(day_ent) if treat_assign == 0, by(day_count)
	egen			cnt_tot= total(day_ent) if treat_assign == 0
	
* create cumulative distribution by household
	sort 			hh day_count
	bys hhid:		gen count = _n	

	gen				cuml = day_ent / hh_ent if count == 1
	by hhid:		replace	cuml = day_ent / hh_ent + cuml[_n-1] if cuml == .
	replace			cuml = 1 if cuml > .9999 & cuml != .
	
	gen				cuml_trt = trt_ent / trt_tot if count == 1	
	by hhid:		replace	cuml_trt = trt_ent / trt_tot + cuml_trt[_n-1] if cuml_trt == .
	replace			cuml_trt = 1 if cuml_trt > .9999 & cuml_trt != .
	
	gen				cuml_cnt = cnt_ent / cnt_tot if count == 1
	by hhid:		replace	cuml_cnt = cnt_ent / cnt_tot + cuml_cnt[_n-1] if cuml_cnt == .
	replace			cuml_cnt = 1 if cuml_cnt > .9999 & cuml_cnt != .
	
* keep only needed variables
	keep			hh treat_assign day_count cuml cuml_trt cuml_cnt
	rename			treat_assign ss 
	
	reshape wide	cuml ss cuml_trt cuml_cnt, i(day_count) j(hh)

	gen				cuml_cnt = cuml_cnt5 
	gen				cuml_trt = cuml_trt18 
	order			cuml_trt cuml_cnt, after(day_count)
	
	forvalues 		i = 1/156 {
		drop 		cuml_cnt`i' cuml_trt`i'
}

* fill in  missing values
	forvalues 		i = 1/156 {
		replace 		ss`i' = ss`i'[1]
}
		
	local 			grt ""
	forvalues 		i = 1/156 {
		local 			grt `grt' line cuml`i' day_count ///
							if ss`i' == 1, lpattern(solid) lcolor(teal%20) lwidth(thin) ||
}
		
	local 			grc ""
	forvalues 		i = 1/156 {
		local 			grc `grc' line cuml`i' day_count ///
							if ss`i' == 0, lpattern(dash) lcolor(sienna%20) lwidth(thin) ||
}
		
* final graph
	sort 			day_count
	twoway 			`grc' `grt' ///
	line 			cuml_cnt day_count, lc(sienna*1.5) lpattern(dash) || ///
	line			cuml_trt day_count, lc(teal*1.5) lpattern(solid)  ///
						xlabel(1 7 14 21 28 35 42) xtitle("Day in Study") ///
						graphregion(fcolor(white)) ytitle("Cumulative Distribution") ///
						title("B: Accumulation of Diary Entries Over Time") ///
						legend(pos(6) cols(2) order(313 314) ///
						label(313 "Treatment") label(314 "Control"))
						
* graph save
	graph export 	"$figure/cuml_entries.pdf", replace as(pdf)		
	
	
************************************************************************
**## 2.3 - number of ingredients in a dish in a day
************************************************************************

preserve
* event study
	reghdfe 		recall ib(42).day_count if treat_assign == 1, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_t

	reghdfe 		recall ib(42).day_count if treat_assign == 0, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_c
	
	coefplot 		(event_dish_t, lc(teal) lpattern(solid) ciopts(recast(rarea) color(teal%20)) ) ///
					(event_dish_c, lc(sienna) lpattern(dash) ciopts(recast(rarea) color(sienna%20)) ), ///
					graphregion(fcolor(white)) xtitle("") ytitle("Share of Recall Entries") ///
						vertical omitted yline(0, lc(black) lw(vthin)) recast(connected) msize(vtiny) ///
						xlabel(1 7 14 21 28 35 42, angle(0) nogrid) drop(_cons)  ///	
						ylabel(-.08 -0.04 0 .04 .08) title("C.1: Ignore Missing Recall Data") ///
						legend(pos(6) cols(2) ) p1(label("Treatment")) p2(label("Control")) ///
						saving("$figure/miss", replace)
restore
		
preserve
	
replace recall = 0 if recall == .

* event study
	reghdfe 		recall ib(42).day_count if treat_assign == 1, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_t

	reghdfe 		recall ib(42).day_count if treat_assign == 0, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_c
	
	coefplot 		(event_dish_t, lc(teal) lpattern(solid) ciopts(recast(rarea) color(teal%20)) ) ///
					(event_dish_c, lc(sienna) lpattern(dash) ciopts(recast(rarea) color(sienna%20)) ), ///
					graphregion(fcolor(white)) xtitle("") ytitle("Share of Recall Entries") ///
						vertical omitted yline(0, lc(black) lw(vthin)) recast(connected) msize(vtiny) ///
						xlabel(1 7 14 21 28 35 42, angle(0) nogrid) drop(_cons)  ///	
						ylabel(-.08 -0.04 0 .04 .08) title("C.2: Treat Missing as Not Based on Recall") ///
						legend(pos(6) cols(2) ) p1(label("Treatment")) p2(label("Control")) ///
						saving("$figure/no", replace)
restore		
		
preserve

replace recall = 1 if recall == .

* event study
	reghdfe 		recall ib(42).day_count if treat_assign == 1, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_t

	reghdfe 		recall ib(42).day_count if treat_assign == 0, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_dish_c
	
	coefplot 		(event_dish_t, lc(teal) lpattern(solid) ciopts(recast(rarea) color(teal%20)) ) ///
					(event_dish_c, lc(sienna) lpattern(dash) ciopts(recast(rarea) color(sienna%20)) ), ///
					graphregion(fcolor(white)) xtitle("Day in Study") ytitle("Share of Recall Entries") ///
						vertical omitted yline(0, lc(black) lw(vthin)) recast(connected) msize(vtiny) ///
						xlabel(1 7 14 21 28 35 42, angle(0) nogrid) drop(_cons)  ///	
						ylabel(-.08 -0.04 0 .04 .08) title("C.3: Treat Missing as Based on Recall") ///
						legend(pos(6) cols(2) ) p1(label("Treatment")) p2(label("Control")) ///
						saving("$figure/yes", replace)
restore
		
	grc1leg2 				"$figure/miss" "$figure/no" "$figure/yes", col(1) iscale(.5) ///
								 commonscheme title("C: Frequency of Entries Based on Recall")
								
* graph save
	graph export 	"$figure/recall.pdf", replace as(pdf)		
	
			
						
************************************************************************
**## 2.4 - diversity of ingredients in in a day
************************************************************************

* event study
	reghdfe 		hdds_day ib(42).day_count if treat_assign == 1, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_day_t

	reghdfe 		hdds_day ib(42).day_count if treat_assign == 0, ///
						absorb(hhid) cl(hhid) 
	estimates 		store event_day_c
	
	coefplot 		(event_day_t, lc(teal) lpattern(solid) ciopts(recast(rarea) color(teal%20)) ) ///
					(event_day_c, lc(sienna) lpattern(dash) ciopts(recast(rarea) color(sienna%20)) ), ///
					graphregion(fcolor(white)) xtitle("Day in Study") ytitle("Distinct Ingredients in a Day") ///
						vertical omitted yline(0, lc(black) lw(vthin)) recast(connected) msize(vtiny) ///
						xlabel(1 7 14 21 28 35 42, angle(0) nogrid) drop(_cons)  ///				
						ylabel(-1 "5.2" -.5 "5.7" 0 "6.2" .5 "6.7" 1 "7.2") ///
						title("D: Number of Distinct Ingredients Recorded in a Day") ///
						legend(pos(6) cols(2) ) p1(label("Treatment")) p2(label("Control"))

* graph save
	graph export 	"$figure/event_diversity.pdf", replace as(pdf)		
	
		
	reg 			ingred_dish day_count, vce(cluster hhid)

	reg 			ingred_dish day_count $$x_cov, vce(cluster hhid)

	reg 			ingred_meal day_count, vce(cluster hhid)

	reg 			ingred_meal day_count $$x_cov, vce(cluster hhid)

	reg 			ingred_day day_count, vce(cluster hhid)

	reg 			ingred_day day_count $$x_cov, vce(cluster hhid)


************************************************************************
**# 3 - LATE outcomes: food diversity
************************************************************************

* Q: Do households with solar stoves change the composition of their diet?
		
************************************************************************
**## 3.1 - household dietary diversity score
************************************************************************

* late hdds (ct) dish outcomes with and without controls using OLS
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
	
	
* late hdds (ct) meal outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week day meal, force	
		
		ivreg2 				hdds_meal (share_meal = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			hdds_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		eststo 				mHDDSIVo	
		
		ivreg2				hdds_meal (share_meal = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			hdds_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		eststo 				mHDDSIVoc	
	restore
	
* late hdds (ct) day outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid week day, force	
		ivreg2				hdds_day (share_day = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			hdds_day if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "No", replace			
		eststo 				daHDDSIVo	
		
		ivreg2				hdds_day (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			hdds_day if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)			
		estadd local 		cov "Yes", replace			
		eststo 				daHDDSIVoc	
	restore
	
* late hdds (ct) week outcomes with and without controls using OLS	
	preserve
		duplicates drop		hhid week, force	
		ivreg2				hdds_week (share_week = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			hdds_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		eststo 				wHDDSIVo
		
		ivreg2				hdds_week (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)  
		summarize 			hdds_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		eststo 				wHDDSIVoc	
	restore 

* late hdds (ct) overall outcomes with and without controls using OLS
	preserve
		duplicates drop		hhid, force	
		ivreg2				hdds_total (share_total = treat_assign) i.aas i.village, robust
		summarize 			hdds_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace				
		eststo 				tHDDSIVo	
		
		ivreg2				hdds_total (share_total = treat_assign) $x_cov i.aas i.village, robust
		summarize 			hdds_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace				
		eststo 				tHDDSIVoc	
	restore
	
* table C1 Panel A: Solar stove use on HDDS
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
							"\multicolumn{11}{l}{\emph{Panel A: Household Dietary Diversity Score}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f))
							
							
************************************************************************
**## 3.2 - species richness 
************************************************************************

* (i) LATE sr dish outcomes with and without controls 
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
	
	
* (ii) LATE sr meal outcomes with and without controls 	
	preserve
		duplicates drop		hhid week day meal, force	
		
		ivreg2 				sr_meal (share_meal = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			sr_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		eststo 				msrIVo	
		
		ivreg2				sr_meal (share_meal = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			sr_meal if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		eststo 				msrIVoc	
	restore
	
* (iii) LATE sr day outcomes with and without controls
	preserve
		duplicates drop		hhid week day, force	
		ivreg2				sr_day (share_day = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			sr_day if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "No", replace			
		eststo 				dasrIVo	
		
		ivreg2				sr_day (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid) 
		summarize 			sr_day if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)			
		estadd local 		cov "Yes", replace			
		eststo 				dasrIVoc	
	restore
	
* (iv)  LATE sr week outcomes with and without controls
	preserve
		duplicates drop		hhid week, force	
		ivreg2				sr_week (share_week = treat_assign) i.aas i.village, cluster (hhid) 
		summarize 			sr_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		eststo 				wsrIVo
		
		ivreg2				sr_week (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)  
		summarize 			sr_week if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		eststo 				wsrIVoc	
	restore 

* (v) LATE sr overall outcomes with and without controls 
	preserve
		duplicates drop		hhid, force	
		ivreg2				sr_total (share_total = treat_assign) i.aas i.village, robust
		summarize 			sr_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace				
		eststo 				tsrIVo	
		
		ivreg2				sr_total (share_total = treat_assign) $x_cov i.aas i.village, robust
		summarize 			sr_total if  treat_assign == 0		
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace				
		eststo 				tsrIVoc	
	restore
								
							
* table C1 Panel B: Solar stove use on species richness 
	esttab 			dsrIVo dsrIVoc msrIVo msrIVoc dasrIVo dasrIVoc wsrIVo wsrIVoc tsrIVo tsrIVoc ///
						using "$output/late_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{11}{l}{\emph{Panel B: Dietary Species Richness}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) 


************************************************************************
**## 3.3 - frequency of legumes
************************************************************************	

* (i) LATE legumes dish outcomes with and without controls
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
	
* (ii) LATE legumes meal outcomes with and without controls
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
			
* (iii) LATE legumes day outcomes with and without controls
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
	
* (iv) LATE legumes week outcomes with and without controls
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

* (v) LATE legumes overall outcomes with and without controls
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


* Table C1, Panel C: Solar stove use on count of legume consumption
	esttab 			dPIV dPIVc mPIV mPIVc daPIV daPIVc wPIV wPIVc tPIV tPIVc ///
						using "$output/late_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{11}{l}{\emph{Panel C: Count of Legume Consumption}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///							
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) ///
							postfoot("\hline \hline \\[-1.8ex] \multicolumn{11}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: Dependent variables are different measure of " ///
							" household dietary composition. In Panel A, we use dietary diversity " ///
							"score. In Panel B, we use species richness. In Panel C, we calculate " ///
							"the number of times legumes are eaten. All regressions include two levels of strata " ///
							"fixed effects: village and nutrition sensitive landscapes (NSL) group. " ///
							"For regressions with more than one observation per houhold (columns 1-8), " ///
							"we calculate Liang-Zeger cluster-robust standard errors since the unit " ///
							"of randomization is the household. For regressions with only one " ///
							"observation per household (columns 9-10), we calculate Eicker-Huber-White " ///
							"(EHW) robust standard errors. Standard errors are presented in " ///
							"parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 

************************************************************************
**# 4 - LATE outcomes: frequency of cooking
************************************************************************		

* Q: Do households with solar stoves change their cooking behavior?

************************************************************************
**## 4.1 - # of dishes prepared
************************************************************************		
	
* number of dishes prepared for a hh in a day using OLS
 	preserve
		duplicates drop		hhid week day, force
		
		ivreg2				dish_day (share_day = treat_assign) i.aas i.village, cluster(hhid)
		summarize 			dish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store ddayIV		
	
		ivreg2 				dish_day (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			dish_day if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store ddayIVc	
	restore				

* number of dishes prepared for a hh in a week using OLS
 	preserve
		duplicates drop		hhid week, force
		
		ivreg2				dish_week (share_week = treat_assign) i.aas i.village, cluster (hhid)
		summarize 			dish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store dweekIV		
	
		ivreg2				dish_week (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			dish_week if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dweekIVc	
	restore				
		
* number of dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		ivreg2				dish_tot (share_total = treat_assign) i.aas i.village, robust
		summarize 			dish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store dtotIV		
	
		ivreg2				dish_tot (share_total = treat_assign) $x_cov i.aas i.village, robust
		summarize 			dish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dtotIVc	
	restore		
	
* table C2, Panel A: Solar stove use on dishes prepared
	esttab 			ddayIV ddayIVc dweekIV dweekIVc dtotIV dtotIVc ///
						using "$output/late_numdish.tex", b(3) se(3) replace ///
							prehead("\begin{tabular}{l*{6}{c}} \\[-1.8ex]\hline \hline \\[-1.8ex] " ///
							"& \multicolumn{2}{c}{Day} & \multicolumn{2}{c}{Week} & " ///
							"\multicolumn{2}{c}{Overall} \\ \cline{2-3} " ///
							"\cline{4-5} \cline{6-7} \\[-1.8ex] " ///	                   
							"& \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} " ///
							"& \multicolumn{1}{c}{(4)} &\multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} " ///
							"\\ \midrule " ///
							"\multicolumn{7}{l}{\emph{Panel A: Number of Dishes Prepared}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f))


************************************************************************
**# 4.2 - # of meals skipped
************************************************************************		

* number of meals skipped in a day using OLS
 	preserve
		duplicates drop		hhid week day, force
		
		ivreg2				day_skip (share_day = treat_assign) i.aas i.village, cluster (hhid)
		summarize 			day_skip if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store dskipIV		
	
		ivreg2 				day_skip (share_day = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			day_skip if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dskipIVc	
	restore				
	
* number of meals skipped in a week using OLS
 	preserve
		duplicates drop		hhid week, force
		
		ivreg2				week_skip (share_week = treat_assign) i.aas i.village, cluster (hhid)
		summarize 			week_skip if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace		
		est					store wskipIV	
		
		ivreg2				week_skip (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
		summarize 			week_skip if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		est					store wskipIVc	
	restore	
						   	
* number of meals skipped overall using OLS
 	preserve
		duplicates drop		hhid, force
		
		ivreg2				tot_skip (share_total = treat_assign) i.aas i.village, robust 
		summarize 			tot_skip if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace		
		est					store tskipIV			
		
		ivreg2				tot_skip  (share_total = treat_assign) $x_cov i.aas i.village, robust
		summarize 			tot_skip  if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store tskipIVc		
	restore

* table C2, Panel B: Solar stove use on skipped meals
	esttab 			dskipIV dskipIVc wskipIV wskipIVc tskipIV tskipIVc ///
						using "$output/late_numdish.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{7}{l}{\emph{Panel B: Number of Meals Skipped}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_meal "Solar Stove Use" ///
							share_day "Solar Stove Use" share_week "Solar Stove Use" share_total "Solar Stove Use") ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) ///
							postfoot("\hline \hline \\[-1.8ex] \multicolumn{7}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: Dependent variables are different measure of " ///
							" frequency of cooking. In Panel A, we use the number of dishes cooked " ///
							"in a meal. In Panel B, we use the number of meals skipped. " ///
							"All regressions include two levels of strata " ///
							"fixed effects: village and nutrition sensitive landscapes (NSL) group. " ///
							"Eicker-Huber-White (EHW) robust standard errors. Standard errors are presented in " ///
							"parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 
						
							
************************************************************************
**# 5 - late outcome: fuel collection
************************************************************************

* collapse to week
	duplicates drop		hhid week, force

* merge in 	hh_w_fuel and hh_t_fuel values
	merge 1:1  		hhid week using "$ans/fuel_cleaned.dta"

	keep if			_merge == 3
	
/* load data
	use					"$ans/fuel_cleaned.dta", clear	
	
	
	order				village village hhid aas cc hh_size ai tli sex age ///
							edu treat_assign week */

* Q: Do households assigned solar stoves spend less on fuel?

************************************************************************
**## 5.1 - late outcome: weekly fuel collection
************************************************************************

* firewood time at week-level use with and without controls 	
	ivreg2 				f_time (share_week = treat_assign) i.aas i.village, cluster (hhid)
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFTIV
	
	ivreg2 				f_time (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFTIVc	
	
* firewood quantity at week-level use with and without controls
	ivreg2 				f_quant_ub (share_week = treat_assign) i.aas i.village, cluster (hhid)
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFQIV
	
	ivreg2 				f_quant_ub (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFQIVc	
	
* charcoal quantity at week-level use with and without controls	
	ivreg2 				c_quant_ub (share_week = treat_assign) i.aas i.village, cluster (hhid)
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wCQIV
	
	ivreg2 				c_quant_ub (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wCQIVc	
	
* fuel value at week-level use with and without controls	
	ivreg2 				val_fuel_ub (share_week = treat_assign) i.aas i.village, cluster (hhid)
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFVIV
	
	ivreg2 				val_fuel_ub (share_week = treat_assign) $x_cov i.aas i.village, cluster (hhid)
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFVIVc	
	
  
* table C3, Panel A: Solar stove assignment on weekly fuel outcomes
	esttab 			wFTIV wFTIVc wFQIV wFQIVc wCQIV wCQIVc wFVIV wFVIVc ///
						using "$output/fuel_late.tex", b(2) se(2) replace ///
							prehead("\begin{tabular}{l*{8}{c}} \\[-1.8ex]\hline \hline \\[-1.8ex] " ///
							"& \multicolumn{2}{c}{Firewood} & \multicolumn{2}{c}{Firewood} " ///
							"& \multicolumn{2}{c}{Charcoal} & \multicolumn{2}{c}{Fuel} \\ " ///
							"& \multicolumn{2}{c}{Time (min)} & \multicolumn{2}{c}{Quantity (kg)} " ///
							"& \multicolumn{2}{c}{Quantity (kg)} & \multicolumn{2}{c}{Value (USD)} \\ " ///
							"\cline{2-3} \cline{4-5} \cline{6-7} \cline{8-9} \\[-1.8ex] " ///	                   
							"& \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} " ///
							"& \multicolumn{1}{c}{(4)} &\multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} " ///
							"& \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)}  \\ \midrule " ///
							"\multicolumn{9}{l}{\emph{Panel A: Weekly Fuel Outcomes}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_week "Solar Stove Use")  ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) 
							

************************************************************************
**## 5.2 - late outcome: total fuel collection
************************************************************************

	collapse 			(sum) f_time f_quant_ub c_quant_ub val_fuel_ub ///
							(mean) cc, ///
							by(share_total village hhid aas hh_size ai tli sex age edu treat_assign)

* firewood time at overall use with and without controls	
	ivreg2 				f_time (share_total = treat_assign) i.aas i.village, robust
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFTIV
	
	ivreg2 				f_time (share_total = treat_assign) $x_cov i.aas i.village, robust
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFTIVc	
	
* firewood quantity at overall use with and without controls	
	ivreg2 				f_quant_ub (share_total = treat_assign) i.aas i.village, robust
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFQIV
	
	ivreg2 				f_quant_ub (share_total = treat_assign) $x_cov i.aas i.village, robust
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFQIVc	
	
* charcoal quantity at overall use with and without controls 
	ivreg2 				c_quant_ub (share_total = treat_assign) i.aas i.village, robust
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tCQIV
	
	ivreg2 				c_quant_ub (share_total = treat_assign) $x_cov i.aas i.village, robust
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tCQIVc	
	
* fuel value at overall use with and without controls 
	ivreg2 				val_fuel_ub (share_total = treat_assign) i.aas i.village, robust
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFVIV
	
	ivreg2 				val_fuel_ub (share_total = treat_assign) $x_cov i.aas i.village, robust
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFVIVc	
	

* table C3, Panel B: Solar stove assignment on overall fuel outcomes
	esttab 			tFTIV tFTIVc tFQIV tFQIVc tCQIV tCQIVc tFVIV tFVIVc ///
						using "$output/fuel_late.tex", b(2) se(2) append ///
							prehead("\midrule \multicolumn{9}{l}{\emph{Panel B: Overall Fuel Outcomes}} \\ ") ///
							drop(hh_size ai tli sex age edu cc _cons *aas *village) noobs ///
							rename(ss_use "Solar Stove Use" share_total "Solar Stove Use")  ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) ///
							postfoot("\hline \hline \\[-1.8ex] \multicolumn{9}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: Dependent variables are different measure of " ///
							" fuel collection at different levels of aggregation. " ///
							"In Panel A, we use values measured each week " ///
							"In Panel B, we sum weekly values to the overall six week total. " ///
							"All regressions include two levels of strata " ///
							"fixed effects: village and nutrition sensitive landscapes (NSL) group. " ///
							"Eicker-Huber-White (EHW) robust standard errors. Standard errors are presented in " ///
							"parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 
						
						
* close the log
	log				close
	
***END
