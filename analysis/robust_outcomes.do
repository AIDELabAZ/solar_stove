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

************************************************************************
**# 4 - averages
************************************************************************


* close the log
	log				close
	
***END
