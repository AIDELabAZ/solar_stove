* project: solar stove - final outcomes: fuel use
* created on: April 2025
* created by: jdm
* edited by: jdm
* edited on: 25 Apr 2025
* stata v.19.5

* does
	* inputs raw fuel data
	* cleans data
	* merges in controls
	* outputs weekly fuel data for regressions

* assumes
	* access to raw data
	* access to cleaned controls

* to do:
	* done
	

************************************************************************
**# 0 - setup
************************************************************************

* define paths
	global	root	=	"$data/raw/dietary/HDDS"
	global	ans		=	"$data/refined"
	global	output	=	"$data/analysis/tables"
	global	figure	=	"$data/analysis/figures"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/fuel_hetero", append

	
* load cleaned fuel data
	use 				"$ans/fuel_cleaned_ind.dta", clear


************************************************************************
**# 1 - rename and relabel fuel variables
************************************************************************

* rename variables	
	rename				cod_hh_collectedbought f_id
	rename				cod_hh_bought c_id	
	lab var				f_id "Firewood Collector ID"
	lab var				c_id "Charcoal Buyer ID"
	
	replace				bght = 0 if bght == .
	rename				bght charcoal	
	lab var				charcoal "Was charcoal bought?"	
	
	replace				times_bght = 0 if times_bght == .
	rename				times_bght c_times	
	lab var				c_times "Number of times charcoal was bought"	
	
	rename				price_bght c_price
	lab var				c_price "Price of charcoal (kwacha)"	
	
	replace				quant_bght = 0 if quant_bght == .
	rename				quant_bght c_quant
	lab var				c_quant "Quantity of charcoal (kg)"
		
	replace				cltd = 0 if cltd == .
	rename				cltd firewood
	lab var				firewood "Was firewood collected?"	
	
	replace				times_cltd = 0 if times_cltd == .
	rename				times_cltd f_times	
	lab var				f_times "Number of times firewood was collected"	
	
	replace				time_cltd = 0 if time_cltd == .
	rename				time_cltd f_time
	lab var				f_time "Time spent collecting firewood (min)"		
	
	rename				price_cltd f_price
	lab var				f_price "Price of firewood collected (kwacha)"	
	
	replace				quant_cltd = 0 if quant_cltd == .
	rename				quant_cltd f_quant
	lab var				f_quant "Quantity of firewood collected (kg)"
	
* drop household that collected for 9 weeks
	drop if				week > 6

* natalia uses a price of $0.12 for charcoal and $0.13 for firewood
* replace price with these values
	replace				c_price = 0.12
	replace				f_price = 0.13
	lab var				f_price "Price of firewood (USD)"
	lab var				c_price "Price of charcoal (USD)"	

* generate upper and lower bound values	
	gen					f_quant_lb = f_quant
	gen					f_quant_ub = f_times*f_quant	
	lab var				f_quant_lb "Quantity of firewood lower bound (kg)"
	lab var				f_quant_ub "Quantity of firewood upper bound (kg)"
	
	gen					f_val_lb = f_price*f_quant_lb
	gen					f_val_ub = f_price*f_quant_ub
	lab var				f_val_lb "Value of firewood lower bound (USD)"
	lab var				f_val_ub "Value of firewood upper bound (USD)"
	
	gen					c_quant_lb = c_quant
	gen					c_quant_ub = c_times*c_quant
	lab var				c_quant_lb "Quantity of charcoal lower bound (kg)"
	lab var				c_quant_ub "Quantity of charcoal upper bound (kg)"
	
	gen					c_val_lb = c_price*c_quant_lb
	gen					c_val_ub = c_price*c_quant_ub
	lab var				c_val_lb "Value of charcoal lower bound (USD)"
	lab var				c_val_ub "Value of charcoal upper bound (USD)"
	
	gen					val_fuel_lb = f_val_lb + c_val_lb
	gen					val_fuel_ub = f_val_ub + c_val_ub
	lab var				val_fuel_lb "Value of all fuel lower bound (USD)"
	lab var				val_fuel_ub "Value of all fuel upper bound (USD)"
	

************************************************************************
**# 2 - add control variables
************************************************************************

* add cloud cover data
	gen 			cc = 100 if week == 1 & village == 1
	replace 		cc = 11.75 if week == 2 & village == 1
	replace 		cc= 99.99 if week == 3 & village == 1
	replace 		cc = 0.00 if week == 4 & village == 1
	replace 		cc = 0.00 if week == 5 & village == 1
	replace 		cc = 0.00 if week == 6 & village == 1
	replace 		cc = 100 if week == 1 & village == 0
	replace 		cc = 1.7 if week == 2 & village == 0
	replace 		cc = 2.06 if week == 3 & village == 0
	replace 		cc = 0 if week == 4 & village == 0
	replace 		cc = 0 if week == 5 & village == 0
	replace 		cc = 0.01 if week == 6 & village == 0
	replace 		cc = 100 if week == 1 & village == 2
	replace 		cc = 35.84 if week == 2 & village == 2
	replace 		cc = 0 if week == 3 & village == 2
	replace 		cc = 0 if week == 4 & village == 2
	replace 		cc = 0 if week == 5 & village == 2
	replace 		cc = 0 if week == 6 & village == 2
		
	lab var 		cc "Cloud Cover"	

* merge in household characteristics
	drop				village
	merge				m:1 hhid using "$export/c_var.dta"
	*** 3 households have fuel data but not household data
	*** hhid 247000, 340000, 347000
	
	keep if				_merge == 3
	drop				_merge


************************************************************************
**# 3 - import individual characeristics
************************************************************************

preserve

* load .csv and save as stata .dta
	import excel 	"$root/Lealui_mapungu_nalitoya_data28_02_2020.xlsx", ///
							sheet("Participants_HHmembers") firstrow case(lower) clear
	
	
* drop variables
	drop			farmers_name village participation_in_nsl_aas_activit ///
						year_of_birth solar_stove
	
* clean and label data
	destring		age_cal, replace
	
	gen				f_age = age_cal
	lab var			f_age "Firewood Collector Age"
	gen				c_age = age_cal
	lab var			c_ag "Charcoal Buyer Age"
	
	gen				f_sex = 0 if gender == "Men"
	replace			f_sex = 1 if gender == "Women"
	gen				c_sex = f_sex
	lab define 		fsex 0 "Male" 1 "Female"
	lab	values 		f_sex fsex
	lab	values 		c_sex fsex
	lab var			f_sex "Firewood Collector Sex"
	lab var			c_sex "Charcoal Buyer Sex"
	drop			gender

* create new ind ID variables
	gen				f_id = codcodhh
	gen				c_id = codcodhh
	lab var			f_id "Firewood Collector ID"
	lab var			c_id "Charcoal Buyer ID"
	
	drop			codcodhh
	
* drop one duplicate
	drop if			f_id == 332000 & f_age == 37
	
* save
	compress
	save 			"$export/fuel_ind.dta", replace
	
restore

* merge in individual data for firewood
	merge			m:1 f_id using "$export/fuel_ind.dta"
	*** 718 merged, 166 not merged in master

* drop observations from using
	drop if			_merge == 2
	drop			_merge
	

* merge in individual data for charcoal
	merge			m:1 c_id using "$export/fuel_ind.dta"
	*** 309 merged, 575 not merged in master	
	
* drop observations from using
	drop if			_merge == 2
	drop			_merge	
	

************************************************************************
**# 2 - final outcome: fuel collection heterogeneity
************************************************************************

* set up global list of control variables, including village dummies
	global 				x_cov hh_size ai tli sex age edu cc 	

	rename				solar treat_assign
	lab var				treat_assign "Solar Stove"
	
	order				village village hhid aas cc hh_size ai tli sex age ///
							edu treat_assign week


************************************************************************
**## 2.1 - final outcome: weekly fuel collection heterogeneity
************************************************************************

* generate interactions since coefplot isn't working
	gen					control_f = 1 if treat_assign == 0 & f_sex == 1
	replace				control_f = 0 if treat_assign == 0 & f_sex == 0
	replace				control_f = 0 if treat_assign == 1 & f_sex == 1
	replace				control_f = 0 if treat_assign == 1 & f_sex == 0
	lab var				control_f "Control x Female"
	
	gen					treat_f = 1 if treat_assign == 1 & f_sex == 1
	replace				treat_f = 0 if treat_assign == 1 & f_sex == 0
	replace				treat_f = 0 if treat_assign == 0 & f_sex == 1
	replace				treat_f = 0 if treat_assign == 0 & f_sex == 0
	lab var				treat_f "Treated x Female"
	
	gen					treat_m = 1 if treat_assign == 1 & f_sex == 0
	replace				treat_m = 0 if treat_assign == 1 & f_sex == 1
	replace				treat_m = 0 if treat_assign == 0 & f_sex == 1
	replace				treat_m = 0 if treat_assign == 0 & f_sex == 0
	lab var				treat_m "Treated x Male"

* firewood time at week-level use with and without controls using LPM	
	reg 				f_time i.treat_assign#i.f_sex i.aas i.village, vce(cluster hhid)
	eststo			 	wFT
	
	reg 				f_time i.treat_assign#i.f_sex $x_cov i.aas i.village, vce(cluster hhid)
	eststo 				wFTc	
	
* firewood quantity at week-level use with and without controls using LPM	
	reg 				f_quant_ub i.treat_assign#i.f_sex i.aas i.village, vce(cluster hhid)
	eststo 				wFQ
	
	reg 				f_quant_ub i.treat_assign#i.f_sex $x_cov i.aas i.village, vce(cluster hhid)
	eststo 				wFQc	
	
* charcoal quantity at week-level use with and without controls using LPM	
	reg 				c_quant_ub i.treat_assign#i.c_sex i.aas i.village, vce(cluster hhid)
	eststo 				wCQ
	
	reg 				c_quant_ub i.treat_assign#i.c_sex $x_cov i.aas i.village, vce(cluster hhid)
	eststo 				wCQc	
	
* fuel value at week-level use with and without controls using LPM	
	reg 				val_fuel_ub i.treat_assign#i.c_sex i.aas i.village, vce(cluster hhid)
	eststo 				wFV
	
	reg 				val_fuel_ub i.treat_assign#i.c_sex $x_cov i.aas i.village, vce(cluster hhid)
	eststo 				wFVc	
	

	coefplot			 (wFQ, label(w/o Controls) keep(1.treat_assign#0.f_sex ///
							1.treat_assign#1.f_sex 0.treat_assign#1.f_sex) msymbol(D) ///
							rename(0.treat_assign#1.f_sex  = "Control x Female " ///
							1.treat_assign#1.f_sex = "Solar Stove x Female " ///
							1.treat_assign#0.f_sex = "Solar Stove x Male ") base ///
							mcolor(gs8) mfcolor(white) ciopts(color(edkblue)) ) ///
							(wFQc, label(w/ Controls) keep(1.treat_assign#0.f_sex ///
							1.treat_assign#1.f_sex 0.treat_assign#1.f_sex) msymbol(X) ///
							rename(0.treat_assign#1.f_sex  = "Control x Female " ///
							1.treat_assign#1.f_sex = "Solar Stove x Female " ///
							1.treat_assign#0.f_sex = "Solar Stove x Male ") base ///
							mcolor(gs8) mfcolor(white) ciopts(color(dkorange)) )    ///
							(wCQ, label(w/o Controls) keep(1.treat_assign#0.c_sex ///
							1.treat_assign#1.c_sex 0.treat_assign#1.c_sex) msymbol(D) ///
							rename(0.treat_assign#1.c_sex  = " Control x Female" ///
							1.treat_assign#1.c_sex = " Solar Stove x Female" ///
							1.treat_assign#0.c_sex = " Solar Stove x Male") base ///
							mcolor(gs8) mfcolor(white) ciopts(color(edkblue)) ) ///
							(wCQc, label(w/ Controls) keep(1.treat_assign#0.c_sex ///
							1.treat_assign#1.c_sex 0.treat_assign#1.c_sex) msymbol(X) ///
							rename(0.treat_assign#1.c_sex  = " Control x Female" ///
							1.treat_assign#1.c_sex = " Solar Stove x Female" ///
							1.treat_assign#0.c_sex = " Solar Stove x Male") base ///
							mcolor(gs8) mfcolor(white) ciopts(color(dkorange)) )  , ///
							xline(0, lcolor(maroon)) levels(95) ciopts(lwidth(*3) lcolor(*3) ) ///
							xtitle("Point Estimates and 95% Confidence Intervals") ///
							headings("Control x Female " = "{bf:Firewood Quantity (kg)}" ///
							" Control x Female" = "{bf:Charcoal Quantity (kg)}")  ///
							legend(pos(12) col(2) order(2 4)) 	
							
				
	graph export 	"$figure/coef_fuel.png", as(png) replace

/* table 4, Panel A: Solar stove assignment on weekly fuel outcomes
	esttab 			wFT wFTc wFQ wFQc wCQ wCQc wFV wFVc ///
						using "$output/fuel_out.tex", b(3) se(3) replace ///
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
							keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) 
							

************************************************************************
**## 2.2 - final outcome: total fuel collection
************************************************************************

collapse 				(sum) f_time f_quant_ub c_quant_ub val_fuel_ub ///
							  f_quant_lb c_quant_lb val_fuel_lb ///
						(mean) cc, ///
							by(village hhid aas hh_size ai tli sex age edu treat_assign)

* firewood time at overall use with and without controls using LPM	
	reg 				f_time treat_assign i.aas i.village, vce(robust)
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFT
	
	reg 				f_time treat_assign $x_cov i.aas i.village, vce(robust)
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFTc	
	
* firewood quantity at overall use with and without controls using LPM	
	reg 				f_quant_ub treat_assign i.aas i.village, vce(robust)
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFQ
	
	reg 				f_quant_ub treat_assign $x_cov i.aas i.village, vce(robust)
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFQc	
	
* charcoal quantity at overall use with and without controls using LPM	
	reg 				c_quant_ub treat_assign i.aas i.village, vce(robust)
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tCQ
	
	reg 				c_quant_ub treat_assign $x_cov i.aas i.village, vce(robust)
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tCQc	
	
* fuel value at overall use with and without controls using LPM	
	reg 				val_fuel_ub treat_assign i.aas i.village, vce(robust)
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFV
	
	reg 				val_fuel_ub treat_assign $x_cov i.aas i.village, vce(robust)
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFVc	
	

* table 4, Panel B: Solar stove assignment on overall fuel
	esttab 			tFT tFTc tFQ tFQc tCQ tCQc tFV tFVc ///
						using "$output/fuel_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{9}{l}{\emph{Panel B: Overall Fuel Outcomes}} \\ ") ///
							keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
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
	log	close

/* END */					