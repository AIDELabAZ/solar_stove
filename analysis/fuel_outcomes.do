* project: solar stove - fuel outcomes
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 16 Sep 2024
* stata v.18.5

* does
	* inputs cleaned fuel data
	* outputs results and table code for fuel outcomes
	* outcomes limited to JDE short paper (not full PAP)

* assumes
	* access to cleaned fuel data
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
	log using			"$logout/fuel_outcomes", append
	
	
************************************************************************
**# 1 - load and prepare data
************************************************************************

* load data
	use					"$ans/fuel_cleaned.dta", clear	
						
* set up global list of control variables, including village dummies
	global 				x_cov hh_size ai tli sex age edu cc 	

	rename				solar treat_assign
	lab var				treat_assign "Solar Stove"
	
	order				village village hhid aas cc hh_size ai tli sex age ///
							edu treat_assign week


************************************************************************
**# 2 - final outcome: fuel collection
************************************************************************

* Q: Do households assigned solar stoves spend less on fuel?


************************************************************************
**## 2.1 - final outcome: weekly fuel collection
************************************************************************

* firewood time at week-level use with and without controls using LPM	
	reg 				f_time treat_assign i.aas i.village, vce(cluster hhid)
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFT
	
	reg 				f_time treat_assign $x_cov i.aas i.village, vce(cluster hhid)
	summarize 			f_time if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFTc	
	
* firewood quantity at week-level use with and without controls using LPM	
	reg 				f_quant_ub treat_assign i.aas i.village, vce(cluster hhid)
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFQ
	
	reg 				f_quant_ub treat_assign $x_cov i.aas i.village, vce(cluster hhid)
	summarize 			f_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFQc	
	
* charcoal quantity at week-level use with and without controls using LPM	
	reg 				c_quant_ub treat_assign i.aas i.village, vce(cluster hhid)
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wCQ
	
	reg 				c_quant_ub treat_assign $x_cov i.aas i.village, vce(cluster hhid)
	summarize 			c_quant_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wCQc	
	
* fuel value at week-level use with and without controls using LPM	
	reg 				val_fuel_ub treat_assign i.aas i.village, vce(cluster hhid)
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFV
	
	reg 				val_fuel_ub treat_assign $x_cov i.aas i.village, vce(cluster hhid)
	summarize 			val_fuel_ub if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFVc	
	
  
* table 4, Panel A: Solar stove assignment on weekly fuel outcomes
	esttab 			wFT wFTc wFQ wFQc wCQ wCQc wFV wFVc ///
						using "$output/fuel_out.tex", b(2) se(2) replace ///
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
						using "$output/fuel_out.tex", b(2) se(2) append ///
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
	log				close
	
***END
													
							
							
							