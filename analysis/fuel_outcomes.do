* project: solar stove - fuel outcomes
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 18 july 2025
* stata v.19.5

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

* generate parameters
	egen			f_quant_m = rowmean(f_quant_ub f_quant_lb)
	lab var			f_quant_m "Quantity of firewood (mean)"
	
	egen			c_quant_m = rowmean(c_quant_ub c_quant_lb)
	lab var			c_quant_m "Quantity of charcoal (mean)"

	gen				scc	= 0.42
	lab var			scc "Social Cost of Carbon USD/kg"
	
	gen				co2e_f_m = 1.59
	lab var			co2e_f_m "CO2e firewood (mean)"
	
	gen				co2e_f_ub = 1.62
	lab var			co2e_f_ub "CO2e firewood (upper)"
	
	gen				co2e_f_lb = 1.56
	lab var			co2e_f_lb "CO2e firewood (lower)"

	gen				co2e_c_m = 10.461
	lab var			co2e_c_m "CO2e charcoal (mean)"

	gen				co2e_c_ub = 2.567 + 9.0
	lab var			co2e_c_ub "CO2e charcoal (upper)"
	
	gen				co2e_c_lb = 2.155 + 7.2
	lab var			co2e_c_lb "CO2e charcoal (lower)"

	gen				fNRB_m = .35
	lab var			fNRB_m "fNRB (mean)"

	gen				fNRB_ub = .39
	lab var			fNRB_ub "fNRB (upper)"
	
	gen				fNRB_lb = .31
	lab var			fNRB_lb "fNRB (lower)"
	
	
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
	reg 				f_quant_m treat_assign i.aas i.village, vce(cluster hhid)
	summarize 			f_quant_m if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wFQ
	
	reg 				f_quant_m treat_assign $x_cov i.aas i.village, vce(cluster hhid) coefleg
	predict				f_hat_m, xb
	gen					f_itt = _b[treat_assign]
	summarize 			f_quant_m if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				wFQc	
	
* charcoal quantity at week-level use with and without controls using LPM	
	reg 				c_quant_m treat_assign i.aas i.village, vce(cluster hhid)
	summarize 			c_quant_m if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				wCQ
	
	reg 				c_quant_m treat_assign $x_cov i.aas i.village, vce(cluster hhid)
	predict				c_hat_m, xb
	gen					c_itt = _b[treat_assign]
	summarize 			c_quant_m if treat_assign == 0	
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
**## 2.2 - CO2e
************************************************************************
	
* calculate SSC via ITT
	gen				co2e_itt = (f_itt * co2e_f_m + c_itt * co2e_c_m) * fNRB_m
	lab var			co2e_itt "CO2e (ITT)"
	
	gen				scc_itt = scc * co2e_itt
	lab var			scc_itt "Social Cost of Carbon (ITT)"
	
* summarize weekly savings
	sum				co2e_itt scc_itt
	
* run regressions	
	reg 			f_quant_ub treat_assign $x_cov i.aas i.village, vce(robust)
	predict			f_hat_ub, xb
	
	reg 			f_quant_lb treat_assign $x_cov i.aas i.village, vce(robust)
	predict			f_hat_lb, xb
	
	reg 			c_quant_ub treat_assign $x_cov i.aas i.village, vce(robust)
	predict			c_hat_ub, xb
	
	reg 			c_quant_lb treat_assign $x_cov i.aas i.village, vce(robust)
	predict			c_hat_lb, xb

* generate parameters
	gen				co2e_m = (f_hat_m * co2e_f_m + c_hat_m * co2e_c_m) * fNRB_m
	lab var			co2e_m "CO2e (mean)"
	
	gen				co2e_ub = (f_hat_ub * co2e_f_ub + c_hat_ub * co2e_c_ub) * fNRB_ub
	lab var			co2e_ub "CO2e (upper)"
	
	gen				co2e_lb = (f_hat_lb * co2e_f_lb + c_hat_lb * co2e_c_lb) * fNRB_lb
	lab var			co2e_lb "CO2e (lower)"
	
	gen				scc_m = scc * co2e_m
	lab var			scc_m "Social Cost of Carbon (mean)"
	
	gen				scc_ub = scc * co2e_ub
	lab var			scc_ub "Social Cost of Carbon (upper))"
	
	gen				scc_lb = scc * co2e_lb
	lab var			scc_lb "Social Cost of Carbon (lower))"
	
	
	sort 			scc_m
	gen 			obs = _n
	
	sum				scc_m if treat_assign == 0
	global			scc_c = r(mean)
	
	sum				scc_m if treat_assign == 1
	global			scc_t = r(mean)
	
	gen				scc_d = $scc_t - $scc_c
	sum				scc_d
	
	twoway 			(scatter scc_m obs if treat_assign == 0, mcolor(sienna%75) msymbol(Th) msize(tiny) ) || ///
						(scatter scc_m obs if treat_assign == 1, mcolor(teal%75) msymbol(Th) msize (tiny) ) || ///
						(rbar scc_lb scc_ub obs if treat_assign == 0, msize(tiny) barwidth(.2) color(sienna%50) ) || ///
						(rbar  scc_lb scc_ub obs if treat_assign == 1, msize(tiny) barwidth(.2) color(teal%50) ///
						yline($scc_c, lcolor(sienna) lstyle(solid) ) yline($scc_t, lcolor(teal) lstyle(solid) ) ///
						ytitle("Weekly SCC (in USD)") xtitle("Weekly Obs. - Sorted by Effect Size") ///
						text(34 55 "$32.85", color(sienna) place(nw) size(small)) ylabel(0 20 40 60 80) ///
						text(31 55 "$31.78", color(teal) place(sw) size(small)) ), ///
						legend(order(4 3) cols(2)  label(4 "Treatment") label(3 "Control") ///
						size(small) rowgap(.5) pos(6) ring(1)) 
	
* graph save
	graph export 	"$figure/scc_week.pdf", replace as(pdf)										

	
************************************************************************
**## 2.3 - final outcome: total fuel collection
************************************************************************

collapse 				(sum) f_time f_quant_m f_quant_ub c_quant_m c_quant_ub  ///
							  val_fuel_ub f_quant_lb c_quant_lb val_fuel_lb ///
						(mean) cc scc co2e_f_m co2e_f_ub co2e_f_lb co2e_c_m ///
								co2e_c_ub co2e_c_lb fNRB_m fNRB_ub fNRB_lb, ///
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
	reg 				f_quant_m treat_assign i.aas i.village, vce(robust)
	summarize 			f_quant_m if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tFQ
	
	reg 				f_quant_m treat_assign $x_cov i.aas i.village, vce(robust)
	predict				f_hat_m, xb
	gen					f_itt = _b[treat_assign]
	summarize 			f_quant_m if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "Yes", replace		
	eststo 				tFQc	
	
* charcoal quantity at overall use with and without controls using LPM	
	reg 				c_quant_m treat_assign i.aas i.village, vce(robust)
	summarize 			c_quant_m if treat_assign == 0	
	estadd scalar		dep_mean = r(mean)		
	estadd local 		cov "No", replace	
	eststo 				tCQ
	
	reg 				c_quant_m treat_assign $x_cov i.aas i.village, vce(robust)
	predict				c_hat_m, xb
	gen					c_itt = _b[treat_assign]
	summarize 			c_quant_m if treat_assign == 0	
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
						
					
************************************************************************
**## 2.4 - CO2e
************************************************************************
	
* calculate SSC via ITT
	gen				co2e_itt = (f_itt * co2e_f_m + c_itt * co2e_c_m) * fNRB_m
	lab var			co2e_itt "CO2e (ITT)"
	
	gen				scc_itt = scc * co2e_itt
	lab var			scc_itt "Social Cost of Carbon (ITT)"
	
* summarize weekly savings
	sum				co2e_itt scc_itt
	
* run regressions	
	reg 			f_quant_ub treat_assign $x_cov i.aas i.village, vce(robust)
	predict			f_hat_ub, xb
	
	reg 			f_quant_lb treat_assign $x_cov i.aas i.village, vce(robust)
	predict			f_hat_lb, xb
	
	reg 			c_quant_ub treat_assign $x_cov i.aas i.village, vce(robust)
	predict			c_hat_ub, xb
	
	reg 			c_quant_lb treat_assign $x_cov i.aas i.village, vce(robust)
	predict			c_hat_lb, xb

* generate parameters
	gen				co2e_m = (f_hat_m * co2e_f_m + c_hat_m * co2e_c_m) * fNRB_m
	lab var			co2e_m "CO2e (mean)"
	
	gen				co2e_ub = (f_hat_ub * co2e_f_ub + c_hat_ub * co2e_c_ub) * fNRB_ub
	lab var			co2e_ub "CO2e (upper)"
	
	gen				co2e_lb = (f_hat_lb * co2e_f_lb + c_hat_lb * co2e_c_lb) * fNRB_lb
	lab var			co2e_lb "CO2e (lower)"
	
	gen				scc_m = scc * co2e_m
	lab var			scc_m "Social Cost of Carbon (mean)"
	
	gen				scc_ub = scc * co2e_ub
	lab var			scc_ub "Social Cost of Carbon (upper))"
	
	gen				scc_lb = scc * co2e_lb
	lab var			scc_lb "Social Cost of Carbon (lower))"
	
	
	sort 			scc_m
	gen 			obs = _n
	
	sum				scc_m if treat_assign == 0
	global			scc_c = r(mean)
	
	sum				scc_m if treat_assign == 1
	global			scc_t = r(mean)
	
	gen				scc_d = $scc_t - $scc_c
	sum				scc_d
	
	twoway 			(scatter scc_m obs if treat_assign == 0, mcolor(sienna%75) msymbol(Th) msize(tiny) ) || ///
						(scatter scc_m obs if treat_assign == 1, mcolor(teal%75) msymbol(Th) msize (tiny) ) || ///
						(rbar scc_lb scc_ub obs if treat_assign == 0, msize(tiny) barwidth(.2) color(sienna%50) ) || ///
						(rbar  scc_lb scc_ub obs if treat_assign == 1, msize(tiny) barwidth(.2) color(teal%50) ///
						yline($scc_c, lcolor(sienna) lstyle(solid) ) yline($scc_t, lcolor(teal) lstyle(solid) ) ///
						ytitle("Overall SCC (in USD)") xtitle("Household # - Sorted by Effect Size") ///
						text(184 155 "$184", color(sienna) place(nw) size(small)) ylabel(0 150 300 450) ///
						text(172 155 "$172", color(teal) place(sw) size(small)) ), ///
						legend(order(4 3) cols(2)  label(4 "Treatment") label(3 "Control") ///
						size(small) rowgap(.5) pos(6) ring(1)) 
	
* graph save
	graph export 	"$figure/scc.pdf", replace as(pdf)			
	
************************************************************************
**# 3- examining trends for reviewer
************************************************************************
	

*******************************************************
* 1.  Run the model and store the estimates
*******************************************************

reg f_quant_m i.treat_assign#ibn.week i.aas i.village if treat_assign == 1, nocon vce(cluster hhid)
est store event_sum1

reg f_quant_m i.treat_assign#ibn.week i.aas i.village if treat_assign == 0, nocon vce(cluster hhid)
est store event_sum2

*******************************************************
* 2.  Build lists that keep / rename the interaction
*     terms so that week-specific coefficients in the
*     two series share the *same* name.
*     (Adjust the week list to match your data.)
*******************************************************
local weeks 1 2 3 4 5 6       // example

*  Build rename and keep lists on the fly
local ren_T ""     // rename rules for Treatment
local ren_C ""     // rename rules for Control
local keep_T ""    // keep list  for Treatment
local keep_C ""    // keep list  for Control

foreach w of local weeks {
    * coefficient names Stata created:
    *   1.treat_assign#`w'.week   (Treatment)
    *   0.treat_assign#`w'.week   (Control)
    
    local ren_T `ren_T'  1.treat_assign#`w'.week = week`w'
    local ren_C `ren_C'  0.treat_assign#`w'.week = week`w'
    
    local keep_T `keep_T'  1.treat_assign#`w'.week
    local keep_C `keep_C'  0.treat_assign#`w'.week
}

*******************************************************
* 3.  Plot: two sub-plots that draw over the same
*     x–positions (week1 … week10) because we renamed
*     the coefficients to the identical base names.
*******************************************************
coefplot                                                     ///
    (event_sum1,                                               ///
        keep(`keep_T')                                        ///
        rename(`ren_T')                                       ///
        label("Treatment")  ylabel(0(10)50)                                   ///
        recast(connected) lw(thick) lc(sea) mc(sea)  msymbol(O) )   ///
    (event_sum2,                                               ///
        keep(`keep_C')                                        ///
        rename(`ren_C')                                       ///
        label("Control")                                      ///
        recast(connected) lw(thick) lc(reddish) mc(reddish)  msymbol(T) ),    ///
    vertical  title("Firewood Quantity")                      ///
    yline(0, lc(black) lw(vthin))                             ///
    ciopts(recast(rline) lw(thin) lc(black) lp(dash))         ///
    graphregion(fcolor(white))                                ///
    legend(col(2) order(4 2))    			  ///
	saving("$figure/fire", replace)                 
		
	
*******************************************************
* 1.  Run the model and store the estimates
*******************************************************

reg c_quant_m i.treat_assign#ibn.week i.aas i.village if treat_assign == 1, nocon vce(cluster hhid)
est store event_sum1

reg c_quant_m i.treat_assign#ibn.week i.aas i.village if treat_assign == 0, nocon vce(cluster hhid)
est store event_sum2

*******************************************************
* 2.  Build lists that keep / rename the interaction
*     terms so that week-specific coefficients in the
*     two series share the *same* name.
*     (Adjust the week list to match your data.)
*******************************************************
local weeks 1 2 3 4 5 6       // example

*  Build rename and keep lists on the fly
local ren_T ""     // rename rules for Treatment
local ren_C ""     // rename rules for Control
local keep_T ""    // keep list  for Treatment
local keep_C ""    // keep list  for Control

foreach w of local weeks {
    * coefficient names Stata created:
    *   1.treat_assign#`w'.week   (Treatment)
    *   0.treat_assign#`w'.week   (Control)
    
    local ren_T `ren_T'  1.treat_assign#`w'.week = week`w'
    local ren_C `ren_C'  0.treat_assign#`w'.week = week`w'
    
    local keep_T `keep_T'  1.treat_assign#`w'.week
    local keep_C `keep_C'  0.treat_assign#`w'.week
}

*******************************************************
* 3.  Plot: two sub-plots that draw over the same
*     x–positions (week1 … week10) because we renamed
*     the coefficients to the identical base names.
*******************************************************
coefplot                                                     ///
    (event_sum1,                                               ///
        keep(`keep_T')                                        ///
        rename(`ren_T')                                       ///
        label("Treatment")  ylabel(0(10)50)                                  ///
        recast(connected) lw(thick) lc(sea) mc(sea)  msymbol(O) )   ///
    (event_sum2,                                               ///
        keep(`keep_C')                                        ///
        rename(`ren_C')                                       ///
        label("Control")                                      ///
        recast(connected) lw(thick) lc(reddish) mc(reddish)  msymbol(T) ),    ///
    vertical  title("Charcoal Quantity")                      ///
    yline(0, lc(black) lw(vthin))                             ///
    ciopts(recast(rline) lw(thin) lc(black) lp(dash))         ///
    graphregion(fcolor(white))  saving("$figure/coal", replace)                 
		
*combine and save
	grc1leg	"$figure/fire" "$figure/coal"
		
	graph export 	"$figure/trend.pdf", replace as(pdf)			

* close the log
	log				close
	
***END
													
							
							
							