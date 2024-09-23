* project: solar stove - food outcomes
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 23 Sep 2024
* stata v.18.5

* does
	* inputs cleaned dietary data
	* outputs results and table code for food outcomes
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
	log using			"$logout/food_outcomes", append
	
	
************************************************************************
**# 1 - load and prepare data
************************************************************************

* load data
	use					"$ans/dietary_cleaned.dta", clear	
						
* set up global list of control variables, including village dummies
	global 				x_cov hh_size ai tli sex age i.edu cc 	

	lab var				treat_assign "Solar Stove"
	
	
************************************************************************
**# 2 - intermediate outcome: take-up measured by ss use indicator
************************************************************************

* Q: Do households assigned solar stoves use solar stoves?

* dish-level use with and without controls using LPM	
	reg 				ss_use treat_assign i.aas i.village, vce(cluster hhid)
	estadd local 		cov "No", replace	
	eststo 				dLPM
	
	reg 				ss_use treat_assign $x_cov i.aas i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace		
	eststo 				dLPMc	
				
* meal-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid week day meal, force	
		
		reg 			share_meal treat_assign i.aas i.village, vce(cluster hhid)	
		estadd local 	cov "No", replace		
		eststo 			mshare
			
		reg 			share_meal treat_assign $x_cov i.aas i.village, vce(cluster hhid)	
		estadd local 	cov "Yes", replace		
		eststo 			msharec	
	restore					 
	
* day-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid week day, force	
		
		reg 			share_day treat_assign i.aas i.village, vce(cluster hhid)  
		estadd local 	cov "No", replace	
		eststo 			dshare	
		
		reg 			share_day treat_assign $x_cov i.aas i.village, vce(cluster hhid)
		estadd local 	cov "Yes", replace	
		eststo 			dsharec	
	restore
	
* week-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid week, force	
		
		reg 			share_week treat_assign i.aas i.village, vce(cluster hhid) 
		estadd local 	cov "No", replace	
		eststo 			wshare	
					
		reg 			share_week treat_assign $x_cov i.aas i.village, vce(cluster hhid)
		estadd local 	cov "Yes", replace		
		eststo 			wsharec
	restore
	
* overall (6 week)-level share of ss use with and without controls using OLS
	preserve
		duplicates drop		hhid, force	
		
		reg 			share_tot treat_assign i.aas i.village, vce(cluster hhid) 
		estadd local 	cov "No", replace	
		eststo 			tshare	
		
		reg 			share_tot treat_assign $x_cov i.aas i.village, vce(cluster hhid)	
		estadd local 	cov "Yes", replace		
		eststo 			tsharec	
	restore			
			
* table 1: OLS use of solar stoves
	esttab 			dLPM dLPMc mshare msharec dshare dsharec wshare wsharec tshare tsharec ///
						using "$output/ss_use.tex", b(3) se(3) replace ///
							prehead("\begin{tabular}{l*{10}{c}} \\[-1.8ex]\hline \hline \\[-1.8ex] " ///
							"& \multicolumn{2}{c}{Dish} & \multicolumn{2}{c}{Meal} & \multicolumn{2}{c}{Day} " ///
							"& \multicolumn{2}{c}{Week} & \multicolumn{2}{c}{Overall} \\ \cline{2-3} " ///
							"\cline{4-5} \cline{6-7} \cline{8-9} \cline{10-11} \\[-1.8ex] " ///		                     
							"& \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} " ///
							"& \multicolumn{1}{c}{(4)} &\multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} " ///
							"& \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} & \multicolumn{1}{c}{(9)} " ///
							"& \multicolumn{1}{c}{(10)} \\ \midrule")  keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(N cov r2_a, labels("Observations" ///
							"Covariates" "Adjusted R$^2$") fmt(%9.0fc %4.3f)) ///
							postfoot("\hline \hline \\[-1.8ex] \multicolumn{11}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: Dependent variable is the number of dishes, or " ///
							"the share of dishes in a gven meal, day, week, etc., for which a " ///
							"solar stove was used. All regressions include two levels of strata " ///
							"fixed effects: village and Agricultural and Aquatic Systems (AAS) group. " ///
							"For regressions with more than one observation per houhold (columns 1-8), " ///
							"we calculate Liang-Zeger cluster-robust standard errors since the unit " ///
							"of randomization is the household. For regressions with only one " ///
							"observation per household (columns 9-10), we calculate Eicker-Huber-White " ///
							"(EHW) robust standard errors. Standard errors are presented in " ///
							"parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 
		
	
************************************************************************
**# 3 - final outcomes: food diversity
************************************************************************

* Q: Do households with solar stoves change the composition of their diet?


************************************************************************
**## 3.1 - household dietary diversity score
************************************************************************

* final hdds dish outcomes with and without controls using OLS
		reg 				hdds_dish treat_assign i.aas i.village, vce(cluster hhid) 
		summarize 			hdds_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		est					store dHDDS
		
		reg					hdds_dish treat_assign $x_cov i.aas i.village, vce(cluster hhid)
		summarize 			hdds_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dHDDSc
	
* final hdds meal outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day meal, force
			
			reg 			hdds_meal treat_assign i.aas i.village, vce(cluster hhid)
			summarize 		hdds_meal if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store mHDDS
			
			reg				hdds_meal treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		hdds_meal if treat_assign == 0		
			estadd scalar	dep_mean = r(mean)			
			estadd local 	cov "Yes", replace		
			est				store mHDDSc
		restore
			
* final hdds day outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day, force
			
			reg				hdds_day treat_assign i.aas i.village, vce(cluster hhid)  	
			summarize 		hdds_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "No", replace		
			est				store daHDDS	
			
			reg				hdds_day treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		hdds_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store daHDDSc	
		restore
	
* final hdds week outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week, force	
			
			reg				hdds_week treat_assign i.aas i.village, vce(cluster hhid) 
			summarize 		hdds_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store wHDDS
			
			reg				hdds_week treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		hdds_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store wHDDSc
		restore 

* final hdds overall outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid, force	
	
			reg				hdds_total treat_assign i.aas i.village, vce(robust)  
			summarize 		hdds_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace			
			est				store tHDDS
			
			reg				hdds_total treat_assign $x_cov i.aas i.village, vce(robust)
			summarize 		hdds_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "Yes", replace		
			est				store tHDDSc	
		restore


* table 2, Panel A: Solar stove assignment on HDDS
	esttab 			dHDDS dHDDSc mHDDS mHDDSc daHDDS daHDDSc wHDDS wHDDSc tHDDS tHDDSc ///
						using "$output/diverse_out.tex", b(3) se(3) replace ///
							prehead("\begin{tabular}{l*{10}{c}} \\[-1.8ex]\hline \hline \\[-1.8ex] " ///
							"& \multicolumn{2}{c}{Dish} & \multicolumn{2}{c}{Meal} & \multicolumn{2}{c}{Day} " ///
							"& \multicolumn{2}{c}{Week} & \multicolumn{2}{c}{Overall} \\ \cline{2-3} " ///
							"\cline{4-5} \cline{6-7} \cline{8-9} \cline{10-11} \\[-1.8ex] " ///	                   
							"& \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} " ///
							"& \multicolumn{1}{c}{(4)} &\multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} " ///
							"& \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)} & \multicolumn{1}{c}{(9)} " ///
							"& \multicolumn{1}{c}{(10)} \\ \midrule " ///
							"\multicolumn{11}{l}{\emph{Panel A: Dietary Diversity Score}} \\ ") ///
							keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f))

							
************************************************************************
**## 3.2 - species richness
************************************************************************	

* final sr dish outcomes with and without controls using OLS
		reg 				sr_dish treat_assign i.aas i.village, vce(cluster hhid) 
		summarize 			sr_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		est					store dSR
		
		reg					sr_dish treat_assign $x_cov i.aas i.village, vce(cluster hhid)
		summarize 			sr_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dSRc
	
* final sr meal outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day meal, force
			
			reg 			sr_meal treat_assign i.aas i.village, vce(cluster hhid)
			summarize 		sr_meal if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store mSR
			
			reg				sr_meal treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		sr_meal if treat_assign == 0		
			estadd scalar	dep_mean = r(mean)			
			estadd local 	cov "Yes", replace		
			est				store mSRc
		restore
			
* final sr day outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day, force
			
			reg				sr_day treat_assign i.aas i.village, vce(cluster hhid)  	
			summarize 		sr_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "No", replace		
			est				store daSR	
			
			reg				sr_day treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		sr_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store daSRc
		restore
	
* final sr week outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week, force	
			
			reg				sr_week treat_assign i.aas i.village, vce(cluster hhid) 
			summarize 		sr_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store wSR
			
			reg				sr_week treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		sr_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store wSRc
		restore 

* final sr overall outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid, force	
	
			reg				sr_total treat_assign i.aas i.village, vce(robust)  
			summarize 		sr_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace			
			est				store tSR
			
			reg				sr_total treat_assign $x_cov i.aas i.village, vce(robust)
			summarize 		sr_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "Yes", replace		
			est				store tSRc	
		restore


* table 2, Panel B: Solar stove assignment on SR
	esttab 			dSR dSRc mSR mSRc daSR daSRc wSR wSRc tSR tSRc ///
						using "$output/diverse_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{11}{l}{\emph{Panel B: Species Richness}} \\ ") ///
							keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) 


							
************************************************************************
**## 3.3 - frequency of legumes
************************************************************************	

* final legumes dish outcomes with and without controls using OLS
		reg 				p_dish treat_assign i.aas i.village, vce(cluster hhid) 
		summarize 			p_dish if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "No", replace			
		est					store dP
		
		reg					p_dish treat_assign $x_cov i.aas i.village, vce(cluster hhid)
		summarize 			p_dish if treat_assign == 0		
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dPc
	
* final legumes meal outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day meal, force
			
			reg 			p_meal treat_assign i.aas i.village, vce(cluster hhid)
			summarize 		p_meal if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store mP
			
			reg				p_meal treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		p_meal if treat_assign == 0		
			estadd scalar	dep_mean = r(mean)			
			estadd local 	cov "Yes", replace		
			est				store mPc
		restore
			
* final legumes day outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week day, force
			
			reg				p_day treat_assign i.aas i.village, vce(cluster hhid)  	
			summarize 		p_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "No", replace		
			est				store daP	
			
			reg				p_day treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		p_day if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store daPc
		restore
	
* final legumes week outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid week, force	
			
			reg				p_week treat_assign i.aas i.village, vce(cluster hhid) 
			summarize 		p_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace		
			est				store wP
			
			reg				p_week treat_assign $x_cov i.aas i.village, vce(cluster hhid)
			summarize 		p_week if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)		
			estadd local 	cov "Yes", replace			
			est				store wPc
		restore 

* final legumes overall outcomes with and without controls using OLS
		preserve
			duplicates drop		hhid, force	
	
			reg				p_total treat_assign i.aas i.village, vce(robust)  
			summarize 		p_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "No", replace			
			est				store tP
			
			reg				p_total treat_assign $x_cov i.aas i.village, vce(robust)
			summarize 		p_total if treat_assign == 0	
			estadd scalar	dep_mean = r(mean)	
			estadd local 	cov "Yes", replace		
			est				store tPc	
		restore


* Table 2, Panel C: Solar stove assignment on legumes
	esttab 			dP dPc mP mPc daP daPc wP wPc tP tPc ///
						using "$output/diverse_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{11}{l}{\emph{Panel C: Count of Legume Consumption}} \\ ") ///
							keep(treat_assign) noobs ///
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
**# 4 - final outcomes: frequency of cooking
************************************************************************		

* Q: Do households with solar stoves change their cooking behavior?


************************************************************************
**## 4.1 - # of dishes prepared
************************************************************************		
	
* number of breakfast dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					hhbrdish_tot treat_assign i.aas i.village, vce(robust) 
		summarize 			hhbrdish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store bnump		
	
		reg 				hhbrdish_tot treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhbrdish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store bnumpc	
	restore				
	
* number of lunch dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					hhlundish_tot treat_assign i.aas i.village, vce(robust) 
		summarize 			hhlundish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace		
		est					store lnump	
		
		reg					hhlundish_tot treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhlundish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)	
		estadd local 		cov "Yes", replace			
		est					store lnumpc	
	restore	
						   	
* number of dinner dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force
		
		reg					hhdindish_tot treat_assign i.aas i.village, vce(robust)  
		summarize 			hhdindish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace		
		est					store dnump			
		
		reg					hhdindish_tot  treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhdindish_tot  if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "Yes", replace			
		est					store dnumpc		
	restore

* number of all dishes prepared for a hh over the time period using OLS
 	preserve
		duplicates drop		hhid, force	
		
		reg 				hhdish_tot treat_assign i.aas i.village, vce(robust) 
		summarize 			hhdish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)		
		estadd local 		cov "No", replace			
		est					store tnump			
		
		reg 				hhdish_tot treat_assign $x_cov i.aas i.village, vce(robust) 
		summarize 			hhdish_tot if treat_assign == 0	
		estadd scalar		dep_mean = r(mean)
		estadd local 		cov "Yes", replace			
		est					store tnumpc			
	restore		

* table 3, Panel A: Solar stove assignment on dishes prepared
	esttab 			bnump bnumpc lnump lnumpc dnump dnumpc tnump tnumpc ///
						using "$output/freq_out.tex", b(3) se(3) replace ///
							prehead("\begin{tabular}{l*{8}{c}} \\[-1.8ex]\hline \hline \\[-1.8ex] " ///
							"& \multicolumn{2}{c}{Breakfast} & \multicolumn{2}{c}{Lunch} & " ///
							"\multicolumn{2}{c}{Dinner} & \multicolumn{2}{c}{Overall} \\ \cline{2-3} " ///
							"\cline{4-5} \cline{6-7} \cline{8-9} \\[-1.8ex] " ///	                   
							"& \multicolumn{1}{c}{(1)} & \multicolumn{1}{c}{(2)} & \multicolumn{1}{c}{(3)} " ///
							"& \multicolumn{1}{c}{(4)} &\multicolumn{1}{c}{(5)} & \multicolumn{1}{c}{(6)} " ///
							"& \multicolumn{1}{c}{(7)} & \multicolumn{1}{c}{(8)}  \\ \midrule " ///
							"\multicolumn{9}{l}{\emph{Panel A: Number of Dishes Per Meal}} \\ ") ///
							keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f))
							
							
************************************************************************
**## 4.2 - # of meals skipped
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

* table 3, Panel B: Solar stove assignment on skipped meals
	esttab 			bskip bskipc lskip lskipc dskip dskipc tskip tskipc ///
						using "$output/freq_out.tex", b(3) se(3) append ///
							prehead("\midrule \multicolumn{9}{l}{\emph{Panel B: Number of Meals Skipped}} \\ ") ///
							keep(treat_assign) noobs ///
							booktabs nonum nomtitle collabels(none) nobaselevels nogaps ///
							fragment label stat(dep_mean N cov r2_a, labels( "Mean in Control" ///
							"Observations" "Covariates" "Adjusted R$^2$") fmt(%4.3f %9.0fc %4.3f)) ///
							postfoot("\hline \hline \\[-1.8ex] \multicolumn{9}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: Dependent variables are different measure of " ///
							" frequency of cooking. In Panel A, we use the number of dishes cooked " ///
							"in a meal. In Panel B, we use the number of meals skipped. " ///
							"All regressions include two levels of strata " ///
							"fixed effects: village and Agricultural and Aquatic Systems (AAS) group. " ///
							"Eicker-Huber-White (EHW) robust standard errors. Standard errors are presented in " ///
							"parentheses (*** p$<$0.001, ** p$<$0.01, * p$<$0.05).}  \end{tabular}") 
						

* close the log
	log				close
	
***END
