* project: solar stove - food outcomes
* created on: Aug 2021
* created by: lem
* edited by: jdm
* edited on: 24 Sep 2024
* stata v.18.5

* does
	* inputs cleaned dietary data
	* outputs results and table code for food outcomes
	* outcomes limited to JDE short paper (not full PAP)

* assumes
	* access to cleaned dietary data
	* estout

* to do:
	* revise table in section 1.2 with new variables on frequency

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
	log using			"$logout/desc_stats", append

* load data
	use					"$ans/dietary_cleaned.dta", clear		
					
						
************************************************************************
**# 1 - outcome variables
************************************************************************

************************************************************************
**## 1.1 - intermediate and dietary outcomes
************************************************************************

* sum dish
	estpost sum 		ss_use hdds_dish sr_dish p_dish
	est store 			sum_dish

* sum meal
	preserve 
		duplicates drop		hhid week day meal, force
		estpost sum 		share_meal hdds_meal sr_meal p_meal
		est store			sum_meal
	restore	

* sum day
	preserve 
		duplicates drop		hhid week day, force
		estpost sum 		share_day hdds_day sr_day p_day
		est store			sum_day
	restore	

* sum week
	preserve 
		duplicates drop		hhid week, force
		estpost sum 		share_week hdds_week sr_week p_week
		est store			sum_week
	restore	

* sum total
	preserve 
		duplicates drop		hhid, force
		estpost sum 		share_total hdds_total sr_total p_total
		est store			sum_total
	restore	
		
* output table of descriptive statistics for categorical variables
	esttab 			sum_dish sum_meal sum_day sum_week sum_total ///
						using "$output/descriptive/out_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{5}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
						main(mean) aux(sd) label  mlabels("Dish" "Meal" "Day" "Week" "Total") collabels(,none) ///
						nomtitle nonumber fragment nogap noobs  ///
						rename(ss_use "Solar Stove Used" hdds_dish "HDDS" sr_dish "Species Richness" ///
						p_dish "Legumes Cooked" share_meal "Solar Stove Used" hdds_meal "HDDS" ///
						sr_meal "Species Richness" p_meal "Legumes Cooked" share_day "Solar Stove Used" ///
						hdds_day "HDDS" sr_day "Species Richness" p_day "Legumes Cooked" ///
						share_week "Solar Stove Used" hdds_week "HDDS" sr_week "Species Richness" ///
						p_week "Legumes Cooked" share_total "Solar Stove Used" hdds_total "HDDS" ///
						sr_total "Species Richness" p_total "Legumes Cooked") ///
						postfoot("\midrule \multicolumn{1}{l}{Total} &  30,314 & 15,896 & " ///
							" 6,013 & 912 & 156\\ " "\hline \hline \\[-1.8ex] " ///
							"\multicolumn{6}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays means and standard deviations, " ///
							"in parentheses, treatment assignment, use of solar stoves, " ///
							"and continuous control variables.}  \end{tabular}") 	
							
			
************************************************************************
**## 1.2 - cooking frequency outcomes
************************************************************************

* sum breakfast
	preserve
		keep if				meal == 0
		estpost sum 		brdish_tot br_skip
		est store 			sum_brk
	restore

* sum lunch
	preserve
		keep if				meal == 1
		estpost sum 		lundish_tot lun_skip
		est store 			sum_lun
	restore
	
* sum dinner
	preserve
		keep if				meal == 2
		estpost sum 		din_skip dindish_tot
		est store 			sum_din
	restore
	
* sum total
	estpost sum 		dish_tot tot_skipped
	est store 			sum_tot
		
* output table of descriptive statistics for categorical variables
	esttab 			sum_brk sum_lun sum_din sum_tot ///
						using "$output/descriptive/cook_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
						main(mean) aux(sd) label  mlabels("Breakfast" "Lunch" "Dinner" "Total") collabels(,none) ///
						nomtitle nonumber fragment nogap noobs  ///
						rename(hhbrdish_tot "Dishes per Meal" hhlundish_tot "Dishes per Meal" ///
						hhdindish_tot "Dishes per Meal" hhdish_tot "Dishes per Meal" ///
						hhbr_skipped "Meals Skipped" hhlun_skipped "Meals Skipped" ///
						hhdin_skipped "Meals Skipped" hhtot_skipped "Meals Skipped" ) ///
						postfoot("\midrule \multicolumn{1}{l}{Total} &  5,690 & 12,701 & " ///
							" 11,923 & 30,314 \\ " "\hline \hline \\[-1.8ex] " ///
							"\multicolumn{5}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays means and standard deviations, " ///
							"in parentheses, for dishes per meal and meals skipped.}  \end{tabular}") 	
		
		
************************************************************************
**## 1.3 - fuel outcomes
************************************************************************

* load fuel data
	use					"$ans/fuel_cleaned.dta", clear	
						
* fuel week
	preserve
		duplicates drop		hhid week, force
		estpost sum 		f_time f_quant_ub c_quant_ub val_fuel_ub
		est store 			fuel_week
	restore

	
* fuel total
	preserve
		duplicates drop		hhid, force
		estpost sum 		f_time f_quant_ub c_quant_ub val_fuel_ub
		est store 			fuel_tot
	restore
		
* output table of descriptive statistics for categorical variables
	esttab 			fuel_week fuel_tot ///
						using "$output/descriptive/fuel_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{2}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
						main(mean) aux(sd) label  mlabels("Week" "Total") collabels(,none) ///
						nomtitle nonumber fragment nogap noobs  ///
						rename(f_time "Firewood Time (min)" f_quant_ub "Firewood Quantity (kg)" ///
						c_quant_ub "Charcoal Quantity (kg)" val_fuel_ub "Fuel Value (USD)" ) ///
						postfoot("\midrule \multicolumn{1}{l}{Total} &  870 & 157 \\ " "\hline \hline \\[-1.8ex] " ///
							"\multicolumn{3}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays means and standard deviations, " ///
							"in parentheses, for collected and purchased fuel.}  \end{tabular}") 	

							
************************************************************************
**# 2 - covariates
************************************************************************

* re-load data
	use					"$ans/dietary_cleaned.dta", clear		
	
************************************************************************
**## 2.1 - non-categorical covariates
************************************************************************

	preserve // dropping dupes to give clearer idea of hh breakdown
		duplicates drop		hhid, force
		
		* post frequency table
		estpost sum			age hh_size tli ai cc
		est store 			convar
		
	restore						
						
* output table of descriptive statistics for categorical variables
	esttab 			convar using "$output/descriptive/convars_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
						cells("mean(label(Mean) fmt(a3)) sd(label(St. Dev.) fmt(a3)) min(label(Min) fmt(a3)) max(label(Max) fmt(a3))") ///
						nomtitle nonumber fragment nogap noobs  label ///
						postfoot("\midrule \multicolumn{4}{l}{Total Households} &      156 \\ " ///
							"\hline \hline \\[-1.8ex] \multicolumn{5}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays summar statistics for " ///
							"treatment assignment, use of solar stoves, and continuous control variables.}  \end{tabular}") 		
	

************************************************************************
**## 2.2 - categorical covariates
************************************************************************	

	preserve // dropping dupes to give clearer idea of hh breakdown
		duplicates drop		hhid, force	
		
		* post frequency table for treatment assignment
		estpost tab			treat_assign, sort nototal 
		est store 			treat_t	
		
		esttab 			 treat_t using "$output/descriptive/catvars_tab.tex", replace booktabs ///
							prehead("\begin{tabular}{l*{2}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
							posthead("\cline{2-3} \\[-1.8ex] \multicolumn{3}{l}{\emph{Treatment Assignment}} \\ ") ///
							cells("b(label(Frequency) fmt(%9.0gc)) pct(label(Percent) fmt(2))") ///
							nonumber nomtitle fragment nomtitles noobs 
		
		* post frequency table for aas group
		estpost tab			aas, sort nototal 
		est store 			group_t	
		
		esttab 			 group_t using "$output/descriptive/catvars_tab.tex", append booktabs ///
							posthead("\multicolumn{3}{l}{\emph{AAS Group}} \\ ") ///
							cells("b( fmt(%9.0gc)) pct( fmt(2))") ///
							nonumber nomtitle fragment nomtitles noobs mlabels(,none) collabels(,none)
							
		* post frequency table for gender
		estpost tab			sex, sort nototal 
		est store 			gender_t
		
		esttab 			 gender_t using "$output/descriptive/catvars_tab.tex", append booktabs ///
							posthead("\multicolumn{3}{l}{\emph{Gender of Head of Household}} \\ ") ///
							cells("b( fmt(%9.0gc)) pct( fmt(2))") ///
							nonumber nomtitle fragment nomtitles noobs mlabels(,none) collabels(,none)
		
		* post frequency table for educational attainment
		estpost tab			edu, sort nototal 
		est store 			educ_t
		
		esttab 			 educ_t using "$output/descriptive/catvars_tab.tex", append booktabs ///
							posthead("\multicolumn{3}{l}{\emph{Education of Head of Household}} \\ ") ///
							cells("b(label(Frequency) fmt(%9.0gc)) pct(label(Percent) fmt(2))") ///
							nonumber nomtitle fragment nomtitles noobs  mlabels(,none) collabels(,none)
		
		* post frequency table for villages
		estpost tab			village, sort nototal 
		est store 			village_t
		
		esttab 			 village_t using "$output/descriptive/catvars_tab.tex", append booktabs ///
							posthead("\multicolumn{3}{l}{\emph{Village}} \\ ") ///
							cells("b(label(Frequency) fmt(%9.0gc)) pct(label(Percent) fmt(2))") ///
							nonumber nomtitle fragment nomtitles noobs mlabels(,none) collabels(,none) ///
							postfoot("\midrule \multicolumn{2}{l}{Total Households} &      156 \\ " ///
							"\hline \hline \\[-1.8ex] \multicolumn{3}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays the number and " ///
							"frequency of each categorical control variable.}  \end{tabular}") 				
		
		
	restore
		
************************************************************************
**# 3 - END
************************************************************************

* close the log
	log				close
	
***END
