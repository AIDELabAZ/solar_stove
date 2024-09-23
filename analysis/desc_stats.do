* project: solar stove - food outcomes
* created on: Aug 2021
* created by: lem
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
	* add avg HDDS, SR, and legumes
	* create summary table for other outcomes

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
**## 1.1 - intermediate outcomes
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
							"\noindent \textit{Note}: The table displays summary statistics for " ///
							"treatment assignment, use of solar stoves, and continuous control variables.}  \end{tabular}") 	
							
			
************************************************************************
**## 1.2 - final outcomes: dietary composition by level of aggregation
************************************************************************

* (1bii) household dietary diversity scores: average
	
* hdds: avg meal
	preserve 
		duplicates drop		hhid week day meal, force
		estpost sum 		hdds_avg_meal
		est store 			suma_hdds_m
	restore
	
* hdds: avg day	
	preserve 
		duplicates drop		hhid week day, force		
		estpost sum 		hdds_avg_day					
		est store 			suma_hdds_da
	restore
	
* hdds: avg week
	preserve 
		duplicates drop		hhid week, force	
		estpost sum 		hdds_avg_week
		est store 			suma_hdds_w		
	restore	

* hdds: avg total	
	preserve 
		duplicates drop		hhid, force	
		estpost sum 		hdds_total
		est store 			suma_hdds_t	
	restore	
	


* (1d) number of dishes in a meal: avg	

* avg number of dishes in meal total
	preserve // dropping dupes to give clearer idea of hh breakdown
		duplicates drop		hhid, force	
		estpost sum 		hhbrdish_tot 
		est store 			sum_avg_brdish
		estpost sum 		hhlundish_tot
		est store 			sum_avg_lundish
		estpost sum 		hhdindish_tot				
		est store 			sum_avg_dindish
		estpost sum 		hhdish_tot
		est store 			sum_avg_dish

* (1e) total number meals skipped: count
		
* total meals skipped	
		estpost sum 		hhbr_skipped
		est store 			sum_hhbr_skipped
		estpost sum 		hhlun_skipped
		est store 			sum_hhlun_skipped
		estpost sum 		hhdin_skipped				
		est store 			sum_hhdin_skipped
		estpost sum 		hhtot_skipped
		est store 			sum_hhtot_skipped
	restore
	
* (1f) total legumes consumed: count 	
	
* number of times legumes consumed in a day
	preserve 
		duplicates drop		hhid week day, force	
		estpost sum 		p_day
		est store 			sum_legume_ct_day
	restore

* number of times legumes consumed in a week	
	preserve 
		duplicates drop		hhid week, force	
		estpost sum 		p_week
		est store 			sum_legume_ct_week		
	restore

* number of times legumes consumed total
	preserve 
		duplicates drop		hhid, force	
		estpost sum 		p_total		
		est store 			sum_legume_ct_tot	
	restore
	
* output table of descriptive statistics for outcome variables
	esttab 				sum_assign sum_use sum_sharem sum_shareda sum_sharew ///
							sum_sharet sumc_hdds_d sumc_hdds_m sumc_hdds_da ///
							sumc_hdds_w sumc_hdds_t suma_hdds_m suma_hdds_da ///
							suma_hdds_w suma_hdds_t sumc_sr_d sumc_sr_m ///
							sumc_sr_da sumc_sr_w sumc_sr_t sum_avg_brdish ///
							sum_avg_lundish sum_avg_dindish sum_avg_dish ///
							sum_hhbr_skipped sum_hhlun_skipped sum_hhdin_skipped ///
							sum_hhtot_skipped sum_legume_ct_day ///
							sum_legume_ct_week sum_legume_ct_tot using "$output/descriptive/outcomev_tab.tex", replace booktabs ///
						cells("indent count(label(count)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") ///
						 nonumber noobs nomtitle f
						
************************************************************************
**# 2 - covariates
************************************************************************

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
