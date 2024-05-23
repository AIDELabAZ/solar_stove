* project: solar stove - intermediate outcomes
* created on: August 2021
* created by: lem
* edited by:
* Stata v.15.1 (mac)

* does


***********************************************************************
* 0 - setup
***********************************************************************

* define
	global				ans		=	"$data"
	global				output	=	"$data/analysis/tables"
	global				figure	=	"$data/analysis/figures"
	global				logout	=	"$data/logs"
	local 				tabnum  =   1

* open log
	cap log 			close 
	log using			"$logout/outcomes", append

* load data
	use					"$ans/refined/dietary_cleaned.dta", clear		
								
* set up global list of control variables, including village dummies
	global 				x_cov age gender educ hh_size tli cc 	

************************************************************************
* 1 - outcome variables
************************************************************************

************************************************************************
* 1 (a) - intermediate outcomes
************************************************************************

* treatment assignment variable
	estpost sum 		treat_assign 
	est store 			sum_assign
	
* treatment use variable
	estpost sum 		ss_use
	est store 			sum_use
		
* share of meals prepared on solar stoves	
	preserve 
		duplicates drop		hhid week day meal, force
		estpost sum 		share_meal 
		est store			sum_sharem
	restore	
	
	preserve 
		duplicates drop		hhid week day, force
		estpost sum 		share_day 
		est store			sum_sharew
	restore	
	
	preserve 
		duplicates drop		hhid week, force
		estpost sum 		share_week 
		est store			sum_shareda
	restore	
	
	preserve 
		duplicates drop		hhid, force
		estpost sum 		share_tot 
		est store			sum_sharet
	restore	
			
************************************************************************
* 1 (b) - final outcomes: dietary composition by level of aggregation
************************************************************************
	
* (1bi) household dietary diversity scores: count

* hdds: count dish
	estpost sum 		HDDS_ct_d 
	est store 			sumc_hdds_d
		
* hdds: count meal	
	preserve 
		duplicates drop		hhid week day meal, force		
		estpost sum 		HDDS_ct_m 
		est store 			sumc_hdds_m
	restore
	
* hdds: count day		
	preserve 
		duplicates drop		hhid week day, force		
		estpost sum 		HDDS_ct_da 					
		est store 			sumc_hdds_da
	restore
	
* hdds: count week			
	preserve 
		duplicates drop		hhid week, force		
		estpost sum 		HDDS_ct_w 
		est store 			sumc_hdds_w
	restore
	
* hdds: count total		
	preserve 
		duplicates drop		hhid, force		
		estpost sum 		HDDS_ct_t 
		est store 			sumc_hdds_t
	restore	
	
* (1bii) household dietary diversity scores: average
	
* hdds: avg meal
	preserve 
		duplicates drop		hhid week day meal, force
		estpost sum 		HDDS_avg_m 
		est store 			suma_hdds_m
	restore
	
* hdds: avg day	
	preserve 
		duplicates drop		hhid week day, force		
		estpost sum 		HDDS_avg_da 					
		est store 			suma_hdds_da
	restore
	
* hdds: avg week
	preserve 
		duplicates drop		hhid week, force	
		estpost sum 		HDDS_avg_w 
		est store 			suma_hdds_w		
	restore	

* hdds: avg total	
	preserve 
		duplicates drop		hhid, force	
		estpost sum 		HDDS_avg_t 
		est store 			suma_hdds_t	
	restore	
	
* (1c) dietary species richness scores: count

* dsr: count dish
	estpost sum 		DSR_dish 
	est store 			sumc_dsr_d
	
* dsr: count meal	
	preserve 
		duplicates drop		hhid week day meal, force
		estpost sum 		DSR_meal
		est store 			sumc_dsr_m
	restore

* dsr: count day	
	preserve 
		duplicates drop		hhid week day, force	
		estpost sum 		DSR_day					
		est store 			sumc_dsr_da
	restore

* dsr: count week	
	preserve 
		duplicates drop		hhid week, force	
		estpost sum 		DSR_week
		est store 			sumc_dsr_w
	restore

* dsr: count total	
	preserve 
		duplicates drop		hhid, force	
		estpost sum 		DSR_total 
		est store 			sumc_dsr_t
	restore

* (1d) number of dishes in a meal: avg	

* avg number of dishes in meal total
	preserve // dropping dupes to give clearer idea of hh breakdown
		duplicates drop		hhid, force	
		estpost sum 		avg_brdish 
		est store 			sum_avg_brdish
		estpost sum 		avg_lundish
		est store 			sum_avg_lundish
		estpost sum 		avg_dindish					
		est store 			sum_avg_dindish
		estpost sum 		avg_dish
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
		estpost sum 		legume_ct_day
		est store 			sum_legume_ct_day
	restore

* number of times legumes consumed in a week	
	preserve 
		duplicates drop		hhid week, force	
		estpost sum 		legume_ct_week
		est store 			sum_legume_ct_week		
	restore

* number of times legumes consumed total
	preserve 
		duplicates drop		hhid, force	
		estpost sum 		legume_ct_tot				
		est store 			sum_legume_ct_tot	
	restore
	
* output table of descriptive statistics for outcome variables
	esttab 				sum_assign sum_use sum_sharem sum_shareda sum_sharew ///
							sum_sharet sumc_hdds_d sumc_hdds_m sumc_hdds_da ///
							sumc_hdds_w sumc_hdds_t suma_hdds_m suma_hdds_da ///
							suma_hdds_w suma_hdds_t sumc_dsr_d sumc_dsr_m ///
							sumc_dsr_da sumc_dsr_w sumc_dsr_t sum_avg_brdish ///
							sum_avg_lundish sum_avg_dindish sum_avg_dish ///
							sum_hhbr_skipped sum_hhlun_skipped sum_hhdin_skipped ///
							sum_hhtot_skipped sum_legume_ct_day ///
							sum_legume_ct_week sum_legume_ct_tot using "$output/descriptive/outcomev_tab.tex", replace booktabs ///
						cells("indent count(label(count)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") ///
						 nonumber noobs nomtitle f
						
************************************************************************
* 2 - covariates
************************************************************************

************************************************************************
* 2 (a) - categorical covariates
************************************************************************	

* (2ai) summary statistics for categorical covariates

	preserve // dropping dupes to give clearer idea of hh breakdown
		duplicates drop		hhid, force	
		* post frequency table for villages
		estpost tab			village	
		est store 			village_t
		* post frequency table for gender
		estpost tab			gender
		est store 			gender_t
		* post frequency table for educational attainment
		estpost tab			educ
		est store 			educ_t
		* post frequency table for aas group
		estpost tab			group
		est store 			group_t		
	restore
				
* output table of descriptive statistics for categorical variables
	esttab group_t educ_t gender_t village_t   using "$output/descriptive/catvars_tab.tex", replace booktabs ///
						cells("indent b(label(freq)) pct(fmt(3)) cumpct(fmt(3))") ///
						varlabels(, blist(Total "{hline @width}{break}")) ///
						nonumber noobs nomtitle  f

************************************************************************
* 2 (b) - non-categorical covariates
************************************************************************
						
* (2bi) summary statistics for non-categorical covariates
	preserve // dropping dupes to give clearer idea of hh breakdown
		duplicates drop		hhid, force	
		* post frequency table for treatment assignment
		estpost sum			treat_assign	
		est store 			treat_assign
		* post frequency table for ss use
		estpost sum			ss_use
		est store 			ss_use	
		* post frequency table for aas group
		estpost sum			group
		est store 			group_s			
		* post frequency table for age
		estpost sum			age	
		est store 			age		
		* post frequency table for cc
		estpost sum			cc
		est store 			cc			
		* post frequency table for educational attainment
		estpost sum			educ
		est store 			educ_s		
		* post frequency table for gender
		estpost sum			gender
		est store 			gender_s
		* post frequency table for hh_size
		estpost sum			hh_size
		est store 			hh_size		
		* post frequency table for tli
		estpost sum			tli
		est store 			tli			
		* post frequency table for villages
		estpost sum			village	
		est store 			village_s
	restore						
						
* output table of descriptive statistics for categorical variables
	esttab treat_assign ss_use group_s age cc educ_s gender_s hh_size tli village_s   using "$output/descriptive/convars_tab.tex", replace booktabs ///
						cells("indent count(label(count)) mean(fmt(3)) sd(fmt(3)) min(fmt(3)) max(fmt(3))") ///
						 nonumber noobs nomtitle f
						
************************************************************************
* 3 - END
************************************************************************
