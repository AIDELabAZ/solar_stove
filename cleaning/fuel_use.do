* project: solar stove - final outcomes: fuel use
* created on: February 2021
* created by: lem
* Stata v.15.1 (mac)

* does [UPDATE]
	* inputs raw_fuels.dta
	* relabels data
	* 

* assumes
	* some cleaning has already been done, but not sure where that code is

* to do:
	* formatting
	* clarify collected values (is estimated worth or actual amt sold?)

* **********************************************************************
* 0 - setup
* **********************************************************************

* define paths
	global	root	=	"$data"
	global	export	=	"$data/refined"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/raw_cleaning_lm", append

	
* load .csv and save as stata .dta
	*import delimited 				"$root/raw/Fuel/raw_fuel_data.csv", varn(1)
	*save 							"$root/raw/Fuel/raw_fuels.dta", replace

* data file in Box
	*import delimited 	"$root/raw/Fuel/fuel_final.csv", varn(1) clear
	
* save as data in memory	
	*save 				"$root/raw/Fuel/raw_fuels.dta", replace
	
* use .dta file
	use 			 	"$root/raw/Fuel/raw_fuels.dta", clear	

************************************************************************
* 1a - rename and relabel WEEKLY vars in fuel_final sheet
************************************************************************
		
* rename and relabel fuel indicator variables (used fuel type in week 1 = 
		* value of 1 for all observations within week 1

	rename				f_m_week ind_dung_week
	label var 			ind_dung_week "Weekly indicator for cow dung"
	
	rename				firewood_week ind_firewood_week
	label var 			ind_firewood_week "Weekly indicator for firewood"	
		
	rename				charcoal_week ind_charcoal_week
	label var 			ind_charcoal_week  "Weekly indicator for charcoal"	


* rename and relabel variables for total time spent collecting fuels each week,
		* earnings from fuels collected each week, and expenditures on 
		* fuels collected each week

	rename				time_col_min_week wk_allfuel_collect
	label var 			wk_allfuel_collect "Minutes collecting fuel per week"
	
	rename				money_col_kwacha_week wk_allfuel_sales
	label var 			wk_allfuel_sales "Weekly hh fuel sales (Kwacha)"	
		
	rename				money_pur_kwacha_week wk_allfuel_expend
	label var 			wk_allfuel_expend  "Weekly hh fuel expenditure (Kwacha)"


* rename and relabel variables for total weekly minutes spent collecting each fuel type
	* firewood, charcoal, and manure 	
	
	rename				f_time_col_min_week wk_firewood_collect
	label var 			wk_firewood_collect "Minutes collecting firewood per week"
	
	rename				f_m_time_col_min_week wk_dung_collect
	label var 			wk_dung_collect "Minutes collecting dung per week"
		
	rename				c_time_col_min_week wk_charcoal_collect
	label var 			wk_charcoal_collect "Minutes collecting charcoal per week"
		
* rename and relabel variables for weekly hh sales of each fuel type
	* firewood, charcoal, and manure 	

	rename				f_money_col_kwacha_week wk_firewood_sales
	label var 			wk_firewood_sales "Weekly HH firewood sales (Kwacha)"
	
	rename				f_m_money_col_kwacha_week wk_dung_sales
	label var 			wk_dung_sales "Weekly HH dung sales (Kwacha)"
		
	rename				c_money_col_kwacha_week wk_charcoal_sales
	label var 			wk_charcoal_sales "Weekly HH charcoal sales (Kwacha)"		 
		
* rename and relabel variables for weekly hh expenditures for each fuel type
	* firewood, charcoal, and manure 	

	rename				f_money_pur_kwacha_week wk_firewood_expend
	label var 			wk_firewood_expend "Weekly HH firewood expenditure (Kwacha)"
	
	rename				f_m_money_pur_kwacha_week wk_dung_expend
	label var 			wk_dung_expend "Weekly HH dung expenditure (Kwacha)"
		
	rename				c_money_pur_kwacha_week wk_charcoal_expend
	label var 			wk_charcoal_expend "Weekly HH charcoal expenditure (Kwacha)"		 	
		
* rename and relabel variables for kg of weekly hh sales for each fuel type
	* firewood, charcoal, and manure 	

	rename				f_q_col_kg_week wk_firewood_quantcollect
	label var 			wk_firewood_quantcollect "Quantity weekly HH firewood collected (kg)"
	
	rename				f_m_q_col_kg_week wk_dung_quantcollect
	label var 			wk_dung_quantcollect "Quantity weekly HH dung collected (kg)"
		
	rename				c_q_col_kg_week wk_charcoal_quantcollect
	label var 			wk_charcoal_quantcollect "Quantity weekly HH charcoal collected (kg)"		
		
* rename and relabel variables for kg of weekly hh expenditures for each fuel type
	* firewood, charcoal, and manure 		
		
	rename				f_q_pur_kg_week wk_firewood_quantbought
	label var 			wk_firewood_quantbought "Quantity weekly HH firewood purchased (kg)"
	
	rename				f_m_q_pur_kg_week wk_dung_quantbought
	label var 			wk_dung_quantbought "Quantity weekly HH dung purchased (kg)"
		
	rename				c_q_pur_kg_week wk_charcoal_quantbought
	label var 			wk_charcoal_quantbought "Quantity weekly HH charcoal purchased (kg)"			
			
************************************************************************
* 1b - rename and relabel TOTAL (6 WEEKS) vars in fuel_final sheet
************************************************************************		

* rename and relabel variables for total time spent collecting fuels each week,
		* earnings from fuels collected each week, and expenditures on 
		* fuels collected each week

	rename				time_col_min_tweek tot_allfuel_collect
	label var 			tot_allfuel_collect "Minutes collecting fuel total"
	
	rename				money_col_kwacha_tweek tot_allfuel_sales
	label var 			tot_allfuel_sales "Total hh fuel sales (Kwacha)"	
		
	rename				money_pur_kwacha_tweek tot_allfuel_expend
	label var 			tot_allfuel_expend  "Total hh fuel expenditure (Kwacha)"


* rename and relabel variables for total total minutes spent collecting each fuel type
	* firewood, charcoal, and manure 	
	
	rename				f_time_col_min_tweek tot_firewood_collect
	label var 			tot_firewood_collect "Minutes collecting firewood"
	
	rename				f_m_time_col_min_tweek tot_dung_collect
	label var 			tot_dung_collect "Minutes collecting dung"
		
	rename				c_time_col_min_tweek tot_charcoal_collect
	label var 			tot_charcoal_collect "Minutes collecting charcoal"
		
* rename and relabel variables for total hh sales of each fuel type
	* firewood, charcoal, and manure 	

	rename				f_money_col_kwacha_tweek tot_firewood_sales
	label var 			tot_firewood_sales "Total HH firewood sales (Kwacha)"
	
	rename				f_m_money_col_kwacha_tweek tot_dung_sales
	label var 			tot_dung_sales "Total HH dung sales (Kwacha)"
		
	rename				c_money_col_kwacha_tweek tot_charcoal_sales
	label var 			tot_charcoal_sales "Total HH charcoal sales (Kwacha)"		 
		
* rename and relabel variables for total hh expenditures for each fuel type
	* firewood, charcoal, and manure 	

	rename				f_money_pur_kwacha_tweek tot_firewood_expend
	label var 			tot_firewood_expend "Total HH firewood expenditure (Kwacha)"
	
	rename				f_m_money_pur_kwacha_tweek tot_dung_expend
	label var 			tot_dung_expend "Total HH dung expenditure (Kwacha)"
		
	rename				c_money_pur_kwacha_tweek tot_charcoal_expend
	label var 			tot_charcoal_expend "Total HH charcoal expenditure (Kwacha)"		 	
		
* rename and relabel variables for kg of total hh sales for each fuel type
	* firewood, charcoal, and manure 	

	rename				f_q_col_kg_tweek tot_firewood_quantcollect
	label var 			tot_firewood_quantcollect "Quantity total HH firewood collected (kg)"
	
	rename				f_m_q_col_kg_tweek tot_dung_quantcollect
	label var 			tot_dung_quantcollect "Quantity total HH dung collected (kg)"
		
	rename				c_q_col_kg_tweek tot_charcoal_quantcollect
	label var 			tot_charcoal_quantcollect "Quantity total HH charcoal collected (kg)"		
		
* rename and relabel variables for kg of total hh expenditures for each fuel type
	* firewood, charcoal, and manure 		
		
	rename				f_q_pur_kg_tweek tot_firewood_quantbought
	label var 			tot_firewood_quantbought "Quantity total HH firewood purchased (kg)"
	
	rename				f_m_q_pur_kg_tweek tot_dung_quantbought
	label var 			tot_dung_quantbought "Quantity total HH dung purchased (kg)"
		
	rename				c_q_pur_kg_tweek tot_charcoal_quantbought
	label var 			tot_charcoal_quantbought "Quantity total HH charcoal purchased (kg)"	
		
	
* rename village, household, week, and day variables
	rename 				cod hhid
	lab var 			hhid "HH ID"


************************************************************************
* 2 - move later after figuring out merge etc; estimating ITT for ss assignment on minutes collecting fuels
************************************************************************

	reg 				wk_allfuel_collect treat_assign i.group i.village, vce(cluster hhid)		
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store fcoll
	eststo 				clear
	
	reg 				wk_allfuel_collect treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store fcollc		
	eststo 				clear
	
* dish-level use with and without controls using OLS
	reg 				wk_allfuel_sales treat_assign i.group i.village, vce(cluster hhid)		
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store fsale
	eststo 				clear
	
	reg 				wk_allfuel_sales treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store fsalec		
	eststo 				clear
	
	
* dish-level use with and without controls using OLS	
	reg 				wk_allfuel_expend treat_assign i.group i.village, vce(cluster hhid)		
	estadd local 		cov "No", replace
	estadd local 		fe "Yes", replace
	est					store fexpend
	eststo 				clear
	
	reg 				wk_allfuel_expend treat_assign $x_cov i.group i.village, vce(cluster hhid)
	estadd local 		cov "Yes", replace			
	estadd local 		fe "Yes", replace	
	est					store fexpendc		
	eststo 				clear
	
	
* table 4: dish use ss probit, lpm
	esttab store fcoll store fcollc fsale fsalec fexpend fexpendc using "$output/intermediate_outcomes/io_dish.tex", replace f ///
		label booktabs b(3) se(3) eqlabels(none) alignment(S)  ///
		keep(treat_assign) ///
		star(* 0.10 ** 0.05 *** 0.01) nogaps ///
		stats(margins N cov fe r2, fmt(3 0 0 0 3 3 3) layout( ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{c}{@}" ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}" "\multicolumn{1}{S}{@}" ///
		"\multicolumn{1}{S}{@}") ///
		labels( `"Observations"' `"Covariates"' `"Village Fixed Effects"' `"\(R^{2}\)"' ))


