* project: solar stove - food outcomes
* created on: Aug 2021
* created by: lem
* edited by: jdm
* edited on: 11 Mar 25
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
		eststo 			o_cntrl :	estpost ///
						sum ss_use hdds_dish sr_dish p_dish ///
						if treat_assign == 0
		
		eststo 			o_trtmnt : estpost ///
						sum ss_use hdds_dish sr_dish p_dish ///
						if treat_assign == 1

		eststo 			o_tot : estpost ///
						sum ss_use hdds_dish sr_dish p_dish
						
		eststo 			o_diff : estpost ///
						ttest ss_use hdds_dish sr_dish p_dish, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlo : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmnto : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			toto : display %4.0f `temp3a'

* output table of summary of dietary outcomes
	esttab 			o_cntrl o_trtmnt o_diff o_tot ///
						using "$output/out_tab.tex", replace ///
						prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total}  \\ \midrule " ///
						"\multicolumn{5}{l}{\emph{Panel A: Dish-Level Outcomes}} \\ " ) ///
						booktabs main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1)) p(fmt(3) pattern(0 0 1 0 ))"  ///
						sd(fmt(3)par pattern(1 1 0 1) ) ) ///
						rename(ss_use "Solar Stove Use" hdds_dish "HDDS" ///
						sr_dish "Species Richness" p_dish "Legumes Cooked") ///
						prefoot(" \midrule Observations & `cntrlo' & `trtmnto' & & `toto' \\ ")
									
* sum meal
	preserve 
		duplicates drop		hhid week day meal, force
		
		eststo 			m_cntrl :	estpost ///
						sum share_meal hdds_meal sr_meal p_meal ///
						if treat_assign == 0
		
		eststo 			m_trtmnt : estpost ///
						sum share_meal hdds_meal sr_meal p_meal ///
						if treat_assign == 1

		eststo 			m_tot : estpost ///
						sum share_meal hdds_meal sr_meal p_meal
						
		eststo 			m_diff : estpost ///
						ttest share_meal hdds_meal sr_meal p_meal, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlm : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntm : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			totm : display %4.0f `temp3a'
	restore	
			

* output table of summary of dietary outcomes
	esttab 			m_cntrl m_trtmnt m_diff m_tot ///
						using "$output/out_tab.tex", append ///
						posthead("\midrule \multicolumn{5}{l}{\emph{Panel B: Meal-Level Outcomes}} \\ " ) ///
						booktabs main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1)) p(fmt(3) pattern(0 0 1 0 ))"  ///
						sd(fmt(3)par pattern(1 1 0 1) ) ) ///
						rename(share_meal "Solar Stove Use" hdds_meal "HDDS" ///
						sr_meal "Species Richness"	p_meal "Legumes Cooked") ///
						prefoot(" \midrule Observations & `cntrlm' & `trtmntm' & & `totm' \\ ")
				
* sum day
	preserve 
		duplicates drop		hhid week day, force
		
		eststo 			d_cntrl :	estpost ///
						sum share_day hdds_day sr_day p_day ///
						if treat_assign == 0
		
		eststo 			d_trtmnt : estpost ///
						sum share_day hdds_day sr_day p_day ///
						if treat_assign == 1

		eststo 			d_tot : estpost ///
						sum share_day hdds_day sr_day p_day
						
		eststo 			d_diff : estpost ///
						ttest share_day hdds_day sr_day p_day, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrld : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntd : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			totd : display %4.0f `temp3a'
	restore	
			

* output table of summary of dietary outcomes
	esttab 			d_cntrl d_trtmnt d_diff d_tot ///
						using "$output/out_tab.tex", append ///
						posthead("\midrule \multicolumn{5}{l}{\emph{Panel C: Day-Level Outcomes}} \\ " ) ///
						booktabs main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1)) p(fmt(3) pattern(0 0 1 0 ))"  ///
						sd(fmt(3)par pattern(1 1 0 1) ) ) ///
						rename(share_day "Solar Stove Use" hdds_day "HDDS" ///
						sr_day "Species Richness"	p_day "Legumes Cooked") ///
						prefoot(" \midrule Observations & `cntrld' & `trtmntd' & & `totd' \\ ")
			
* sum week
	preserve 
		duplicates drop		hhid week, force
		
		eststo 			w_cntrl :	estpost ///
						sum share_week hdds_week sr_week p_week ///
						if treat_assign == 0
		
		eststo 			w_trtmnt : estpost ///
						sum share_week hdds_week sr_week p_week ///
						if treat_assign == 1

		eststo 			w_tot : estpost ///
						sum share_week hdds_week sr_week p_week
						
		eststo 			w_diff : estpost ///
						ttest share_week hdds_week sr_week p_week, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlw : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntw : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			totw : display %4.0f `temp3a'
	restore	
			

* output table of summary of dietary outcomes
	esttab 			w_cntrl w_trtmnt w_diff w_tot ///
						using "$output/out_tab.tex", append ///
						posthead("\midrule \multicolumn{5}{l}{\emph{Panel C: Week-Level Outcomes}} \\ " ) ///
						booktabs main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1)) p(fmt(3) pattern(0 0 1 0 ))"  ///
						sd(fmt(3)par pattern(1 1 0 1) ) ) ///
						rename(share_week "Solar Stove Use" hdds_week "HDDS" ///
						sr_week "Species Richness"	p_week "Legumes Cooked") ///
						prefoot(" \midrule Observations & `cntrlw' & `trtmntw' & & `totw' \\ ")
		
* sum week
	preserve 
		duplicates drop		hhid, force
		
		eststo 			t_cntrl :	estpost ///
						sum share_total hdds_total sr_total p_total ///
						if treat_assign == 0
		
		eststo 			t_trtmnt : estpost ///
						sum share_total hdds_total sr_total p_total ///
						if treat_assign == 1

		eststo 			t_tot : estpost ///
						sum share_total hdds_total sr_total p_total
						
		eststo 			t_diff : estpost ///
						ttest share_total hdds_total sr_total p_total, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlt : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntt : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			tott : display %4.0f `temp3a'
	restore	
			

* output table of summary of dietary outcomes
	esttab 			t_cntrl t_trtmnt t_diff t_tot ///
						using "$output/out_tab.tex", append ///
						posthead("\midrule \multicolumn{5}{l}{\emph{Panel C: Overall Outcomes}} \\ " ) ///
						booktabs main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1)) p(fmt(3) pattern(0 0 1 0 ))"  ///
						sd(fmt(3)par pattern(1 1 0 1) ) ) ///
						rename(share_total "Solar Stove Use" hdds_total "HDDS" ///
						sr_total "Species Richness"	p_total "Legumes Cooked") ///
						postfoot(" \midrule Observations & `cntrlt' & `trtmntt' & & `tott' \\ " ///
						"\hline \hline \\[-1.8ex] \multicolumn{5}{J{\linewidth}}{\small " ///
						"\noindent \textit{Note}: The table displays means, for solar stove use and outcome variables " ///
						"by treatment assignment and for the total sample. Standard deviations are in " ///
						"parentheses. We also report \$p\$-values " ///
						"on a \$t\$-test for the equality of means between treatment and control.}  \end{tabular}")
						
					

			
			
			
			
			
* output table of summary of dietary outcomes
	esttab 			o_cntrl o_trtmnt o_diff o_tot ///
						using "$output/out_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
						"\multicolumn{5}{c}{\emph{Panel A: Daily Cooking Frequency} &  \multicolumn{4}{c}{Weekly Cooking Frequency} " ///
						"& \multicolumn{4}{c}{Total Cooking Frequency} \\ " ///
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} " /// 
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} " /// 
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} \\ ") ///
						main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1 1 1 0 1 1 1 0 1)) p(fmt(3) pattern(0 0 1 0 0 0 1 0 0 0 1 0))"  ///
						sd(fmt(3)par pattern(1 1 0 1 1 1 0 1 1 1 0 1) ) ) ///
						rename(dish_eat "Dishes per Meal" dish_skp "Meals Skipped") ///
						postfoot(" \midrule Observations & `cntrld' & `trtmntd' & & `totd' & " ///
						" `cntrlw' & `trtmntw' & & `totw' & `cntrlt' & `trtmntt' & & `tott' \\ " ///
						"\hline \hline \\[-1.8ex] \multicolumn{13}{J{\linewidth}}{\small " ///
						"\noindent \textit{Note}: The table displays means, for solar stove use and outcome variables " ///
						"by treatment assignment and for the total sample. Standard deviations are in " ///
						"parentheses. We also report \$p\$-values " ///
						"on a \$t\$-test for the equality of means between treatment and control.}  \end{tabular}") 

* sum week
	preserve 
		duplicates drop		hhid week, force
		estpost sum 		
		est store			sum_week
	restore	

* sum total
	preserve 
		duplicates drop		hhid, force
		estpost sum 		share_total hdds_total sr_total p_total
		est store			sum_total
	restore	

************************************************************************
**## 1.2 - cooking frequency outcomes
************************************************************************

* daily frequency
	preserve
		duplicates drop		hhid week day, force
		
		rename			dish_day dish_eat
		rename			day_skip dish_skp
		
		eststo 			d_cntrl :	estpost ///
						sum dish_eat dish_skp ///
						if treat_assign == 0
		
		eststo 			d_trtmnt : estpost ///
						sum dish_eat dish_skp ///
						if treat_assign == 1

		eststo 			d_tot : estpost ///
						sum dish_eat dish_skp
						
		eststo 			d_diff : estpost ///
						ttest dish_eat dish_skp, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrld : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntd : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			totd : display %4.0f `temp3a'
	restore
				
* weekly frequency
	preserve
		duplicates drop		hhid week, force
		
		rename			dish_week dish_eat
		rename			week_skip  dish_skp
		
		eststo 			w_cntrl :	estpost ///
						sum dish_eat dish_skp ///
						if treat_assign == 0
		
		eststo 			w_trtmnt : estpost ///
						sum dish_eat dish_skp ///
						if treat_assign == 1

		eststo 			w_tot : estpost ///
						sum dish_eat dish_skp
						
		eststo 			w_diff : estpost ///
						ttest dish_eat dish_skp, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlw : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntw : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			totw : display %4.0f `temp3a'
	restore

	
* total frequency
	preserve
		duplicates drop		hhid, force
		
		rename			dish_tot dish_eat
		rename			tot_skip dish_skp
				
		eststo 			t_cntrl :	estpost ///
						sum dish_eat dish_skp ///
						if treat_assign == 0
		
		eststo 			t_trtmnt : estpost ///
						sum dish_eat dish_skp ///
						if treat_assign == 1

		eststo 			t_tot : estpost ///
						sum dish_eat dish_skp
						
		eststo 			t_diff : estpost ///
						ttest dish_eat dish_skp, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlt : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntt : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			tott : display %4.0f `temp3a'
	restore

		
* output table of summary of cooking frequency
	esttab 			d_cntrl d_trtmnt d_diff d_tot w_cntrl w_trtmnt w_diff w_tot t_cntrl t_trtmnt t_diff t_tot ///
						using "$output/cook_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{12}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
						"& \multicolumn{4}{c}{Daily Cooking Frequency} &  \multicolumn{4}{c}{Weekly Cooking Frequency} " ///
						"& \multicolumn{4}{c}{Total Cooking Frequency} \\ " ///
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} " /// 
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} " /// 
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} \\ ") ///
						main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(3) pattern(1 1 0 1 1 1 0 1 1 1 0 1)) p(fmt(3) pattern(0 0 1 0 0 0 1 0 0 0 1 0))"  ///
						sd(fmt(3)par pattern(1 1 0 1 1 1 0 1 1 1 0 1) ) ) ///
						rename(dish_eat "Dishes per Meal" dish_skp "Meals Skipped") ///
						postfoot(" \midrule Observations & `cntrld' & `trtmntd' & & `totd' & " ///
						" `cntrlw' & `trtmntw' & & `totw' & `cntrlt' & `trtmntt' & & `tott' \\ " ///
						"\hline \hline \\[-1.8ex] \multicolumn{13}{J{\linewidth}}{\small " ///
						"\noindent \textit{Note}: The table displays means, for dishes per meal and meals skipped " ///
						"by treatment assignment and for the total sample. Standard deviations are in " ///
						"parentheses. We also report \$p\$-values " ///
						"on a \$t\$-test for the equality of means between treatment and control.}  \end{tabular}") 
		
				
************************************************************************
**## 1.3 - fuel outcomes
************************************************************************

* load fuel data
	use					"$ans/fuel_cleaned.dta", clear	
	
	rename				solar treat_assign
						
* fuel week
	preserve
		duplicates drop		hhid week, force
		
		eststo 			w_cntrl :	estpost ///
						sum f_time f_quant_ub c_quant_ub val_fuel_ub ///
						if treat_assign == 0
		
		eststo 			w_trtmnt : estpost ///
						sum f_time f_quant_ub c_quant_ub val_fuel_ub ///
						if treat_assign == 1

		eststo 			w_tot : estpost ///
						sum f_time f_quant_ub c_quant_ub val_fuel_ub
						
		eststo 			w_diff : estpost ///
						ttest f_time f_quant_ub c_quant_ub val_fuel_ub, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlob : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmntob : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			totob : display %4.0f `temp3a'
	restore

	
* fuel total
	preserve
		duplicates drop		hhid, force
				
		eststo 			t_cntrl :	estpost ///
						sum f_time f_quant_ub c_quant_ub val_fuel_ub ///
						if treat_assign == 0
		
		eststo 			t_trtmnt : estpost ///
						sum f_time f_quant_ub c_quant_ub val_fuel_ub ///
						if treat_assign == 1

		eststo 			t_tot : estpost ///
						sum f_time f_quant_ub c_quant_ub val_fuel_ub
						
		eststo 			t_diff : estpost ///
						ttest f_time f_quant_ub c_quant_ub val_fuel_ub, ///
						by(treat_assign) unequal
	
		distinct		hhid if treat_assign == 0
		local 			temp1a = r(N) 
			local 			cntrlhh : display %4.0f `temp1a'
		
		distinct		hhid if treat_assign == 1
		local 			temp2a = r(N) 
			local 			trtmnthh : display %4.0f `temp2a'
		
		distinct		hhid
		local 			temp3a = r(N) 
			local 			tothh : display %4.0f `temp3a'
	restore

		
* output table of fuel collection
	esttab 			w_cntrl w_trtmnt w_diff w_tot t_cntrl t_trtmnt t_diff t_tot ///
						using "$output/fuel_tab.tex", replace booktabs ///
						prehead("\begin{tabular}{l*{8}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
						"& \multicolumn{4}{c}{Weekly Fuel Collection} & \multicolumn{4}{c}{Total Fuel Collection} \\ " ///
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} " /// 
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total} \\ ") ///
						main(mean) aux(sd) nostar unstack nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(1 2 2 3) pattern(1 1 0 1 1 1 0 1)) p(fmt(3) pattern(0 0 1 0 0 0 1 0))"  ///
						sd(fmt(1 2 2 3)par pattern(1 1 0 1 1 1 0 1) ) ) ///
						rename(f_time "Firewood Time (min)" f_quant_ub "Firewood Quantity (kg)" ///
						c_quant_ub "Charcoal Quantity (kg)" val_fuel_ub "Fuel Value (USD)" ) ///
						postfoot(" \midrule Observations & `cntrlob' & `trtmntob' & & `totob' & " ///
						" `cntrlhh' & `trtmnthh' & & `tothh' \\ " ///
						"\hline \hline \\[-1.8ex] \multicolumn{9}{J{\linewidth}}{\small " ///
						"\noindent \textit{Note}: The table displays means, for collected and purchased fuel " ///
						"by treatment assignment and for the total sample. Standard deviations are in " ///
						"parentheses. We also report \$p\$-values " ///
						"on a \$t\$-test for the equality of means between treatment and control.}  \end{tabular}") 
		
				
************************************************************************
**# 2 - covariates
************************************************************************

* re-load data
	use					"$ans/dietary_cleaned.dta", clear		
	
preserve // dropping dupes to give clearer idea of hh breakdown
	duplicates drop		hhid, force
	
* generate indicators for education and sex
	qui tab 			edu, generate(edu_)
	
	replace				sex = 0 if sex == 1
	replace				sex = 1 if sex == 2
	
	lab var				sex "Female Head of Household"
	lab var				edu_1 "No Education"
	lab var				edu_2 "Primary Education"
	lab var				edu_3 "Secondary Education"
	lab var				edu_4 "Higher Education"
	
* summary stats by treatment with t-test
	eststo 			cntrl :	estpost ///
						sum age sex  edu_* hh_size tli ai cc ///
						if treat_assign == 0
		
	eststo 			trtmnt : estpost ///
						sum age sex  edu_* hh_size tli ai cc ///
						if treat_assign == 1

	eststo 			tot : estpost ///
						sum age sex  edu_* hh_size tli ai cc
						
	eststo 			diff : estpost ///
						ttest age sex  edu_* hh_size tli ai cc, ///
						by(treat_assign) unequal
	
	distinct		hhid if treat_assign == 0
	local 			temp1a = r(N) 
		local 			cntrlhh : display %4.0f `temp1a'
		
	distinct		hhid if treat_assign == 1
	local 			temp2a = r(N) 
		local 			trtmnthh : display %4.0f `temp2a'
		
	distinct		hhid
	local 			temp3a = r(N) 
		local 			tothh : display %4.0f `temp3a'
						
	esttab 			cntrl trtmnt diff tot ///
						using "$output/convars_tab.tex", replace ///
						prehead("\begin{tabular}{l*{4}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] " ///
						"& \multicolumn{1}{c}{Control} & \multicolumn{1}{c}{Treatment} &  " ///
						"\multicolumn{1}{c}{\$p\$-value} & \multicolumn{1}{c}{Total}  \\ ") ///
						main(mean) aux(sd) nostar unstack label booktabs nonum ///
						collabels(none) f noobs nomtitle nogaps ///
						cells("mean(fmt(2 3 3 3 3 3 3 3 3 2) pattern(1 1 0 1)) p(fmt(3) pattern(0 0 1 0))"  ///
						sd(fmt(2 3 3 3 3 3 3 2 2 2)par pattern(1 1 0 1) )  ) ///
						postfoot(" \midrule Households & `cntrlhh' & `trtmnthh' & & `tothh' \\ " ///
						"\hline \hline \\[-1.8ex] \multicolumn{5}{J{\linewidth}}{\small " ///
						"\noindent \textit{Note}: The table displays means for controls " ///
						"by treatment assignment and for the total sample. Standard deviations are in " ///
						"parentheses. We also report \$p\$-values " ///
						"on a \$t\$-test for the equality of means between treatment and control.}  \end{tabular}") 

restore

		
************************************************************************
**# 3 - END
************************************************************************

* close the log
	log				close
	
***END
