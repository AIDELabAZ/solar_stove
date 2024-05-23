* project: solar stove - simple mean differences
* created on: June 2021
* created by: lem
* edited by:
* Stata v.15.1 (mac)

* does
	* inputs cleaned dietary data
	* outputs results and table code for simple mean differences

* to do:
	* finish storing outcome 1b3-1b4 in matrix
	* find better way do store estimates for tables


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
	
************************************************************************
* 1 - simple mean differences
************************************************************************	
	
************************************************************************
* 1a - simple mean differences in intermediate outcomes
************************************************************************	
	
* We conduct a comparison of means for the intermediate outcomes using the 
	* Mann-Whitney-Wilcoxon rank-sum test.

* (i) across the treatment and control arms for the entire sample

	ranksum 			ss_use, by(treat_assign)	
	
* (ii) across the treatment and control arms for each group (cooking 
* demonstrations, participation in agricultural demonstrations, or both) in
* the sample.

* (iia) across those who participated in nutrition clubs (nc) 
	
 tabstat ss_use treat_assign if group == 1, stats (median) save

* (iib) across those who participated in learning plots	(lp) 

	ranksum 			ss_use if group == 1, by(treat_assign)
	
* (iic) across those who participated in both nc and lp 

	ranksum 			ss_use if group == 3, by(treat_assign)
	
* (iic) across those who participated in neither nc nor lp	

	ranksum 			ss_use if group == 0, by(treat_assign)
	
************************************************************************
* 1b1 - simple mean differences in final outcomes: ENTIRE SAMPLE
************************************************************************	
	
* We conduct a comparison of means for the final outcomes using the 
	* Mann-Whitney-Wilcoxon rank-sum test.

	* load data
	use					"$ans/refined/dietary_cleaned.dta", clear	
	

** (i) hdds count	
	* (ia) across the treatment and control arms: HDDS count at meal level
		preserve
			duplicates drop		hhid week day meal, force
			estpost tabstat 	HDDS_ct_m, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_m, by(treat_assign)	
			estadd scalar		pval_m = ((2 * normprob(-abs(r(z)))))	
		restore		
		
		* store results in matrix because i've spent too long on esttab
		* mat a1 is mean over stdev
		mat 				a1 = e(mean)\e(sd)
		
		* mat a2 is the pval from mww test. not standard output so have to 
			* transform it; putting it over "." so it's conformable with a1
		mat 				a2 = ((2 * normprob(-abs(r(z)))))\.
		
		* save all that into bigger matrix
		mat 				HDDS_ct_m = a1, a2
		
		* renaming columns and rows
		mat 				coln HDDS_ct_m = control_m treat_m mww_p_m
		mat 				rown HDDS_ct_m = HDDSct_meal_mean HDDSct_meal_sd
		
	* table 1: entire sample: hdds count: meal
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_m) nonotes nostar unstack replace

		
	* (ib) across the treatment and control arms: hdds count at day level	
	
		preserve
			duplicates drop		hhid week day, force
			estpost tabstat 	HDDS_ct_d, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_d, by(treat_assign)	
			estadd scalar		pval_d = ((2 * normprob(-abs(r(z)))))	
		restore				
		
	* store results in matrix 
		mat 				b1 = e(mean)\e(sd)
		mat 				b2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_ct_d = b1, b2	
		
		mat 				coln HDDS_ct_d = control_d treat_d mww_p_d
		mat 				rown HDDS_ct_d = HDDSct_day_mean HDDSct_day_sd
		
	* table 1: entire sample: hdds count: day
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_d) nonotes nostar unstack append
				
	
	* (ic) across the treatment and control arms: hdds count at week level		

		preserve
			duplicates drop		hhid week, force
			estpost tabstat 	HDDS_ct_w, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_w, by(treat_assign)	
			estadd scalar		pval_w = ((2 * normprob(-abs(r(z)))))	
		restore	
		
	* store results in matrix 			
		mat 				c1 = e(mean)\e(sd)
		mat 				c2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_ct_w = c1, c2	
		
		mat 				coln HDDS_ct_w = control_w treat_w mww_p_w
		mat 				rown HDDS_ct_w = HDDSct_week HDDSct_week_sd
		
	* table 1: entire sample: hdds count: week
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_w) nonotes nostar unstack append
		
	* (id) across the treatment and control arms: hdds count overall	

		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_ct_t, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_t, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 	
		mat 				d1 = e(mean)\e(sd)
		mat 				d2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_ct_t = d1, d2	
		mat 				coln HDDS_ct_t = control_t treat_t mww_p_t
		mat 				rown HDDS_ct_t = HDDSct_tot_mean HDDSct_tot_sd
					
	* combine all hdds_avg results for entire sample into this thing	
		mat 				HDDS_ct = HDDS_ct_m, HDDS_ct_d, HDDS_ct_w, HDDS_ct_t
		mat 				li HDDS_ct
		*** this matrix is first "row" of results table	
		
	* table 1: entire sample: hdds count: total
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_t) nonotes nostar unstack append
		
** (ii) hdds avg

	* (ii) across the treatment and control arms: hdds avg at meal level 
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			

	* store results in matrix 			
		mat 				e1 = e(mean)\e(sd)
		mat 				e2 = ((2 * normprob(-abs(r(z)))))\.
		mat				 	HDDS_avg_m = e1, e2	
		
		mat 				coln HDDS_avg_m = control_m treat_m mww_p_m
		mat 				rown HDDS_avg_m = HDDSavg_meal_mean HDDSavg_meal_sd

	* (iib) across the treatment and control arms: hdds avg at day level	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
	
	* store results in matrix 	
		mat 				f1 = e(mean)\e(sd)
		mat 				f2 = ((2 * normprob(-abs(r(z)))))\.
		mat		 			HDDS_avg_d = f1, f2	
		
		mat coln HDDS_avg_d = control_d treat_d mww_p_d
		mat rown HDDS_avg_d = HDDSavg_day_mean HDDSavg_day_sd
		
	* (iic) across the treatment and control arms: hdds avg at week level
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore		
		
		* store results in matrix 
		
		mat 				g1 = e(mean)\e(sd)
		mat 				g2 = ((2 * normprob(-abs(r(z)))))\.
		mat					HDDS_avg_w = g1, g2	
		
		mat 				coln HDDS_avg_w = control_w treat_w mww_p_w
		mat 				rown HDDS_avg_w = HDDSavg_week_mean HDDSavg_week_sd	
			
	* (iid) [<-- iid!] across the treatment and control arms: hdds avg overall	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 
		
		mat 				h1 = e(mean)\e(sd)
		mat 				h2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_avg_t = h1, h2	
		
		mat 				coln HDDS_avg_t = control treat mww_p
	    mat 				rown HDDS_avg_t = HDDSavg_tot_mean HDDSavg_tot_sd
				
	* combine all hdds_avg results for entire sample into 1 matrix		
		mat				 	HDDS_avg = HDDS_avg_m, HDDS_avg_d, HDDS_avg_w, HDDS_avg_t
		mat 				li HDDS_avg	
		*** this matrix is second "row" of results table

** (iii) dsr count

	* (iiia) across the treatment and control arms: dsr count at meal level 
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_meal, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_meal, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 	
		mat 				i1 = e(mean)\e(sd)
		mat	 				i2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_meal = i1, i2	
		
		mat 				coln DSR_meal = control_t treat_t mww_p_t
		mat 				rown DSR_meal = DSR_meal_mean DSR_meal_sd
		
	* (iiib) across the treatment and control arms: dsr count at day level	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_day, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_day, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 		
		mat 				j1 = e(mean)\e(sd)
		mat 				j2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_day = j1, j2	
		
		mat 				coln DSR_day = control_d treat_d mww_p_d
		mat 				rown DSR_day = DSR_day_mean DSR_day_sd
		
	* (iiic) across the treatment and control arms: dsr count at week level	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_week, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_week, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore		
		
	* store results in matrix 	
		mat 				k1 = e(mean)\e(sd)
		mat 				k2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_week = k1, k2	
		
		mat 				coln DSR_week = control_w treat_w mww_p_w
		mat 				rown DSR_week = DSR_week_mean DSR_week_sd
		
		
	* (iiid) across the treatment and control arms: dsr count overall	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_total, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_total, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
	
	* store results in matrix 
		mat 				l1 = e(mean)\e(sd)
		mat 				l2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_total = l1, l2	
		
		mat 				coln DSR_total = control_t treat_t mww_p_t
		mat 				rown DSR_total = DSR_tot_mean DSR_tot_sd
		
	* combine all dsr results for entire sample into 1 matrix	
		mat 				DSR = DSR_meal, DSR_day, DSR_week, DSR_total
		mat 				li DSR
		*** this matrix is third "row" of results table		
	
	* combine hdds_ct, hdds_avg, and dsr results into even bigger matrix
		mat 				overall = HDDS_ct \ HDDS_avg \DSR
		mat 				li overall
	
	* export and never talk about this again	
		esttab 				mat(overall) using "$output/test.tex", replace
		
		svmat double overall
	
	preserve
		drop 					village-cc
		export 					delimited using "$output/matrix.csv", replace
	restore

************************************************************************
* 1b2 - simple mean differences in final outcomes: NUTRITION CLUBS
************************************************************************	

** (i) hdds count	
	* (ia) across the treatment and control arms: HDDS count at meal level
		preserve
			duplicates drop		hhid week day meal, force
			estpost tabstat 	HDDS_ct_m if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_m if group == 2, by(treat_assign)	
			estadd scalar		pval_m = ((2 * normprob(-abs(r(z)))))	
		restore		
		
		* store results in matrix because i've spent too long on esttab
		* mat a1 is mean over stdev
		mat 				m1 = e(mean)\e(sd)
		
		* mat a2 is the pval from mww test. not standard output so have to 
			* transform it; putting it over "." so it's conformable with a1
		mat 				m2 = ((2 * normprob(-abs(r(z)))))\.
		
		* save all that into bigger matrix
		mat 				HDDS_ct_m = m1, m2
		
		* renaming columns and rows
		mat 				coln HDDS_ct_m = control_m treat_m mww_p_m
		mat 				rown HDDS_ct_m = HDDSct_meal_mean HDDSct_meal_sd
		
	* table 1: entire sample: hdds count: meal
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_m) nonotes nostar unstack replace

		
	* (ib) across the treatment and control arms: hdds count at day level	
	
		preserve
			duplicates drop		hhid week day, force
			estpost tabstat 	HDDS_ct_d if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_d if group == 2, by(treat_assign)	
			estadd scalar		pval_d = ((2 * normprob(-abs(r(z)))))	
		restore				
		
	* store results in matrix 
		mat 				n1 = e(mean)\e(sd)
		mat 				n2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_ct_d = n1, n2	
		
		mat 				coln HDDS_ct_d = control_d treat_d mww_p_d
		mat 				rown HDDS_ct_d = HDDSct_day_mean HDDSct_day_sd
		
	* table 1: entire sample: hdds count: day
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_d) nonotes nostar unstack append
				
	
	* (ic) across the treatment and control arms: hdds count at week level		

		preserve
			duplicates drop		hhid week, force
			estpost tabstat 	HDDS_ct_w if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_w if group == 2, by(treat_assign)	
			estadd scalar		pval_w = ((2 * normprob(-abs(r(z)))))	
		restore	
		
	* store results in matrix 			
		mat 				o1 = e(mean)\e(sd)
		mat 				o2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_ct_w = o1, o2	
		
		mat 				coln HDDS_ct_w = control_w treat_w mww_p_w
		mat 				rown HDDS_ct_w = HDDSct_week HDDSct_week_sd
		
	* table 1: entire sample: hdds count: week
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_w) nonotes nostar unstack append
		
	* (id) across the treatment and control arms: hdds count overall	

		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_ct_t if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_ct_t if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 	
		mat 				p1 = e(mean)\e(sd)
		mat 				p2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_ct_t = p1, p2	
		mat 				coln HDDS_ct_t = control_t treat_t mww_p_t
		mat 				rown HDDS_ct_t = HDDSct_tot_mean HDDSct_tot_sd
					
	* combine all hdds_avg results for entire sample into this thing	
		mat 				HDDS_ct = HDDS_ct_m, HDDS_ct_d, HDDS_ct_w, HDDS_ct_t
		mat 				li HDDS_ct
		*** this matrix is first "row" of results table	
		
	* table 1: entire sample: hdds count: total
	*	esttab using "$output/io_summ2.tex", main(mean) aux(sd) scalars(pval_t) nonotes nostar unstack append
		
** (ii) hdds avg

	* (ii) across the treatment and control arms: hdds avg at meal level 
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			

	* store results in matrix 			
		mat 				q1 = e(mean)\e(sd)
		mat 				q2 = ((2 * normprob(-abs(r(z)))))\.
		mat				 	HDDS_avg_m = q1, q2	
		
		mat 				coln HDDS_avg_m = control_m treat_m mww_p_m
		mat 				rown HDDS_avg_m = HDDSavg_meal_mean HDDSavg_meal_sd

	* (iib) across the treatment and control arms: hdds avg at day level	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
	
	* store results in matrix 	
		mat 				r1 = e(mean)\e(sd)
		mat 				r2 = ((2 * normprob(-abs(r(z)))))\.
		mat		 			HDDS_avg_d = r1, r2	
		
		mat coln HDDS_avg_d = control_d treat_d mww_p_d
		mat rown HDDS_avg_d = HDDSavg_day_mean HDDSavg_day_sd
		
	* (iic) across the treatment and control arms: hdds avg at week level
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore		
		
		* store results in matrix 
		
		mat 				s1 = e(mean)\e(sd)
		mat 				s2 = ((2 * normprob(-abs(r(z)))))\.
		mat					HDDS_avg_w = s1, s2	
		
		mat 				coln HDDS_avg_w = control_w treat_w mww_p_w
		mat 				rown HDDS_avg_w = HDDSavg_week_mean HDDSavg_week_sd	
			
	* (iid) [<-- iid!] across the treatment and control arms: hdds avg overall	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	HDDS_avg_t if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			HDDS_avg_t if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 
		
		mat 				t1 = e(mean)\e(sd)
		mat 				t2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				HDDS_avg_t = t1, t2	
		
		mat 				coln HDDS_avg_t = control treat mww_p
	    mat 				rown HDDS_avg_t = HDDSavg_tot_mean HDDSavg_tot_sd
				
	* combine all hdds_avg results for entire sample into 1 matrix		
		mat				 	HDDS_avg = HDDS_avg_m, HDDS_avg_d, HDDS_avg_w, HDDS_avg_t
		mat 				li HDDS_avg	
		*** this matrix is second "row" of results table

** (iii) dsr count

	* (iiia) across the treatment and control arms: dsr count at meal level 
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_meal if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_meal if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 	
		mat 				u1 = e(mean)\e(sd)
		mat	 				u2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_meal = u1, u2	
		
		mat 				coln DSR_meal = control_t treat_t mww_p_t
		mat 				rown DSR_meal = DSR_meal_mean DSR_meal_sd
		
	* (iiib) across the treatment and control arms: dsr count at day level	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_day if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_day if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
		
	* store results in matrix 		
		mat 				v1 = e(mean)\e(sd)
		mat 				v2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_day = v1, v2	
		
		mat 				coln DSR_day = control_d treat_d mww_p_d
		mat 				rown DSR_day = DSR_day_mean DSR_day_sd
		
	* (iiic) across the treatment and control arms: dsr count at week level	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_week if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_week if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore		
		
	* store results in matrix 	
		mat 				q1 = e(mean)\e(sd)
		mat 				q2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_week = q1, q2	
		
		mat 				coln DSR_week = control_w treat_w mww_p_w
		mat 				rown DSR_week = DSR_week_mean DSR_week_sd
		
		
	* (iiid) across the treatment and control arms: dsr count overall	
	
		preserve
			duplicates drop		hhid, force
			estpost tabstat 	DSR_total if group == 2, by(treat_assign) statistics(mean sd) nototal
			ranksum 			DSR_total if group == 2, by(treat_assign)	
			estadd scalar		pval_t = ((2 * normprob(-abs(r(z)))))	
		restore			
	
	* store results in matrix 
		mat 				r1 = e(mean)\e(sd)
		mat 				r2 = ((2 * normprob(-abs(r(z)))))\.
		mat 				DSR_total = r1, r2	
		
		mat 				coln DSR_total = control_t treat_t mww_p_t
		mat 				rown DSR_total = DSR_tot_mean DSR_tot_sd
		
	* combine all dsr results for entire sample into 1 matrix	
		mat 				DSR = DSR_meal, DSR_day, DSR_week, DSR_total
		mat 				li DSR
		*** this matrix is third "row" of results table		
	
	* combine hdds_ct, hdds_avg, and dsr results into even bigger matrix
		mat 				overall = HDDS_ct \ HDDS_avg \DSR
		mat 				li overall
	
	* export and never talk about this again	
		*esttab 				mat(overall) using "$output/test.tex", replace
		
		svmat double nutrition_clubs
	
	preserve
		drop 					village-cc
		export 					delimited using "$output/matrix.csv", replace
	restore



************************************************************************
* 1b3 - simple mean differences in final outcomes: LEARNING PLOTS
************************************************************************	

** (i) hdds count	
	* (ia) across the treatment and control arms: HDDS count at meal level

		ranksum 			ss_use, by(treat_assign)	
		
	* (ib) across the treatment and control arms: hdds count at day level	
		
		
	* (ic) across the treatment and control arms: hdds count at week level		
		
		
	* (id) across the treatment and control arms: hdds count overall	

		
** (ii) hdds avg
	
	* (ii) across the treatment and control arms: hdds avg at meal level 


	* (iib) across the treatment and control arms: hdds avg at day level	
		
		
	* (iic) across the treatment and control arms: hdds avg at week level		
		
		
	* (iid) across the treatment and control arms: hdds avg overall	


** (iii) dsr count

	* (iiia) across the treatment and control arms: dsr count at meal level 


	* (iiib) across the treatment and control arms: dsr count at day level	
		
		
	* (iiic) across the treatment and control arms: dsr count at week level		
		
		
	* (iiid) across the treatment and control arms: dsr count overall	


************************************************************************
* 1b4 - simple mean differences in final outcomes: BOTH GROUPS
************************************************************************	

** (i) hdds count	
	* (ia) across the treatment and control arms: HDDS count at meal level

		ranksum 			ss_use, by(treat_assign)	
		
	* (ib) across the treatment and control arms: hdds count at day level	
		
		
	* (ic) across the treatment and control arms: hdds count at week level		
		
		
	* (id) across the treatment and control arms: hdds count overall	

		
** (ii) hdds avg
	
	* (ii) across the treatment and control arms: hdds avg at meal level 


	* (iib) across the treatment and control arms: hdds avg at day level	
		
		
	* (iic) across the treatment and control arms: hdds avg at week level		
		
		
	* (iid) across the treatment and control arms: hdds avg overall	


** (iii) dsr count

	* (iiia) across the treatment and control arms: dsr count at meal level 


	* (iiib) across the treatment and control arms: dsr count at day level	
		
		
	* (iiic) across the treatment and control arms: dsr count at week level		
		
		
	* (iiid) across the treatment and control arms: dsr count overall	


************************************************************************
* 1b5 - simple mean differences in final outcomes: NEITHER GROUP
************************************************************************	

** (i) hdds count	
	* (ia) across the treatment and control arms: HDDS count at meal level

		ranksum 			ss_use, by(treat_assign)	
		
	* (ib) across the treatment and control arms: hdds count at day level	
		
		
	* (ic) across the treatment and control arms: hdds count at week level		
		
		
	* (id) across the treatment and control arms: hdds count overall	

		
** (ii) hdds avg
	
	* (ii) across the treatment and control arms: hdds avg at meal level 


	* (iib) across the treatment and control arms: hdds avg at day level	
		
		
	* (iic) across the treatment and control arms: hdds avg at week level		
		
		
	* (iid) across the treatment and control arms: hdds avg overall	


** (iii) dsr count

	* (iiia) across the treatment and control arms: dsr count at meal level 


	* (iiib) across the treatment and control arms: dsr count at day level	
		
		
	* (iiic) across the treatment and control arms: dsr count at week level		
		
		
	* (iiid) across the treatment and control arms: dsr count overall	

	
	estpost sum
	
* output table of ingredient frequencies
	esttab 			 using "$output/desc_tab.tex", replace booktabs ///
						cells("b(label(count)) count(fmt(2)) mean(fmt(2))  sd(fmt(2)) min(fmt(2)) max(fmt(2))") ///
						varlabels(, blist("{hline @width}{break}")) ///
						nonumber nomtitle noobs f	
	
	
	
	
	
	
	
	
	
	
	

************************************************************************
* 2b - final outcomes: fuel, composition of diet, boiling liquids
************************************************************************

	
	save 			"$root/analysis/simplemean.dta", replace	
	
	
	
* close the log
	log				close
	
***END
