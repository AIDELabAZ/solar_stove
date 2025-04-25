* project: solar stove - final outcomes: fuel use
* created on: February 2021
* created by: lem
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
	global	root	=	"$data/raw/fuel"
	global	export	=	"$data/refined"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/raw_cleaning_lm", append

	
* load .csv and save as stata .dta
	import delimited 	"$root/fuel_consumption.csv", clear


************************************************************************
**# 1 - rename and relabel fuel variables
************************************************************************

* drop unnecessary variables / anonymize
	drop 				household_name name_collected ///
							name_whopayed type_lozi v27

	rename				cod hhid	
		
	gen 				Village = 0 if village == "Lealui" | ///
							village == "LeaLui" | village == "lealui"
	replace 			Village = 1 if village == "Mapungu"
	replace 			Village = 2 if village == "Nalitoya"
	lab var 			Village "Village"
	lab def 			village 0 "Lealui" 1 "Mapungu" 2 "Nalitoya"
	lab val 			Village village
	
	drop 				village
	rename 				Village village
	order 				village		
						
	drop if 			hhid == .

* rename and redefine variables	
	gen					fuel = 0 if type_en == "Charcoal"
	replace				fuel = 1 if type_en == "Firewood/Caw dong" | ///
							type_en == "Firewood"
	replace				fuel = 0 if type_en == "undefined" & bought == 1					
	
	lab define 			fuel 0 "Charcoal" 1 "Firewood"
	lab values 			fuel fuel
	order				fuel, after(week)
	lab var				fuel "Type of fuel"
	drop				type_en
	drop if				fuel == .

	rename				collected cltd
	lab define 			yesno 0 "No" 1 "Yes"
	lab values 			cltd yesno 
	
	rename				time_collection_min time_cltd
	replace				time_cltd = 0 if time_cltd == .
	
	rename				price_collected_kwacha price_cltd
		
	rename				q_collected_kg quant_cltd
	replace				quant_cltd = 0 if quant_cltd == .
		
	replace				times_collected_week = "" if times_collected_week == "-"
	destring			times_collected_week, replace
	rename				times_collected_week times_cltd
	replace				times_cltd = 0 if times_cltd == .

	rename				bought bght
	lab values 			bght yesno 
	
	rename				time_bought_week times_bght
	replace				times_bght = 0 if times_bght == .
	
	rename				price_bought_kwacha price_bght
	
	rename				q_bought_kg quant_bght
	replace				quant_bght = 0 if quant_bght == .
	
	drop				source_or_price_written_collecti time_collection_week ///
							quantity_collected_written quantity_collected_clean ///
							units_clean source_or_price_written_bought ///
							quantity_written_bought
	
	order				cltd times_cltd time_cltd price_cltd quant_cltd ///
							bght times_bght price_bght quant_bght, ///
							after(fuel)
	
	replace				cod_hh_collectedbought = "" if cod_hh_collectedbought == "Missing name "
	replace				cod_hh_bought = "" if cod_hh_bought == "Missing name "

	destring			cod_hh_collectedbought, replace
	destring			cod_hh_bought, replace
	
	
* collapse to combine firewood and firewood/dung
	collapse (sum)		times_cltd time_cltd price_cltd quant_cltd times_bght ///
						price_bght quant_bght ///
			 (max)		cltd bght, by(village hhid week cod_hh_collectedbought cod_hh_bought)


	replace				price_cltd = . if price_cltd == 0
	replace				price_bght = . if price_bght == 0
	
* save individual
	compress
	save 				"$export/fuel_cleaned_ind.dta", replace


* collapse to combine firewood and firewood/dung
	collapse (sum)		times_cltd time_cltd price_cltd quant_cltd times_bght ///
						price_bght quant_bght ///
			 (max)		cltd bght, by(village hhid week)
			 
	replace				price_cltd = . if price_cltd == 0
	replace				price_bght = . if price_bght == 0

* rename variables		
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
	
* save
	compress
	save 				"$export/fuel_cleaned.dta", replace

* close the log
	log	close

/* END */					