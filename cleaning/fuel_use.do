* project: solar stove - final outcomes: fuel use
* created on: February 2021
* created by: lem
* edited by: jdm
* last edit: 11 Sep 2024
* stata v.18.5

* does [UPDATE]
	* inputs raw_fuels.dta
	* relabels data
	* 

* assumes
	* some cleaning has already been done, but not sure where that code is

* to do:
	* formatting
	* clarify collected values (is estimated worth or actual amt sold?)

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
	import delimited 				"$root/fuel_consumption.csv", clear


************************************************************************
**# 1 - rename and relabel fuel variables
************************************************************************

* drop unnecessary variables / anonymize
	drop 				household_name cod_hh_collectedbought name_collected ///
							cod_hh_bought name_whopayed type_lozi v27

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
	
* collapse to combine firewood and firewood/dung
	collapse (sum)		times_cltd time_cltd price_cltd quant_cltd times_bght ///
						price_bght quant_bght ///
			 (max)		cltd bght, by(village hhid week fuel)
	
	replace				price_cltd = . if price_cltd == 0
	replace				price_bght = . if price_bght == 0
	
* reshape panel
	isid				village hhid week fuel
	reshape wide 		cltd times_cltd time_cltd price_cltd quant_cltd bght ///
							times_bght price_bght quant_bght, ///
							i(village hhid week) j(fuel)

* rename variables
	order				cltd0 times_cltd0 time_cltd0 price_cltd0 quant_cltd0 ///
							bght0 times_bght0 price_bght0 quant_bght0 ///
							cltd1 times_cltd1 time_cltd1 price_cltd1 quant_cltd1 ///
							bght1 times_bght1 price_bght1 quant_bght1, ///
							after(week)
		
	drop				cltd0 times_cltd0 time_cltd0 price_cltd0 quant_cltd0
		
	replace				bght0 = 0 if bght0 == .
	rename				bght0 c_bhgt	
	lab var				c_bhgt "Was charcoal bought?"	
	
	replace				times_bght0 = 0 if times_bght0 == .
	rename				times_bght0 c_times	
	lab var				c_times "Number of times charcoal was bought"	
	
	rename				price_bght0 c_price
	lab var				c_price "Price of charcoal (kwacha)"	
	
	replace				quant_bght0 = 0 if quant_bght0 == .
	rename				quant_bght0 c_quant
	lab var				c_quant "Quantity of charcoal (kg)"
		
	replace				cltd1 = 0 if cltd1 == .
	rename				cltd1 f_cltd
	lab var				f_cltd "Was firewood collected?"	
	
	replace				times_cltd1 = 0 if times_cltd1 == .
	rename				times_cltd1 f_cltd_times	
	lab var				f_cltd_times "Number of times firewood was collected"	
	
	replace				time_cltd1 = 0 if time_cltd1 == .
	rename				time_cltd1 f_time
	lab var				f_time "Time spent collecting firewood (min)"		
	
	rename				price_cltd1 f_cltd_price
	lab var				f_cltd_price "Price of firewood collected (kwacha)"	
	
	replace				quant_cltd1 = 0 if quant_cltd1 == .
	rename				quant_cltd1 f_cltd_quant
	lab var				f_cltd_quant "Quantity of firewood collected (kg)"
	
	replace				bght1 = 0 if bght1 == .
	rename				bght1 f_bhgt	
	lab var				f_bhgt "Was firewood bought?"	
	
	replace				times_bght1 = 0 if times_bght1 == .
	rename				times_bght1 f_bght_times	
	lab var				f_bght_times "Number of times firewood was bought"	
	
	rename				price_bght1 f_bght_price
	lab var				f_bght_price "Price of bought firewood (kwacha)"	
	
	replace				quant_bght1 = 0 if quant_bght1 == .
	rename				quant_bght1 f_bght_quant
	lab var				f_bght_quant "Quantity of firewood bought (kg)"

* drop household that collected for 9 weeks
	drop if				week > 6

* natalia uses a price of $8.33 for charcoal and $7.69 for firewood
* replace price with these values
	replace				c_price = 8.33
	generate			f_price = 7.69
	drop				f_bght_price f_cltd_price
	lab var				f_price "Price of firewood (USD)"
	lab var				c_price "Price of charcoal (USD)"	

* generate upper and lower bound values	
	gen					f_quant_lb = f_cltd_quant + f_bght_quant	
	gen					f_quant_ub = f_cltd_times*f_cltd_quant + f_bght_times*f_bght_quant	
	lab var				f_quant_lb "Quantity of firewood lower bound (kg)"
	lab var				f_quant_ub "Quantity of firewood upper bound (kg)"
	
	gen					f_val_lb = f_price*f_quant_lb
	gen					f_val_ub = f_price*f_quant_ub
	lab var				f_val_lb "Value of firewood lower bound (USD)"
	lab var				f_val_ub "Value of firewood upper bound (USD)"
	
	gen					c_quant_lb = c_quant
	gen					c_quant_ub = c_times* c_quant
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
	merge				m:1 hhid using "$export/c_var.dta"

		
	save 				"$export/fuel_cleaned.dta", replace


*** END
