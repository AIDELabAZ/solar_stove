* project: solar stove - final outcomes: fuel use
* created on: February 2021
* created by: lem
* edited by: lem
* last edit: 23 may 2024 
* stata v.17 (mac)

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
	global	root	=	"$data/raw/fuel"
	global	export	=	"$data/refined"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/raw_cleaning_lm", append

	
* load .csv and save as stata .dta
	import delimited 				"$root/fuel_consumption.csv", clear


************************************************************************
* 1a - rename and relabel WEEKLY vars in fuel_final sheet
************************************************************************

* drop unnecessary variables / anonymize
	drop 				household_name cod_hh_collectedbought name_collected ///
							cod_hh_bought name_whopayed type_lozi v28 v29 ///
							quantity_written_bought time_collection_week ///
							quantity_collected_written quantity_collected_clean ///
							units_clean source_or_price_written_bought ///
							source_or_price_written_collecti v27 type_en

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

	replace 			times_collected_week = "" if times_collected_week == "-"

	destring	 		times_collected_week, replace 
			
	mvdecode 			time_collection_min price_collected_kwacha q_collected_kg ///
							times_collected_week time_bought_week ///
							price_bought_kwacha q_bought_kg, mv(0)
		
		
	mvencode 			time_collection_min price_collected_kwacha q_collected_kg ///
							times_collected_week time_bought_week ///
							price_bought_kwacha q_bought_kg, mv(0)	
						
						
						
	collapse (sum) 		time_collection_min price_collected_kwacha q_collected_kg ///
							times_collected_week time_bought_week ///
							price_bought_kwacha q_bought_kg, by(village hhid week)	
						
						
	save 				"$export/fuel_cleaned.dta", replace


*** END
