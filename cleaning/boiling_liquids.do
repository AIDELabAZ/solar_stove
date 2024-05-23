* project: solar stove - final outcomes: boiling of liquids
* created on: February 2021
* created by: lem
* Stata v.15.1 (mac)

* does [UPDATE]
	* inputs liquids.dta
	* generates count variables for times liquids boiled
	* generates volume for liquids boiled
	* outputs data as liquids_boiled.dta

* assumes
	* nothing

* to do:
	* formatting


* **********************************************************************
* 0 - setup
* **********************************************************************

* define paths
	global	root	=	"$data"
	global	export	=	"$data/refined"
	global	logout	=	"$data/logs"
	
* open log
	cap log 		close 
	log using		"$logout/raw_cleaning_lm", append


* ***********************************************************************
* 1 - prepare ingredients data
* ***********************************************************************

* load data
	* use liquids output from raw_cleaning.do to avoid cleaning again
	use 			"$root/refined/Liquids/liquids.dta"
	sort 			hhid week day meal
	
	drop 			Ingredient*

	
* generate number of times in a day liquid was boiled by a household	
		* uses "dish" as a count variable
	egen				num_milkday = sum(dish) if type == 1, by(hhid week day)
	egen				num_waterday = sum(dish)if type == 0, by(hhid week day)
	egen				num_bothday = sum(dish)if type != ., by(hhid week day)
	
* generate number of times in a week liquid was boiled by a household		
	egen				num_milkwk = sum(dish) if type == 1, by(hhid week)
	egen				num_waterwk = sum(dish) if type == 0, by(hhid week)
	egen				num_bothdwk = sum(dish)if type != ., by(hhid week)
	
* generate number of times total (6 weeks) liquid was boiled by a household		
	egen				num_milktwk = sum(dish) if type == 1, by(hhid)
	egen				num_watertwk = sum(dish) if type == 0, by(hhid)
	egen				num_bothtwk = sum(dish)if type != ., by(hhid week)
	
* generate volume of liquids (in L) boiled daily by hh	
		* uses "dish" as a count variable	
	egen				vol_milkday = sum(vol) if type == 1, by(hhid week day)
	egen				vol_waterday = sum(vol)if type == 0, by(hhid week day)
	egen				vol_bothday = sum(vol)if type != ., by(hhid week day)
	
* generate volume of liquids (in L) boiled weekly by hh			
	egen				vol_milkwk = sum(vol) if type == 1, by(hhid week)
	egen				vol_waterwk = sum(vol) if type == 0, by(hhid week)
	egen				vol_bothdwk = sum(vol)if type != ., by(hhid week)
	
* generate volume of liquids (in L) boiled total by hh		
	egen				vol_milktwk = sum(vol) if type == 1, by(hhid)
	egen				vol_watertwk = sum(vol) if type == 0, by(hhid)
	egen				vol_bothtwk = sum(vol)if type != ., by(hhid)	
	

* **********************************************************************
* 4 - end matter, clean up to save
* **********************************************************************
	
/* check for duplicates
	duplicates			report hhid plotnum crop_code
	
	* keep what we want, get rid of what we don't
	keep 				hhid plotnum plot_id crop_code crop_id clusterid ///
							strataid hhweight region district ward ea ///
							any_* pure_stand percent_field mz_hrv hvst_value ///
							mz_damaged y4_rural
	order				hhid plotnum plot_id crop_code crop_id clusterid ///
							strataid hhweight region district ward ea
							
* renaming and relabelling variables
	lab var				hhid "Unique Household Identification NPS Y4"
	*** there are 0 duplicates	
	
* prepare for export
*	isid				hhid plotnum crop_code
*	compress
*	describe
*	summarize 
*	sort            	plot_id*/
	save          	  "$export/liquids_boiled.dta", replace
	
* close the log
	log					close
	
/* END */	
		
