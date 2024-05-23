* project: solar stove - final outcomes: fuel use
* created on: February 2021
* created by: lem
* Stata v.15.1 (mac)

* does
	* inputs dsr.dta
	* cleans legumes data
	* generates count and volume vars for legumes cooked
	* outputs legumes_cooked.dta

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
	use				"$root/refined/DSR/dsr.dta", clear

* destring

	destring 		legumes, force replace
	gen 			legumes_ind = 1 if legumes != .
	label var 		legumes_ind "Legumes Indicator"
	
* generate count of times legumes cooked day, week, total
	
	egen			legumes_ct_day = sum(legumes_ind), by(hhid week day)		
	egen			legumes_ct_week = sum(legumes_ind), by(hhid week)
	egen			legumes_ct_tweek = sum(legumes_ind), by(hhid)

* label variables	
	label var		legumes_ct_day "Times Legumes Cooked Daily (HH)"
	label var		legumes_ct_week "Times Legumes Cooked Weekly (HH)"
	label var		legumes_ct_tweek "Total Times Legumes Cooked (HH)"
	
* generate volume of legumes cooked day, week, total
	
	egen			legumes_vol_day = sum(legumes), by(hhid week day)	
	egen			legumes_vol_week = sum(legumes), by(hhid week)
	egen			legumes_vol_tweek = sum(legumes), by(hhid)

* label variables	
	label var		legumes_vol_day "Daily Volume of Legumes Cooked"
	label var		legumes_vol_week "Weekly Volume of Legumes Cooked"
	label var		legumes_vol_tweek "Total Volume of Legumes Cooked"	
	

* **********************************************************************
* 4 - end matter, clean up to save
* **********************************************************************	
	* save			"$root/refined/DSR/dsr.dta", replace
	save			"$root/refined/legumes_cooked.dta", replace
	
	
	log				close
