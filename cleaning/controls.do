* project: solar stove - control variables
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 12 Sep 2024
* stata v.18.5

* does
	* inputs household characteristics
	* adds cloud cover
	* saves household level control vars

* assumes
	* access to raw data

* to do:
	* done

************************************************************************
**# 0 - setup
************************************************************************

* define paths
	global	root	=	"$data/raw/controls"
	global	export	=	"$data/refined"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/controls", append

* load set of control variables
	import delimited 	using		"$root/main_hh_covariates.csv", clear

	
*************************************************************************
**# 1 - encode and label control variables
*************************************************************************

* drop indicators variables created by natalia
	drop				village_cor_lealui village_cor_mapungu village_cor_nalitoya ///
							gender_men gender_women highest_grade_higher ///
							highest_grade_none highest_grade_primary ///
							highest_grade_secondary solar_stove_no solar_stove_yes ///
							aas_involvement_ag_nut aas_involvement_agriculture ///
							aas_involvement_none aas_involvement_nutrition

* encode string variables
	gen 				Village = 0 if village == "Lealui" | ///
							village == "LeaLui" | village == "lealui"
	replace 			Village = 1 if village == "Mapungu"
	replace 			Village = 2 if village == "Nalitoya"
	lab var 			Village "Village"
	lab def 			village 0 "Village L" 1 "Village M" 2 "Village N"
	lab val 			Village village
	
	drop 				village
	rename 				Village village
	order 				village		

* rename some control variables
	rename			cod hhid
	lab var			hhid "Household Identifier"
	
	rename			tlu tli
	lab var 		tli "Tropical Livestock Index"	
	
	rename			phy_assets ai
	lab var			ai "Asset Index"
	
* convert gender
	encode			gender, gen(sex)
	lab var			sex "Gender of HOH"
	drop			gender

* encode age and household size
	replace			age_cal = "" if age_cal == "NA"
	destring		age_cal, gen(age)
	lab var			age "Age of Head of Household"
	drop			age_cal
	
	replace			hh_no = "" if hh_no == "NA"
	destring		hh_no, gen(hh_size)
	lab var			hh_size "Household Size"
	drop			hh_no

* convert education variable
	gen				edu = 0 if highest_grade == "None"
	replace			edu = 1 if highest_grade == "Primary"
	replace			edu = 2 if highest_grade == "Secondary"
	replace			edu = 3 if highest_grade == "Higher"
	replace			edu = 0 if edu == .
	lab var 		edu "Educational Attainment"
	lab def 		educ 0 "None" 1 "Primary" 2 "Secondary" 3 "Higher", replace
	lab val 		edu educ
	drop 			highest_grade
	
* create solar stove treatment indicator
	gen 			solar = 0 if solar_stove == "No"
	replace 		solar = 1 if solar_stove == "Yes"
	lab var 		solar "Treatment (Solar Stove)"
	lab define 		treatment 0 "Control" 1 "Treatment"
	lab val 		solar treatment
	drop			solar_stove
	
* create AAS group indicator
	gen 			aas = 0 if aas_involvement == "None"
	replace 		aas = 1 if aas_involvement == "agriculture"
	replace 		aas = 2 if aas_involvement == "nutrition"
	replace 		aas = 3 if aas_involvement == "Ag_nut"
	lab var 		aas "AAS group"
	lab define 		AAS 0 "None" 1 "Learning Plot" 2 "Nutrition Club" 3 "Both"
	lab val 		aas AAS
	drop 			aas_involvement
	

*************************************************************************
**# 2 - clean up and save
*************************************************************************

* order variables
	order 			village hhid solar aas hh_size ai tli sex age edu
	
* save
	compress
	save 			"$export/c_var.dta", replace

* close the log
	log	close

/* END */					