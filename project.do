* project: solar stoves
* created on: July 2020
* created by: lem
* Stata v.19.5

* does
	* establishes an identical workspace between users
	* sets globals that define absolute paths
	* serves as the starting point to find any .do file, dataset or output
	* runs all .do files needed for data work.
	* loads any user written packages needed for analysis

* assumes
	* access to all data (Dropbox) and code (GitHub)

* TO DO:
	* add .do files as they are completed

	
***********************************************************************
* 0 - setup
***********************************************************************

* set $pack to 0 to skip package installation
	global 			pack			0
		
* Specify Stata version in use
    global stataVersion 18.5    // set Stata version
    version $stataVersion
		
***********************************************************************
* 0 (a) - Create user specific paths
***********************************************************************

* Define root folder globals
    if `"`c(username)'"' == "jdmic" {
    global 		code  	"C:/Users/jdmic/git/solar_stove"
	global 		data	"C:/Users/jdmic/git/solar_stove/data"
    }

    if `"`c(username)'"' == "lauramccann" {
    global 		code  	"/Users/lauramccann/Documents/GitHub/solar_stove"
	global 		data	"/Users/lauramccann/Dropbox/Solar_Stoves"
    }

***********************************************************************
* 0 (b) - Check if any required packages are installed:
***********************************************************************

* install packages if global is set to 1
if $pack == 1 {
	
	* for packages/commands, make a local containing any required packages
		loc userpack "blindschemes mdesc estout distinct winsor2 mipolate egenmore reghdfe ftools coefplot ivreg2 ranktest grc1leg2" 
	
	* install packages that are on ssc	
		foreach package in `userpack' {
			capture : which `package', all
			if (_rc) {
				capture window stopbox rusure "You are missing some packages." "Do you want to install `package'?"
				if _rc == 0 {
					capture ssc install `package', replace
					if (_rc) {
						window stopbox rusure `"This package is not on SSC. Do you want to proceed without it?"'
					}
				}
				else {
					exit 199
				}
			}
		}
	
	* update all ado files
		ado update, update

	* set graph and Stata preferences
		set scheme plotplain, perm
		set more off
}

* Set log preferences
	set 		logtype text    // so logs can be opened without Stata

***********************************************************************
* 1 - run ingredients data cleaning .do file
***********************************************************************

	do				"$code/cleaning/food_converter.do"
	do				"$code/cleaning/raw_cleaning.do"

***********************************************************************
* 2 - run cleaning code creating outcome variables
***********************************************************************
	
	do				"$code/cleaning/controls.do"	
	do				"$code/cleaning/diet_clean.do"
	do				"$code/cleaning/fuel_use.do"
	
**********************************************************************
* 3 - run analysis .do files
**********************************************************************	

	do				"$code/analysis/desc_stats.do"
	do				"$code/analysis/food_outcomes"
	do				"$code/analysis/fuel_outcomes"
	do				"$code/analysis/robust_outcomes"


* END *

