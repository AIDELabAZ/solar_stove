* project: solar stoves (thesis)
* created on: July 2020
* created by: lem
* edited by: 
* Stata v.15.1 (mac)

* does
	* establishes an identical workspace between users
	* sets globals that define absolute paths
	* serves as the starting point to find any .do file, dataset or output
	* runs all .do files needed for data work.
	* loads any user written packages needed for analysis

* assumes
	* access to all data (Google Drive) and code (GitHub)

* TO DO:
	* add .do files as they are completed

***********************************************************************
* 0 - setup
***********************************************************************

* set $pack to 0 to skip package installation
	global 			pack			0
	
* dependencies
	* for packages/commands, make a local containing any required packages
        local userpack "blindschemes mdesc estout distinct winsor2 mipolate asdoc"

		*set scheme plotplainblind, perm
***********************************************************************
* 0 (a) - Create user specific paths
***********************************************************************

* Define root folder globals
    if `"`c(username)'"' == "jdmichler" {
    global 		code  	"C:/Users/jdmichler/git/solar_stove"
	global 		data	"G:/My Drive/solar_stoves"
    }

    if `"`c(username)'"' == "lauramccann" {
    global 		code  	"/Users/lauramccann/Documents/GitHub/solar_stove/"
	global 		data	"/Volumes/GoogleDrive/My Drive/shared_data/solar_stoves"
    }

***********************************************************************
* 0 (b) - Check if any required packages are installed:
***********************************************************************

* install packages if global is set to 1
if $pack == 1 {
	
	* temporarily set delimiter to ; so can break the line
		#delimit ;
	* for packages/commands, make a local containing any required packages
		loc userpack "blindschemes mdesc estout distinct winsor2" ;
		#delimit cr
	
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

***********************************************************************
* 2 - run .do file that converts Lozi to English names
***********************************************************************

	do				"$code/cleaning/raw_cleaning.do"
	
**********************************************************************
* 3 - run .do file that generates HDDS and SR indices
**********************************************************************	
	
	do				"$code/cleaning/dietary_comp.do"
	
***********************************************************************
* 4 - run .do file for itt outocmes
***********************************************************************

	do				"$code/analysis/itt_outcomes.do"

***********************************************************************
* 5 - run .do file for late outcomes
***********************************************************************

	do				"$code/analysis/late_outcomes"

***********************************************************************
* END
***********************************************************************
