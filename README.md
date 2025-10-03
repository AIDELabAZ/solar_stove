# Food Without Fire: Environmental and Nutritional Impacts from a Solar Stove Field Experiment: Replication Package
This README describes the directory structure & Stata packages necessary to replicate all analysis for the paper "Food Without Fire: Nutritional and Environmental Impacts from a Solar Stove Field Experiment" in *American Journal of Agricultural Economics*. We provide raw data as part of this current repo. We make no guarantee that variables not used in the analysis are cleaned or accurate. The analysis is based on a [pre-analysis plan][1] filed with the American Economic Association's registry for randomized controlled trials (ID: AEARCTR-0004054).

[![DOI](https://zenodo.org/badge/404853161.svg)](https://doi.org/10.5281/zenodo.17260907)

Last updated: October 2025. 

For issues or concerns with this repo, please contact Jeffrey Michler.

 ## Index
 
 - [Contributors](#contributors)
 - [Data cleaning](#data-cleaning)
 - [Developing Environment](#developing-environment)

## Contributors
* Laura E. McCann (Writing - original draft, Formal Analysis, Validation) 
* Jeffrey D. Michler [jdmichler@arizona.edu] (Writing - review & editing, Writing - original draft, Supervision, Project administration, Formal analysis, Conceptualization, Data curation)
* Maybin Mwangala (Investigation)
* Osaretin Olurotimi (Writing - review & editing, Writing - original draft)
* Natalia Estrada Carmona [n.e.carmona@cgiar.org] (Resources, Funding acquisition, Conceptualization)

## Data cleaning

### Pre-requisites

The data processing and analysis requires a number of user-written Stata programs:

* 1. `blindschemes`
* 2. `mdesc`
* 3. `estout`
* 4. `distinct`
* 5. `winsor2`
* 6. `mipolate`
* 7. `egenmore`
* 8. `reghdfe`
* 9. `ftools`
* 10. `coefplot`
* 11. `ivreg2`
* 12. `ranktest`
* 13. `grc1leg2`

The `project.do` file will help you install these.

## Developing Environment

### Step 1

Clone this repository [https://github.com/AIDELabAZ/solar_stove][2]. The general repo structure looks as follows:<br>

```stata
solar_stove
├────README.md
├────project.do
├────LICENSE
├────.gitignore
├────cleaning            /* script files for cleaning data */
├────analysis            /* script files for conducting analysis */
└────data				 /* data for analysis */
     ├────logs			 /* log files */
     ├────raw			 /* raw data */
     ├────refined		 /* cleaned data */
     └────analysis		 /* results */
		   ├────figures /* figures in paper */
		   └────tables	/* tables in paper */
```

### Step 2

Open the project.do file and update the global filepath with your username in Section 0 (a).

   ```
    if `"`c(username)'"' == "USERNAME" {
       	global 		code  	"C:/Users/USERNAME/git/solar_stove"
		global 		data	"C:/Users/USERNAME/solar_stove/data"
    }
   ```

### Step 3

Run the `project.do` file. Output tables and figures will be saved to the relevant subfolders in the `analysis` folder. 


[1]: https://www.socialscienceregistry.org/trials/4054
[2]: https://github.com/AIDELabAZ/solar_stove
