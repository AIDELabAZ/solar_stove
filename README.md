# Food Without Fire: Nutritional and Environmental Impacts from a Solar Stove Field Experiment
This README describes the directory structure & Stata packages necessary to replicate all analysis for the paper "Food Without Fire: Nutritional and Environmental Impacts from a Solar Stove Field Experiment" in *American Journal of Agricultural Economics*. The work relies on the World Bank LSMS and World Bank COVID phone surveys. For more information and to access these surveys, visit the [World Bank Microdata Library][4]. The relevant surveys are available under under the [High-Frequency Phone Survey collection][2] and the [LSMS - Integrated Surveys on Agriculture (ISA) collection][3]. To replicate the analysis, one needs to download the LSMS-ISA data and merge it with the already cleaned phone survey data. We provide the cleaned data as part of this current repo. The replication code does the merging. We make no guarantee that variables not used in the analysis are cleaned or accurate. THe analysis is based on a [pre-analysis plan][1] filed with the American Economic Association's registry for randomized controlled trials (ID: AEARCTR-0004054).



Last updated: October 2025. 

For issues or concerns with this repo, please contact Jeffrey Michler.

 ## Index
 
 - [Contributors](#contributors)
 - [Data](#data)
 - [Data cleaning](#data-cleaning)
 - [Developing Environment](#developing-environment)

## Contributors
* Laura E. McCann (Writing - original draft, Formal Analysis, Validation) 
* Jeffrey D. Michler [jdmichler@arizona.edu] (Writing - review & editing, Writing - original draft, Supervision, Project administration, Formal analysis, Conceptualization, Data curation)
* Maybin Mwangala (Investigation)
* Osaretin Olurotimi (Writing - review & editing, Writing - original draft, Formal analysis)
* Natalia Estrada Carmona [n.e.carmona@cgiar.org] (Resources, Funding acquisition, Conceptualization)

* ## Data cleaning

### Pre-requisites

The data processing and analysis requires a number of user-written Stata programs:
   * 1. `blindschemes`
   * 2. `mdesc`
   * 3. `estout`
     4. `distinct`
     5. `winsor2`
     6. `mipolate`
     7. `egenmore`
     8. `reghdfe`
     9. `ftools`
     10. `coefplot`
     11. `ivreg2`
     12. `ranktest`
     13. `grc1leg2`

The `project.do` file will help you install these.

## Developing Environment

### Step 1

Clone this  repository https://github.com/AIDELabAZ/livelihood_div. The general repo structure looks as follows:<br>

```stata
solar_stove
├────README.md
├────project.do
├────LICENSE
├────.gitignore
├────analysis            /* overall analysis */
└────cleaned_data	/* data to be moved into data folder */
```

### Step 2

Open the project.do file and update the global filepath with your username in Section 0 (a).

   ```
    if `"`c(username)'"' == "USERNAME" {
       	global 		code  	"C:/Users/USERNAME/git/solar_stove"
		global 		data	"C:/Users/USERNAME/solar_stove/data"
		global 		output  "C:/Users/USERNAME/solar_stove/output"
    }
   ```

### Step 3

Set up the file structure on your local machine as outlined below: 

```stata
C:/Users/USERNAME/solar_stove
├────output
│    ├──logs
│    ├──figures
│    └──tables
└────data
     │    ├──logs
     │    ├──refined
     │    └──raw
     └──cleaned_data
```

### Step 4

Download the LSMS-ISA microdata Stata files from World Bank Microdata Library. You will need to create an account with the World Bank if you do not already have one and will be asked to provide a reason for downloading the data. Once data are downloaded, save the data files to the corresponding folders created in Step 3. 

### Step 5

Move the data sets in the `cleaned_data` folder in this repo into the `cleaned_data` folder you created in step 3.

### Step 6

Run the `project.do` file. Output tables and figures will be saved to the relevant subfolders in the `output` folder. 


[1]: https://www.socialscienceregistry.org/trials/4054
[2]: http://bit.ly/microdata-hfps
[3]: https://www.worldbank.org/en/programs/lsms/initiatives/lsms-ISA
[4]: https://microdata.worldbank.org/index.php/home
[5]: https://github.com/AIDELabAZ/solar_stove
