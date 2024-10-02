# Food Without Fire: Nutritional and Environmental Impacts from a Solar Stove Field Experiment
Code for solar stove RCT project
 ## Index

 - [Introduction](#introduction)
 - [Data cleaning](#data-cleaning)
 - [Pre-requisites](#pre-requisites)
 - [Folder structure](#folder-structure)
 - Estimation

## Introduction

This is the repo for the weather project.<br>

Contributors:
* Jeffrey D. Michler
* Natalia Estrada Carmona
* Laura McCann

As described in more detail below, scripts variously
go through each step, from cleaning raw data to analysis.

## Data cleaning

The code in `masterDoFile.do` (to be done) replicates
    the data cleaning and analysis.

### Pre-requisites

#### Stata req's

  * The data processing and analysis requires a number of user-written
    Stata programs:
    1. `blindschemes`
    2. `mdesc`
    3. `estout`
    4. `reghdfe`
    5. `ftools`
    6. `distinct`
    7. `winsor2`
    8. `xfill`


#### Folder structure

The [OSF project page][1] provides more details on the data cleaning. --> PAP in AEA reg

The general repo structure looks as follows:<br>

```stata
irri_strv
├────README.md
├────projectdo.do
│    
├────raw          
│────Analysis          
│    ├──code
│    └──output
│       ├──tables
│       └──figures
│   
└────config
```

  [1]: https://osf.io/wkc3p/
