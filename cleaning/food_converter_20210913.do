* project: solar stove
* created on: july 2020
* created by: lem
* edited by:
* last edit: 7 june 2021 
* stata v.15.1 (mac)

* does
	* inputs food_converter.dta
	* converts Lozi to English names
	* outputs data as food_match.dta
		* this outputted .do file will be used to match translated names of 	
		* ingredients, their food groups, and their level of processing to 
		* ingredient observations in our raw ingredient data

* assumes
	* mipolate is installed

* to do:
	* formatting

***********************************************************************
* 0 - setup
***********************************************************************

* define paths
	global			root	=	"$data"
	global			export	=	"$data/refined"
	global			logout	=	"$data/logs"
	
* open log
	cap log 		close 
	log using		"$logout/raw_cleaning", append
	
************************************************************************
* 1 - prepare ingredients data
************************************************************************

* import dictionary sheet and save as .dta
	*import 			excel using "$root/raw/Dietary/HDDS/Lealui_mapungu_nalitoya_data28_02_2020.xlsx", sheet("dictionary") firstrow clear
	*save 				"$root/raw/dietary/Hdds/food_converter.dta", replace

* load data
	use				"$root/raw/dietary/Hdds/food_converter.dta", clear
					
* drop missing values
	drop if 		english == "na"
	*** 237 observations dropped
			
 drop if english == "water"

	replace 		foodgroup = "" if foodgroup == "na"
	*** 36 changes made

* create group food variable
	egen 			food = group(english)
	*** 1315 missing values generated

* interpolate missing food group values
	by english, sort: stripolate foodgroup food, gen(foo) groupwise
	drop 			foodgroup
	rename			foo foodgroup
	lab var 		foodgroup "food Group"
	
* interpolate missing process values
	by english, sort: stripolate proccess food, gen(process) groupwise
	drop 			proccess
	lab var 		process "Type of Processing"

* drop duplicates
	duplicates drop lozi, force
	*** 1346 observations dropped
	
************************************************************************
* filling in missing values
************************************************************************	
	
* sort lozi and inspect
	sort 			lozi
	drop 			if lozi == ""
	*** 1 observation dropped
		
************************************************************************
* filling in missing food group values
************************************************************************	

* sort food group values to find blanks
	* add "food" sort level to ensure that these missing values have english 
		* translations
	sort 			foodgroup food
	*** 103 observations, many are duplicates

* check # duplicates
	* duplicates drop english, force
	  *** 25 duplicates

* fill in food group values
	replace 		foodgroup = "Sweets" if english == "candy"
	replace 		foodgroup = "White roots and tubers and plantains" if ///
						english == "cassava flour"
	replace 		foodgroup = "White roots and tubers and plantains" if ///
						english == "cassavagroundnut"
	replace 		foodgroup = "Meat and poultry" if english == "catterpillar"
	replace 		foodgroup = "Savoury and fried snacks" if english == "chips"
	replace 		foodgroup = "Condiments and seasonings" if english == "dip"
	replace 		foodgroup = "Other vegetables" if english == "finger okra"
	replace 		foodgroup = "Pulses (beans, peas and lentils)" if ///
						english == "fishgroundnut"
	replace 		foodgroup = "Other vegetables" if english == "green pepper"
	replace 		foodgroup = "Vitamin A-rich vegetables, roots and tubers" ///
						if english == "livingtone yam"
	replace 		foodgroup = "Grains" if english == "maize yellow"
	replace 		foodgroup = "Vitamin A-rich fruits" if english == "mango"
	replace 		foodgroup = "Other fruits" if english == "mauberry fruits"
	replace 		foodgroup = "Grains" if english == "millet"
	replace 		foodgroup = "Grains" if english == "millet flour"
	replace 		foodgroup = "Other oils and fats" if english == "nut oil"
	replace 		foodgroup = "Grains" if english == "oats"
	replace 		foodgroup = "Other oils and fats" if english == "peanut oil"
	replace 		foodgroup = "Grains" if english == "pearl millet flour"
	replace 		foodgroup = "Grains" if english == "porridge"
	replace 		foodgroup = "Nuts and seeds" if english == "pumpkin seed"
	replace 		foodgroup = "Condiments and seasonings" if english == "relish"
	replace 		foodgroup = "Sweets" if english == "watersugar"
	replace 		foodgroup = "Other fruits" if english == "wildfruit"
	replace 		foodgroup = "Other vegetables" if lozi == "Vegetables"
	replace 		foodgroup = "Nuts and seeds" if lozi == "peanut"
	
************************************************************************
* filling in missing processing values
************************************************************************	
	
* sort to look for blanks
	* add "food" sort level to ensure that these missing values have english 
		* translations
	sort 			process food	
	
* check # duplicates
	*duplicates drop english, force
	  *** 25 duplicates	
	
* fill in processing values
	replace 		process = "Ultra-processed products" if english == "candy"
	replace 		process = "Processed culinary ingredients" if ///
						english == "cassava flour"
	replace 		process = "Unprocessed" if english == "cassavagroundnut"
	replace 		process = "Unprocessed" if english == "catterpillar"
	replace 		process = "Ultra-processed products" if english == "chips"
	replace 		process = "Ultra-processed products" if english == "dip"
	replace 		process = "Unprocessed" if english == "finger okra"
	replace 		process = "Unprocessed" if english == "fishgroundnut"
	replace			process = "Unprocessed" if english == "green pepper"
	replace 		process = "Unprocessed" if english == "livingtone yam"
	replace 		process = "Unprocessed" if english == "maize yellow"
	replace 		process = "Unprocessed" if english == "mango"
	replace 		process = "Unprocessed" if english == "mauberry fruits"
	replace 		process = "Unprocessed" if english == "millet"
	replace 		process = "Processed culinary ingredients" if ///
						english == "millet flour"
	replace 		process = "Processed culinary ingredients" if ///
						english == "nut oil"
	replace 		process = "Unprocessed" if english == "oats"
	replace 		process = "Unprocessed" if english == "peanut"
	replace			process = "Processed culinary ingredients" if ///
						english == "peanut oil"
	replace 		process = "Processed culinary ingredients" if ///
						english == "pearl millet flour"
	replace 		process = "Processed foods" if english == "porridge"
	replace			process = "Unprocessed" if english == "pumpkin seed"
	replace 		process = "Processed culinary ingredients" if ///
						english == "relish"
	replace 		process = "Processed culinary ingredients" if ///
						english == "watersugar"
	replace 		process = "Unprocessed" if english == "wildfruit"
	replace 		process = "Unprocessed" if lozi == "Vegetables"
	
************************************************************************
* filling in missing english values
************************************************************************	

* sort english words to see blanks
	sort 			english
	*** 129 blanks		

* fill in missing english words
	replace 		english = "rice" if lozi == "Rice"
	replace 		english = "cassava leaves" if lozi == "Shombo"	
	replace 		english = "okra" if lozi == "Ndelele"
	replace 		english = "sweet potato leaves" if lozi == "Kalembula"
	replace 		english = "pumpkin" if lozi == "Namundalangwe"
	replace	 		english = "fish" if lozi == "Litapi"
	replace 		english = "meat" if lozi == "Meat"
	replace 		english = "hibiscus" if lozi == "Mundambi"
	replace 		english = "cowpea" if lozi == "Manawa"
	replace 		english = "maize" if lozi == "Orange Maize"
	replace 		english = "cassava" if lozi == "Mwanja"
	replace 		english = "african eggplant" if lozi == "Impwa"
	replace 		english = "pumpkin leaves" if lozi == "Mangambwa"
	replace 		english = "amaranth" if lozi == "Libowa"
	replace 		english = "meat" if lozi == "Nama"
	replace 		english = "potato" if lozi == "Potato"
	replace 		english = "not polished maize" if lozi == "Likobe"
	replace 		english = "pumpkin leaves" if lozi == "Pumpkin leaves"
	replace 		english = "fish" if lozi == "Kapenta"
	replace 		english = "millet flour" if lozi == "millet Nshima"
	replace 		english = "sweet potato leaves" if lozi == "Sweet Potato Leaves"
	replace 		english = "amaranth" if lozi == "Tepe"
	replace 		english = "sour milk" if lozi == "Mabisi"
	replace 		english = "eggplant" if lozi == "Eggplant"
	replace 		english = "bean" if lozi == "Beans"
	replace 		english = "cocoa" if lozi == "Cocoa"
	replace 		english = "maize" if lozi == "Maize"
	replace 		english = "chicken" if lozi == "Chicken"
	replace 		english = "amaranth" if lozi == "Amaranthus"
	replace 		english = "tea leaves" if lozi == "Tea"
	replace 		english = "fish" if lozi == "Tapi"
	replace 		english = "bambara nuts" if lozi == "Lituu"
	replace 		english = "sour milk" if lozi == "Sour Milk"
	replace 		english = "rape" if lozi == "Muloho"
	replace 		english = "pumpkin leaves" if lozi == "Pumpkin Leaves"
	replace 		english = "maize" if lozi == "Roasted Maize"
	replace 		english = "hibiscus" if lozi == "Hibiscus"
	replace 		english = "cassava" if lozi == "Cassava"
	replace 		english = "hibiscus" if lozi == "Sindambi"
	replace 		english = "sweet potato" if lozi == "Ngulu"
	replace 		english = "maize flour" if lozi == "Nsima"
	replace 		english = "maize flour" if lozi == "mukeme"
	replace 		english = "rape" if lozi == "Rape"
	replace 		english = "pumpkin" if lozi == "Pumpkin"
	replace 		english = "maize" if lozi == "Mbonyi"
	replace 		english = "mushroom" if lozi == "Mbowa"
	replace 		english = "pumpkin" if lozi == "Pumkin"
	replace 		english = "fish" if lozi == "Fried Fish"
	replace 		english = "maize" if lozi == "Cooked Maize"
	replace 		english = "mushroom" if lozi == "Mushrooms"
	replace 		english = "okra" if lozi == "Okra"
	replace 		english = "maize" if lozi == "Fresh Maize"
	replace 		english = "chicken" if lozi == "Kuhu"
	replace 		english = "cabbage" if lozi == "cabici"
	replace 		english = "cabbage" if lozi == "Cabbage"
	replace 		english = "egg" if lozi == "Eggs"
	replace 		english = "orange" if lozi == "Orange"
	replace 		english = "egg" if lozi == "Mai"
	replace 		english = "pumpkin seed" if lozi == "Litoze"
	replace 		english = "pumpkin" if lozi == "Mundalangwe"
	replace 		english = "bread" if lozi == "Bread"
	replace 		english = "salt" if lozi == "Salt"
	replace 		english = "maize flour" if lozi == "Maheu"
	replace 		english = "fishgroundnut" if lozi == "Mulelengwa"
	replace 		english = "not polished maize" if lozi == "likombe"
	replace			english = "fish" if lozi == "B/fish"
	replace 		english = "sweet potato leaves" if lozi == "Milo"
	replace 		english = "squash orange" if lozi == "Mupusi"
	replace 		english = "livingtone yam" if lozi == "Sikuswani"
	replace 		english = "chips" if lozi == "Fritters"
	replace 		english = "eggplant" if lozi == "Malembeka"
	replace 		english = "fish" if lozi == "B/ Fish"
	replace 		english = "fish" if lozi == "dry kapenta"
	replace 		english = "wild vegetable" if lozi == "Mucelo"
	replace 		english = "cassava" if lozi == "Boiled Cassava"
	replace 		english = "fish" if lozi == "Lutapi"
	replace 		english = "milk" if lozi == "Fresh Milk"
	replace 		english = "relish" if lozi == "Relish"
	replace 		english = "maize" if lozi == "Boiled Maize"
	replace 		english = "vegetable" if lozi == "Vegetables"	
	replace 		english = "chicken" if lozi == "huku"
	replace 		english = "rice" if lozi == "inende"
	replace 		english = "pumpkin leaves" if lozi == "mango"
	replace 		english = "maize" if lozi == "mbunai"
	replace 		english = "sorghum" if lozi == "mulleme"
	replace 		english = "rape" if lozi == "muloba"
	replace 		english = "fishgroundnut" if lozi == "mutetengwa"
	replace 		english = "rape" if lozi == "mwalelo"
	replace 		english = "sweet potato" if lozi == "njulu"
	replace 		english = "sweet potato" if lozi == "nolu"
	replace 		english = "maize" if lozi == "saapu"
	replace 		english = "african eggplant" if lozi == "tembe"
	replace 		english = "maize flour" if lozi == "bebe"	
	replace 		english = "porridge" if lozi == "pola"
	replace 		english = "meho" if lozi == "meho"
	replace 		english = "silelemu" if lozi == "silelemu"
	replace 		english = "wakufetani" if lozi == "wakufetani"
	replace 		english = "yakulya" if lozi == "yakulya"
	replace 		english = "Roasted" if lozi == "Roasted"
	replace 		english = "liminga" if lozi == "liminga"
	replace 		english = "amaranth" if lozi == "Ondwe"	
	replace 		english = "fish" if lozi == "Small fish"	
	replace 		english = "na" if lozi == "gcpi"	
	replace 		english = "na" if lozi == "mimeno"	
	replace			english = "na" if lozi == "Nahobi999/"
	replace 		english = "na" if lozi == "kufeta"
	replace 		english = "na" if lozi == "999/"
	replace 		english = "na" if lozi == "meho"
	replace 		english = "na" if lozi == "swopo"
	replace 		english = "na" if lozi == "Fresh"
	replace 		english = "na" if lozi == "tembe"
	replace 		english = "na" if lozi == "wakufetani"
	replace 		english = "na" if lozi == "muloba"
	replace 		english = "na" if lozi == "saapu"
	replace 		english = "na" if lozi == "yakulya"
	replace 		english = "na" if lozi == "mulleme"
	replace 		english = "na" if lozi == "huku"
	replace 		english = "na" if lozi == "njulu"
	replace 		english = "na" if lozi == "mwalelo"
	replace 		english = "na" if lozi == "mwacha"
	replace 		english = "na" if lozi == "silelemo"
	replace 		english = "na" if lozi == "soup"
	replace 		english = "na" if lozi == "gcpi"
	replace 		english = "na" if lozi == "drink"
	replace		    english = "na" if lozi == "Roasted"
	replace 		english = "na" if lozi == "likota"
	replace 		english = "na" if lozi == "silelemu"
	replace 		english = "na" if lozi == "Jolly Juice"
	replace 		english = "na" if lozi == "mbunai"
	replace 		english = "na" if lozi == "lakubeya"
	replace 		english = "na" if lozi == "wakutateba"
	replace 		english = "na" if lozi == "Lin/un999/"
	replace 		english = "na" if lozi == "sheña"
	replace 		english = "na" if lozi == "masoka"
	replace 		english = "na" if lozi == "mulelekw"
	replace 		english = "na" if lozi == "Kayobe999/"
	replace 		english = "na" if lozi == "muholo"
	replace 		english = "na" if lozi == "wakubeya"
	replace 		english = "na" if lozi == "moekete"
	replace 		english = "na" if lozi == "mutetengwa"
	replace 		english = "na" if lozi == "juice"
	replace 		english = "na" if lozi == "mungundumbwa"
	replace 		english = "na" if lozi == "bebe"
	replace 		english = "na" if lozi == "liminga"
	replace 		english = "na" if lozi == "lisame"
	replace 		english = "na" if lozi == "munda kawe"
	replace 		english = "na" if lozi == "nolu"
	
************************************************************************
* re-interpolating the data
************************************************************************	
	
* drop "food" variable so we can reuse above code to reinterpolate
	drop 			food	
	
* drop missing values
	*drop if 		english == "na"
	*** 44 observations dropped

* create group food variable
	egen 			food = group(english)

* interpolate missing food group values
	by english, sort: stripolate foodgroup food, gen(foo) groupwise
	drop 			foodgroup
	rename			foo foodgroup
	lab var 		foodgroup "Food Group"
	
* interpolate missing process values
	by english, sort: stripolate process food, gen(proccess) groupwise
	drop 			process
	rename 			proccess process
	lab var 		process "Type of Processing"
	
* check the data 
	sort 			lozi
	*** 0 blanks - phew!
	
	sort 			english
	*** 0 blanks
	
	sort 			foodgroup
	*** 0 blanks
	
	sort 			process
	*** 0 blanks

* drop duplicates
	duplicates drop lozi, force
	*** 0 observations dropped	
	
	replace foodgroup = "na" if english == "na"
	replace process = "na" if english == "na"
* save
	*save 			"$root/refined/Hdds/food_match.dta", replace
	
************************************************************************
* ingredient reference sheet complete!
* proceed to raw_cleaning.do
************************************************************************	
	
************************************************************************
* interpolating unmatched observations for ingredient1
************************************************************************	
		
* load food_match.dta
	*use 			"$root/refined/Hdds/food_match.dta", clear	
	
* insert observations for the above words
	insobs 1
	replace 		lozi = "mezi" if lozi == ""
	insobs 1
	replace 		lozi = "water" if lozi == ""
	insobs 1
	replace 		lozi = "mwenzi" if lozi == ""
	insobs 1
	replace 		lozi = "masi" if lozi == ""
	insobs 1
	replace 		lozi = "mizi" if lozi == ""
	insobs 1
	replace 		lozi = "kamezi" if lozi == ""

* english
	replace 		english = "water" if lozi == "mezi"
	replace 		english = "water" if lozi == "water"
	replace 		english = "water" if lozi == "mwenzi"	
	replace 		english = "water" if lozi == "masi"
	replace 		english = "water" if lozi == "mizi"
	replace 		english = "water" if lozi == "kamezi"		

* food groups 
	replace 		foodgroup = "na" if lozi == "mezi"
	replace 		foodgroup = "na" if lozi == "water"
	replace 		foodgroup = "na" if lozi == "mwenzi"	
	replace 		foodgroup = "na" if lozi == "masi"
	replace 		foodgroup = "na" if lozi == "mizi"
	replace 		foodgroup = "na" if lozi == "kamezi"

* processing
	replace 		process = "na" if lozi == "mezi"
	replace 		process = "na" if lozi == "water"
	replace 		process = "na" if lozi == "mwenzi"	
	replace 		process = "na" if lozi == "masi"
	replace 		process = "na" if lozi == "mizi"
	replace 		process = "na" if lozi == "kamezi"
	
* save
	*save 			"$root/refined/Hdds/food_match.dta", replace	

* return to raw_cleaning.dta to merge in ingredient2
	
************************************************************************
* interpolating unmatched observations for ingredient 2
************************************************************************	
* order variables
*order lozi english food foodgroup _merge



* drop duplicates
	duplicates drop lozi, force
	*** 30,154 observations dropped, 43 blank observations

* sort
	sort 			english


	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	
	insobs 1
	replace 		lozi = "Bionde" if lozi == ""
	insobs 1	
	replace 		lozi = "cassava leaves" if lozi == ""
	insobs 1
	replace 		lozi = "Onion" if lozi == ""
	insobs 1
	replace 		lozi = "Lizwai" if lozi == ""
	insobs 1
	replace 		lozi = "Mabele" if lozi == ""
	insobs 1
	replace 		lozi = "Mafula" if lozi == ""
	insobs 1
	replace 		lozi = "Masamba" if lozi == ""
	insobs 1
	replace 		lozi = "Mazauli" if lozi == ""
	insobs 1
	replace 		lozi = "Nyamasoya" if lozi == ""
	insobs 1
	replace 		lozi = "sonda" if lozi == ""
	insobs 1
	replace 		lozi = "sweet potato" if lozi == ""
	insobs 1
	replace 		lozi = "Yellow Maize" if lozi == ""
	insobs 1
	replace 		lozi = "duck" if lozi == ""
	insobs 1
	replace 		lozi = "kutateka" if lozi == ""
	insobs 1
	replace 		lozi = "likuya" if lozi == ""
	insobs 1
	replace 		lozi = "mailo" if lozi == ""
	insobs 1	
	replace 		lozi = "matula" if lozi == ""
	insobs 1
	replace 		lozi = "mukoho" if lozi == ""
	insobs 1	
	replace 		lozi = "mundolongo" if lozi == ""
	insobs 1
	replace 		lozi = "tasi" if lozi == ""
	insobs 1
	replace		    lozi = "wabiñwalya" if lozi == ""















* insert observations for the above words
	replace 		lozi = "huku" if lozi == ""
	insobs 1
	replace 		lozi = "lakubeya" if lozi == ""
	insobs 1
	replace 		lozi = "Mupusi" if lozi == ""
	insobs 1
	replace 		lozi = "mango" if lozi == ""
	insobs 1
	replace 		lozi = "masoke" if lozi == ""
	insobs 1
	replace 		lozi = "mbunai" if lozi == ""
	insobs 1
	replace 		lozi = "mulleme" if lozi == ""
	insobs 1
	replace 		lozi = "muloba" if lozi == ""
	insobs 1
	replace 		lozi = "mutetengwa" if lozi == ""
	insobs 1
	replace 		lozi = "mwalelo" if lozi == ""
	insobs 1
	replace 		lozi = "njulu" if lozi == ""
	insobs 1
	replace 		lozi = "nolu" if lozi == ""
	insobs 1
	replace 		lozi = "saapi" if lozi == ""
	insobs 1
	replace 		lozi = "tembe" if lozi == ""
	insobs 1
	replace 		lozi = "bebe" if lozi == ""

* fill in english values
	replace 		english = "chicken" if lozi == "huku"
	replace 		english = "rice" if lozi == "inende"
	replace 		english = "squash orange" if lozi == "Mupusi"
	replace 		english = "pumpkin leaves" if lozi == "mango"
	replace 		english = "maize" if lozi == "masoke"
	replace 		english = "maize" if lozi == "mbunai"
	replace 		english = "sorghum" if lozi == "mulleme"
	replace 		english = "rape" if lozi == "muloba"
	replace 		english = "fishgroundnut" if lozi == "mutetengwa"
	replace 		english = "rape" if lozi == "mwalelo"
	replace 		english = "sweet potato" if lozi == "njulu"
	replace 		english = "sweet potato" if lozi == "nolu"
	replace 		english = "maize" if lozi == "saapu"
	replace 		english = "african eggplant" if lozi == "tembe"
	replace 		english = "maize flour" if lozi == "bebe"

* fill in foodgroup values
	replace 		foodgroup = "Meat and poultry" if ///
						english == "chicken"
	replace 		foodgroup = "Grains" if english == "rice"
	replace 		foodgroup = "Dark green leafy vegetables" if ///
						english == "pumpkin leaves"
	replace 		foodgroup = "Grains" if english == "maize"
	replace 		foodgroup = "Grains" if english == "sorghum"
	replace 		foodgroup = "Dark green leafy vegetables" if ///
						english == "rape"
	replace 		foodgroup = "Pulses (beans, peas and lentils)" if ///
						english == "fishgroundnut"
	replace 		foodgroup = "White roots and tubers and plantains" if ///
						english == "sweet potato"
	replace 		foodgroup = "Dark green leafy vegetables" if ///
						english == "african eggplant"
	replace 		foodgroup = "Grains" if english == "maize flour"

* fill in process values
	replace 		process = "Unprocessed" if english == "chicken"
	replace 		process = "Unprocessed" if english == "rice"
	replace 		process = "Unprocessed" if english == "pumpkin leaves"
	replace 		process = "Unprocessed" if english == "maize"
	replace 		process = "Unprocessed" if english == "sorghum"
	replace 		process = "Unprocessed" if english == "rape"
	replace 		process = "Unprocessed" if english == "fishgroundnut"
	replace 		process = "Unprocessed" if english == "sweet potato"
	replace 		process = "Unprocessed" if english == "african eggplant"
	replace 		process = "Processed culinary ingredients" if ///
						english == "maize flour"

* fill in missing words identified when we merge ingredient2 in raw_cleaing
* fill in missing lozi words
	insobs 1
	replace 		lozi = "Bionde" if lozi == ""
	insobs 1	
	replace 		lozi = "cassava leaves" if lozi == ""
	insobs 1
	replace 		lozi = "Onion" if lozi == ""
	insobs 1
	replace 		lozi = "Lizwai" if lozi == ""
	insobs 1
	replace 		lozi = "Mabele" if lozi == ""
	insobs 1
	replace 		lozi = "Mafula" if lozi == ""
	insobs 1
	replace 		lozi = "Masamba" if lozi == ""
	insobs 1
	replace 		lozi = "Mazauli" if lozi == ""
	insobs 1
	replace 		lozi = "Nyamasoya" if lozi == ""
	insobs 1
	replace 		lozi = "sonda" if lozi == ""
	insobs 1
	replace 		lozi = "sweet potato" if lozi == ""
	insobs 1
	replace 		lozi = "Yellow Maize" if lozi == ""
	insobs 1
	replace 		lozi = "duck" if lozi == ""
	insobs 1
	replace 		lozi = "kutateka" if lozi == ""
	insobs 1
	replace 		lozi = "likuya" if lozi == ""
	insobs 1
	replace 		lozi = "mailo" if lozi == ""
	insobs 1	
	replace 		lozi = "matula" if lozi == ""
	insobs 1
	replace 		lozi = "mukoho" if lozi == ""
	insobs 1	
	replace 		lozi = "mundolongo" if lozi == ""
	insobs 1
	replace 		lozi = "tasi" if lozi == ""
	insobs 1
	replace		    lozi = "wabiñwalya" if lozi == ""

** fill in missing english values
	replace 		english = "mushroom" if lozi == "Bionde"
	replace 		english = "cassava leaves" if lozi == "cassava leaves"
	replace 		english = "cooking oil" if lozi == "cooking Oil"
	replace 		english = "cooking oil" if lozi == "fulawa"
	replace 		english = "groundnut" if lozi == "Ground nuts"
	replace 		english = "maize flour" if lozi == "Maize Meal"
	replace 		english = "pearl millet" if lozi == "Mauza"
	replace 		english = "water" if lozi == "Mezi"
	replace 		english = "millet flour" if lozi == "Millet meal"
	replace 		english = "oil" if lozi == "Oil"
	replace 		english = "oil" if lozi == "salad"
	replace 		english = "oil" if lozi == "saladi"
	replace			english = "baking soda" if lozi == "soda"
	replace 		english = "sugar" if lozi == "sugar"
	replace 		english = "tomato" if lozi == "Tomato"
	replace 		english = "water" if lozi == "Water"
	replace 		english = "onion" if lozi == "Onion"
	replace 		english = "salt" if lozi == "Lizwai"
	replace 		english = "sorghum" if lozi == "Mabele"
	replace 		english = "oil" if lozi == "Mafula"
	replace 		english = "tea leaves" if lozi == "Masamba"
	replace 		english = "pearl millet" if lozi == "Mazauli"
	replace 		english = "soya" if lozi == "Nyamasoya"
	replace 		english = "baking soda" if lozi == "sonda"
	replace 		english = "oil" if lozi == "sweet potato"
	replace 		english = "oil" if lozi == "Yellow Maize"
	replace 		english = "bird" if lozi == "duck"
	replace 		english = "sugar" if lozi == "kutateka"
	replace 		english = "tomato" if lozi == "likuya"
	replace 		english = "sweet potato leaves" if lozi == "mailo"
	replace 		english = "oil" if lozi == "matula"
	replace 		english = "rape" if lozi == "mukoho"
	replace 		english = "pumpkin" if lozi == "mundolongo"
	replace 		english = "fish" if lozi == "tasi"
	replace 		english = "pumpkin" if lozi == "wabiñwalya"

** fill in missing foodgroup values
	replace 		foodgroup = "Other vegetables" if english == "mushroom"
	replace 		foodgroup = "Dark green leafy vegetables" if ///
						english == "cassava leaves"
	replace 		foodgroup = "Grains" if english == "pearl millet"
	replace 		foodgroup = "Grains" if english == "millet flour"
	replace 		foodgroup = "Other oils and fats" if english == "oil"
	replace 		foodgroup = "Condiments and seasonings" if ///
						english == "baking soda"
	replace 		foodgroup = "Condiments and seasonings" if ///
						english == "sugar"
	replace 		foodgroup = "Other vegetables" if english == "tomato"
	replace 		foodgroup = "Other vegetables" if english == "onion"
	replace 		foodgroup = "Dark green leafy vegetables" if ///
						english == "sweet potato leaves"
	replace 		foodgroup = "Condiments and seasonings" if english == "salt"
	replace 		foodgroup = "Grains" if english == "sorghum"
	replace 		foodgroup = "Other beverages and foods" if ///
						english == "tea leaves"
	replace 		foodgroup = "Pulses (beans, peas and lentils)" if ///
						english == "soya"
	replace 		foodgroup = "Fish and sea food" if ///
						english == "fish"
	replace 		foodgroup = "Meat and poultry" if english == "bird"
	replace 		foodgroup = "Vitamin A-rich vegetables, roots and tubers" if ///
						english == "pumpkin"
	replace 		foodgroup = "Dark green leafy vegetables" if english == "rape"

** fill in missing process values
	replace 		process = "Unprocessed" if english == "mushroom"
	replace 		process = "Unprocessed" if english == "cassava leaves"
	replace 		process = "Unprocessed" if english == "groundnut"
	replace 		process = "Unprocessed" if english == "pearl millet"
	replace 		process = "Unprocessed" if english == "millet flour"
	replace 		process = "Processed culinary ingredients" if ///
						english == "oil"
	replace 		process = "Ultra-processed products" if ///
						english == "baking soda"
	replace 		process = "Processed culinary ingredients" if ///
						english == "sugar"
	replace 		process = "Unprocessed" if english == "tomato"
	replace 		process = "Unprocessed" if english == "onion"
	replace 		process = "Processed culinary ingredients" if ///
						english == "salt"
	replace 		process = "Ultra-processed products" if english == "soya"
	replace 		process = "Unprocessed" if english == "bird"
	replace 		process = "Unprocessed" if english == "sweet potato leaves"
	replace 		process = "Unprocessed" if english == "fish"
	replace 		process = "Unprocessed" if english == "pumpkin"
	replace 		process = "Unprocessed" if english == "rape"


* fill in missing words identified when we merge ingredient3 in raw_cleaing
* fill in missing lozi words
	insobs 1
	replace 		lozi = "cassava Meal" if lozi == ""
	insobs 1
	replace 		lozi = "cassava Leaves" if lozi == ""
	insobs 1
	replace 		lozi = "cooking" if lozi == ""
	insobs 1
	replace 		lozi = "Lizwai" if lozi == ""
	insobs 1
	replace 		lozi = "Maize Meal" if lozi == ""
	insobs 1
	replace 		lozi = "Mabele" if lozi == ""
	insobs 1
	replace 		lozi = "Moringa" if lozi == ""
	insobs 1
	replace 		lozi = "Peanut" if lozi == ""
	insobs 1
	replace 		lozi = "Peanut Butter" if lozi == ""
	insobs 1
	replace 		lozi = "salr" if lozi == ""
	insobs 1
	replace 		lozi = "Teabag" if lozi == ""
	insobs 1
	replace 		lozi = "kambaula" if lozi == ""
	insobs 1
	replace 		lozi = "mulilo" if lozi == ""
	insobs 1
	replace 		lozi = "mwanb" if lozi == ""

** fill in missing english values
	replace 		english = "cassava" if lozi == "cassava Meal"
	replace 		english = "cassava leaves" if lozi == "cassava Leaves"
	replace 		english = "oil" if lozi == "cooking"
	replace 		english = "salt" if lozi == "Lizwai"
	replace 		english = "maize flour" if lozi == "Maize Meal"
	replace 		english = "sorghum" if lozi == "Mabele"
	replace 		english = "moringa" if lozi == "Moringa"
	replace 		english = "peanut" if lozi == "Peanut"
	replace 		english = "peanut butter" if lozi == "Peanut Butter"
	replace			english = "salt" if lozi == "salr"
	replace 		english = "tea leaves" if lozi == "Teabag"
	replace 		english = "sweet potato leaves" if lozi == "kambaula"
	replace 		english = "rape" if lozi == "mulilo"
	replace 		english = "cassava" if lozi == "mwanb"

** fill in missing foodgroup values
	replace foodgroup = "White roots and tubers and plantains" if english == "cassava"
	replace foodgroup = "Dark green leafy vegetables" if english == "cassava leaves"
	replace foodgroup = "Other oils and fats" if english == "oil"
	replace foodgroup = "Condiments and seasonings" if english == "salt"
	replace foodgroup = "Grains" if english == "maize flour"
	replace foodgroup = "Grains" if english == "sorghum"
	replace foodgroup = "Dark green leafy vegetables" if english == "moringa"
	replace foodgroup = "Nuts and seeds" if english == "peanut butter"
	replace foodgroup = "Other beverages and foods" if english == "tea leaves"
	replace foodgroup = "Dark green leafy vegetables" if english == "sweet potato leaves"
	replace foodgroup = "Dark green leafy vegetables" if english == "rape"
	replace foodgroup = "Other oils and fats" if english == "cassava"

** fill in missing process values
	replace process = "Unprocessed" if english == "cassava"
	replace process = "Unprocessed" if english == "cassava leaves"
	replace process = "Processed culinary ingredients" if english == "oil"
	replace process = "Processed culinary ingredients" if english == "salt"
	replace process = "Processed culinary ingredients" if english == "maize flour"
	replace process = "Unprocessed" if english == "sorghum"
	replace process = "Unprocessed" if english == "moringa"
	replace process = "Ultra-processed products" if english == "peanut butter"
	replace process = "Unprocessed" if english == "tea leaves"
	replace process = "Unprocessed" if english == "sweet potato leaves"
	replace process = "Unprocessed" if english == "rape"

* fill in missing words identified when we merge ingredient4 in raw_cleaning
* fill in missing lozi words
	insobs 1
	replace lozi = "cassava meal" if lozi == ""
	insobs 1
	replace lozi = "Egg" if lozi == ""
	insobs 1
	replace lozi = "flour" if lozi == ""
	insobs 1
	replace lozi = "Lizwai" if lozi == ""
	insobs 1
	replace lozi = "Mabele" if lozi == ""
	insobs 1
	replace lozi = "Masamba" if lozi == ""
	insobs 1
	replace lozi = "Peanut" if lozi == ""
	insobs 1
	replace lozi = "sweet potato leaves" if lozi == ""
	insobs 1
	replace lozi = "Teabag" if lozi == ""
	insobs 1
	replace lozi = "libawo" if lozi == ""

** fill in missing english values
	replace english = "cassava" if lozi == "cassava meal"
	replace english = "egg" if lozi == "Egg"
	replace english = "flour" if lozi == "flour"
	replace english = "salt" if lozi == "Lizwai"
	replace english = "sorghum" if lozi == "Mabele"
	replace english = "tea leaves" if lozi == "Masamba"
	replace english = "peanut" if lozi == "Peanut"
	replace english = "sweet potato leaves" if lozi == "sweet potato leaves"
	replace english = "tea leaves" if lozi == "Teabag"
	replace english = "amaranth" if lozi == "libawo"

** fill in missing foodgroup values
	replace foodgroup = "White roots and tubers and plantains" if english == "cassava"
	replace foodgroup = "Eggs" if english == "egg"
	replace foodgroup = "Grains" if english == "flour"
	replace foodgroup = "Condiments and seasonings" if english == "salt"
	replace foodgroup = "Grains" if english == "sorghum"
	replace foodgroup = "Other beverages and foods" if english == "tea leaves"
	replace foodgroup = "Dark green leafy vegetables" if english == "sweet potato leaves"
	replace foodgroup = "Dark green leafy vegetables" if english == "amaranth"

** fill in missing process values
	replace process = "Unprocessed" if english == "cassava"
	replace process = "Unprocessed" if english == "egg"
	replace process = "Processed culinary ingredients" if english == "flour"
	replace process = "Processed culinary ingredients" if english == "salt"
	replace process = "Unprocessed" if english == "sorghum"
	replace process = "Unprocessed" if english == "tea leaves"
	replace process = "Unprocessed" if english == "sweet potato leaves"
	replace process = "Unprocessed" if english == "tea leaves"

* fill in missing words identified when we merge ingredient5 in raw_cleaing
* fill in missing lozi words
	insobs 1
	replace lozi = "Baking Powder" if lozi == ""
	insobs 1
	replace lozi = "fresh milk" if lozi == ""
	insobs 1
	replace lozi = "Lizwai" if lozi == ""
	insobs 1
	replace lozi = "Nyanyisi" if lozi == ""
	insobs 1
	replace lozi = "Onga" if lozi == ""
	insobs 1
	replace lozi = "Peanut Butter" if lozi == ""
	insobs 1
	replace lozi = "Teabag" if lozi == ""
	insobs 1
	replace lozi = "bean" if lozi == ""
	insobs 1
	replace lozi = "funani" if lozi == ""
	insobs 1
	replace lozi = "kakunota" if lozi == ""
	insobs 1
	replace lozi = "kinaluna" if lozi == ""

** fill in missing english values
	replace english = "baking soda" if lozi == "Baking Powder"
	replace english = "milk" if lozi == "fresh milk"
	replace english = "salt" if lozi == "Lizwai"
	replace english = "onion" if lozi == "Nyanyisi"
	*replace english = "soup powder" if lozi == "Onga"
	replace english = "peanut butter" if lozi == "Peanut Butter"
	replace english = "tea leaves" if lozi == "Teabag"
	replace english = "sweet potato leaves" if lozi == "bean"
	replace english = "tea leaves" if lozi == "funani"
	replace english = "amaranth" if lozi == "kakunota"

** fill in missing foodgroup values
	replace 	foodgroup = "Condiments and seasonings" if english == "baking soda"
	replace 	foodgroup = "Milk and milk products" if english == "milk"
	replace 	foodgroup = "Condiments and seasonings" if english == "salt"
	replace 	foodgroup = "Other vegetables" if english == "onion"
	*replace 	foodgroup = "Grains" if english == "soup powder"
	replace 	foodgroup = "Nuts and seeds" if english == "peanut butter"
	replace 	foodgroup = "Other beverages and foods" if english == "tea leaves"
	replace 	foodgroup = "Dark green leafy vegetables" if english == "sweet potato leaves"
	replace	foodgroup = "Nuts and seeds" if english == "peanut"
	*replace 	foodgroup = "Other beverages and foods" if english == "tea leaves"
	replace 	foodgroup = "Dark green leafy vegetables" if english == "amaranth"

** fill in missing process values
	replace 	process = "Ultra-processed products" if english == "baking soda"
	replace 	process = "Unprocessed" if english == "milk"
	replace 	process = "Processed culinary ingredients" if english == "salt"
	replace 	process = "Unprocessed" if english == "onion"
	replace 	process = "Unprocessed" if english == "tea leaves"
	replace 	process = "Ultra-processed products" if english == "peanut butter"
	replace 	process = "Unprocessed" if english == "sweet potato leaves"
	*replace	process = "Unprocessed" if english == "peanut"
	*replace	process = "Unprocessed" if english == "tea leaves"
	replace 	process = "Unprocessed" if english == "amaranth"

* re-run the code to interpolate missing values
* create group food variable
	egen 		food = group(english)

* interpolate missing food group values
	by english, sort: stripolate foodgroup food, gen(foo) groupwise
	drop 		foodgroup
	rename 		foo foodgroup
	lab var 	foodgroup "food Group"
	
* interpolate missing process values
	by english, sort: stripolate process food, gen(proccess) groupwise
	drop 		process
	lab var 	proccess "Type of Processing"

* drop duplicates
 	duplicates drop lozi, force
	*** 24 observations dropped

	drop food

* fill in any remaining missing foodgroup and process values
	replace 	english = "sugar" if lozi == "Jolly Juice"
	replace 	english = "amaranth" if lozi == "Ondwe"
	replace 	english = "squash orange" if lozi == "Mupusi"
	replace 	english = "mango" if lozi == "Mango"
	replace 	english = "pumpkin" if lozi == "munda kawe"
	replace 	english = "porridge" if lozi == "pola"
	replace 	english = "cassava" if lozi == "Boiled cassava"
	replace 	english = "pumpkin" if lozi == "mungundumbwa"
	replace 	english = "cassava" if lozi == "mwacha"
	replace 	english = "rice" if lozi == "linende"
	replace 	english = "rape" if lozi == "muholo"

* create group food variable
	egen 		food = group(english)

* interpolate missing food group values
	by english, sort: stripolate foodgroup food, gen(foo) groupwise
	drop 		foodgroup
	rename 		foo foodgroup
	lab var 	foodgroup "food Group"
	
* interpolate missing process values
	by english, sort: stripolate proccess food, gen(process) groupwise
	drop 		proccess
	lab var 	process "Type of Processing"

* trim
	replace 	lozi = strtrim(lozi)
	replace		english = strtrim(english)
	replace 	foodgroup = strtrim(foodgroup)
	replace 	process = strtrim(process)
	
* drop duplicates
	duplicates drop lozi, force
	*** 67 observations dropped
	drop food

	drop if lozi != "" & english == ""
	*** 31 observations dropped

	insobs 1
	
	 drop if lozi == ""

* save

save "$root/refined/Hdds/food_match.dta", replace



** END **
