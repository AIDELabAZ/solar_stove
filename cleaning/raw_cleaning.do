* project: solar stove
* created on: July 2020
* created by: lem
* edited by: jdm
* edited on: 14 Mar 25
* stata v.18.5

* does
	* inputs raw_ingredients.dta
	* cleans ingredients data
	* generates and outputs liquids.dta
	* outputs cleaned ingredients data as meals.dta

* assumes
	* access to raw data
	* cleaned ingredient dictionary

* to do:
	* left off around line 700


***********************************************************************
**# 0 - setup
***********************************************************************

* define paths
	global	root	=	"$data/raw/dietary/HDDS"
	global	export	=	"$data/refined"
	global	output	=	"$data/analysis/tables"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/raw_cleaning", append

* import ingredient sheet and save as .dta
	import 			excel using "$root/Lealui_mapungu_nalitoya_data28_02_2020.xlsx", ///
						sheet("Lealui_Mapungu_Nalitoya_ingridi") firstrow clear
	
************************************************************************
**# 1 - prepare ingredients data
************************************************************************

	sum Dish
	***	43,127 household observations
	*** 34,420 dish observations

* anonymize data
	drop			A Whoenterthedata Name

* rename village, household, week, and day variables
	rename 			Sourcepdf village
	rename 			COD hhid
	lab var 		hhid "HH ID"
	rename 			Week week
	lab var 		week "Week"
	rename 			Day day
	lab var 		day "Day"
	lab var			Meal "Meal"
	
* order variables	
	order 			hhid week day Meal Dish
	drop			Group Solar Gender village

* create meal indicator
	gen 			meal = 0 if Meal == "Breakfast" | Meal == "breakfast"
	replace 		meal = 1 if Meal == "Lunch" | Meal == "lunch"
	replace 		meal = 2 if Meal == "Dinner" | Meal == "dinner"
	replace 		meal = 3 if Meal == "Boiled Liquids" | ///
					Meal == "Boiled liquids" | Meal == "Boiled water" | ///
						Meal == "boiled liquids"
	lab var 		meal "Meal"
	lab define 		meal 0 "Breakfast" 1 "Lunch" 2 "Dinner" 3 "Boiled Liquid"
	lab val 		meal meal
	drop 			Meal
	order 			meal, after(day)

* create cooking method indicator
	drop 			Cookingmethod
	gen 			cook = 0 if Cookingmethod_modified == "No meal"
	replace 		cook = 1 if Cookingmethod_modified == "C-Charcoal"
	replace 		cook = 2 if Cookingmethod_modified == "E-Electric Stove"
	replace 		cook = 3 if Cookingmethod_modified == "F- Firewood" | ///
						Cookingmethod_modified == "F-Firewood"
	replace 		cook = 4 if Cookingmethod_modified == "M-Cow dung" | ///
						Cookingmethod_modified == "M-Manure" | /// 
						Cookingmethod_modified == "M-Manure " | ///
						Cookingmethod_modified == "M-Manure  " 
	replace 		cook = 5 if Cookingmethod_modified == "S-Solar Stove" | ///
						Cookingmethod_modified == "S-Solar stove"
	replace 		cook = 6 if Cookingmethod_modified == "Boiled liquids"
	replace 		cook = 7 if Cookingmethod_modified == "unspecified"
	lab var 		cook "Cooking Method"
	lab define 		method 0 "No Meal" 1 "Charcoal" 2 "Electric" 3 ///
						"Firewood" 4 "Manure" 5 "Solar Stove" ///
						6 "Boiled Liquid" 7 "Unspecified"
	lab val 		cook method
	drop 			Cookingmethod_modified
	order 			cook, after(Dish)

* create dish indicators
	rename 			Dish dish_num
	lab var 		dish_num "Dish Number in Meal"
	
* input missing values for dish_num 	

	tab 			dish_num, m

	* village	group			hhid	gender	solar week	day	 meal  dish_num
	
	* Mapungu	Learning Plot	238000	Male	0	   5	5	Dinner    .
		*** change to "2"	
		
	* Mapungu	None	        219000	Male	0	   6	1	Breakfast .
		*** change to "1"	

	* Mapungu	None            219000	Male	0	   6	3	Breakfast .	
		*** change to "1"	

	* Mapungu	Both	        234000	Female	1	   1	5	Breakfast .
		*** change to "1"	

	* Nalitoay	Both	        371000	Male	0	   2	2	Breakfast .
		*** change to "1"	

	* Mapungu	Both	        233000	Female	1	   1	4	Breakfast .	
		*** change to "1"	
		
	* Mapungu	Learning Plot	238000	Male	0	   6	6	Breakfast .
		*** changed to "1"	

	* Mapungu	Learning Plot	238000	Male	0	   3	6	Breakfast .	
		*** change to "1"	
		
	* Nalitoay	Nutrition Club	320000	Female	0	   6	6	Breakfast .	
		*** change to "1"

	sort 			hhid week day meal dish_num	
	
* replace the single missing obs that should be "2"
	replace 		dish_num = 2 if hhid == 238000 & week == 5 & ///
						day == 5 & meal == 2 & dish_num == .
						
* replace the all other missing obs with "1"
	replace 		dish_num = 1 if dish_num == .
		
	gen 			dish = 1 if dish_num != .
	lab var 		dish "Dish Counter"
	gen 			ss_count = 1 if cook == 5
	replace 		ss_count = 0 if cook == . | dish != .
	lab var 		ss_count "Meal Cooked with Solar Stove"
	order 			dish ss_count, after(dish_num)

* replace cooking method with liquid cooking method
	replace 		Method = "" if cook != 6
	replace 		cook = 1  if cook == 7 & Method == "C- Charcol" | /// 
						Method == "C-Charcoal" | Method == "C-charcoal" | ///
						Method == "Charcoal" | Method == "c-charcoal" 
	replace 		cook = 2  if Method == "E-Electric Stove" | ///
						Method == "E-Electric stove"
	replace 		cook = 3  if Method == "F- Firewood" | ///
						Method == "F- Firewood " | Method == /// 
						"F- firewood"  | Method == "F- firewoods" | ///
						Method == "F-Firewood" | Method == "F-Frewood" | ///
						Method == "F-firewood" | Method == "F-firewoods" | ///  
						Method == "Firewood" | Method == "f-firewood"
	replace 		cook = 4  if Method == "M-Cow dung" | Method == /// 
						"M-Manure" | Method == "M-Manure " | Method == ///
						"M-Manure  " 
	replace 		cook = 5  if Method == "S- Solar stove" | Method == ///
						"S-Solar Stove" | Method == "S-Solar stove" | ///
						Method == "S-Stove Solar" | Method == "s-solar stove"
	replace 		cook = 7  if Method == "Missing" | Method == "Other" | ///
						Method == "missing"
	replace 		cook = 7 if cook == 6
	drop 			Method
	
	
************************************************************************
**# 2 - clean up liquid and legume data
************************************************************************

* create liquid quantity
	replace 		Quantity = "2.5" if Quantity == "2,5"
	replace 		Quantity = "2.5" if Quantity == "2..5"
	replace 		Quantity = "3.5" if Quantity == "3,5"
	replace		   	Quantity = "5" if Quantity == "5lt"
	replace 		Quantity = "8" if Quantity == "8cm "
	replace 		Quantity = "" if Quantity == "lt"
	replace 		Quantity = "" if Quantity == "na"
	gen 			vol = Quantity
	destring 		vol, replace
	lab var 		vol "Volume Liquid (L)"
	replace 		vol = vol*0.001 if Units == "mL"

* create milk/water indicator
	gen 			type = 0 if Type == "Mezi" | Type == "Water" | Type == ///
						"kasana mazi" | Type == "masamba" | Type == "mazi" | ///
						Type == "mezi" | Type == "mezi " | Type == ///
						"mezi akutapa" | Type == "mezi hakunwa" | Type == ///
						"mezi hakutapa " | Type == "hakunwa" | ///
						Type == "hakutapa" | Type == "ni mezi" | ///
						Type == "tea" | Type == "water"
	replace 		type = 1 if Type == "Mabisi" | Type == "Milk" | ///
						Type == "fresh milk" | Type == "mabilisi" | ///
						Type == "mabis" | Type == "mabisa" | ///
						Type == "mabisi" | Type == "mabsisi" | ///
						Type == "milk" | Type == "milk "
	lab var type 		"Type of Liquid"
	lab 				define watermilk 0 "Water" 1 "Milk"
	lab val type 		watermilk
	drop 				Type
	order 				type, before(vol)
	replace 			vol = 7.5 if vol == 750
	replace 			vol = 5 if vol == 500
	replace 			vol = .25 if vol == .0025
	replace 			type = 0 if type == . & vol != .
	replace 			type = . if vol == .
	
* clean up legume variables	
	rename 			Noofcupsoflegumes legumes
	sort 			legumes
	
* replace values to be consistent
	replace 		legumes = "0" if legumes == ""
	replace 		legumes = "0" if legumes == "Faibi"
	replace 		legumes = "0" if legumes == "Mbonyi"
	replace 		legumes = "0" if legumes == "na "
	replace 		legumes = "0" if legumes == "na  "	
	replace 		legumes = "0" if legumes == "na"
	replace 		legumes = "0" if legumes == "number of cups not added"
	replace 		legumes = "0" if legumes == " number of cups not added"
	replace 		legumes = "0" if legumes == "5Kg vegetable"	
	replace 		legumes = "0" if legumes == "no"
	replace 		legumes = "0" if legumes == "no meal"
	replace 		legumes = "0" if legumes == "number of cups not mentioned"
	replace 		legumes = "0" if legumes == "numer of cups not added"		
	
	
	replace 		legumes = "1" if legumes == "1cup"
	replace 		legumes = "1.5" if legumes == "1 1/2 Cups"
	replace 		legumes = "1.5" if legumes == "1.5cups"
	replace 		legumes = "1.5" if legumes == "1,5"
	replace			legumes = "2.5" if legumes == "2,5cups"
	replace 		legumes = "2.5" if legumes == "2,5"
	replace			legumes = "2.5" if legumes == "2.5cups"
	replace 		legumes = "2" if legumes == "2cups"
	replace 		legumes = "3" if legumes == "3cups"
	replace 		legumes = "3" if legumes == "3 Cups"
	replace 		legumes = "3" if legumes == "3 Cups"
	replace 		legumes = "3" if legumes == "3 Cups"
	replace 		legumes = "4" if legumes == "4cups"
	replace 		legumes = "5" if legumes == "5cups"
	replace 		legumes = "0.5" if legumes == "o.5"
	
	
	replace 		legumes = "1.5" if legumes == "1 1/2 cup of 10 cm"
	replace 		legumes = "1" if legumes == "1 Cup of 10 Cm"
	replace 		legumes = "1" if legumes == "1 cup of 10 cm"
	replace 		legumes = "1" if legumes == "1 x 10 cm cup"
	replace 		legumes = "1" if legumes == "1 x 8 Cm cowpeas"
	replace 		legumes = "1" if legumes == "1 cup of 10cm"
	replace 		legumes = "1" if legumes == "10 cm cup of beans"
	replace 		legumes = "1" if legumes == "10 cm yamanawa"	
	replace 		legumes = "2" if legumes == "2 Cups 8 Cm Manawa"
	replace 		legumes = "2" if legumes == "2 Cups 8CM Manawa"
	replace 		legumes = "2" if legumes == "2 X 8CM cups"	
	replace 		legumes = "2" if legumes == "2 x 8CM Cup"
	replace 		legumes = "2" if legumes == "2 x 8CM Cups"
	replace 		legumes = "2" if legumes == "2 x 9cm ya manawa, but not mentioned as an ingredient for the day"	
	replace 		legumes = "2" if legumes == "2*8 cm"	
	replace 		legumes = "2" if legumes == "2*8cm"
	replace 		legumes = "3" if legumes == "3 Cups Cm Mbonyi"
	replace 		legumes = "3" if legumes == "3 cm yamanawa"
	replace 		legumes = "3" if legumes == "3 x 8CM Cups"
	replace 		legumes = "3" if legumes == "3*8cm"	
	replace 		legumes = "4" if legumes == "4*8cm"	
	replace 		legumes = "1" if legumes == "8 cm cup beans"	
	replace 		legumes = "2" if legumes == "8 Cm 2 Cups Mbonyi"		
	replace 		legumes = "1" if legumes == "8cm yamanawa"	
	replace 		legumes = "1" if legumes == "9cm"	
	replace 		legumes = "2" if legumes == "Lituu Barbara 2.8 Cm Cups"	
	replace 		legumes = "1" if legumes == "Manawa 8CM Cup"	
	replace 		legumes = "1" if legumes == "9cm"	
	replace 		legumes = "2" if legumes == "Manawa 2.10 Cm Cups"	
	replace 		legumes = "3" if legumes == "Manawa 3 Cups 10 Cm"	
	replace 		legumes = "2" if legumes == "Mbonyi 2 Cups 8Cm"	
	replace 		legumes = "2" if legumes == "Manawa 2 Cups 8 CM"	
	replace 		legumes = "2" if legumes == "Manawa 8Cm 2 Cups"	
	replace 		legumes = "3" if legumes == "Mbonyi 8Cm 3 Cups"	
	replace 		legumes = "2" if legumes == "Mbonyi Oange 2 Cups 10 CM"	
	
* get rid of text
	replace 		legumes = "0" if legumes == "no cooking method"
	replace 		legumes = "0" if legumes == "no cups added"
	replace 		legumes = "0" if legumes == "numbe of cups not added"
	replace 		legumes = "0" if legumes == "number  of cups not added"
	replace 		legumes = "0" if legumes == "number of cups added"
	replace 		legumes = "0" if legumes == "number of cups not aadded"
	replace 		legumes = "0" if legumes == "number of cups ont added"
	replace 		legumes = "0" if legumes == "an"

* drop unneeded variables
	drop 			Liquid Quantity Units AH
	drop 			Other Notes Note  ///
						Notesorspecifyifotherunits AB

************************************************************************
**# 3 - clean up ingredient data
************************************************************************

* replace "na" values with blanks for ingredient variables
	replace 		Ingredient1 = "" if Ingredient1 == "na" | ///
						Ingredient1 == "n/a" | Ingredient1 == "na " | ///
						Ingredient1 == "ma" | Ingredient1 == "no meal"
	replace 		Ingredient2 = "" if Ingredient2 == "na" | ///
						Ingredient2 == "n/a" | Ingredient2 == "na " | ///
						Ingredient2 == "ma" | Ingredient2 == "no meal"
	replace 		Ingredient3 = "" if Ingredient3 == "na" | ///
						Ingredient3 == "n/a" | Ingredient3 == "na " | ///
						Ingredient3 == "ma" | Ingredient3 == "no meal"
	replace 		Ingredient4 = "" if Ingredient4 == "na" | ///
						Ingredient4 == "n/a" | Ingredient4 == "na " | ///
						Ingredient4 == "ma" | Ingredient4 == "no meal"
	replace 		Ingredient5 = "" if Ingredient5 == "na" | ///
						Ingredient5 == "n/a" | Ingredient5 == "na " | ///
						Ingredient5 == "ma" | Ingredient5 == "no meal"
	replace 		Ingredient6 = "" if Ingredient6 == "na" | ///
						Ingredient6 == "n/a" | Ingredient6 == "na " | ///
						Ingredient6 == "ma" | Ingredient6 == "no meal"
	replace 		Ingredient7 = "" if Ingredient7 == "na" | ///
						Ingredient7 == "n/a" | Ingredient7 == "na " | ///
						Ingredient7 == "ma" | Ingredient7 == "no meal"

* replace "na" values with blanks for obs that record boiling liquids (vs cooking a meal)											
	replace			Ingredient1 = "" if meal == 3
	replace 		Ingredient2 = "" if meal == 3
	replace	 		Ingredient3 = "" if meal == 3
	replace	 		Ingredient4 = "" if meal == 3
	replace 		Ingredient5 = "" if meal == 3
	replace 		Ingredient6 = "" if meal == 3
	replace			Ingredient7 = "" if meal == 3

	replace type 	= . if meal != 3
	replace 		vol = . if meal != 3

	replace 		dish_num = . if meal == 3
	replace 		dish = 1
	replace 		ss_count = 1 if cook == 5
	replace 		ss_count = 0 if cook != 5

	drop if 		meal == .

* replace missing weeks
	replace 		week = 4 if week == . & hhid == 338000
	replace 		week = 5 if week == . & hhid == 322000
	replace 		week = 2 if week == . & hhid == 246000

* replace missing days
	replace 		day = 7 if day == . & hhid == 133000
	replace 		day = 7 if day == . & hhid == 141000
	replace 		day = 7 if day == . & hhid == 153000
	replace 		day = 2 if day == . & hhid == 213000
	replace 		day = 3 if day == . & hhid == 245000
	replace 		day = 6 if day == . & hhid == 359000
	replace 		day = 5 if day == . & week == 1 & hhid == 366000
	replace 		day = 5 if day == . & week == 3 & hhid == 366000
	replace 		day = 6 if day == . & week == 6 & hhid == 366000

	
************************************************************************
**## 3.1 - cut out liquid data
************************************************************************
	
* cut out liquid data
	preserve
		keep if 	meal == 3
		save 		"$export/Liquids/liquids.dta", replace
	restore


************************************************************************
**## 3.2 - clean meal data
************************************************************************

* load just meal data
	keep if 	meal != 3
	*** 6,987 observations deleted
	
* drop data with missing values
	drop type 		vol
	drop if 		cook == 0 | cook == 7
	*** 4,908 obs dropped
	drop if 		Ingredient1  == "" & Ingredient2 == "" & /// 
						Ingredient3 == "" & Ingredient4 == "" & /// 
						Ingredient5 == "" & Ingredient6 == "" & ///
						Ingredient7 == ""
	*** 7 obs dropped

	replace 		Ingredient1 = strtrim(Ingredient1)
	replace 		Ingredient2 = strtrim(Ingredient2)
	replace 		Ingredient3 = strtrim(Ingredient3)
	replace 		Ingredient4 = strtrim(Ingredient4)
	replace 		Ingredient5 = strtrim(Ingredient5)
	replace 		Ingredient6 = strtrim(Ingredient6)
	replace 		Ingredient7 = strtrim(Ingredient7)
	
	replace 		Ingredient1 = stritrim(Ingredient1)	
	replace 		Ingredient2 = stritrim(Ingredient2)	
	replace 		Ingredient3 = stritrim(Ingredient3)	
	replace 		Ingredient4 = stritrim(Ingredient4)	
	replace 		Ingredient5 = stritrim(Ingredient5)	
	replace 		Ingredient6 = stritrim(Ingredient6)	
	replace 		Ingredient7 = stritrim(Ingredient7)	
	
	
* check for duplicates of hhid week day meal dish_num	
	
	duplicates 		li hhid week day meal dish_num
		*** 2 duplicates
		
	  /*| group:   obs:     hhid   week   day     meal   dish_num |
	    |---------------------------------------------------------|
	    |      1      1   358000      3     1   Dinner          1 |
	    |      1   9997   358000      3     1   Dinner          1 |
	    |      2      2   370000      1     1   Dinner          1 |
	    |      2   1107   370000      1     1   Dinner          1 |*/
		
	sort 			hhid week day meal dish_num
	
* renumber the dishes for hhid 358000 
	replace 		dish_num = 2 if hhid == 358000 & week == 3 & ///
						day == 1 & meal == 2 & Ingredient1 == "mundambi"	

* renumber the dishes for hhid 370000 
	replace 		dish_num = 2 if hhid == 370000 & week == 1 & ///
						day == 1 & meal == 2 & Ingredient1 == "Buhobe"
	
* check for duplicates	
	duplicates li hhid week day meal dish_num
		*** 0 duplicates	
		
* another check		
	isid 			hhid week day meal dish_num
		*** 0 duplicates	
	
* merge in data of Ingredient1
	rename 			Ingredient1 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"
	drop if 		_merge == 2
	sort 			_merge lozi
	*** 30,736 observations matched, 207 observations didn't match

	replace 		lozi = "" if _merge == 1
	drop 			_merge
	rename 			lozi lozi1
	rename 			english english1
	rename 			foodgroup foodgroup1
	rename 			process process1

* merge in data of Ingredient2
	rename 			Ingredient2 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"
	drop if 		_merge == 2
	sort 			_merge lozi
	*** 29,824 observations matched, 1,119 observations didn't match

	replace 		lozi = "" if _merge == 1	
	drop 			_merge
	rename 			lozi lozi2
	rename 			english english2
	rename 			foodgroup foodgroup2
	rename 			process process2

* merge in data of Ingredient3
	rename 			Ingredient3 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"	
	drop if 		_merge == 2
	sort 			_merge lozi
	*** 30,016 observations matched, 927 observations didn't match
	
	replace 		lozi = "" if _merge == 1		
	drop 			_merge
	rename 			lozi lozi3
	rename 			english english3
	rename 			foodgroup foodgroup3
	rename 			process process3

* merge in data of Ingredient4
	rename 			Ingredient4 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"
	drop if 		_merge == 2
	sort 			_merge lozi
	*** 30,249 observations matched, 694 observations didn't match

	replace 		lozi = "" if _merge == 1		
	drop			_merge
	rename 			lozi lozi4
	rename 			english english4
	rename 			foodgroup foodgroup4
	rename 			process process4

* merge in data of Ingredient5
	rename 			Ingredient5 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"
	drop if 		_merge == 2
	sort 			_merge lozi	
	*** 30,581 observations matched, 362 observations didn't match

	replace 		lozi = "" if _merge == 1		
	drop 			_merge
	rename 			lozi lozi5
	rename 			english english5
	rename			 foodgroup foodgroup5
	rename 			process process5

* merge in data of Ingredient6
	rename 			Ingredient6 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"
	drop if 		_merge == 2
	sort 			_merge lozi		
	*** 330,857 observations matched, 86 observations didn't match

	replace 		lozi = "" if _merge == 1		
	drop 			_merge
	rename 			lozi lozi6
	rename 			english english6
	rename 			foodgroup foodgroup6
	rename 			process process6

* merge in data of Ingredient7
	rename 			Ingredient7 lozi
	merge m:1 		lozi using "$export/HDDS/food_match.dta"
	drop if 		_merge == 2	
	sort 			_merge lozi	
	*** 330,933 observations matched, 10 observations didn't match

	replace 		lozi = "" if _merge == 1		
	drop 			_merge
	rename 			lozi lozi7
	rename 			english english7
	rename 			foodgroup foodgroup7
	rename 			process process7

* define Labels
	lab define 		foodgroup 0 "None" 1 "Cereals" 2 "Tubers" ///
						3 "Vitamin-A Vegetables" 4 "Leafy Greens" ///
						5 "Vegetables" 6 "Vitamin-A Fruits" 7 "Fruits" ///
						8 "Organ Meat" 9 "Flesh Meat" 10 "Eggs" ///
						11 "Fish" 12 "Pulses" 13 "Milk" ///
						14 "Oils & Fats" 15 "Sweets" ///
						16 "Spices & Condiments" 17 "Fried Snacks" ///
						18 "Beverages" 19 "Nuts & seeds", replace
						

	lab define 		process 0 "None" 1 "Unprocessed" 2 "Proccessed Food" ///
						3 "Processed Ingredient" 4 "Ultra-Processed"
	
* create numerical versions of foodgroup and process
	forval				 x = 1/7 {
		gen 			fg`x' = 16 if foodgroup`x' == "Condiments and seasonings"
		replace 		fg`x' = 4 if foodgroup`x' == "Dark green leafy vegetables"
		replace			fg`x' = 10 if foodgroup`x' == "Eggs"
		replace 		fg`x' = 11 if foodgroup`x' == "Fish and sea food"
		replace 		fg`x' = 1 if foodgroup`x' == "Grains"
		replace 		fg`x' = 9 if foodgroup`x' == "Meat and poultry"
		replace 		fg`x' = 13 if foodgroup`x' == "Milk and milk products"
		replace 		fg`x' = 19 if foodgroup`x' == "Nuts and seeds"
		replace 		fg`x' = 8 if foodgroup`x' == "Organ meat"
		replace 		fg`x' = 18 if foodgroup`x' == "Other beverages and foods"
		replace 		fg`x' = 7 if foodgroup`x' == "Other fruits"
		replace 		fg`x' = 14 if foodgroup`x' == "Other oils and fats"
		replace 		fg`x' = 5 if foodgroup`x' == "Other vegetables"
		replace 		fg`x' = 12 if foodgroup`x' == "Pulses (beans, peas and lentils)"
		replace 		fg`x' = 17 if foodgroup`x' == "Savoury and fried snacks"
		replace 		fg`x' = 15 if foodgroup`x' == "Sweets"
		replace 		fg`x' = 6 if foodgroup`x' == "Vitamin A-rich fruits"
		replace 		fg`x' = 3 if foodgroup`x' == "Vitamin A-rich vegetables, roots and tubers"
		replace 		fg`x' = 2 if foodgroup`x' == "White roots and tubers and plantains"
		replace 		fg`x' = 0 if fg`x' == .
		lab var 		fg`x' "Food Group"
		lab val 		fg`x' foodgroup
		}
		drop 			foodgroup*
	
	forval 				x = 1/7 {
		gen 			proc`x' = 1 if process`x' == "Unprocessed"
		replace 		proc`x' = 2 if process`x' == "Processed foods"
		replace 		proc`x' = 3 if process`x' == "Processed culinary ingredients"
		replace 		proc`x' = 4 if process`x' == "Ultra-processed products"
		replace 		proc`x' = 0 if proc`x' == .
		lab var 		proc`x' "Type of Processing"
		lab val 		proc`x' process
		}
	drop 				process*

* clean memory variable
	gen 			recall = 0 if Basedonmemory == "no" | Basedonmemory == "no " | ///
						Basedonmemory == "na"
	replace			recall = 1 if Basedonmemory == "Nahupula" | Basedonmemory == "S" | ///
						Basedonmemory == "S999/" | Basedonmemory == "c" | ///
						Basedonmemory == "yes"
	lab var			recall "Was Entry Based on Recall?"	
	drop			Basedonmemory
						
* order variables
	drop 			lozi*
	order 			english1 fg1 proc1 ///
					english2 fg2 proc2 ///
					english3 fg3 proc3 ///
					english4 fg4 proc4 ///
					english5 fg5 proc5 ///
					english6 fg6 proc6 ///
					english7 fg7 proc7, after(cook)
	
***********************************************************************
**# 4 - end matter, clean up to save
***********************************************************************
	
	save 			"$export/HDDS/cleaned_ingredients.dta", replace	
	
/* END */
