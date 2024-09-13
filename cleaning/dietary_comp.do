* project: solar stoves - final outcomes: composition of diet
* created on: January 2021
* created by: lem
* edited by: jdm
* edited on: 12 Sep 2024
* stata v.18.5

* does
	* inputs cleaned ingredient data
	* generates HDDS and SR indices
	* generates variables for number of meals, number of meals skipped, and
		* average number of dishes per meal type
	* outputs regression ready data

* assumes
	* access to cleaned ingredient dictionary

* to do:
	* done


***********************************************************************
* 0 - setup
***********************************************************************

* define paths
	global	root	=	"$data/raw/dietary"
	global	export	=	"$data/refined"
	global	logout	=	"$data/logs"
	
* open log
	cap log 			close 
	log using			"$logout/dietary_comp", append
	
* load data
	use 				"$export/HDDS/cleaned_ingredients.dta", clear


***********************************************************************
* 1 - generate variables for intermediate dietary outcomes (2a in PAP)
***********************************************************************		

* merge in control variables
	merge m:1		hhid using "$export/c_var.dta"
	*** 30,602 merged, 341 not matched
	*** all from households 340000 and 347000
	
	drop if			_merge == 1
	drop			_merge
	
	order 			village hhid aas hh_size ai tli sex age edu ///
						solar

* rename treatment and assignment variables
	rename 			solar treat_assign
	rename 			ss_count ss_use
	
* (1) An indicator equal to 1 if a solar stove was used to prepare a
	* given dish, boil a given quantity of liquid, or cook a given 
	* quantity of legumes, and zero otherwise.

	* indicator = ss_use (created in raw_cleaning.dta)	
	
* (2) The share of all dishes (alt: liquids, legumes) prepared using a solar
	* stove during a given timeframe. 
	

** generate meal_share variable
* generate total times ss used to prepare dishes within meal
	egen			hh_m_use = total(ss_use), by(hhid week day meal)	
	lab var			hh_m_use "SS Use: Meal"

* sum of dishes per meal cooked using ANY fuel	
	egen 			hh_m_fuel = count(dish), by(hhid week day meal)
	lab var			hh_m_fuel "Fuel Used: Meal"

* the share of all dishes prepared using a solar stove during a meal
	egen 			share_meal = max(hh_m_use/hh_m_fuel), by(hhid week day meal)
	lab var			share_meal "Share of SS Used Per Meal"

** generate day_share variable	
* generate a count of times ss used to prepare dishes within a day
	egen			hh_da_use = total(ss_use), by(hhid week day)
	lab var			hh_da_use "SS Use: Day"

* sum of dishes per day cooked using ANY fuel	
	egen 			hh_da_fuel = count(dish), by(hhid week day)
	lab var			hh_da_fuel "Fuel Used: Meal"

* the share of all dishes prepared using a solar stove during a day
	egen 			share_day = max(hh_da_use/hh_da_fuel), by(hhid week day)	
	lab var			share_day "Share of SS Used Per Day"	
	
** generate week_share variable			
* generate a count of times ss used to prepare dishes within a week
	egen 			hh_w_use = total(ss_use), by(hhid week)
	lab var			hh_w_use "SS Use: Week"
		
* sum of dishes per week cooked using ANY fuel	
	egen 			hh_w_fuel = count(dish), by(hhid week)
	lab var			hh_w_fuel "Fuel Used: Week"	

* the share of all dishes prepared using a solar stove during a week
	egen 			share_week = max(hh_w_use/hh_w_fuel), by(hhid week)
	lab var			share_week "Share of SS Used Per Week"	
	
** generate tot_share variable				
* generate a count of times ss used to prepare dishes overall
	egen 			hh_t_use = total(ss_use), by(hhid)
	lab var			hh_t_use "SS Use: Total"
	
* sum of overall dishes cooked using ANY fuel	
	egen 			hh_t_fuel = count(dish), by(hhid)
	lab var			hh_t_fuel "Fuel Used: Total"	

* the overall share of all dishes prepared using a solar stove
	egen 			share_total = max(hh_t_use/hh_t_fuel), by(hhid)	
	lab var			share_total "Share of SS Used Total"	

	
***********************************************************************
* 2 - generate variables for final hdds outcomes
***********************************************************************		
	
* household dietary diversity scores (hdds)
										
* dish DDS: The HDDS for a given dish, calculated as the total count of all 
	* food groups represented in the dish.

* generate indicator variables for each dish category (1-12)
	sort 				hhid week day meal dish_num

	forval 				x = 1/12 {
		gen 				d`x' = 0
	}	
	
	lab var 			d1 "Dish Total: cereals"
	lab var 			d2 "Dish Total: white tubers and roots"
	lab var 			d3 "Dish Total: vegetables"
	lab var 			d4 "Dish Total: fruits"
	lab var 			d5 "Dish Total: meat"
	lab var 			d6 "Dish Total: eggs"
	lab var 			d7 "Dish Total: fish and other seafood"
	lab var 			d8 "Dish Total: legumes, nuts and seeds"
	lab var 			d9 "Dish Total: milk and milk products"
	lab var 			d10 "Dish Total: oils and fats"
	lab var 			d11 "Dish Total: sweets"
	lab var 			d12 "Dish Total: spices, condiments and beverages"
		
* replace indicator variables based on food groups (sort the 18 food groups 
	* into 12 categories)

	forval 				x = 1/7 {
		replace 			d1 = 1 if fg`x' == 1
		replace				d2 = 1 if fg`x' == 2
		replace 			d3 = 1 if fg`x' == 3 | fg`x' == 4 | fg`x' == 5
		replace 			d4 = 1 if fg`x' == 6 | fg`x' == 7
		replace 			d5 = 1 if fg`x' == 8 | fg`x' == 9
		replace				d6 = 1 if fg`x' == 10
		replace 			d7 = 1 if fg`x' == 11
		replace 			d8 = 1 if fg`x' == 12
		replace 			d9 = 1 if fg`x' == 13
		replace 			d10 = 1 if fg`x' == 14
		replace 			d11 = 1 if fg`x' == 15 | fg`x' == 17
		replace 			d12 = 1 if fg`x' == 16 | fg`x' == 18
	}

* total the food groups in a dish 	
	egen				HDDS_ct_d = rowtotal(d1-d12)
	lab var				HDDS_ct_d "HDDS: Dish"

* drop observations with no HDDS 
	drop if 			HDDS_ct_d == 0
	*** 58 dropped
	
* meal HDDS: the HDDS for a given meal, calculated as

* (1) The total count of all food groups represented in the meal.

* generate indicator variables for food categories included in a meal with
	* max possible value = 1 to avoid double-counting
	sort 				hhid week day meal

	forval 				x = 1/12 {
		egen 				m`x' = max(d`x'), by(hhid week day meal)
		}
		
* total the food groups in a meal 	
	egen 				HDDS_ct_m = rowtotal(m1-m12)
	lab var 			HDDS_ct_m "HDDS: Meal Count"

* generate total dishes per meal
	egen 				dishes_meal = sum(dish), by(hhid week day meal)

* (2) The average of the HDDS over dishes in the meal.
* average the HDDS score over total dishes in meal	
	gen 				HDDS_avg_m = HDDS_ct_m/dishes_meal
	lab var 			HDDS_avg_m "HDDS: Meal Average"
		
* label variables		
	lab var 			m1 "Meal Total: cereals"
	lab var 			m2 "Meal Total: white tubers and roots"
	lab var 			m3 "Meal Total: vegetables"
	lab var 			m4 "Meal Total: fruits"
	lab var 			m5 "Meal Total: meat"
	lab var 			m6 "Meal Total: eggs"
	lab var 			m7 "Meal Total: fish and other seafood"
	lab var 			m8 "Meal Total: legumes, nuts and seeds"
	lab var 			m9 "Meal Total: milk and milk products"
	lab var 			m10 "Meal Total: oils and fats"
	lab var 			m11 "Meal Total: sweets"
	lab var 			m12 "Meal Total: spices, condiments and beverages"
	lab var				dishes_meal "Dishes Per meal"
		
* day HDDS: The HDDS for a given day calculated as

* (1) The total count of all groups represented in the day.
* generate indicator variables for food categories included in a day with
	* max possible value = 1 to avoid double-counting
	sort 				hhid week day

	forval 				x = 1/12 {
		egen 				da`x' = max(d`x'), by(hhid week day)
	}

* label variables
	lab var 			da1 "Day Total: cereals"
	lab var 			da2 "Day Total: white tubers and roots"
	lab var 			da3 "Day Total: vegetables"
	lab var 			da4 "Day Total: fruits"
	lab var 			da5 "Day Total: meat"
	lab var 			da6 "Day Total: eggs"
	lab var 			da7 "Day Total: fish and other seafood"
	lab var 			da8 "Day Total: legumes, nuts and seeds"
	lab var 			da9 "Day Total: milk and milk products"
	lab var 			da10 "Day Total: oils and fats"
	lab var 			da11 "Day Total: sweets"
	lab var 			da12 "Day Total: spices, condiments and beverages"	

* total the food groups in a day		
	egen 				HDDS_ct_da = rowtotal(da1-da12)
	lab var 			HDDS_ct_da "HDDS: Day Count"

* (2) The average of the HDDS over meals in the day.
* average the HDDS score over 3 meals in a day	
	gen 				HDDS_avg_da = HDDS_ct_da/3
	lab var 			HDDS_avg_da "HDDS: Day Average"
		
* week HDDS: The HDDS for a given week calculated as

* (1) The total count of all groups represented in the week.

* generate indicator variables for food categories included in a week with
	* max possible value = 1 to avoid double-counting
	sort 				hhid week

	forval 				x = 1/12 {
		egen 				w`x' = max(d`x'), by(hhid week)
	}

* total the food groups in a week	
	egen 				HDDS_ct_w = rowtotal(w1-w12)
	lab var 			HDDS_ct_w "HDDS: Week Count"
	
* (2) The average of the HDDS over days in the week.
* average the HDDS score over 7 days in a week
	gen 				HDDS_avg_w = HDDS_ct_w/7
	lab var 			HDDS_avg_w "HDDS: Week Average"	

* label variables
	lab var 			w1 "Week Total: cereals"
	lab var 			w2 "Week Total: white tubers and roots"
	lab var 			w3 "Week Total: vegetables"
	lab var 			w4 "Week Total: fruits"
	lab var 			w5 "Week Total: meat"
	lab var 			w6 "Week Total: eggs"
	lab var 			w7 "Week Total: fish and other seafood"
	lab var 			w8 "Week Total: legumes, nuts and seeds"
	lab var 			w9 "Week Total: milk and milk products"
	lab var 			w10 "Week Total: oils and fats"
	lab var 			w11 "Week Total: sweets"
	lab var 			w12 "Week Total: spices, condiments and beverages"	
	
* total HDDS: The HDDS for all six weeks calculated as

* (1) The total count of all groups represented in the six weeks.
* generate indicator variables for food categories overall with
	* max possible value = 1 to avoid double-counting
	sort 				hhid

	forval 				x = 1/12 {
							egen t`x' = max(d`x'), by(hhid)
	}

* total the food groups over 6 weeks 	
	egen 				HDDS_ct_t = rowtotal(t1-t12)
	lab var 			HDDS_ct_t "HDDS: Total Count"

* (2) The average of the HDDS over days in the six weeks.
* average the HDDS score over 42 days in 6 weeks
	gen 				HDDS_avg_t = HDDS_ct_t/42
	lab var 			HDDS_avg_t "HDDS: Total Average"		

* label variables
	lab var 			t1 "6 Week Total: cereals"
	lab var 			t2 "6 Week Total: white tubers and roots"
	lab var 			t3 "6 Week Total: vegetables"
	lab var 			t4 "6 Week Total: fruits"
	lab var 			t5 "6 Week Total: meat"
	lab var 			t6 "6 Week Total: eggs"
	lab var 			t7 "6 Week Total: fish and other seafood"
	lab var 			t8 "6 Week Total: legumes, nuts and seeds"
	lab var 			t9 "6 Week Total: milk and milk products"
	lab var 			t10 "6 Week Total: oils and fats"
	lab var 			t11 "6 Week Total: sweets"
	lab var 			t12 "6 Week Total: spices, condiments and beverages"
	
* save HDDS data
	save 			  	"$export/HDDS/dds.dta", replace	
	
	
	
	

***********************************************************************
* 3 - generate variables for final dsr outcomes
***********************************************************************	

* import species data sheet and save as .dta
	*import 			delimited using "$root/raw/dietary/DSR/species.csv", clear
	*save 				"$root/refined/HDDS/dds.dta", replace

* load data with matched ingredients
	use 				"$export/HDDS/dds.dta", clear
	
* fill in scientific names for species by matching on english words for 
	* ingredients, using a reference document (species.dta)
	
* matching for ingredient 1 
	* merge in data of species, match on english1
	rename 				english1 english
	merge 				m:1 english using "$root/DSR/species.dta"
	*** 21,485 observations matched, 12,114 observations didn't match
	
	drop if				_merge == 2
	*** 25 observations dropped

* rename variables and sort	
	rename 				scientificname scientificname1
	rename 				sppno sppno1
	sort 				_merge scientificname1	
	
* rename back to english1 and drop _merge
	rename 				english english1	
	drop 				_merge	
		
* matching for ingredient 2 
	* merge in data of species, match on english2
	rename 				english2 english
	merge m:1 			english using "$root/DSR/species.dta"
	*** 15,938 observations matched, 17,667 observations didn't match
	
	drop if	 			_merge == 2
	*** 31 observations dropped
		
* rename variables and sort	
	rename 				scientificname scientificname2
	rename 				sppno sppno2
	sort 				_merge scientificname2	
	sort english	
	
* rename back to english2 and drop _merge
	rename 				english english2
	drop 				_merge	
	
* Matching for ingredient 3**		
* merge in species.dta, match on english3
	rename 				english3 english
	merge m:1 			english using "$root/DSR/species.dta"
	*** 26,183 observations matched, 7,423 observations didn't match

	drop if 			_merge == 2
	*** 32 observations dropped

* rename variables and sort		
    rename 				scientificname scientificname3
	rename 				sppno sppno3
	sort 				_merge scientificname3		
	
* rename back to english3 and drop _merge	
	rename 				english english3
	drop 				_merge		
	
* Matching for ingredient 4		
	* merge in species.dta, match on english4
	rename 				english4 english
	merge m:1 			english using "$root/DSR/species.dta"
	*** 18,020 observations matched, 15,593 observations didn't match
	
	drop if 			_merge == 2
	*** 39 observations dropped

* rename variables and sort		
    rename 				scientificname scientificname4
	rename 				sppno sppno4
	sort				_merge scientificname4	
	
* rename back to english4 and drop _merge	
	rename 				english english4
	drop 				_merge	
	
* Matching for ingredient 5	
	* merge in species.dta, match on english5
	rename 				english5 english
	merge m:1 			english using "$root/DSR/species.dta"
	*** 25,616 observations matched, 8,003 observations didn't match
	
	drop if 			_merge == 2
	*** 45 observations dropped	

* rename variables and sort		
    rename 				scientificname scientificname5
	rename 				sppno sppno5
	sort				_merge scientificname5		
	
* rename back to english5 and drop _merge		
	rename 				english english5
	drop 				_merge
		
* Matching for ingredient 6
	* merge in species.dta, match on english
	rename 				english6 english
	merge m:1 			english using "$root/DSR/species.dta"
	*** 2,258 observations matched, 31,373 observations didn't match
	
	drop if 			_merge == 2
	*** 57 observations dropped	

* rename variables and sort		
    rename 				scientificname scientificname6
	rename 				sppno sppno6
	sort				_merge scientificname6	
		
	* rename back to english5 and drop _merge		
	rename 				english english6
	drop 				_merge		
	
* Matching for ingredient 7
	* merge in species.dta, match on english6
	rename 				english7 english
	merge m:1 			english using "$root/DSR/species.dta"
	*** 252 observations matched, 31,391 observations didn't match
	
	drop if 			_merge == 2
	*** 69 observations dropped

* rename variables and sort	
    rename 				scientificname scientificname7
	rename 				sppno sppno7
	sort 				_merge scientificname7
	
* rename back to english5 and drop _merge		
	rename 				english english7
	drop 				_merge		
	
* replace the following values with "na" for ingredients 1-7
	
	forval 				x = 1/7 {
		replace 			scientificname`x' = "na" if english`x' == "" | ///
								english`x' ==  "porridge" | english`x' ==  "water" | ///
								english`x' == "relish" | english`x' == "powder soup" | ///
								english`x' == "fishgroundnut" | ///
								english`x' == "watersugar" | ///
								english`x' == "cassavagroundnut"
		replace 			sppno`x' = 0 if english`x' == "" | ///
								english`x' == "porridge" | ///
								english`x' == "water" | english`x' == "relish" | ///
								english`x' == "powder soup" | ///
								english`x' == "fishgroundnut" | ///
								english`x' == "watersugar" | ///
								english`x' == "cassavagroundnut"				
		replace				scientificname`x'= "not identified z fct" if ///
								english`x' == "wild fruit"
		replace 			scientificname`x' = "not identified" if ///
								english`x' == "vegetable"  
		}
				
	
* loop to replace other missing values with 0
	forval 				i = 1 / 7 {
		replace 			sppno`i' = 0 if scientificname`i' == ""
		replace 			sppno`i' = 0 if scientificname`i' == "na"
	}
		

* generate DSR variables
		
* (i) The SR for a given dish, calculated as a count of the number of species 
	* used as ingredients in the dish.
	
* generate ingredient indicators = 0, label variables
	sort 				hhid week day meal dish_num
		
	forval 				x = 1/7 {
		gen 				dsr_d`x' = 0	
		lab var 			dsr_d`x' "Species Count: Ingredient `x'"
	}
	
* replace ingredients = 1 if there is a 1 for a species value
	forval 				x = 1/7 {
		replace 			dsr_d`x' = 1 if sppno`x' > 0
	}
	
* total the ingredients for the count of species per dish
	egen 				DSR_dish = rowtotal(dsr_d1-dsr_d7)
	lab var 			DSR_dish "DSR: Dish"

* (ii) The SR for a given meal, calculated as a count of the number of 
	* species used as ingredients in the meal.
	
* aggregate dish score to meal level by adding dsr_dish for each meal	
	sort 				hhid week day meal

	forval 				x = 1/7 {
		egen 				dsr_m`x' = sum(dsr_d`x'), by(hhid week day meal)
		lab var 			dsr_m`x' "Species Count: Ingredient `x' by Meal"
	}

	egen 				DSR_meal = rowtotal(dsr_m1-dsr_m7)
	lab var 			DSR_meal "DSR: Meal"

* (iii) The SR for a given day, calculated as a count of the number of 
	* species used as ingredients in all meals that day.
	
* generate a day sum by adding dsr_meal for each day

	sort 				hhid week day

	forval 				x = 1/7 {
		egen 				dsr_da`x' = sum(dsr_d`x'), by(hhid week day)
		lab var 			dsr_da`x' "Species Count: Ingredient `x' by Day"
	}

	egen 				DSR_day = rowtotal(dsr_da1-dsr_da7)
	lab var 			DSR_day "DSR: Day"

* (iv) The SR for a given week, calculated as a count of the number of 
	* species used as ingredients in all meals that week.

* generate a week sum by adding dsr_meal for each week

	sort 				hhid week

	forval 				x = 1/7 {
		egen 				dsr_w`x' = sum(dsr_d`x'), by(hhid week)
		lab var 			dsr_w`x' "Species Count: Ingredient `x' by Week"
	}

	egen 				DSR_week = rowtotal(dsr_w1-dsr_w7)
	lab var 			DSR_week "DSR: Week"

* (v) The SR for the six weeks, calculated as a count of the number of 
	* species used as ingredients in all meals over the six weeks.

* generate a total sum by adding dsr_meal for all 6 weeks

	sort 				hhid

	forval 				x = 1/7 {
		egen 				dsr_t`x' = sum(dsr_d`x'), by(hhid)
		lab var 			dsr_t`x' "Species Count: Ingredient `x' Overall (6 Weeks)"
	}

	egen 				DSR_total = rowtotal(dsr_t1-dsr_t7)
	lab var 			DSR_total "DSR: Total"

* save DSR data	
	save 				"$export/DSR/dsr.dta", replace		
		
***********************************************************************
* 4 - generate variables for final outcomes: meals skipped
***********************************************************************	

* generate indicator variable for the first meal observation in panel	
	gen byte 			first_obs = 0
	replace 			first_obs = 1 if dish_num == 1
	label var 			first_obs "First Observation in Meal"

* check	
* add up total meals per day grouped by hhid, week, day
	sort 				hhid week day
	by hhid week day: 	egen hhmeals_day = total(first_obs) 
	label var			hhmeals_day "Household Meals Per Day"		
	
* check
* add up total meals per week grouped by hhid and week	
	sort 				hhid week
	by hhid week: 		egen hhmeals_week = total(first_obs) 
	label var			hhmeals_week "Household Meals Per Week"	
					
* add up total hh breakfast meals 
	sort 				hhid
	by hhid: 			egen hhbr_tot = total(first_obs) if meal == 0
	replace				hhbr_tot = 0 if meal != 0
	label var			hhbr_tot "Total Household Breakfast Meals"

* add up total hh lunch meals 
	sort 				hhid
	by hhid: 			egen hhlun_tot = total(first_obs) if meal == 1
	replace				hhlun_tot = 0 if meal != 1
	label var 			hhlun_tot "Total Household Lunch Meals"
	
* add up total hh dinner meals 
	sort 				hhid
	by hhid: 			egen hhdin_tot = total(first_obs) if meal == 2
	replace				hhdin_tot = 0 if meal != 2	
	label var			hhdin_tot "Total Household Dinner Meals"
	
* add up total hh meals 
	sort 				hhid
	by hhid: 			egen hhmeals_total = total(first_obs) 
	label var 			hhmeals_total "Total Household Meals"	
	
* generate variable that stores max value of potential meals over the RCT period	
	* calculate number of possible meals @ 3 meals a day, 7 days a week, for 6 weeks
	display  			3*7*6
	*** answer = 126
	
	gen 				mealct_tot = 126	
	label var 			mealct_tot "Number of Possible Meals"
	
	* calculate max possible breakfast/lunch/dinner meals over the RCT period
		* divide total possible meals by number of meal types (3)
	display  			126/3
	*** br, lunch, dinner each have 42 max meals over the time period

	gen 				mealct = 42
	label var			mealct "Number of Possible Breakfast/Lunch/Dinner Meals"
	
* generate variables for the difference between max potential meals and total hh meals	

* (i) Total number of meals skipped over all six weeks.	
	gen 				hhtot_skipped = mealct_tot - hhmeals_total
	label var			hhtot_skipped "Total Number of Meals Skipped per Household"		
	
	replace 			hhtot_skipped = 0 if hhtot_skipped < 0
	

* (ii) Total number of breakfast meals skipped over all six weeks.		
* generate variable that calculates the difference in max breakfast meals (42) and how
* many meals a hh consumed
	gen 				hhbr_skipped_temp = mealct - hhbr_tot
	
* replace this var with missing if not a breakfast meal
	replace				hhbr_skipped_temp = . if meal != 0
* generate a variable for hh skipped that takes on the max value for hh 
* breakfast meals skipped. this means that every observation for a hh will 
* have the # br meals skipped
	by hhid:  			egen hhbr_skipped = max(hhbr_skipped_temp) 
	drop 				hhbr_skipped_temp
	replace 			hhbr_skipped = 0 if hhbr_skipped < 0
	label var 			hhbr_skipped "Number of Breakfasts Skipped"

* (iii) Total number of lunch meals skipped over all six weeks.			
	gen 				hhlun_skipped_temp = mealct - hhlun_tot
	replace				hhlun_skipped_temp = 0 if meal != 1	
	by hhid:  			egen hhlun_skipped = max(hhlun_skipped_temp) 	
	drop 				hhlun_skipped_temp	
	label var			hhlun_skipped "Number of Breakfasts Skipped"

* (iv) Total number of dinner meals skipped over all six weeks.		
	gen 				hhdin_skipped_temp = mealct - hhdin_tot
	replace				hhdin_skipped_temp = 0 if meal != 2	
	by hhid:  			egen hhdin_skipped = max(hhdin_skipped_temp) 
	drop 				hhdin_skipped_temp		
	label var			hhdin_skipped "Number of Breakfasts Skipped"
	

***********************************************************************
* 5 - generate variables for final outcomes: avg number of dishes per meal
***********************************************************************	
	
** generate avg number of dishes in each meal type over 6 weeks	
* add up total number of breakfast dishes 
	egen				hhbrdish_tot_temp = count(dish) if meal == 0, by(hhid) 
	replace				hhbrdish_tot_temp = 0 if meal != 0
	by hhid:  			egen hhbrdish_tot = max(hhbrdish_tot_temp) 
	drop 				hhbrdish_tot_temp		
	label var			hhbrdish_tot "Total Number of Breakfast Dishes by HH"
	
* add up total number of lunch dishes
	egen				hhlundish_tot_temp = count(dish) if meal == 1, by(hhid) 
	replace				hhlundish_tot_temp = 0 if meal != 1	
	by hhid:  			egen hhlundish_tot = max(hhlundish_tot_temp) 
	drop 				hhlundish_tot_temp			
	label var			hhlundish_tot "Total Number of Lunch Dishes by HH"	
	
* add up total number of dinner dishes 
	egen				hhdindish_tot_temp = count(dish) if meal == 2, by(hhid) 
	replace				hhdindish_tot_temp = 0 if meal != 2
	by hhid:  			egen hhdindish_tot = max(hhdindish_tot_temp) 
	drop 				hhdindish_tot_temp		
	label var			hhdindish_tot "Total Number of Dinner Dishes by HH"	
	
* add up total number of dishes
	egen				hhdish_tot = count(dish), by(hhid) 	
	label var			hhdish_tot "Total Number of Dishes by HH"	

* (i) Average number of dishes in a meal over the six weeks. (Add up the 
	* number of dishes in each meal and divide by the total number of meals 
	* the household ate in the six weeks).	
	
* generate average by dividing total dishes/meal by total dishes
	gen					avg_dish = hhdish_tot/hhmeals_total
	label var			avg_dish "Average Number of Dishes for All Meals"	
	
* (ii) Average number of dishes in breakfast over the six weeks. (Add up 
	* the number of dishes in each breakfast and divide by the total 
	* number of breakfast meals the household ate in the six weeks).

* generate average by dividing total dishes/breakfast by total dishes
	gen					avg_brdish_temp = hhbrdish_tot/hhbr_tot
	by hhid:  			egen avg_brdish = max(avg_brdish_temp) 	
	drop 				avg_brdish_temp	
	label var			avg_brdish "Average Number of Breakfast Dishes"
	
* (iii) Average number of dishes in lunch over the six weeks. (Add up the 
	* number of dishes in each lunch and divide by the total number of 
	* lunch meals the household ate in the six weeks).

* generate average by dividing total dishes/lunch by total dishes
	gen					avg_lundish_temp = hhlundish_tot/hhlun_tot
	by hhid:  			egen avg_lundish = max(avg_lundish_temp) 	
	drop 				avg_lundish_temp	
	label var			avg_lundish "Average Number of Lunch Dishes"
	
* (iv) Average number of dishes in dinner over the six weeks. (Add up 
	* the number of dishes in each dinner and divide by the total number 
	* of dinner meals the household ate in the six weeks).

* generate average by dividing total dishes/dinner by total dishes
	gen					avg_dindish_temp = hhdindish_tot/hhdin_tot
	by hhid:  			egen avg_dindish = max(avg_dindish_temp) 	
	drop 				avg_dindish_temp
	label var			avg_dindish "Average Number of Dinner Dishes"
	
* load cleaned dietary data
	save 				"$export/DSR/dietary.dta", replace	
	

* ***********************************************************************
* 1b - merge in control variables and clean up
* ***********************************************************************

* load cleaned dietary data
	use 			"$export/DSR/dietary.dta", clear

* rename treatment and assignment variables
	*rename 			solar treat_assign
	*rename 			ss_count ss_use

* merge in control variables
	merge m:1 		hhid using "$export/c_var.dta", force
	*** 30,659 observations matched, 2,915 observations not matched

	drop if 		_merge == 1
	*** 2915 observations deleted 
	
* drop merge variable
	drop 			_merge

		
***********************************************************************
* 6 - generate legume variables
***********************************************************************		

* generate indicator variable for legumes in dish
	gen 			legume_ind = 1 if legumes != "0" & legumes != "" 
	*** 1,035 observations
	
	lab var			legume_ind "Legume Dish Cooked"

* generate a variable for the number of times legumes were cooked in a given day
	egen			legume_ct_day = total(legume_ind), by(hhid week day)
	lab var			legume_ct_day "Number of Times HH Cooked Legumes: Day"

* generate a variable for the number of times legumes were cooked in a given week
	egen			legume_ct_week = total(legume_ind), by(hhid week)	
	lab var			legume_ct_week "Number of Times HH Cooked Legumes: Week"	

* generate a variable for the number of times legumes were cooked over 6 weeks
	egen			legume_ct_tot = total(legume_ind), by(hhid)		
	lab var			legume_ct_tot "Number of Times HH Cooked Legumes: Total"
	
				
***********************************************************************
* 7 - end matter, clean up to save
***********************************************************************
	
* prepare for export
	lab var				food_group "Food Group"
*	compress
*	describe
*	summarize      	
	
	save 				"$export/dietary_cleaned.dta", replace	
	
* close the log
	log					close
	
*** END
