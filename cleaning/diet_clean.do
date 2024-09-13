* project: solar stoves - final outcomes: composition of diet
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 13 Sep 2024
* stata v.18.5

* does
	* inputs cleaned ingredient data
	* generates HDDS and SR indices
	* generates variables for number of meals, number of meals skipped, and
		* average number of dishes per meal type
	* outputs regression ready data

* assumes
	* access to cleaned ingredient dictionary
	* egenmore

* to do:
	* done


***********************************************************************
**# 0 - setup
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
**# 1 - generate variables for intermediate dietary outcomes (2a in PAP)
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
	
* reshape data
	reshape 		long english fg proc, ///
						i(hhid week day meal dish_num) j(ingred_num)

* rearrange and label variables
	lab var			english "English Name of Ingredient"
	lab var			fg "Food Group"
	lab var			proc "Level of Processing"
	
	drop			legumes
	gen				legumes = 1 if fg == 12
	replace			legumes = 0 if legumes == .
	lab var			legumes "Were Legumes Cooked?"
	
* drop non-used ingredients
	drop			dish
	rename			dish_num dish
	drop if			english == ""
	drop if			english == "water"
	
	drop			ingred_num
	bys 			hhid week day meal dish: ///
					gen	ingred_num = _n
	lab var			ingred_num "Ingredient Number in Dish"
	
	order 			village hhid aas hh_size ai tli sex age edu treat_assign ///
						week day meal dish ss_use cook ingred_num

* clean a last few ingredients
	replace			english = "maize" if english == "maize yellow"		
	replace			english = "oil" if english == "cooking oil"			
	replace			english = "oil" if english == "peanut oil"			
	replace			english = "sugar" if english == "candy"				
	replace			english = "chili" if english == "green pepper"		
	replace			english = "spinach" if english == "aquatic plant"			
						
	merge			m:1 english using "$root/sci_names.dta"
	*** 94,269 merged, only 1 in master didn't merge
	
	tab				english if _merge == 1
	
	replace			scientific_name = "morus" if english == "mauberry fruits"
	lab var			scientific_name "Scientific Name"
	
	drop if			_merge == 2
	drop			_merge
	
	encode			scientific_name, gen(sci)
	replace			sci = 0 if sci == .
	lab var			sci "Scientific Name"
	drop			scientific_name

	
***********************************************************************
**# 2 - generate variables for final hdds outcomes
***********************************************************************		
		
* generate hdds and sr for dish
	egen 			hdds_dish = nvals(fg), by(hhid week day meal dish)
	lab var			hdds_dish "HDDS: Dish"
	egen 			sr_dish   = nvals(sci), by(hhid week day meal dish)
	lab var			sr_dish "SR: Dish"
	
	egen			hdds_avg_dish = mean(hdds_dish), by(hhid)
	lab var			hdds_avg_dish "HDDS: Average Dish"
	egen			sr_avg_dish = mean(sr_dish), by(hhid)
	lab var			sr_avg_dish "SR: Average Dish"
	
* generate hdds and sr for meal
	egen 			hdds_meal = nvals(fg), by(hhid week day meal)
	lab var			hdds_meal "HDDS: Meal"
	egen 			sr_meal   = nvals(sci), by(hhid week day meal)
	lab var			sr_meal "SR: Meal"
	
	egen			hdds_avg_meal = mean(hdds_meal), by(hhid)
	lab var			hdds_avg_meal "HDDS: Average Meal"
	egen			sr_avg_meal = mean(sr_meal), by(hhid)
	lab var			sr_avg_meal "SR: Average Meal"
	
* generate hdds and sr for day
	egen 			hdds_day = nvals(fg), by(hhid week day)
	lab var			hdds_day "HDDS: Day"
	egen 			sr_day   = nvals(sci), by(hhid week day)
	lab var			sr_day "SR: Day"
	
	egen			hdds_avg_day = mean(hdds_day), by(hhid)
	lab var			hdds_avg_day "HDDS: Average Day"
	egen			sr_avg_day = mean(sr_day), by(hhid)
	lab var			sr_avg_day "SR: Average Day"
	
* generate hdds and sr for week
	egen 			hdds_week = nvals(fg), by(hhid week)
	lab var			hdds_week "HDDS: Week"
	egen 			sr_week   = nvals(sci), by(hhid week)
	lab var			sr_week "SR: Week"
	
	egen			hdds_avg_week = mean(hdds_week), by(hhid)
	lab var			hdds_avg_week "HDDS: Average Week"
	egen			sr_avg_week = mean(sr_week), by(hhid)
	lab var			sr_avg_week "SR: Average Week"

* generate hdds and sr for total
	egen 			hdds_total = nvals(fg), by(hhid)
	lab var			hdds_total "HDDS: Total"
	egen 			sr_total   = nvals(sci), by(hhid)	
	lab var			sr_total "SR: Total"

	
***********************************************************************
**# 3 - generate legume variables and collapse
***********************************************************************		

	collapse		(sum) legumes, ///
					by(village hhid aas hh_size ai tli sex age edu treat_assign ///
						week day meal dish ss_use cook hdds_dish sr_dish hdds_avg_dish sr_avg_dish hdds_meal ///
						sr_meal hdds_avg_meal sr_avg_meal hdds_day sr_day ///
						hdds_avg_day sr_avg_day hdds_week sr_week hdds_avg_week ///
						sr_avg_week hdds_total sr_total)
	
	lab var			legumes "Where Legumes Eaten?"
	
	fddfd
	
***********************************************************************
**# 4 - generate variables for final outcomes: meals skipped
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
