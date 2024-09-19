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
	* xfill

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
**# 1 - clean and transform data
***********************************************************************		

* merge in control variables
	merge m:1		hhid using "$export/c_var.dta"
	*** 30,602 merged, 341 not matched
	*** all from households 340000 and 347000
	
	drop if			_merge == 1
	drop			_merge

* household kept diary for 9 weeks
	drop if			week > 6

* only recorded breakfast
	drop if			hhid == 226000
	
	
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
	
* generate ingredient count per dish
	egen			ingred_dish = nvals(english), by(hhid week day meal dish)
	lab var			ingred_dish "Unique Ingredients: Dish"
	
	egen			ingred_meal = nvals(english), by(hhid week day meal)
	lab var			ingred_meal "Unique Ingredients: Meal"
	
	egen			ingred_day = nvals(english), by(hhid week day)
	lab var			ingred_day "Unique Ingredients: Day"
	
	egen			ingred_week = nvals(english), by(hhid week)
	lab var			ingred_week "Unique Ingredients: Week"
	
	
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

	collapse		(max) legumes, ///
					by(village hhid aas hh_size ai tli sex age edu treat_assign ///
						week day meal dish ss_use cook hdds_dish sr_dish hdds_avg_dish sr_avg_dish hdds_meal ///
						sr_meal hdds_avg_meal sr_avg_meal hdds_day sr_day ///
						hdds_avg_day sr_avg_day hdds_week sr_week hdds_avg_week ///
						sr_avg_week hdds_total sr_total ingred_dish ingred_meal ///
						ingred_day ingred_week)
	
	lab var			legumes "Where Legumes Eaten?"
	
	isid			hhid week day meal dish
	
* generate legumes (pulses) for dish
	rename 			legumes p_dish
	lab var			p_dish "Pulse: Dish"
	
* generate legumes (pulses) for meal
	egen 			p_meal = total(p_dish), by(hhid week day meal)
	lab var			p_meal "Pulse: Meal"
	
* generate legumes (pulses) for day
	egen 			p_day = total(p_dish), by(hhid week day)
	lab var			p_day "Pulse: Day
	
* generate legumes (pulses) for weak
	egen 			p_week = total(p_dish), by(hhid week)
	lab var			p_week "Pulse: Week"
	
* generate legumes (pulses) for total
	egen 			p_total = total(p_dish), by(hhid)
	lab var			p_total "Pulse: Total"
	
	
***********************************************************************
**# 4 - generate variables for meals skipped
***********************************************************************	

* generate indicator variable for the first meal observation in panel	
	gen byte 			first_obs = 0
	replace 			first_obs = 1 if dish == 1
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
	xfill				hhbr_tot, i(hhid)
	label var			hhbr_tot "Total Household Breakfast Meals"

* add up total hh lunch meals 
	sort 				hhid
	by hhid: 			egen hhlun_tot = total(first_obs) if meal == 1
	xfill				hhlun_tot, i(hhid)
	label var 			hhlun_tot "Total Household Lunch Meals"
	
* add up total hh dinner meals 
	sort 				hhid
	by hhid: 			egen hhdin_tot = total(first_obs) if meal == 2
	xfill				hhdin_tot, i(hhid)
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
	gen 				hhbr_skipped = mealct - hhbr_tot
	label var 			hhbr_skipped "Number of Breakfasts Skipped"

* (iii) Total number of lunch meals skipped over all six weeks.			
	gen 				hhlun_skipped = mealct - hhlun_tot
	label var			hhlun_skipped "Number of Breakfasts Skipped"

* (iv) Total number of dinner meals skipped over all six weeks.		
	gen 				hhdin_skipped = mealct - hhdin_tot
	label var			hhdin_skipped "Number of Breakfasts Skipped"
	

***********************************************************************
**# 5 - generate variables for avg number of dishes per meal
***********************************************************************	
	
** generate avg number of dishes in each meal type over 6 weeks	
* add up total number of breakfast dishes 
	gen					hhbrdish_tot_temp = 1 if meal == 0
	egen				hhbrdish_tot = count(hhbrdish_tot_temp), by(hhid)
	replace				hhbrdish_tot = hhbrdish_tot/hhbr_tot
	drop 				hhbrdish_tot_temp		
	label var			hhbrdish_tot "Total Number of Breakfast Dishes by HH"
	
* add up total number of lunch dishes
	gen					hhlundish_tot_temp = 1 if meal == 1
	egen				hhlundish_tot = count(hhlundish_tot_temp), by(hhid)
	replace				hhlundish_tot = hhlundish_tot/hhlun_tot
	drop 				hhlundish_tot_temp			
	label var			hhlundish_tot "Total Number of Lunch Dishes by HH"	
	
* add up total number of dinner dishes 
	gen					hhdindish_tot_temp = 1 if meal == 2
	egen				hhdindish_tot = count(hhdindish_tot_temp), by(hhid)
	replace				hhdindish_tot = hhdindish_tot/hhdin_tot
	drop 				hhdindish_tot_temp		
	label var			hhdindish_tot "Total Number of Dinner Dishes by HH"	
	
* add up total number of dishes
	bys hhid: gen		hhdish_tot = _N
	label var			hhdish_tot "Total Number of Dishes by HH"	


***********************************************************************
**# 6 - generate meal_share variable
***********************************************************************		
	
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
**# 7 - add cloud cover data
***********************************************************************

* add cloud cover data
	gen 			cc = 100 if week == 1 & village == 1
	replace 		cc = 11.75 if week == 2 & village == 1
	replace 		cc= 99.99 if week == 3 & village == 1
	replace 		cc = 0.00 if week == 4 & village == 1
	replace 		cc = 0.00 if week == 5 & village == 1
	replace 		cc = 0.00 if week == 6 & village == 1
	replace 		cc = 100 if week == 1 & village == 0
	replace 		cc = 1.7 if week == 2 & village == 0
	replace 		cc = 2.06 if week == 3 & village == 0
	replace 		cc = 0 if week == 4 & village == 0
	replace 		cc = 0 if week == 5 & village == 0
	replace 		cc = 0.01 if week == 6 & village == 0
	replace 		cc = 100 if week == 1 & village == 2
	replace 		cc = 35.84 if week == 2 & village == 2
	replace 		cc = 0 if week == 3 & village == 2
	replace 		cc = 0 if week == 4 & village == 2
	replace 		cc = 0 if week == 5 & village == 2
	replace 		cc = 0 if week == 6 & village == 2
		
	lab var 		cc "Cloud Cover"	

	order			cc, after(aas)
	
***********************************************************************
**# 8 - end matter, clean up to save
***********************************************************************
	
* prepare for export
	compress	
	save 				"$export/dietary_cleaned.dta", replace	
	
* close the log
	log					close
	
*** END
