* project: solar stoves - final outcomes: composition of diet
* created on: Sep 2024
* created by: jdm
* edited by: jdm
* edited on: 24 Sep 2024
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
	global	output	=	"$data/analysis/tables"
	global	figure	=	"$data/analysis/figures"
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
	
	bys 			hhid week day meal dish_num: ///
						gen dish = 1 if _n == 1
	order 			dish, after(dish_num)
	bys 			hhid week day meal: ///
						replace dish = sum(dish)
	lab var			dish "Dish"
	
	drop			dish_num
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
	
* clean food group to put it into 12 categories (not 18)
	replace			fg = 14 if fg == 0
	replace			fg = 5 if fg == 3 | fg == 4
	replace			fg = 7 if fg == 6
	replace			fg = 9 if fg == 8
	replace			fg = 12 if fg == 19
	replace			fg = 15 if fg == 17
	replace			fg = 16 if fg == 18
	lab define 		foodgroup 0 "None" 1 "Cereals" 2 "Tubers" 3 "Vitamin-A Vegetables" ///
					4 "Leafy Greens" 5 "Vegetables" 6 "Vitamin-A Fruits" 7 "Fruits" ///
					8 "Organ Meat" 9 "Meat" 10 "Eggs" 11 "Fish" 12 "Legumes, Nuts, & Seeds" ///
					13 "Milk" 14 "Oils & Fats" 15 "Sweets" ///
					16 "Spices, Condiments, & Beverages" 17 "Fried Snacks" ///
					18 "Beverages", replace
	

					
***********************************************************************
**## 1.1 - create ingredient summary stats
***********************************************************************

* encode english string var into a numeric variable
	encode			english, gen(ingredients)
	bys 			ingredients: gen size = _N
	replace			ingredients = 0 if size < 566
	lab define 		ingredients 0 "other", add
	
	gen				science = sci
	replace			science = . if science == 0
	lab values 		science sci
	bys 			science: gen sizes = _N
	replace			science = 0 if sizes < 139
	lab define 		sci 0 "other", add
	
* post frequency table of ingredients
		estpost tab		ingredients, sort nototal
	 
* code for latex tables
	esttab			using "$output/descriptive/ing_tab.tex", replace booktabs ///
							prehead("\begin{tabular}{l*{2}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
							cells("b(label(Frequency) fmt(%9.0gc)) pct(label(Percent) fmt(2))") ///
							nonumber nomtitle noobs fragment ///
							postfoot("\midrule Total       &       93,606&      100 \\ " ///
							"\hline \hline \\[-1.8ex] \multicolumn{3}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays the number of times " ///
							"the top 25 ingredient was recorded in the food diaries and the relative " ///
							"frequency of that ingredient in the entire data set. In total " ///
							"111 different ingredients were recorded, excluding water.}  \end{tabular}")	

* post frequency table of food groups
		estpost tab			fg, sort nototal
	
* output table of food group frequencies
		esttab 			 using "$output/descriptive/fg_tab.tex", replace booktabs ///
							prehead("\begin{tabular}{l*{2}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
							cells("b(label(Frequency) fmt(%9.0gc)) pct(label(Percent) fmt(2))") ///
							nonumber nomtitle noobs fragment varlabels(`e(labels)') ///
							postfoot("\midrule Total       &       93,606&      100 \\ " ///
							"\hline \hline \\[-1.8ex] \multicolumn{3}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays the number of times " ///
							"a food group is represented in the food diaries. There are 12 " ///
							"total food groups.}  \end{tabular}") 		
	
* post frequency table of species
		estpost tab			science, sort nototal
	
* output table of processed food frequencies
		esttab 			 using "$output/descriptive/sci_tab.tex", replace booktabs ///
							prehead("\begin{tabular}{l*{2}{c}} \\ [-1.8ex]\hline \hline \\[-1.8ex] ") ///
							cells("b(label(Frequency) fmt(%9.0gc)) pct(label(Percent) fmt(2))") ///
							nonumber nomtitle noobs fragment ///
							postfoot("\midrule Total       &       81330&      100 \\ " ///
							"\hline \hline \\[-1.8ex] \multicolumn{3}{J{\linewidth}}{\small " ///
							"\noindent \textit{Note}: The table displays the number of times " ///
							"a species is represented in the food diaries. There are 63 " ///
							"unique species total.}  \end{tabular}") 				
	
	
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
						week day meal dish ss_use cook hdds_dish sr_dish ///
						hdds_avg_dish sr_avg_dish hdds_meal ///
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

* generate variable for total meals eaten
	gen					day_temp = 1 if dish == 1

	egen				meal_day = count(day_temp), by(hhid week day)
	label var			meal_day "Meals Per Day"		
	
	egen				meal_week = count(day_temp), by(hhid week)
	label var			meal_week "Meals Per Week"	

	egen				meal_total = count(day_temp), by(hhid)
	label var 			meal_total "Total Meals"	

* generate variable for breakfast meals eaten	
	gen					br_day_temp = 1 if meal == 0 & dish == 1

	egen				br_day = count(br_day_temp), by(hhid week day)
	label var			br_day "Breakfasts Per Day"	
	
	egen				br_week = count(br_day_temp), by(hhid week)
	label var			br_week "Breakfasts Per Week"	

	egen				br_tot = count(br_day_temp), by(hhid)
	label var			br_tot "Total Breakfasts"

* generate variable for lunch meals eaten	
	gen					lun_day_temp = 1 if meal == 1 & dish == 1

	egen				lun_day = count(lun_day_temp), by(hhid week day)
	label var			lun_day "Lunches Per Day"	
	
	egen				lun_week = count(lun_day_temp), by(hhid week)
	label var			lun_week "Lunches Per Week"	

	egen				lun_tot = count(lun_day_temp), by(hhid)
	label var 			lun_tot "Total Lunchs"

* generate variable for dinner meals eaten	
	gen					din_day_temp = 1 if meal == 2 & dish == 1

	egen				din_day = count(din_day_temp), by(hhid week day)
	label var			din_day "Dinners Per Day"	
	
	egen				din_week = count(din_day_temp), by(hhid week)
	label var			din_week "Dinners Per Week"	
	 
	egen				din_tot = count(din_day_temp), by(hhid)
	label var			din_tot "Total Dinners"
	
* generate variables for the difference between max potential meals and total hh meals		
	gen 				day_skip = 3 - meal_day
	label var			day_skip "Number of Meals Skipped in the Day"	
	
	gen 				week_skip = 21 - meal_week
	label var			week_skip "Number of Meals Skipped in the Week"	

	gen 				tot_skip = 126 - meal_total
	label var			tot_skip "Total Number of Meals Skipped"		

* generate variable that calculates the difference in max breakfast meals and meals a hh consumed	
	gen 				brday_skip = 1 - br_day
	label var			brday_skip "Was Breakfast Skipped that Day?"	

	gen 				brweek_skip = 7 - br_week
	label var 			brweek_skip "Number of Breakfasts Skipped in the Week"
	
	gen 				brtot_skip = 42 - br_tot
	label var 			brtot_skip "Total Number of Breakfasts Skipped"

* generate variable that calculates the difference in max lunch meals and meals a hh consumed
	gen 				lunday_skip = 1 - lun_day
	label var			lunday_skip "Was Lunch Skipped that Day?"	

	gen 				lunweek_skip = 7 - lun_week
	label var 			lunweek_skip "Number of Lunches Skipped in the Week"
		
	gen 				luntot_skip = 42 - lun_tot
	label var			luntot_skip "Total Number of Lunches Skipped"

* generate variable that calculates the difference in max dinner meals and meals a hh consumed
	gen 				dinday_skip = 1 - din_day
	label var			dinday_skip "Was Dinner Skipped that Day?"	

	gen 				dinweek_skip = 7 - din_week
	label var 			dinweek_skip "Number of Dinner Skipped in the Week"
	
	gen 				dintot_skip = 42 - din_tot
	label var			dintot_skip "Total Number of Dinners Skipped"

* drop unneeded variables
	drop				day_temp br_day_temp lun_day_temp din_day_temp

	
***********************************************************************
**# 5 - generate variables for avg number of dishes per meal
***********************************************************************	

* add up total number of dishes
	gen					dish_tot_temp = 1
	
	egen				dish_day = count(dish_tot_temp), by(hhid week day)	
	label var			dish_day "Number of Dishes in a Day"
	
	egen				dish_week = count(dish_tot_temp), by(hhid week)	
	label var			dish_week "Number of Dishes in a Week"
	
	egen				dish_tot = count(dish_tot_temp), by(hhid)		
	label var			dish_tot "Total Number of Dishes"
	
* add up total number of breakfast dishes 
	gen					brdish_tot_temp = 1 if meal == 0
	
	egen				brdish_day = count(brdish_tot_temp), by(hhid week day)	
	label var			brdish_day "Number of Breakfast Dishes in a Day"
	
	egen				brdish_week = count(brdish_tot_temp), by(hhid week)	
	label var			brdish_week "Number of Breakfast Dishes in a Week"
	
	egen				brdish_tot = count(brdish_tot_temp), by(hhid)		
	label var			brdish_tot "Total Number of Breakfast Dishes"
	
* add up total number of lunch dishes
	gen					lundish_tot_temp = 1 if meal == 1
	
	egen				lundish_day = count(lundish_tot_temp), by(hhid week day)	
	label var			lundish_day "Number of Lunch Dishes in a Day"
	
	egen				lundish_week = count(lundish_tot_temp), by(hhid week)	
	label var			lundish_week "Number of Lunch Dishes in a Week"
	
	egen				lundish_tot = count(lundish_tot_temp), by(hhid)		
	label var			lundish_tot "Total Number of Lunch Dishes"
	
* add up total number of dinner dishes 
	gen					dindish_tot_temp = 1 if meal == 2
	
	egen				dindish_day = count(dindish_tot_temp), by(hhid week day)	
	label var			dindish_day "Number of Dinner Dishes in a Day"
	
	egen				dindish_week = count(dindish_tot_temp), by(hhid week)	
	label var			dindish_week "Number of Dinner Dishes in a Week"
	
	egen				dindish_tot = count(dindish_tot_temp), by(hhid)		
	label var			dindish_tot "Total Number of Dinner Dishes"

* drop unneeded variables
	drop				dish_tot_temp brdish_tot_temp lundish_tot_temp dindish_tot_temp


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

	lab var				treat_assign "Solar Stove"

* prepare for export
	compress	
	save 				"$export/dietary_cleaned.dta", replace	
	
* close the log
	log					close
	
*** END
