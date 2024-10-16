#libraries====
rm(list=ls())#clean 
library(here)
library(readxl)
library(purrr)
library(dplyr)
library(stringr)
library(janitor)
library(tidyr)
library(fuzzyjoin) 
library(stringi)
library(fastDummies)
library(broom)
library(car)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(sandwich)
library(lfe)
library(rlang)
library(stargazer)
library(DescTools)#winsorize
library(kableExtra) #latex
library(flextable)
library(officer)


#call functions needed
source("Functions.R")

#read file 
all_villages_ing_dish_meals_l_fg<-read.csv(here("anonymized_source_files","all_villages_ing_dish_meals_l_fg.csv"))%>%
  mutate(hdds=if_else(hdds==""|hdds=="na", NA, hdds),
         #hdds= 1.cereals, 2.eggs,	3.fish and other seafood,	4.fruits, 5.hdds	legumes, nuts and seeds, 6.	meat, 7.milk and milk products, 8.oils and fats	spices, 9.condiments and beverages, 10.sweets, 11.vegetables, 12.white tubers and roots
 
         wdds=if_else(wdds==""|wdds=="na", NA, wdds))
          #wdds= 1.dark green leafy vegetables, 2.eggs, 3.legumes, nuts and seeds, 4.meat, insects and fish, 5.milk and milk products, 6.organ meat, 7.other fruits and vegetables, 8.other vitamin a rich fruits and vegetables, 9.starchy staples

       #  zfbdrfg=if_else(zfbdrfg==""|zfbdrfg=="na", NA, zfbdrfg))
          #zfbdrfg= 1.Cereals, starchy roots and tubers, 2. vegetables, 3. frits, 4. fish, insects, and animal source foods, 5. dairy, 6. legumes, pulses and nuts
main_hh_covariates<-read.csv(here("anonymized_source_files", "main_hh_covariates.csv"))


# legumes frequency ====
#getting the total days with recorded information to calculate means
record_days<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week)%>%
  summarise(day_T=max(day))%>%
  group_by(cod)%>%
  summarise(total_days= sum(day_T), total_weeks=n())


legumes<-all_villages_ing_dish_meals_l_fg%>%
  
  filter(Food.group=="Pulses (beans, peas and lentils)")%>%
  left_join(record_days, by="cod")

p_dish<-legumes%>%
  group_by(cod, week, day, meal, dish)%>% #two dishes in the same meal can have legumes and have been cooked with different methods. 
  dplyr::summarise(pulses_dish=n() )%>%
  group_by(cod)%>%
  dplyr::summarise(pulses_dish_sum=sum(pulses_dish))%>%
  left_join(record_days, by="cod")%>%
  mutate(pulses_dish_avg=pulses_dish_sum/total_days)%>%
  select(cod, pulses_dish_avg )

p_meal<-legumes%>%
  group_by(cod, week, day, meal)%>%
  dplyr::summarise(pulses_meal=n())%>%
  group_by(cod, meal)%>%
  dplyr::summarise(pulses_meal_sum=sum(pulses_meal))%>%
  left_join(record_days, by="cod")%>%
  mutate(pulses_meal_avg=pulses_meal_sum/total_days)%>%
  dplyr::select(-c(pulses_meal_sum, total_days, total_weeks))%>%
  pivot_wider(names_from = meal,
              names_prefix="pul_",
              values_from = c(pulses_meal_avg))%>%
  
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))

  p_day<-legumes%>%
    group_by(cod, week, day)%>%
    dplyr::summarise(pulses_day=n())%>%
    group_by(cod)%>%
    dplyr::summarise(pulses_day_sum=sum(pulses_day))%>%
    left_join(record_days, by="cod")%>%
    mutate(pulses_day_avg=pulses_day_sum/total_days)%>%
   select(cod, pulses_day_avg ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  p_week_avg<-legumes%>%
    group_by(cod, week)%>%
    dplyr::summarise(pulses_week=n())%>%
    group_by(cod)%>%
    dplyr::summarise(pulses_week_sum=sum(pulses_week))%>%
    left_join(record_days, by="cod")%>%
    mutate(pulses_week_avg=pulses_week_sum/total_weeks)%>%
    select(cod, pulses_week_avg ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  p_week_tot<-legumes%>%
    group_by(cod)%>%
    dplyr::summarise(pulses_week=n())%>%
    left_join(record_days, by="cod")%>%
    mutate(pulses_week_tot=pulses_week/total_days)%>%
    select(cod, pulses_week_tot ) %>%
    mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
  
  pulses<-cbind(p_dish, p_meal[,-1], p_day[,-1], p_week_avg[,-1], p_week_tot[,-1])

# pulses_dish_avg= he number of times legumes were cooked in a given dish
# pul_meal ["pulbreakfast", "puldinner", "pullunch"]= he number of times legumes were cooked in a meal
# pulses_day_avg=	The number of times legumes were cooked in a given day. (number dishes containing legumes per day)
# pulses_week_avg=	The number of times legumes were cooked in a given week.
# pulses_week_tot=	The number of times legumes were cooked over all six weeks.
# leguminous volumes are ignored due to issues with the units provided. 


#species richness and dietary diversity scores====

dds_dish<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal,  dish)%>%
  dplyr::summarise(hdds_dish=n_distinct(hdds, na.rm=TRUE), wdds_dish=n_distinct(wdds, na.rm=TRUE),sr_dish=n_distinct(scientific_name, na.rm=TRUE) )%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_dish_avg=mean(hdds_dish), wdds_dish_avg=mean(wdds_dish), sr_dish_avg=mean(sr_dish))


dds_meal<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal)%>%
  dplyr::summarise(hdds_meal=n_distinct(hdds, na.rm=TRUE), wdds_meal=n_distinct(wdds, na.rm=TRUE),  spp_meal=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod, meal)%>%
  dplyr::summarise(hdds_meal_avg=mean(hdds_meal), wdds_meal_avg=mean(wdds_meal), sr_meal_avg=mean(spp_meal))%>%
  pivot_wider(names_from = meal,
              values_from = c(hdds_meal_avg, wdds_meal_avg, sr_meal_avg))

dds_day<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day)%>%
  dplyr::summarise(hdds_day=n_distinct(hdds, na.rm=TRUE), wdds_day=n_distinct(wdds, na.rm=TRUE), sr_day=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_day_avg=mean(hdds_day), wdds_day_avg=mean(wdds_day), sr_day_avg=mean(sr_day))


dds_week_avg<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week)%>%
  dplyr::summarise(hdds_week=n_distinct(hdds, na.rm=TRUE), wdds_week=n_distinct(wdds, na.rm=TRUE), sr_week=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_week_avg=mean(hdds_week), wdds_week_avg=mean(wdds_week),sr_week_avg=mean(sr_week) )


dds_week_tot<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_week_tot=n_distinct(hdds, na.rm=TRUE), wdds_week_tot=n_distinct(wdds, na.rm=TRUE), sr_week_tot=n_distinct(scientific_name))

dds<-dds_dish%>%
  left_join(dds_meal, by="cod")%>%
  left_join(dds_day, by="cod")%>%
  left_join(dds_week_avg, by="cod")%>%
  left_join(dds_week_tot, by="cod")%>%
  left_join(pulses, by="cod")%>%
  
  #winsorize variables
  mutate(across(-c(cod), winsorize_column))%>% 
  
  #add hh covariates
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))%>%
  dplyr::select(-c( "village_cor_Lealui","village_cor_Mapungu","village_cor_Nalitoya" ,
                    "gender_Men","gender_Women","highest_grade_Higher","highest_grade_None"   ,"highest_grade_Primary"  ,
                    "highest_grade_Secondary","solar_stove_No","solar_stove_Yes" ,"aas_involvement_Ag_nut", "aas_involvement_agriculture",
                    "aas_involvement_None", "aas_involvement_nutrition"  ,"Cellphone_no_c","boat_no_c","canoe_no_c","moto_no_c", "radio_no_c"  ))

#Getting average and SD for all indicators====
dds_long<- dds %>%
  pivot_longer(cols = c("village_cor", "gender", "highest_grade","solar_stove","aas_involvement"  ), 
               names_to = "covariates", 
               values_to = "cov_values")%>%
  pivot_longer(cols = c("hdds_dish_avg","hdds_meal_avg_breakfast","hdds_meal_avg_lunch","hdds_meal_avg_dinner","hdds_day_avg", "hdds_week_avg","hdds_week_tot",
                        "wdds_dish_avg", "wdds_meal_avg_breakfast","wdds_meal_avg_lunch","wdds_meal_avg_dinner","wdds_day_avg","wdds_week_avg","wdds_week_tot",
                        "zfbdrfg_dish_avg" ,"zfbdrfg_meal_avg_breakfast","zfbdrfg_meal_avg_lunch",  "zfbdrfg_meal_avg_dinner", "zfbdrfg_day_avg" , "zfbdrfg_week_avg" ,  "zfbdrfg_week_tot"  ,     
                        "sr_dish_avg",  "sr_meal_avg_breakfast","sr_meal_avg_lunch","sr_meal_avg_dinner" ,   "sr_day_avg",  "sr_week_avg",  "sr_week_tot" , 
                        "pulses_dish_avg","pul_breakfast", "pul_lunch", "pul_dinner",  "pulses_day_avg","pulses_week_avg","pulses_week_tot"), 
               names_to = "indicators", 
               values_to = "ind_values")%>%
  group_by(covariates, cov_values, indicators)%>%
  summarise(mean=round(mean(ind_values, na.rm=TRUE),1), n=n(), sd=round(sd(ind_values, na.rm=TRUE),1))%>%
  filter(covariates=="aas_involvement" |covariates=="solar_stove")%>%
  mutate(label=paste0(covariates, " / ", cov_values ),
         value=paste0(mean," (", sd, ")" ))%>%
  ungroup()%>%
  dplyr::select(-c("covariates", "cov_values", "mean", "n", "sd"))%>%
  pivot_wider(names_from = label,
              values_from = value)

# Create flextable
ft <- flextable(dds_long)

# Format flextable (optional)
ft <- theme_box(ft)
ft <- autofit(ft)

# Create a Word document
doc <- read_docx()

# Add flextable to the document
doc <- body_add_flextable(doc, value = ft)

# Save the document
print(doc, target = "DDS_SR_Legumes.docx")


#variables interpretation
# dds_dish=The HDDS/WDDS for a given dish, calculated as the total count of all food groups represented in the dish (FAO).
# dds_meal= The HDDS/WDDS for a given meal, calculated as: The average of the HDDS over dishes in the meal
# dds_day=The HDDS/WDDS for a given day calculated as:The average of the HDDS over meals in the day.
# dds_week_avg=	The HDDS/WDDS for a given week calculated as:	The average of the HDDS over days in the week.
# dds_week_tot=The HDDS/WDDS for all six weeks calculated as 	The average of the HDDS over days in the six weeks.
# sr_dish=	The SR for a given dish, calculated as a count of the number of species used as ingredients in the dish.
# sr_meal=The SR for a given meal, calculated as a count of the number of species used as ingredients in the meal.
# sr_day=	The SR for a given day, calculated as a count of the number of species used as ingredients in all meals that day.
# sr_week= The SR for a given week, calculated as a count of the number of species used as ingredients in all meals that week.
# sr_tot=	The SR for the six weeks, calculated as a count of the number of species used as ingredients in all meals over the six weeks.



#felm model accept factor variables, so ensure to remove dummy ones and indicators not specific to each analysis
cov<-c("solar_stove" , "gender", "age_cal" , "highest_grade" ,"hh_no","tli" , 
       "asset_index" , "aas_involvement" ,"village_cor")

##DISH====
pulses_dish_avg <-dds%>%
  dplyr::select(all_of(cov), pulses_dish_avg )

#setting reference levels
pulses_dish_avg $village_cor <- relevel(factor(pulses_dish_avg $village_cor), ref = "Lealui")
pulses_dish_avg $gender <- relevel(factor(pulses_dish_avg $gender), ref = "Women")
pulses_dish_avg $highest_grade <- relevel(factor(pulses_dish_avg $highest_grade), ref = "None")
pulses_dish_avg $aas_involvement <- relevel(factor(pulses_dish_avg $aas_involvement), ref = "None")
pulses_dish_avg $solar_stove <- relevel(factor(pulses_dish_avg $solar_stove), ref = "No")

model_pulses_dish_avg  <- felm(pulses_dish_avg  ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                           data = pulses_dish_avg )
robust_se_pulses_dish_avg  <- vcovCL(model_pulses_dish_avg , type = "HC0")
rt_pulses_dish_avg  <- coeftest(model_pulses_dish_avg , vcov = robust_se_pulses_dish_avg )
tidy_r_pulses_dish_avg  <- tidy(rt_pulses_dish_avg )%>%
  mutate(run="pulses_dish_avg")


sr_dish_avg <-dds%>%
  dplyr::select(all_of(cov), sr_dish_avg )

#setting reference levels
sr_dish_avg $village_cor <- relevel(factor(sr_dish_avg $village_cor), ref = "Lealui")
sr_dish_avg $gender <- relevel(factor(sr_dish_avg $gender), ref = "Women")
sr_dish_avg $highest_grade <- relevel(factor(sr_dish_avg $highest_grade), ref = "None")
sr_dish_avg $aas_involvement <- relevel(factor(sr_dish_avg $aas_involvement), ref = "None")
sr_dish_avg $solar_stove <- relevel(factor(sr_dish_avg $solar_stove), ref = "No")

model_sr_dish_avg  <- felm(sr_dish_avg  ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                      data = sr_dish_avg )
robust_se_sr_dish_avg  <- vcovCL(model_sr_dish_avg , type = "HC0")
rt_sr_dish_avg  <- coeftest(model_sr_dish_avg , vcov = robust_se_sr_dish_avg )
tidy_r_sr_dish_avg  <- tidy(rt_sr_dish_avg )%>%
  mutate(run="sr_dish_avg")

hdds_dish_avg<-dds%>%
  dplyr::select(all_of(cov), hdds_dish_avg)

#setting reference levels
hdds_dish_avg$village_cor <- relevel(factor(hdds_dish_avg$village_cor), ref = "Lealui")
hdds_dish_avg$gender <- relevel(factor(hdds_dish_avg$gender), ref = "Women")
hdds_dish_avg$highest_grade <- relevel(factor(hdds_dish_avg$highest_grade), ref = "None")
hdds_dish_avg$aas_involvement <- relevel(factor(hdds_dish_avg$aas_involvement), ref = "None")
hdds_dish_avg$solar_stove <- relevel(factor(hdds_dish_avg$solar_stove), ref = "No")


model_hdds_dish_avg <- felm(hdds_dish_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                           data = hdds_dish_avg)
robust_se_hdds_dish_avg <- vcovCL(model_hdds_dish_avg, type = "HC0")
rt_hdds_dish_avg <- coeftest(model_hdds_dish_avg, vcov = robust_se_hdds_dish_avg)
tidy_r_hdds_dish_avg <- tidy(rt_hdds_dish_avg)%>%
  mutate(run="hdds_dish_avg")



wdds_dish_avg<-dds%>%
  dplyr::select(all_of(cov), wdds_dish_avg)

#setting reference levels
wdds_dish_avg$village_cor <- relevel(factor(wdds_dish_avg$village_cor), ref = "Lealui")
wdds_dish_avg$gender <- relevel(factor(wdds_dish_avg$gender), ref = "Women")
wdds_dish_avg$highest_grade <- relevel(factor(wdds_dish_avg$highest_grade), ref = "None")
wdds_dish_avg$aas_involvement <- relevel(factor(wdds_dish_avg$aas_involvement), ref = "None")
wdds_dish_avg$solar_stove <- relevel(factor(wdds_dish_avg$solar_stove), ref = "No")


model_wdds_dish_avg <- felm(wdds_dish_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                            data = wdds_dish_avg)
robust_se_wdds_dish_avg <- vcovCL(model_wdds_dish_avg, type = "HC0")
rt_wdds_dish_avg <- coeftest(model_wdds_dish_avg, vcov = robust_se_wdds_dish_avg)
tidy_r_wdds_dish_avg <- tidy(rt_wdds_dish_avg)%>%
  mutate(run="wdds_dish_avg")


zfbdrfg_dish_avg<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_dish_avg)

#setting reference levels
zfbdrfg_dish_avg$village_cor <- relevel(factor(zfbdrfg_dish_avg$village_cor), ref = "Lealui")
zfbdrfg_dish_avg$gender <- relevel(factor(zfbdrfg_dish_avg$gender), ref = "Women")
zfbdrfg_dish_avg$highest_grade <- relevel(factor(zfbdrfg_dish_avg$highest_grade), ref = "None")
zfbdrfg_dish_avg$aas_involvement <- relevel(factor(zfbdrfg_dish_avg$aas_involvement), ref = "None")
zfbdrfg_dish_avg$solar_stove <- relevel(factor(zfbdrfg_dish_avg$solar_stove), ref = "No")


model_zfbdrfg_dish_avg <- felm(zfbdrfg_dish_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                            data = zfbdrfg_dish_avg)
robust_se_zfbdrfg_dish_avg <- vcovCL(model_zfbdrfg_dish_avg, type = "HC0")
rt_zfbdrfg_dish_avg <- coeftest(model_zfbdrfg_dish_avg, vcov = robust_se_zfbdrfg_dish_avg)
tidy_r_zfbdrfg_dish_avg <- tidy(rt_zfbdrfg_dish_avg)%>%
  mutate(run="zfbdrfg_dish_avg")


##pulses MEALS ====
pul_breakfast<-dds%>%
  dplyr::select(all_of(cov), pul_breakfast)

#setting reference levels
pul_breakfast$village_cor <- relevel(factor(pul_breakfast$village_cor), ref = "Lealui")
pul_breakfast$gender <- relevel(factor(pul_breakfast$gender), ref = "Women")
pul_breakfast$highest_grade <- relevel(factor(pul_breakfast$highest_grade), ref = "None")
pul_breakfast$aas_involvement <- relevel(factor(pul_breakfast$aas_involvement), ref = "None")
pul_breakfast$solar_stove <- relevel(factor(pul_breakfast$solar_stove), ref = "No")

model_pul_meal_breakfast <- felm(pul_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                                 data = pul_breakfast)
robust_se_pul_meal_breakfast <- vcovCL(model_pul_meal_breakfast, type = "HC0")
rt_pul_meal_breakfast <- coeftest(model_pul_meal_breakfast, vcov = robust_se_pul_meal_breakfast)
tidy_r_pul_meal_breakfast <- tidy(rt_pul_meal_breakfast)%>%
  mutate(run="pul_breakfast")

pul_dinner<-dds%>%
  dplyr::select(all_of(cov), pul_dinner)

#setting reference levels
pul_dinner$village_cor <- relevel(factor(pul_dinner$village_cor), ref = "Lealui")
pul_dinner$gender <- relevel(factor(pul_dinner$gender), ref = "Women")
pul_dinner$highest_grade <- relevel(factor(pul_dinner$highest_grade), ref = "None")
pul_dinner$aas_involvement <- relevel(factor(pul_dinner$aas_involvement), ref = "None")
pul_dinner$solar_stove <- relevel(factor(pul_dinner$solar_stove), ref = "No")

model_pul_meal_dinner <- felm(pul_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                              data = pul_dinner)
robust_se_pul_meal_dinner <- vcovCL(model_pul_meal_dinner, type = "HC0")
rt_pul_meal_dinner <- coeftest(model_pul_meal_dinner, vcov = robust_se_pul_meal_dinner)
tidy_r_pul_meal_dinner <- tidy(rt_pul_meal_dinner)%>%
  mutate(run="pul_dinner")

pul_lunch<-dds%>%
  dplyr::select(all_of(cov), pul_lunch)

#setting reference levels
pul_lunch$village_cor <- relevel(factor(pul_lunch$village_cor), ref = "Lealui")
pul_lunch$gender <- relevel(factor(pul_lunch$gender), ref = "Women")
pul_lunch$highest_grade <- relevel(factor(pul_lunch$highest_grade), ref = "None")
pul_lunch$aas_involvement <- relevel(factor(pul_lunch$aas_involvement), ref = "None")
pul_lunch$solar_stove <- relevel(factor(pul_lunch$solar_stove), ref = "No")


model_pul_meal_lunch <- felm(pul_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                            data = pul_lunch)
robust_se_pul_meal_lunch <- vcovCL(model_pul_meal_lunch, type = "HC0")
rt_pul_meal_lunch <- coeftest(model_pul_meal_lunch, vcov = robust_se_pul_meal_lunch)
tidy_r_pul_meal_lunch <- tidy(rt_pul_meal_lunch)%>%
  mutate(run="pul_lunch")
##SR MEALS ====
sr_meal_avg_breakfast<-dds%>%
  dplyr::select(all_of(cov), sr_meal_avg_breakfast)

#setting reference levels
sr_meal_avg_breakfast$village_cor <- relevel(factor(sr_meal_avg_breakfast$village_cor), ref = "Lealui")
sr_meal_avg_breakfast$gender <- relevel(factor(sr_meal_avg_breakfast$gender), ref = "Women")
sr_meal_avg_breakfast$highest_grade <- relevel(factor(sr_meal_avg_breakfast$highest_grade), ref = "None")
sr_meal_avg_breakfast$aas_involvement <- relevel(factor(sr_meal_avg_breakfast$aas_involvement), ref = "None")
sr_meal_avg_breakfast$solar_stove <- relevel(factor(sr_meal_avg_breakfast$solar_stove), ref = "No")

model_sr_meal_breakfast <- felm(sr_meal_avg_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                      data = sr_meal_avg_breakfast)
robust_se_sr_meal_breakfast <- vcovCL(model_sr_meal_breakfast, type = "HC0")
rt_sr_meal_breakfast <- coeftest(model_sr_meal_breakfast, vcov = robust_se_sr_meal_breakfast)
tidy_r_sr_meal_breakfast <- tidy(rt_sr_meal_breakfast)%>%
  mutate(run="sr_meal_avg_breakfast")

sr_meal_avg_dinner<-dds%>%
  dplyr::select(all_of(cov), sr_meal_avg_dinner)

#setting reference levels
sr_meal_avg_dinner$village_cor <- relevel(factor(sr_meal_avg_dinner$village_cor), ref = "Lealui")
sr_meal_avg_dinner$gender <- relevel(factor(sr_meal_avg_dinner$gender), ref = "Women")
sr_meal_avg_dinner$highest_grade <- relevel(factor(sr_meal_avg_dinner$highest_grade), ref = "None")
sr_meal_avg_dinner$aas_involvement <- relevel(factor(sr_meal_avg_dinner$aas_involvement), ref = "None")
sr_meal_avg_dinner$solar_stove <- relevel(factor(sr_meal_avg_dinner$solar_stove), ref = "No")

model_sr_meal_dinner <- felm(sr_meal_avg_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                      data = sr_meal_avg_dinner)
robust_se_sr_meal_dinner <- vcovCL(model_sr_meal_dinner, type = "HC0")
rt_sr_meal_dinner <- coeftest(model_sr_meal_dinner, vcov = robust_se_sr_meal_dinner)
tidy_r_sr_meal_dinner <- tidy(rt_sr_meal_dinner)%>%
  mutate(run="sr_meal_avg_dinner")

sr_meal_avg_lunch<-dds%>%
  dplyr::select(all_of(cov), sr_meal_avg_lunch)

#setting reference levels
sr_meal_avg_lunch$village_cor <- relevel(factor(sr_meal_avg_lunch$village_cor), ref = "Lealui")
sr_meal_avg_lunch$gender <- relevel(factor(sr_meal_avg_lunch$gender), ref = "Women")
sr_meal_avg_lunch$highest_grade <- relevel(factor(sr_meal_avg_lunch$highest_grade), ref = "None")
sr_meal_avg_lunch$aas_involvement <- relevel(factor(sr_meal_avg_lunch$aas_involvement), ref = "None")
sr_meal_avg_lunch$solar_stove <- relevel(factor(sr_meal_avg_lunch$solar_stove), ref = "No")


model_sr_meal_lunch <- felm(sr_meal_avg_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                             data = sr_meal_avg_lunch)
robust_se_sr_meal_lunch <- vcovCL(model_sr_meal_lunch, type = "HC0")
rt_sr_meal_lunch <- coeftest(model_sr_meal_lunch, vcov = robust_se_sr_meal_lunch)
tidy_r_sr_meal_lunch <- tidy(rt_sr_meal_lunch)%>%
  mutate(run="sr_meal_avg_lunch")

##HDDS MEALS ====
hdds_meal_avg_breakfast<-dds%>%
  dplyr::select(all_of(cov), hdds_meal_avg_breakfast)

#setting reference levels
hdds_meal_avg_breakfast$village_cor <- relevel(factor(hdds_meal_avg_breakfast$village_cor), ref = "Lealui")
hdds_meal_avg_breakfast$gender <- relevel(factor(hdds_meal_avg_breakfast$gender), ref = "Women")
hdds_meal_avg_breakfast$highest_grade <- relevel(factor(hdds_meal_avg_breakfast$highest_grade), ref = "None")
hdds_meal_avg_breakfast$aas_involvement <- relevel(factor(hdds_meal_avg_breakfast$aas_involvement), ref = "None")
hdds_meal_avg_breakfast$solar_stove <- relevel(factor(hdds_meal_avg_breakfast$solar_stove), ref = "No")


model_hdds_meal_avg_breakfast <- felm(hdds_meal_avg_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                            data = hdds_meal_avg_breakfast)
robust_se_hdds_meal_avg_breakfast <- vcovCL(model_hdds_meal_avg_breakfast, type = "HC0")
rt_hdds_meal_avg_breakfast <- coeftest(model_hdds_meal_avg_breakfast, vcov = robust_se_hdds_meal_avg_breakfast)
tidy_r_hdds_meal_avg_breakfast <- tidy(rt_hdds_meal_avg_breakfast)%>%
  mutate(run="hdds_meal_avg_breakfast")


hdds_meal_avg_dinner<-dds%>%
  dplyr::select(all_of(cov), hdds_meal_avg_dinner)

#setting reference levels
hdds_meal_avg_dinner$village_cor <- relevel(factor(hdds_meal_avg_dinner$village_cor), ref = "Lealui")
hdds_meal_avg_dinner$gender <- relevel(factor(hdds_meal_avg_dinner$gender), ref = "Women")
hdds_meal_avg_dinner$highest_grade <- relevel(factor(hdds_meal_avg_dinner$highest_grade), ref = "None")
hdds_meal_avg_dinner$aas_involvement <- relevel(factor(hdds_meal_avg_dinner$aas_involvement), ref = "None")
hdds_meal_avg_dinner$solar_stove <- relevel(factor(hdds_meal_avg_dinner$solar_stove), ref = "No")


model_hdds_meal_avg_dinner <- felm(hdds_meal_avg_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                      data = hdds_meal_avg_dinner)
robust_se_hdds_meal_avg_dinner <- vcovCL(model_hdds_meal_avg_dinner, type = "HC0")
rt_hdds_meal_avg_dinner <- coeftest(model_hdds_meal_avg_dinner, vcov = robust_se_hdds_meal_avg_dinner)
tidy_r_hdds_meal_avg_dinner <- tidy(rt_hdds_meal_avg_dinner)%>%
  mutate(run="hdds_meal_avg_dinner")

hdds_meal_avg_lunch<-dds%>%
  dplyr::select(all_of(cov), hdds_meal_avg_lunch)

#setting reference levels
hdds_meal_avg_lunch$village_cor <- relevel(factor(hdds_meal_avg_lunch$village_cor), ref = "Lealui")
hdds_meal_avg_lunch$gender <- relevel(factor(hdds_meal_avg_lunch$gender), ref = "Women")
hdds_meal_avg_lunch$highest_grade <- relevel(factor(hdds_meal_avg_lunch$highest_grade), ref = "None")
hdds_meal_avg_lunch$aas_involvement <- relevel(factor(hdds_meal_avg_lunch$aas_involvement), ref = "None")
hdds_meal_avg_lunch$solar_stove <- relevel(factor(hdds_meal_avg_lunch$solar_stove), ref = "No")


model_hdds_meal_avg_lunch <- felm(hdds_meal_avg_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                   data = hdds_meal_avg_lunch)
robust_se_hdds_meal_avg_lunch <- vcovCL(model_hdds_meal_avg_lunch, type = "HC0")
rt_hdds_meal_avg_lunch <- coeftest(model_hdds_meal_avg_lunch, vcov = robust_se_hdds_meal_avg_lunch)
tidy_r_hdds_meal_avg_lunch <- tidy(rt_hdds_meal_avg_lunch)%>%
  mutate(run="hdds_meal_avg_lunch")

##WDDS MEALS ====
wdds_meal_avg_breakfast<-dds%>%
  dplyr::select(all_of(cov), wdds_meal_avg_breakfast)

#setting reference levels
wdds_meal_avg_breakfast$village_cor <- relevel(factor(wdds_meal_avg_breakfast$village_cor), ref = "Lealui")
wdds_meal_avg_breakfast$gender <- relevel(factor(wdds_meal_avg_breakfast$gender), ref = "Women")
wdds_meal_avg_breakfast$highest_grade <- relevel(factor(wdds_meal_avg_breakfast$highest_grade), ref = "None")
wdds_meal_avg_breakfast$aas_involvement <- relevel(factor(wdds_meal_avg_breakfast$aas_involvement), ref = "None")
wdds_meal_avg_breakfast$solar_stove <- relevel(factor(wdds_meal_avg_breakfast$solar_stove), ref = "No")


model_wdds_meal_avg_breakfast <- felm(wdds_meal_avg_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                  data = wdds_meal_avg_breakfast)
robust_se_wdds_meal_avg_breakfast <- vcovCL(model_wdds_meal_avg_breakfast, type = "HC0")
rt_wdds_meal_avg_breakfast <- coeftest(model_wdds_meal_avg_breakfast, vcov = robust_se_wdds_meal_avg_breakfast)
tidy_r_wdds_meal_avg_breakfast <- tidy(rt_wdds_meal_avg_breakfast)%>%
  mutate(run="wdds_meal_avg_breakfast")

wdds_meal_avg_dinner<-dds%>%
  dplyr::select(all_of(cov), wdds_meal_avg_dinner)

#setting reference levels
wdds_meal_avg_dinner$village_cor <- relevel(factor(wdds_meal_avg_dinner$village_cor), ref = "Lealui")
wdds_meal_avg_dinner$gender <- relevel(factor(wdds_meal_avg_dinner$gender), ref = "Women")
wdds_meal_avg_dinner$highest_grade <- relevel(factor(wdds_meal_avg_dinner$highest_grade), ref = "None")
wdds_meal_avg_dinner$aas_involvement <- relevel(factor(wdds_meal_avg_dinner$aas_involvement), ref = "None")
wdds_meal_avg_dinner$solar_stove <- relevel(factor(wdds_meal_avg_dinner$solar_stove), ref = "No")


model_wdds_meal_avg_dinner <- felm(wdds_meal_avg_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                      data = wdds_meal_avg_dinner)
robust_se_wdds_meal_avg_dinner <- vcovCL(model_wdds_meal_avg_dinner, type = "HC0")
rt_wdds_meal_avg_dinner <- coeftest(model_wdds_meal_avg_dinner, vcov = robust_se_wdds_meal_avg_dinner)
tidy_r_wdds_meal_avg_dinner <- tidy(rt_wdds_meal_avg_dinner)%>%
  mutate(run="wdds_meal_avg_dinner")

wdds_meal_avg_lunch<-dds%>%
  dplyr::select(all_of(cov), wdds_meal_avg_lunch)

#setting reference levels
wdds_meal_avg_lunch$village_cor <- relevel(factor(wdds_meal_avg_lunch$village_cor), ref = "Lealui")
wdds_meal_avg_lunch$gender <- relevel(factor(wdds_meal_avg_lunch$gender), ref = "Women")
wdds_meal_avg_lunch$highest_grade <- relevel(factor(wdds_meal_avg_lunch$highest_grade), ref = "None")
wdds_meal_avg_lunch$aas_involvement <- relevel(factor(wdds_meal_avg_lunch$aas_involvement), ref = "None")
wdds_meal_avg_lunch$solar_stove <- relevel(factor(wdds_meal_avg_lunch$solar_stove), ref = "No")


model_wdds_meal_avg_lunch <- felm(wdds_meal_avg_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                   data = wdds_meal_avg_lunch)
robust_se_wdds_meal_avg_lunch <- vcovCL(model_wdds_meal_avg_lunch, type = "HC0")
rt_wdds_meal_avg_lunch <- coeftest(model_wdds_meal_avg_lunch, vcov = robust_se_wdds_meal_avg_lunch)
tidy_r_wdds_meal_avg_lunch <- tidy(rt_wdds_meal_avg_lunch)%>%
  mutate(run="wdds_meal_avg_lunch")

##zfbdrfg MEALS ====
zfbdrfg_meal_avg_breakfast<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_meal_avg_breakfast)

#setting reference levels
zfbdrfg_meal_avg_breakfast$village_cor <- relevel(factor(zfbdrfg_meal_avg_breakfast$village_cor), ref = "Lealui")
zfbdrfg_meal_avg_breakfast$gender <- relevel(factor(zfbdrfg_meal_avg_breakfast$gender), ref = "Women")
zfbdrfg_meal_avg_breakfast$highest_grade <- relevel(factor(zfbdrfg_meal_avg_breakfast$highest_grade), ref = "None")
zfbdrfg_meal_avg_breakfast$aas_involvement <- relevel(factor(zfbdrfg_meal_avg_breakfast$aas_involvement), ref = "None")
zfbdrfg_meal_avg_breakfast$solar_stove <- relevel(factor(zfbdrfg_meal_avg_breakfast$solar_stove), ref = "No")


model_zfbdrfg_meal_avg_breakfast <- felm(zfbdrfg_meal_avg_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                      data = zfbdrfg_meal_avg_breakfast)
robust_se_zfbdrfg_meal_avg_breakfast <- vcovCL(model_zfbdrfg_meal_avg_breakfast, type = "HC0")
rt_zfbdrfg_meal_avg_breakfast <- coeftest(model_zfbdrfg_meal_avg_breakfast, vcov = robust_se_zfbdrfg_meal_avg_breakfast)
tidy_r_zfbdrfg_meal_avg_breakfast <- tidy(rt_zfbdrfg_meal_avg_breakfast)%>%
  mutate(run="zfbdrfg_meal_avg_breakfast")

zfbdrfg_meal_avg_dinner<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_meal_avg_dinner)

#setting reference levels
zfbdrfg_meal_avg_dinner$village_cor <- relevel(factor(zfbdrfg_meal_avg_dinner$village_cor), ref = "Lealui")
zfbdrfg_meal_avg_dinner$gender <- relevel(factor(zfbdrfg_meal_avg_dinner$gender), ref = "Women")
zfbdrfg_meal_avg_dinner$highest_grade <- relevel(factor(zfbdrfg_meal_avg_dinner$highest_grade), ref = "None")
zfbdrfg_meal_avg_dinner$aas_involvement <- relevel(factor(zfbdrfg_meal_avg_dinner$aas_involvement), ref = "None")
zfbdrfg_meal_avg_dinner$solar_stove <- relevel(factor(zfbdrfg_meal_avg_dinner$solar_stove), ref = "No")


model_zfbdrfg_meal_avg_dinner <- felm(zfbdrfg_meal_avg_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                   data = zfbdrfg_meal_avg_dinner)
robust_se_zfbdrfg_meal_avg_dinner <- vcovCL(model_zfbdrfg_meal_avg_dinner, type = "HC0")
rt_zfbdrfg_meal_avg_dinner <- coeftest(model_zfbdrfg_meal_avg_dinner, vcov = robust_se_zfbdrfg_meal_avg_dinner)
tidy_r_zfbdrfg_meal_avg_dinner <- tidy(rt_zfbdrfg_meal_avg_dinner)%>%
  mutate(run="zfbdrfg_meal_avg_dinner")

zfbdrfg_meal_avg_lunch<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_meal_avg_lunch)

#setting reference levels
zfbdrfg_meal_avg_lunch$village_cor <- relevel(factor(zfbdrfg_meal_avg_lunch$village_cor), ref = "Lealui")
zfbdrfg_meal_avg_lunch$gender <- relevel(factor(zfbdrfg_meal_avg_lunch$gender), ref = "Women")
zfbdrfg_meal_avg_lunch$highest_grade <- relevel(factor(zfbdrfg_meal_avg_lunch$highest_grade), ref = "None")
zfbdrfg_meal_avg_lunch$aas_involvement <- relevel(factor(zfbdrfg_meal_avg_lunch$aas_involvement), ref = "None")
zfbdrfg_meal_avg_lunch$solar_stove <- relevel(factor(zfbdrfg_meal_avg_lunch$solar_stove), ref = "No")


model_zfbdrfg_meal_avg_lunch <- felm(zfbdrfg_meal_avg_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                  data = zfbdrfg_meal_avg_lunch)
robust_se_zfbdrfg_meal_avg_lunch <- vcovCL(model_zfbdrfg_meal_avg_lunch, type = "HC0")
rt_zfbdrfg_meal_avg_lunch <- coeftest(model_zfbdrfg_meal_avg_lunch, vcov = robust_se_zfbdrfg_meal_avg_lunch)
tidy_r_zfbdrfg_meal_avg_lunch <- tidy(rt_zfbdrfg_meal_avg_lunch)%>%
  mutate(run="zfbdrfg_meal_avg_lunch")

##DAY====

pulses_day_avg<-dds%>%
  dplyr::select(all_of(cov), pulses_day_avg)

#setting reference levels
pulses_day_avg$village_cor <- relevel(factor(pulses_day_avg$village_cor), ref = "Lealui")
pulses_day_avg$gender <- relevel(factor(pulses_day_avg$gender), ref = "Women")
pulses_day_avg$highest_grade <- relevel(factor(pulses_day_avg$highest_grade), ref = "None")
pulses_day_avg$aas_involvement <- relevel(factor(pulses_day_avg$aas_involvement), ref = "None")
pulses_day_avg$solar_stove <- relevel(factor(pulses_day_avg$solar_stove), ref = "No")

model_pulses_day_avg <- felm(pulses_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                         data = pulses_day_avg)
robust_se_pulses_day_avg <- vcovCL(model_pulses_day_avg, type = "HC0")
rt_pulses_day_avg <- coeftest(model_pulses_day_avg, vcov = robust_se_pulses_day_avg)
tidy_r_pulses_day_avg <- tidy(rt_pulses_day_avg)%>%
  mutate(run="pulses_day_avg")


sr_day_avg<-dds%>%
  dplyr::select(all_of(cov), sr_day_avg)

#setting reference levels
sr_day_avg$village_cor <- relevel(factor(sr_day_avg$village_cor), ref = "Lealui")
sr_day_avg$gender <- relevel(factor(sr_day_avg$gender), ref = "Women")
sr_day_avg$highest_grade <- relevel(factor(sr_day_avg$highest_grade), ref = "None")
sr_day_avg$aas_involvement <- relevel(factor(sr_day_avg$aas_involvement), ref = "None")
sr_day_avg$solar_stove <- relevel(factor(sr_day_avg$solar_stove), ref = "No")

model_sr_day_avg <- felm(sr_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                     data = sr_day_avg)
robust_se_sr_day_avg <- vcovCL(model_sr_day_avg, type = "HC0")
rt_sr_day_avg <- coeftest(model_sr_day_avg, vcov = robust_se_sr_day_avg)
tidy_r_sr_day_avg <- tidy(rt_sr_day_avg)%>%
  mutate(run="sr_day_avg")

hdds_day_avg<-dds%>%
  dplyr::select(all_of(cov), hdds_day_avg)

#setting reference levels
hdds_day_avg$village_cor <- relevel(factor(hdds_day_avg$village_cor), ref = "Lealui")
hdds_day_avg$gender <- relevel(factor(hdds_day_avg$gender), ref = "Women")
hdds_day_avg$highest_grade <- relevel(factor(hdds_day_avg$highest_grade), ref = "None")
hdds_day_avg$aas_involvement <- relevel(factor(hdds_day_avg$aas_involvement), ref = "None")
hdds_day_avg$solar_stove <- relevel(factor(hdds_day_avg$solar_stove), ref = "No")


model_hdds_day_avg <- felm(hdds_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                                  data = hdds_day_avg)
robust_se_hdds_day_avg <- vcovCL(model_hdds_day_avg, type = "HC0")
rt_hdds_day_avg <- coeftest(model_hdds_day_avg, vcov = robust_se_hdds_day_avg)
tidy_r_hdds_day_avg <- tidy(rt_hdds_day_avg)%>%
  mutate(run="hdds_day_avg")

wdds_day_avg<-dds%>%
  dplyr::select(all_of(cov), wdds_day_avg)

#setting reference levels
wdds_day_avg$village_cor <- relevel(factor(wdds_day_avg$village_cor), ref = "Lealui")
wdds_day_avg$gender <- relevel(factor(wdds_day_avg$gender), ref = "Women")
wdds_day_avg$highest_grade <- relevel(factor(wdds_day_avg$highest_grade), ref = "None")
wdds_day_avg$aas_involvement <- relevel(factor(wdds_day_avg$aas_involvement), ref = "None")
wdds_day_avg$solar_stove <- relevel(factor(wdds_day_avg$solar_stove), ref = "No")


model_wdds_day_avg <- felm(wdds_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                           data = wdds_day_avg)
robust_se_wdds_day_avg <- vcovCL(model_wdds_day_avg, type = "HC0")
rt_wdds_day_avg <- coeftest(model_wdds_day_avg, vcov = robust_se_wdds_day_avg)
tidy_r_wdds_day_avg <- tidy(rt_wdds_day_avg)%>%
  mutate(run="wdds_day_avg")

zfbdrfg_day_avg<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_day_avg)

#setting reference levels
zfbdrfg_day_avg$village_cor <- relevel(factor(zfbdrfg_day_avg$village_cor), ref = "Lealui")
zfbdrfg_day_avg$gender <- relevel(factor(zfbdrfg_day_avg$gender), ref = "Women")
zfbdrfg_day_avg$highest_grade <- relevel(factor(zfbdrfg_day_avg$highest_grade), ref = "None")
zfbdrfg_day_avg$aas_involvement <- relevel(factor(zfbdrfg_day_avg$aas_involvement), ref = "None")
zfbdrfg_day_avg$solar_stove <- relevel(factor(zfbdrfg_day_avg$solar_stove), ref = "No")


model_zfbdrfg_day_avg <- felm(zfbdrfg_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                           data = zfbdrfg_day_avg)
robust_se_zfbdrfg_day_avg <- vcovCL(model_zfbdrfg_day_avg, type = "HC0")
rt_zfbdrfg_day_avg <- coeftest(model_zfbdrfg_day_avg, vcov = robust_se_zfbdrfg_day_avg)
tidy_r_zfbdrfg_day_avg <- tidy(rt_zfbdrfg_day_avg)%>%
  mutate(run="zfbdrfg_day_avg")

##WEEK AVG====
pulses_week_avg<-dds%>%
  dplyr::select(all_of(cov), pulses_week_avg)

#setting reference levels
pulses_week_avg$village_cor <- relevel(factor(pulses_week_avg$village_cor), ref = "Lealui")
pulses_week_avg$gender <- relevel(factor(pulses_week_avg$gender), ref = "Women")
pulses_week_avg$highest_grade <- relevel(factor(pulses_week_avg$highest_grade), ref = "None")
pulses_week_avg$aas_involvement <- relevel(factor(pulses_week_avg$aas_involvement), ref = "None")
pulses_week_avg$solar_stove <- relevel(factor(pulses_week_avg$solar_stove), ref = "No")

model_pulses_week_avg <- felm(pulses_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                          data = pulses_week_avg)
robust_se_pulses_week_avg <- vcovHC(model_pulses_week_avg, type = "HC0")
rt_pulses_week_avg <- coeftest(model_pulses_week_avg, vcov = robust_se_pulses_week_avg)
tidy_r_pulses_week_avg <- tidy(rt_pulses_week_avg)%>%
  mutate(run="pulses_week_avg")


sr_week_avg<-dds%>%
  dplyr::select(all_of(cov), sr_week_avg)

#setting reference levels
sr_week_avg$village_cor <- relevel(factor(sr_week_avg$village_cor), ref = "Lealui")
sr_week_avg$gender <- relevel(factor(sr_week_avg$gender), ref = "Women")
sr_week_avg$highest_grade <- relevel(factor(sr_week_avg$highest_grade), ref = "None")
sr_week_avg$aas_involvement <- relevel(factor(sr_week_avg$aas_involvement), ref = "None")
sr_week_avg$solar_stove <- relevel(factor(sr_week_avg$solar_stove), ref = "No")

model_sr_week_avg <- felm(sr_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                      data = sr_week_avg)
robust_se_sr_week_avg <- vcovHC(model_sr_week_avg, type = "HC0")
rt_sr_week_avg <- coeftest(model_sr_week_avg, vcov = robust_se_sr_week_avg)
tidy_r_sr_week_avg <- tidy(rt_sr_week_avg)%>%
  mutate(run="sr_week_avg")

hdds_week_avg<-dds%>%
  dplyr::select(all_of(cov), hdds_week_avg)

#setting reference levels
hdds_week_avg$village_cor <- relevel(factor(hdds_week_avg$village_cor), ref = "Lealui")
hdds_week_avg$gender <- relevel(factor(hdds_week_avg$gender), ref = "Women")
hdds_week_avg$highest_grade <- relevel(factor(hdds_week_avg$highest_grade), ref = "None")
hdds_week_avg$aas_involvement <- relevel(factor(hdds_week_avg$aas_involvement), ref = "None")
hdds_week_avg$solar_stove <- relevel(factor(hdds_week_avg$solar_stove), ref = "No")


model_hdds_week_avg <- felm(hdds_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index + aas_involvement | village_cor,
                           data = hdds_week_avg)
robust_se_hdds_week_avg <- vcovHC(model_hdds_week_avg, type = "HC0")
rt_hdds_week_avg <- coeftest(model_hdds_week_avg, vcov = robust_se_hdds_week_avg)
tidy_r_hdds_week_avg <- tidy(rt_hdds_week_avg)%>%
  mutate(run="hdds_week_avg")

wdds_week_avg<-dds%>%
  dplyr::select(all_of(cov), wdds_week_avg)

#setting reference levels
wdds_week_avg$village_cor <- relevel(factor(wdds_week_avg$village_cor), ref = "Lealui")
wdds_week_avg$gender <- relevel(factor(wdds_week_avg$gender), ref = "Women")
wdds_week_avg$highest_grade <- relevel(factor(wdds_week_avg$highest_grade), ref = "None")
wdds_week_avg$aas_involvement <- relevel(factor(wdds_week_avg$aas_involvement), ref = "None")
wdds_week_avg$solar_stove <- relevel(factor(wdds_week_avg$solar_stove), ref = "No")


model_wdds_week_avg <- felm(wdds_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no +  tli + asset_index + aas_involvement | village_cor,
                            data = wdds_week_avg)
robust_se_wdds_week_avg <- vcovHC(model_wdds_week_avg, type = "HC0")
rt_wdds_week_avg <- coeftest(model_wdds_week_avg, vcov = robust_se_wdds_week_avg)
tidy_r_wdds_week_avg <- tidy(rt_wdds_week_avg)%>%
  mutate(run="wdds_week_avg")

zfbdrfg_week_avg<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_week_avg)

#setting reference levels
zfbdrfg_week_avg$village_cor <- relevel(factor(zfbdrfg_week_avg$village_cor), ref = "Lealui")
zfbdrfg_week_avg$gender <- relevel(factor(zfbdrfg_week_avg$gender), ref = "Women")
zfbdrfg_week_avg$highest_grade <- relevel(factor(zfbdrfg_week_avg$highest_grade), ref = "None")
zfbdrfg_week_avg$aas_involvement <- relevel(factor(zfbdrfg_week_avg$aas_involvement), ref = "None")
zfbdrfg_week_avg$solar_stove <- relevel(factor(zfbdrfg_week_avg$solar_stove), ref = "No")


model_zfbdrfg_week_avg <- felm(zfbdrfg_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no +  tli + asset_index + aas_involvement | village_cor,
                            data = zfbdrfg_week_avg)
robust_se_zfbdrfg_week_avg <- vcovHC(model_zfbdrfg_week_avg, type = "HC0")
rt_zfbdrfg_week_avg <- coeftest(model_zfbdrfg_week_avg, vcov = robust_se_zfbdrfg_week_avg)
tidy_r_zfbdrfg_week_avg <- tidy(rt_zfbdrfg_week_avg)%>%
  mutate(run="zfbdrfg_week_avg")

##WEEK TOT====

pulses_week_tot<-dds%>%
  dplyr::select(all_of(cov), pulses_week_tot)

#setting reference levels
pulses_week_tot$village_cor <- relevel(factor(pulses_week_tot$village_cor), ref = "Lealui")
pulses_week_tot$gender <- relevel(factor(pulses_week_tot$gender), ref = "Women")
pulses_week_tot$highest_grade <- relevel(factor(pulses_week_tot$highest_grade), ref = "None")
pulses_week_tot$aas_involvement <- relevel(factor(pulses_week_tot$aas_involvement), ref = "None")
pulses_week_tot$solar_stove <- relevel(factor(pulses_week_tot$solar_stove), ref = "No")

model_pulses_week_tot <- felm(pulses_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                          data = pulses_week_tot)
robust_se_pulses_week_tot <- vcovHC(model_pulses_week_tot, type = "HC0")
rt_pulses_week_tot <- coeftest(model_pulses_week_tot, vcov = robust_se_pulses_week_tot)
tidy_r_pulses_week_tot <- tidy(rt_pulses_week_tot)%>%
  mutate(run="pulses_week_tot")

sr_week_tot<-dds%>%
  dplyr::select(all_of(cov), sr_week_tot)

#setting reference levels
sr_week_tot$village_cor <- relevel(factor(sr_week_tot$village_cor), ref = "Lealui")
sr_week_tot$gender <- relevel(factor(sr_week_tot$gender), ref = "Women")
sr_week_tot$highest_grade <- relevel(factor(sr_week_tot$highest_grade), ref = "None")
sr_week_tot$aas_involvement <- relevel(factor(sr_week_tot$aas_involvement), ref = "None")
sr_week_tot$solar_stove <- relevel(factor(sr_week_tot$solar_stove), ref = "No")

model_sr_week_tot <- felm(sr_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                          data = sr_week_tot)
robust_se_sr_week_tot <- vcovHC(model_sr_week_tot, type = "HC0")
rt_sr_week_tot <- coeftest(model_sr_week_tot, vcov = robust_se_sr_week_tot)
tidy_r_sr_week_tot <- tidy(rt_sr_week_tot)%>%
  mutate(run="sr_week_tot")

hdds_week_tot<-dds%>%
  dplyr::select(all_of(cov), hdds_week_tot)

#setting reference levels
hdds_week_tot$village_cor <- relevel(factor(hdds_week_tot$village_cor), ref = "Lealui")
hdds_week_tot$gender <- relevel(factor(hdds_week_tot$gender), ref = "Women")
hdds_week_tot$highest_grade <- relevel(factor(hdds_week_tot$highest_grade), ref = "None")
hdds_week_tot$aas_involvement <- relevel(factor(hdds_week_tot$aas_involvement), ref = "None")
hdds_week_tot$solar_stove <- relevel(factor(hdds_week_tot$solar_stove), ref = "No")


model_hdds_week_tot <- felm(hdds_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no +tli + asset_index  + aas_involvement | village_cor,
                            data = hdds_week_tot)
robust_se_hdds_week_tot <-vcovHC(model_hdds_week_tot, type = "HC0")
rt_hdds_week_tot <- coeftest(model_hdds_week_tot, vcov = robust_se_hdds_week_tot)
tidy_r_hdds_week_tot <- tidy(rt_hdds_week_tot)%>%
  mutate(run="hdds_week_tot")


wdds_week_tot<-dds%>%
  dplyr::select(all_of(cov), wdds_week_tot)

#setting reference levels
wdds_week_tot$village_cor <- relevel(factor(wdds_week_tot$village_cor), ref = "Lealui")
wdds_week_tot$gender <- relevel(factor(wdds_week_tot$gender), ref = "Women")
wdds_week_tot$highest_grade <- relevel(factor(wdds_week_tot$highest_grade), ref = "None")
wdds_week_tot$aas_involvement <- relevel(factor(wdds_week_tot$aas_involvement), ref = "None")
wdds_week_tot$solar_stove <- relevel(factor(wdds_week_tot$solar_stove), ref = "No")


model_wdds_week_tot <- felm(wdds_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                            data = wdds_week_tot)
robust_se_wdds_week_tot <- vcovHC(model_wdds_week_tot, type = "HC0")
rt_wdds_week_tot <- coeftest(model_wdds_week_tot, vcov = robust_se_wdds_week_tot)
tidy_r_wdds_week_tot <- tidy(rt_wdds_week_tot)%>%
  mutate(run="wdds_week_tot")

zfbdrfg_week_tot<-dds%>%
  dplyr::select(all_of(cov), zfbdrfg_week_tot)

#setting reference levels
zfbdrfg_week_tot$village_cor <- relevel(factor(zfbdrfg_week_tot$village_cor), ref = "Lealui")
zfbdrfg_week_tot$gender <- relevel(factor(zfbdrfg_week_tot$gender), ref = "Women")
zfbdrfg_week_tot$highest_grade <- relevel(factor(zfbdrfg_week_tot$highest_grade), ref = "None")
zfbdrfg_week_tot$aas_involvement <- relevel(factor(zfbdrfg_week_tot$aas_involvement), ref = "None")
zfbdrfg_week_tot$solar_stove <- relevel(factor(zfbdrfg_week_tot$solar_stove), ref = "No")


model_zfbdrfg_week_tot <- felm(zfbdrfg_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tli + asset_index  + aas_involvement | village_cor,
                            data = zfbdrfg_week_tot)
robust_se_zfbdrfg_week_tot <- vcovHC(model_zfbdrfg_week_tot, type = "HC0")
rt_zfbdrfg_week_tot <- coeftest(model_zfbdrfg_week_tot, vcov = robust_se_zfbdrfg_week_tot)
tidy_r_zfbdrfg_week_tot <- tidy(rt_zfbdrfg_week_tot)%>%
  mutate(run="zfbdrfg_week_tot")

#integrating all SR and DDS indicators====

dds_all<-rbind( tidy_r_sr_dish_avg, tidy_r_hdds_dish_avg,tidy_r_wdds_dish_avg,tidy_r_zfbdrfg_dish_avg, tidy_r_pulses_dish_avg,
                tidy_r_pul_meal_breakfast, tidy_r_pul_meal_dinner, tidy_r_pul_meal_lunch, 
                tidy_r_sr_meal_breakfast, tidy_r_sr_meal_dinner, tidy_r_sr_meal_lunch, tidy_r_pul_meal_lunch, 
                 tidy_r_hdds_meal_avg_breakfast,tidy_r_hdds_meal_avg_dinner,tidy_r_hdds_meal_avg_lunch,
                tidy_r_wdds_meal_avg_breakfast,tidy_r_wdds_meal_avg_dinner,tidy_r_wdds_meal_avg_lunch,
               tidy_r_zfbdrfg_meal_avg_breakfast,tidy_r_zfbdrfg_meal_avg_dinner,tidy_r_zfbdrfg_meal_avg_lunch,
                tidy_r_sr_day_avg, tidy_r_hdds_day_avg,tidy_r_wdds_day_avg,tidy_r_zfbdrfg_day_avg,tidy_r_pulses_day_avg, 
                tidy_r_sr_week_avg, tidy_r_hdds_week_avg,tidy_r_wdds_week_avg,tidy_r_zfbdrfg_week_avg,tidy_r_pulses_week_avg,
                tidy_r_sr_week_tot, tidy_r_hdds_week_tot, tidy_r_wdds_week_tot, tidy_r_zfbdrfg_week_tot, tidy_r_pulses_week_tot)%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))

dds_all$term <- ordered(dds_all$term,
                           levels = c(
                             "hh_no",
                             "tli",
                             "asset_index",
                             "highest_gradePrimary" ,
                             "highest_gradeSecondary",
                             "highest_gradeHigher",
                             "genderMen",
                             "age_cal",
                             "aas_involvementagriculture",
                             "aas_involvementnutrition",
                             "aas_involvementAg_nut",
                             "solar_stoveYes"))


desired_order <- c("hdds_dish_avg",          "wdds_dish_avg",           "zfbdrfg_dish_avg",            "sr_dish_avg",            "pulses_dish_avg", 
                   "hdds_meal_avg_breakfast","wdds_meal_avg_breakfast", "zfbdrfg_meal_avg_breakfast",  "sr_meal_avg_breakfast",  "pul_breakfast" ,
                   "hdds_meal_avg_lunch",    "wdds_meal_avg_lunch",     "zfbdrfg_meal_avg_lunch",       "sr_meal_avg_lunch",     "pul_lunch",
                   "hdds_meal_avg_dinner",   "wdds_meal_avg_dinner",    "zfbdrfg_meal_avg_dinner",     "sr_meal_avg_dinner",      "pul_dinner",
                   "hdds_day_avg" ,          "wdds_day_avg",            "zfbdrfg_day_avg",             "sr_day_avg",              "pulses_day_avg",
                   "hdds_week_avg"  ,        "wdds_week_avg" ,          "zfbdrfg_week_avg" ,           "sr_week_avg" ,            "pulses_week_avg",
                   "hdds_week_tot",          "wdds_week_tot",           "zfbdrfg_week_tot" ,            "sr_week_tot" ,            "pulses_week_tot")




dds_all$run <- factor(dds_all$run, levels = desired_order)

# custom_labels <- c("hdds_day_avg" = "Mean HDDS / day", 
#                    "hdds_dish_avg" = "Mean HDDS / dish", 
#                    "hdds_meal_avg_breakfast" = "Mean HDDS / breakfast",
#                    "hdds_meal_avg_dinner" = "Mean HDDS / dinner",
#                    "hdds_meal_avg_lunch" = "Mean HDDS / lunch", 
#                    "hdds_week_avg" = "Mean HDDS / week", 
#                    "hdds_week_tot" = "Total HDDS / week",
#                    "wdds_day_avg" = "Mean WDDS / day",
#                    "wdds_dish_avg"="Mean WDDS / dish",
#                    "wdds_meal_avg_breakfast"="Mean WDDS / breakfast",
#                    "wdds_meal_avg_dinner"="Mean WDDS / dinner",
#                    "wdds_meal_avg_lunch"="Mean WDDS / lunch",
#                    "wdds_week_avg"="Mean WDDS / week",
#                    "wdds_week_tot"="Total WDDS / dinner")


dds_all_F_f<-ggplot(dds_all, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",   ncol = 5, nrow = 7)+ #labeller = labeller(group = wrap_text),
  theme(legend.position="bottom")


ggsave(here("figures", "dds_all_F_f.png"),
       dds_all_F_f,
       dpi =300,
       width = 12,
       height = 12,
       units ="in")
