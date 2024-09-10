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


#call functions needed
source("Functions.R")

#read file 
all_villages_ing_dish_meals_l_fg<-read.csv(here("anonymized_source_files","all_villages_ing_dish_meals_l_fg.csv"))%>%
  mutate(hdds=if_else(hdds=="", NA, hdds),
         wdds=if_else(wdds=="", NA, wdds))
main_hh_covariates<-read.csv(here("anonymized_source_files", "main_hh_covariates.csv"))

  
#HDDS and WDDS====
#Calculate HDDS and WDDS at different levels

dds_dish<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal,  dish)%>%
  dplyr::summarise(hdds_dish=n_distinct(hdds, na.rm=TRUE), wdds_dish=n_distinct(wdds, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_dish_avg=mean(hdds_dish), wdds_dish_avg=mean(wdds_dish))
  

dds_meal<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal)%>%
  dplyr::summarise(hdds_meal=n_distinct(hdds, na.rm=TRUE), wdds_meal=n_distinct(wdds, na.rm=TRUE))%>%
group_by(cod, meal)%>%
  dplyr::summarise(hdds_meal_avg=mean(hdds_meal), wdds_meal_avg=mean(wdds_meal))%>%
  pivot_wider(names_from = meal,
              values_from = c(hdds_meal_avg, wdds_meal_avg))

dds_day<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day)%>%
  dplyr::summarise(hdds_day=n_distinct(hdds, na.rm=TRUE), wdds_day=n_distinct(wdds, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_day_avg=mean(hdds_day), wdds_day_avg=mean(wdds_day))

dds_week_avg<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week)%>%
  dplyr::summarise(hdds_week=n_distinct(hdds, na.rm=TRUE), wdds_week=n_distinct(wdds, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_week_avg=mean(hdds_week), wdds_week_avg=mean(wdds_week))

dds_week_tot<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod)%>%
  dplyr::summarise(hdds_week_tot=n_distinct(hdds, na.rm=TRUE), wdds_week_tot=n_distinct(wdds, na.rm=TRUE))

dds<-dds_dish%>%
  left_join(dds_meal, by="cod")%>%
  left_join(dds_day, by="cod")%>%
  left_join(dds_week_avg, by="cod")%>%
  left_join(dds_week_tot, by="cod")%>%
  mutate(across(-c(cod), winsorize_column))%>% #winsorize variables
left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))


#variables interpretation
# dds_dish=The HDDS/WDDS for a given dish, calculated as the total count of all food groups represented in the dish (FAO).
# dds_meal= The HDDS/WDDS for a given meal, calculated as: The average of the HDDS over dishes in the meal
# dds_day=The HDDS/WDDS for a given day calculated as:The average of the HDDS over meals in the day.
# dds_week_avg=	The HDDS/WDDS for a given week calculated as:	The average of the HDDS over days in the week.
# dds_week_tot=The HDDS/WDDS for all six weeks calculated as 	The average of the HDDS over days in the six weeks.


#felm model accept factor variables, so ensure to remove dummy ones and indicators not specific to each analysis
cov<-c("solar_stove" , "gender", "age_cal" , "highest_grade" ,"hh_no","tlu" , 
       "phy_assets" , "aas_involvement" ,"village_cor")

hdds_dish_avg<-dds%>%
  dplyr::select(all_of(cov), hdds_dish_avg)

#setting reference levels
hdds_dish_avg$village_cor <- relevel(factor(hdds_dish_avg$village_cor), ref = "Lealui")
hdds_dish_avg$gender <- relevel(factor(hdds_dish_avg$gender), ref = "Women")
hdds_dish_avg$highest_grade <- relevel(factor(hdds_dish_avg$highest_grade), ref = "None")
hdds_dish_avg$aas_involvement <- relevel(factor(hdds_dish_avg$aas_involvement), ref = "None")
hdds_dish_avg$solar_stove <- relevel(factor(hdds_dish_avg$solar_stove), ref = "No")


model_hdds_dish_avg <- felm(hdds_dish_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                           data = hdds_dish_avg)
robust_se_hdds_dish_avg <- vcovCL(model_hdds_dish_avg, type = "CR")
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


model_wdds_dish_avg <- felm(wdds_dish_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = wdds_dish_avg)
robust_se_wdds_dish_avg <- vcovCL(model_wdds_dish_avg, type = "CR")
rt_wdds_dish_avg <- coeftest(model_wdds_dish_avg, vcov = robust_se_wdds_dish_avg)
tidy_r_wdds_dish_avg <- tidy(rt_wdds_dish_avg)%>%
  mutate(run="wdds_dish_avg")


hdds_meal_avg_breakfast<-dds%>%
  dplyr::select(all_of(cov), hdds_meal_avg_breakfast)

#setting reference levels
hdds_meal_avg_breakfast$village_cor <- relevel(factor(hdds_meal_avg_breakfast$village_cor), ref = "Lealui")
hdds_meal_avg_breakfast$gender <- relevel(factor(hdds_meal_avg_breakfast$gender), ref = "Women")
hdds_meal_avg_breakfast$highest_grade <- relevel(factor(hdds_meal_avg_breakfast$highest_grade), ref = "None")
hdds_meal_avg_breakfast$aas_involvement <- relevel(factor(hdds_meal_avg_breakfast$aas_involvement), ref = "None")
hdds_meal_avg_breakfast$solar_stove <- relevel(factor(hdds_meal_avg_breakfast$solar_stove), ref = "No")


model_hdds_meal_avg_breakfast <- felm(hdds_meal_avg_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = hdds_meal_avg_breakfast)
robust_se_hdds_meal_avg_breakfast <- vcovCL(model_hdds_meal_avg_breakfast, type = "CR")
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


model_hdds_meal_avg_dinner <- felm(hdds_meal_avg_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                      data = hdds_meal_avg_dinner)
robust_se_hdds_meal_avg_dinner <- vcovCL(model_hdds_meal_avg_dinner, type = "CR")
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


model_hdds_meal_avg_lunch <- felm(hdds_meal_avg_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                   data = hdds_meal_avg_lunch)
robust_se_hdds_meal_avg_lunch <- vcovCL(model_hdds_meal_avg_lunch, type = "CR")
rt_hdds_meal_avg_lunch <- coeftest(model_hdds_meal_avg_lunch, vcov = robust_se_hdds_meal_avg_lunch)
tidy_r_hdds_meal_avg_lunch <- tidy(rt_hdds_meal_avg_lunch)%>%
  mutate(run="hdds_meal_avg_lunch")

wdds_meal_avg_breakfast<-dds%>%
  dplyr::select(all_of(cov), wdds_meal_avg_breakfast)

#setting reference levels
wdds_meal_avg_breakfast$village_cor <- relevel(factor(wdds_meal_avg_breakfast$village_cor), ref = "Lealui")
wdds_meal_avg_breakfast$gender <- relevel(factor(wdds_meal_avg_breakfast$gender), ref = "Women")
wdds_meal_avg_breakfast$highest_grade <- relevel(factor(wdds_meal_avg_breakfast$highest_grade), ref = "None")
wdds_meal_avg_breakfast$aas_involvement <- relevel(factor(wdds_meal_avg_breakfast$aas_involvement), ref = "None")
wdds_meal_avg_breakfast$solar_stove <- relevel(factor(wdds_meal_avg_breakfast$solar_stove), ref = "No")


model_wdds_meal_avg_breakfast <- felm(wdds_meal_avg_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                  data = wdds_meal_avg_breakfast)
robust_se_wdds_meal_avg_breakfast <- vcovCL(model_wdds_meal_avg_breakfast, type = "CR")
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


model_wdds_meal_avg_dinner <- felm(wdds_meal_avg_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                      data = wdds_meal_avg_dinner)
robust_se_wdds_meal_avg_dinner <- vcovCL(model_wdds_meal_avg_dinner, type = "CR")
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


model_wdds_meal_avg_lunch <- felm(wdds_meal_avg_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                   data = wdds_meal_avg_lunch)
robust_se_wdds_meal_avg_lunch <- vcovCL(model_wdds_meal_avg_lunch, type = "CR")
rt_wdds_meal_avg_lunch <- coeftest(model_wdds_meal_avg_lunch, vcov = robust_se_wdds_meal_avg_lunch)
tidy_r_wdds_meal_avg_lunch <- tidy(rt_wdds_meal_avg_lunch)%>%
  mutate(run="wdds_meal_avg_lunch")


hdds_day_avg<-dds%>%
  dplyr::select(all_of(cov), hdds_day_avg)

#setting reference levels
hdds_day_avg$village_cor <- relevel(factor(hdds_day_avg$village_cor), ref = "Lealui")
hdds_day_avg$gender <- relevel(factor(hdds_day_avg$gender), ref = "Women")
hdds_day_avg$highest_grade <- relevel(factor(hdds_day_avg$highest_grade), ref = "None")
hdds_day_avg$aas_involvement <- relevel(factor(hdds_day_avg$aas_involvement), ref = "None")
hdds_day_avg$solar_stove <- relevel(factor(hdds_day_avg$solar_stove), ref = "No")


model_hdds_day_avg <- felm(hdds_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                  data = hdds_day_avg)
robust_se_hdds_day_avg <- vcovCL(model_hdds_day_avg, type = "CR")
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


model_wdds_day_avg <- felm(wdds_day_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                           data = wdds_day_avg)
robust_se_wdds_day_avg <- vcovCL(model_wdds_day_avg, type = "CR")
rt_wdds_day_avg <- coeftest(model_wdds_day_avg, vcov = robust_se_wdds_day_avg)
tidy_r_wdds_day_avg <- tidy(rt_wdds_day_avg)%>%
  mutate(run="wdds_day_avg")


hdds_week_avg<-dds%>%
  dplyr::select(all_of(cov), hdds_week_avg)

#setting reference levels
hdds_week_avg$village_cor <- relevel(factor(hdds_week_avg$village_cor), ref = "Lealui")
hdds_week_avg$gender <- relevel(factor(hdds_week_avg$gender), ref = "Women")
hdds_week_avg$highest_grade <- relevel(factor(hdds_week_avg$highest_grade), ref = "None")
hdds_week_avg$aas_involvement <- relevel(factor(hdds_week_avg$aas_involvement), ref = "None")
hdds_week_avg$solar_stove <- relevel(factor(hdds_week_avg$solar_stove), ref = "No")


model_hdds_week_avg <- felm(hdds_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                           data = hdds_week_avg)
robust_se_hdds_week_avg <- vcovCL(model_hdds_week_avg, type = "HC1")
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


model_wdds_week_avg <- felm(wdds_week_avg ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = wdds_week_avg)
robust_se_wdds_week_avg <- vcovCL(model_wdds_week_avg, type = "HC1")
rt_wdds_week_avg <- coeftest(model_wdds_week_avg, vcov = robust_se_wdds_week_avg)
tidy_r_wdds_week_avg <- tidy(rt_wdds_week_avg)%>%
  mutate(run="wdds_week_avg")

hdds_week_tot<-dds%>%
  dplyr::select(all_of(cov), hdds_week_tot)

#setting reference levels
hdds_week_tot$village_cor <- relevel(factor(hdds_week_tot$village_cor), ref = "Lealui")
hdds_week_tot$gender <- relevel(factor(hdds_week_tot$gender), ref = "Women")
hdds_week_tot$highest_grade <- relevel(factor(hdds_week_tot$highest_grade), ref = "None")
hdds_week_tot$aas_involvement <- relevel(factor(hdds_week_tot$aas_involvement), ref = "None")
hdds_week_tot$solar_stove <- relevel(factor(hdds_week_tot$solar_stove), ref = "No")


model_hdds_week_tot <- felm(hdds_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = hdds_week_tot)
robust_se_hdds_week_tot <- vcovCL(model_hdds_week_tot, type = "HC1")
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


model_wdds_week_tot <- felm(wdds_week_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = wdds_week_tot)
robust_se_wdds_week_tot <- vcovCL(model_wdds_week_tot, type = "HC1")
rt_wdds_week_tot <- coeftest(model_wdds_week_tot, vcov = robust_se_wdds_week_tot)
tidy_r_wdds_week_tot <- tidy(rt_wdds_week_tot)%>%
  mutate(run="wdds_week_tot")


dds_all<-rbind(tidy_r_hdds_dish_avg,tidy_r_wdds_dish_avg,tidy_r_hdds_meal_avg_breakfast,tidy_r_hdds_meal_avg_dinner,
tidy_r_hdds_meal_avg_lunch,tidy_r_wdds_meal_avg_breakfast,tidy_r_wdds_meal_avg_dinner,tidy_r_wdds_meal_avg_lunch,
tidy_r_hdds_day_avg,tidy_r_wdds_day_avg,tidy_r_hdds_week_avg,tidy_r_wdds_week_avg,tidy_r_hdds_week_tot, tidy_r_wdds_week_tot)%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))

dds_all_F<-dds_all%>%
  bind_rows(data.frame(run = c("Dummy1", "Dummy2"), term = "", estimate = NA,std.error=NA, statistic=NA, p.value =NA))



dds_all_F$term <- ordered(dds_all_F$term,
                           levels = c(
                             "hh_no",
                             "tlu",
                             "phy_assets",
                             "highest_gradePrimary" ,
                             "highest_gradeSecondary",
                             "highest_gradeHigher",
                             "genderMen",
                             "age_cal",
                             "aas_involvementagriculture",
                             "aas_involvementnutrition",
                             "aas_involvementAg_nut",
                             "solar_stoveYes"))



desired_order <- c("wdds_day_avg", "wdds_week_avg" ,"wdds_week_tot", "Dummy2",
                   "hdds_day_avg" ,  "hdds_week_avg"  ,"hdds_week_tot","Dummy1",
                   "wdds_dish_avg","wdds_meal_avg_breakfast", "wdds_meal_avg_lunch", "wdds_meal_avg_dinner",
                   "hdds_dish_avg", "hdds_meal_avg_breakfast", "hdds_meal_avg_lunch", "hdds_meal_avg_dinner")
dds_all_F$run <- factor(dds_all_F$run, levels = desired_order)

custom_labels <- c("hdds_day_avg" = "Mean HDDS / day", 
                   "hdds_dish_avg" = "Mean HDDS / dish", 
                   "hdds_meal_avg_breakfast" = "Mean HDDS / breakfast",
                   "hdds_meal_avg_dinner" = "Mean HDDS / dinner",
                   "hdds_meal_avg_lunch" = "Mean HDDS / lunch", 
                   "hdds_week_avg" = "Mean HDDS / week", 
                   "hdds_week_tot" = "Total HDDS / week",
                   "wdds_day_avg" = "Mean WDDS / day",
                   "wdds_dish_avg"="Mean WDDS / dish",
                   "wdds_meal_avg_breakfast"="Mean WDDS / breakfast",
                   "wdds_meal_avg_dinner"="Mean WDDS / dinner",
                   "wdds_meal_avg_lunch"="Mean WDDS / lunch",
                   "wdds_week_avg"="Mean WDDS / week",
                   "wdds_week_tot"="Total WDDS / dinner",
                   "Dummy2"="",
                   "Dummy1"="")


dds_all_F_f<-ggplot(dds_all_F, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",  labeller = labeller(group = wrap_text), ncol = 4, nrow = 4)+
  theme(legend.position="bottom")


ggsave(here("figures", "dds_all_F_f.png"),
       dds_all_F_f,
       dpi =300,
       width = 7,
       height = 9,
       units ="in")



#getting the total days with recorded information to calculate means
record_days<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week)%>%
  summarise(day_T=max(day))%>%
  group_by(cod)%>%
  summarise(total_days= sum(day_T), total_weeks=n())

# legumes frequency ====
legumes<-all_villages_ing_dish_meals_l_fg%>%

  filter(Food.group=="Pulses (beans, peas and lentils)")%>%
  group_by(cod, week, day, meal, dish)%>% #two dishes in the same meal can have legumes and have been cooked with different methods. 
  dplyr::summarise(freq=n_distinct(Food.group))%>%
  group_by(cod, week, day)%>%
  dplyr::summarise(freq_tot=sum(freq))%>%
  group_by(cod)%>%
  dplyr::summarise(leg_tot=sum(freq_tot))%>%
  left_join(record_days, by ="cod")%>%
  mutate(leg_day=leg_tot/total_days,
         leg_week=leg_tot/total_weeks)%>%
  mutate(across(c(leg_day,leg_week, leg_tot), winsorize_column))#winsorize variables

# leg_day=	The number of times legumes were cooked in a given day. (no dishes containing legumes per day)
# leg_week=	The number of times legumes were cooked in a given week.
# leg_tot=	The number of times legumes were cooked over all six weeks.
# leguminous volumes are ignored due to issues with the units provided. 

##integrate number of dishes with hh characteristics and covariables====
legumes_6w<-legumes%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))

leg_day<-legumes_6w%>%
  dplyr::select(all_of(cov), leg_day)

#setting reference levels
leg_day$village_cor <- relevel(factor(leg_day$village_cor), ref = "Lealui")
leg_day$gender <- relevel(factor(leg_day$gender), ref = "Women")
leg_day$highest_grade <- relevel(factor(leg_day$highest_grade), ref = "None")
leg_day$aas_involvement <- relevel(factor(leg_day$aas_involvement), ref = "None")
leg_day$solar_stove <- relevel(factor(leg_day$solar_stove), ref = "No")


model_leg_day <- felm(leg_day ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = leg_day)
robust_se_leg_day <- vcovCL(model_leg_day, type = "CR")
rt_leg_day <- coeftest(model_leg_day, vcov = robust_se_leg_day)
tidy_r_leg_day <- tidy(rt_leg_day)%>%
  mutate(run="leg_day")


leg_week<-legumes_6w%>%
  dplyr::select(all_of(cov), leg_week)

#setting reference levels
leg_week$village_cor <- relevel(factor(leg_week$village_cor), ref = "Lealui")
leg_week$gender <- relevel(factor(leg_week$gender), ref = "Women")
leg_week$highest_grade <- relevel(factor(leg_week$highest_grade), ref = "None")
leg_week$aas_involvement <- relevel(factor(leg_week$aas_involvement), ref = "None")
leg_week$solar_stove <- relevel(factor(leg_week$solar_stove), ref = "No")


model_leg_week <- felm(leg_week ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                      data = leg_week)
robust_se_leg_week <- vcovCL(model_leg_week, type = "CR")
rt_leg_week <- coeftest(model_leg_week, vcov = robust_se_leg_week)
tidy_r_leg_week <- tidy(rt_leg_week)%>%
  mutate(run="leg_week")

leg_tot<-legumes_6w%>%
  dplyr::select(all_of(cov), leg_tot)

#setting reference levels
leg_tot$village_cor <- relevel(factor(leg_tot$village_cor), ref = "Lealui")
leg_tot$gender <- relevel(factor(leg_tot$gender), ref = "Women")
leg_tot$highest_grade <- relevel(factor(leg_tot$highest_grade), ref = "None")
leg_tot$aas_involvement <- relevel(factor(leg_tot$aas_involvement), ref = "None")
leg_tot$solar_stove <- relevel(factor(leg_tot$solar_stove), ref = "No")


model_leg_tot <- felm(leg_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                       data = leg_tot)
robust_se_leg_tot <- vcovCL(model_leg_tot, type = "HC1")
rt_leg_tot <- coeftest(model_leg_tot, vcov = robust_se_leg_tot)
tidy_r_leg_tot <- tidy(rt_leg_tot)%>%
  mutate(run="leg_tot")

leg_freq<-rbind(tidy_r_leg_day, tidy_r_leg_week, tidy_r_leg_tot)%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))



leg_freq$term <- ordered(leg_freq$term,
                          levels = c(
                            "hh_no",
                            "tlu",
                            "phy_assets",
                            "highest_gradePrimary" ,
                            "highest_gradeSecondary",
                            "highest_gradeHigher",
                            "genderMen",
                            "age_cal",
                            "aas_involvementagriculture",
                            "aas_involvementnutrition",
                            "aas_involvementAg_nut",
                            "solar_stoveYes"))



desired_order <- c("leg_day", "leg_week" ,"leg_tot")
leg_freq$run <- factor(leg_freq$run, levels = desired_order)

custom_labels <- c("leg_day" = "Times legumes cooked / day", 
                   "leg_week" = "Times legumes cooked / week", 
                   "leg_tot" = "Times legumes cooked  / 6weeks")


leg_freq_f<-ggplot(leg_freq, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",  labeller = labeller(group = wrap_text), ncol = 3, nrow = 1)+
  theme(legend.position="bottom")


ggsave(here("figures", "leg_freq_f.png"),
       leg_freq_f,
       dpi =300,
       width = 7,
       height = 9,
       units ="in")

#species richness====

sr_dish<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal,  dish)%>%
  dplyr::summarise(no_spp=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(sr_dish=mean(no_spp))%>%
  mutate(across(c(sr_dish), winsorize_column)) #winsorize variables

sr_meal<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal)%>%
  dplyr::summarise(no_spp=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(sr_meal=mean(no_spp))%>%
  mutate(across(c(sr_meal), winsorize_column))

sr_day<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day)%>%
  dplyr::summarise(no_spp=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(sr_day=mean(no_spp))%>%
  mutate(across(c(sr_day), winsorize_column))

sr_week<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week)%>%
  dplyr::summarise(no_spp=n_distinct(scientific_name, na.rm=TRUE))%>%
  group_by(cod)%>%
  dplyr::summarise(sr_week=mean(no_spp))%>%
  mutate(across(c(sr_week), winsorize_column))

sr_tot<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod)%>%
  dplyr::summarise(sr_tot=n_distinct(scientific_name))%>%
  mutate(across(c(sr_tot), winsorize_column)) #winsorize variables

sr<-cbind(sr_dish, sr_meal[,-1], sr_day[,-1],sr_week[,-1], sr_tot[,-1] )

#variables interpretation
# sr_dish=	The SR for a given dish, calculated as a count of the number of species used as ingredients in the dish.
# sr_meal=The SR for a given meal, calculated as a count of the number of species used as ingredients in the meal.
# sr_day=	The SR for a given day, calculated as a count of the number of species used as ingredients in all meals that day.
# sr_week= The SR for a given week, calculated as a count of the number of species used as ingredients in all meals that week.
# sr_tot=	The SR for the six weeks, calculated as a count of the number of species used as ingredients in all meals over the six weeks.

##integrate number of dishes with hh characteristics and covariables====
sr_cov<-sr%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))

sr_dish<-sr_cov%>%
  dplyr::select(all_of(cov), sr_dish)

#setting reference levels
sr_dish$village_cor <- relevel(factor(sr_dish$village_cor), ref = "Lealui")
sr_dish$gender <- relevel(factor(sr_dish$gender), ref = "Women")
sr_dish$highest_grade <- relevel(factor(sr_dish$highest_grade), ref = "None")
sr_dish$aas_involvement <- relevel(factor(sr_dish$aas_involvement), ref = "None")
sr_dish$solar_stove <- relevel(factor(sr_dish$solar_stove), ref = "No")

model_sr_dish <- felm(sr_dish ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                      data = sr_dish)
robust_se_sr_dish <- vcovCL(model_sr_dish, type = "CR")
rt_sr_dish <- coeftest(model_sr_dish, vcov = robust_se_sr_dish)
tidy_r_sr_dish <- tidy(rt_sr_dish)%>%
  mutate(run="sr_dish")

sr_meal<-sr_cov%>%
  dplyr::select(all_of(cov), sr_meal)

#setting reference levels
sr_meal$village_cor <- relevel(factor(sr_meal$village_cor), ref = "Lealui")
sr_meal$gender <- relevel(factor(sr_meal$gender), ref = "Women")
sr_meal$highest_grade <- relevel(factor(sr_meal$highest_grade), ref = "None")
sr_meal$aas_involvement <- relevel(factor(sr_meal$aas_involvement), ref = "None")
sr_meal$solar_stove <- relevel(factor(sr_meal$solar_stove), ref = "No")

model_sr_meal <- felm(sr_meal ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                      data = sr_meal)
robust_se_sr_meal <- vcovCL(model_sr_meal, type = "CR")
rt_sr_meal <- coeftest(model_sr_meal, vcov = robust_se_sr_meal)
tidy_r_sr_meal <- tidy(rt_sr_meal)%>%
  mutate(run="sr_meal")

sr_day<-sr_cov%>%
  dplyr::select(all_of(cov), sr_day)

#setting reference levels
sr_day$village_cor <- relevel(factor(sr_day$village_cor), ref = "Lealui")
sr_day$gender <- relevel(factor(sr_day$gender), ref = "Women")
sr_day$highest_grade <- relevel(factor(sr_day$highest_grade), ref = "None")
sr_day$aas_involvement <- relevel(factor(sr_day$aas_involvement), ref = "None")
sr_day$solar_stove <- relevel(factor(sr_day$solar_stove), ref = "No")

model_sr_day <- felm(sr_day ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                      data = sr_day)
robust_se_sr_day <- vcovCL(model_sr_day, type = "CR")
rt_sr_day <- coeftest(model_sr_day, vcov = robust_se_sr_day)
tidy_r_sr_day <- tidy(rt_sr_day)%>%
  mutate(run="sr_day")

sr_week<-sr_cov%>%
  dplyr::select(all_of(cov), sr_week)

#setting reference levels
sr_week$village_cor <- relevel(factor(sr_week$village_cor), ref = "Lealui")
sr_week$gender <- relevel(factor(sr_week$gender), ref = "Women")
sr_week$highest_grade <- relevel(factor(sr_week$highest_grade), ref = "None")
sr_week$aas_involvement <- relevel(factor(sr_week$aas_involvement), ref = "None")
sr_week$solar_stove <- relevel(factor(sr_week$solar_stove), ref = "No")

model_sr_week <- felm(sr_week ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                     data = sr_week)
robust_se_sr_week <- vcovCL(model_sr_week, type = "CR")
rt_sr_week <- coeftest(model_sr_week, vcov = robust_se_sr_week)
tidy_r_sr_week <- tidy(rt_sr_week)%>%
  mutate(run="sr_week")

sr_tot<-sr_cov%>%
  dplyr::select(all_of(cov), sr_tot)

#setting reference levels
sr_tot$village_cor <- relevel(factor(sr_tot$village_cor), ref = "Lealui")
sr_tot$gender <- relevel(factor(sr_tot$gender), ref = "Women")
sr_tot$highest_grade <- relevel(factor(sr_tot$highest_grade), ref = "None")
sr_tot$aas_involvement <- relevel(factor(sr_tot$aas_involvement), ref = "None")
sr_tot$solar_stove <- relevel(factor(sr_tot$solar_stove), ref = "No")

model_sr_tot <- felm(sr_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                      data = sr_tot)
robust_se_sr_tot <- vcovCL(model_sr_tot, type = "HC1")
rt_sr_tot <- coeftest(model_sr_tot, vcov = robust_se_sr_tot)
tidy_r_sr_tot <- tidy(rt_sr_tot)%>%
  mutate(run="sr_tot")

sr_div=rbind(tidy_r_sr_dish, tidy_r_sr_meal, tidy_r_sr_day,tidy_r_sr_week, tidy_r_sr_tot)%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))


sr_div$term <- ordered(sr_div$term,
                         levels = c(
                           "hh_no",
                           "tlu",
                           "phy_assets",
                           "highest_gradePrimary" ,
                           "highest_gradeSecondary",
                           "highest_gradeHigher",
                           "genderMen",
                           "age_cal",
                           "aas_involvementagriculture",
                           "aas_involvementnutrition",
                           "aas_involvementAg_nut",
                           "solar_stoveYes"))



desired_order <- c("sr_dish", "sr_meal" ,"sr_day","sr_week" , "sr_tot")
sr_div$run <- factor(sr_div$run, levels = desired_order)

custom_labels <- c("sr_dish" = "Sp. consumed / dish", 
                   "sr_meal" = "Sp. consumed / meal", 
                   "sr_day" = "Sp. consumed /  day", 
                   "sr_week" = "Sp. consumed / week",
                   "sr_tot" = "Sp. consumed / 6weeks")



sr_div_f<-ggplot(sr_div, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",  labeller = labeller(group = wrap_text), ncol = 5, nrow = 1)+
  theme(legend.position="bottom")


ggsave(here("figures", "sr_div_f.png"),
       sr_div_f,
       dpi =300,
       width = 7,
       height = 9,
       units ="in")

