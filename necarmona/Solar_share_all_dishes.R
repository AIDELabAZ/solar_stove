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



#call functions needed
source("Functions.R")

#read file 
all_villages_ing_dish_meals_l_fg<-read.csv( here("anonymized_source_files","all_villages_ing_dish_meals_l_fg.csv"))
main_hh_covariates<-read.csv(here("anonymized_source_files", "main_hh_covariates.csv"))
boiled_liquids_cov<-read.csv(here("anonymized_source_files","all_villages_boiled_liquids_cov.csv"))


shared_solar<-all_villages_ing_dish_meals_l_fg%>%
  mutate(cooking_method_r=if_else(cooking_method=="S-Soar Stove"|cooking_method=="S-Solar stove"|cooking_method=="S-Solar Stove", "Solar_stove",
                                  if_else(cooking_method=="C- Charcol"|cooking_method=="C-Charcaol"|cooking_method=="c-charcoal"|cooking_method=="c-Charcoal"|cooking_method=="C-charcoal"|cooking_method=="C-Charcoal"|cooking_method== "C-Charcol"|cooking_method=="C-Charcooal"|cooking_method=="M-Manure  C-Charcoal", "Charcoal",
                                          if_else(cooking_method=="f- firewood"|cooking_method=="F- firewood"|cooking_method=="F- Firewood"|cooking_method=="F-F-rewood"|cooking_method=="f-firewood"|cooking_method=="F-firewood"|cooking_method=="F-Firewood", "Firewood",
                                                  if_else(cooking_method=="E-Electric Stove", "Electric_stove",
                                                          if_else(cooking_method=="M-Cow dung"|cooking_method=="M-Manure"|cooking_method=="M-Manure  C-Charcoal", "Cow_dung",
                                                                  if_else(cooking_method=="Other", "Other",
                                                                          if_else(cooking_method=="", "Missing", "Missing"))))))))%>%
  group_by(cod, week, day, meal,  dish, cooking_method_r)%>%
  summarise(n_ing=n())%>%
  group_by(cod, cooking_method_r)%>%
  summarise(n_dishes=n(), total_ing=sum(n_ing))%>%
  mutate(avg_ing_meal_cook_method=total_ing/n_dishes,
         cooking_method_r_r=if_else(is.na(cooking_method_r), "Missing", cooking_method_r))%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))%>%
dplyr::select(cod, village_cor, gender, age_cal, highest_grade, solar_stove, hh_no, aas_involvement, tlu, phy_assets, cooking_method_r_r, n_dishes)%>%
  pivot_wider(names_from = cooking_method_r_r,
              values_from = n_dishes,
              values_fn = sum,
              values_fill = 0)%>%
  mutate(per_solar_dish=Solar_stove*100/(Charcoal+Firewood+Missing+Electric_stove+Solar_stove+Cow_dung+Other))%>%
  select(-c("Charcoal","Firewood","Missing","Electric_stove","Solar_stove","Cow_dung","Other"   ))%>%
 mutate(across(c(per_solar_dish), winsorize_column)) #winsorize variables

#Variable interpretation
#per_solar_dish=The share of all dishes (alt: liquids, legumes) prepared using a solar stove 

#felm model accept factor variables, so ensure to remove dummy ones and indicators not specific to each analysis
cov<-c("solar_stove" , "gender", "age_cal" , "highest_grade" ,"hh_no","tlu" , 
       "phy_assets" , "aas_involvement" ,"village_cor")

shared_solar<-shared_solar%>%
  dplyr::select(all_of(cov), per_solar_dish)

#setting reference levels
shared_solar$village_cor <- relevel(factor(shared_solar$village_cor), ref = "Lealui")
shared_solar$gender <- relevel(factor(shared_solar$gender), ref = "Women")
shared_solar$highest_grade <- relevel(factor(shared_solar$highest_grade), ref = "None")
shared_solar$aas_involvement <- relevel(factor(shared_solar$aas_involvement), ref = "None")
shared_solar$solar_stove <- relevel(factor(shared_solar$solar_stove), ref = "No")

#totals Eicker-Huber-White (EHW) robust standard errors
model_shared_solar <- felm(per_solar_dish ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                 data = shared_solar)
robust_se_shared_solar <- vcovCL(model_shared_solar, type = "HC1")
rt_shared_solar <- coeftest(model_shared_solar, vcov = robust_se_shared_solar)
tidy_r_shared_solar <- tidy(rt_shared_solar)%>%
  mutate(run="shared_solar")%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))

tidy_r_shared_solar$term <- ordered(tidy_r_shared_solar$term,
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

shared_dishes_solar<-ggplot(tidy_r_shared_solar, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  #facet_wrap(~run, scales = "free_x",  labeller = labeller(group = wrap_text), ncol = 4, nrow = 4)+
  theme(legend.position="bottom")

ggsave(here("figures", "shared_dishes_solar.png"),
       shared_dishes_solar,
       dpi =300,
       width = 6.5,
       height = 8,
       units ="in")
