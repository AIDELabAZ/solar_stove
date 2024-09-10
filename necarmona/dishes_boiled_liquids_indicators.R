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
all_villages_ing_dish_meals_l_fg<-read.csv( here("anonymized_source_files","all_villages_ing_dish_meals_l_fg.csv"))
main_hh_covariates<-read.csv(here("anonymized_source_files", "main_hh_covariates.csv"))
boiled_liquids_cov<-read.csv(here("anonymized_source_files","all_villages_boiled_liquids_cov.csv"))

#Dishes Cooked ====

#Calculate the number of average number of dishes cooked in each meal during the whole period

dish_meal<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal)%>%
  dplyr::summarise(no_dishes=as.numeric(max(dish)))%>%
  group_by(cod, meal)%>%
  dplyr::summarise(avg_no_dishes=mean(no_dishes))%>%
  pivot_wider(names_from = meal,
              names_prefix ="dish_",
              values_from = avg_no_dishes   ,
              values_fill = 0)

dish_tot<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day, meal, dish)%>%
  dplyr::summarise(no_dishes=n())%>%
  group_by(cod )%>%
  dplyr::summarise(dish_day=mean(no_dishes))

# dates_r<-all_villages_ing_dish_meals_l_fg%>%
#   group_by(cod)%>%
#   dplyr::summarise(week_r=max(week), day_r=max(day))%>%
#   mutate(days_r=week_r*day_r)

skip_meal<-all_villages_ing_dish_meals_l_fg%>%
group_by(cod, week,  day, meal)%>%
  summarise(dish_meal=max(dish))%>%
  group_by(cod, meal)%>%
  summarise(no_meal=n())%>%
  left_join(dates_r, by="cod")%>%
  mutate(skipped_meal=days_r-no_meal)%>%
  dplyr::select(cod, meal, skipped_meal)%>%
  pivot_wider(names_from = meal,
              names_prefix ="skipped_",
              values_from = skipped_meal,
              values_fill = 0)%>%
  mutate(skip_tot=(skipped_breakfast+skipped_dinner+skipped_lunch))

dish_meals<-dish_meal%>%
  left_join(dish_tot, by="cod" )%>%
  left_join(skip_meal, by="cod")%>%
  mutate(across(c(dish_breakfast,dish_dinner,dish_lunch,dish_day,skipped_breakfast,skipped_dinner,skipped_lunch,skip_tot), winsorize_column))  

#variable explanation
# dish_breakfast=ii.	Average number of dishes in breakfast over the six weeks
#   dish_dinner=iv.	Average number of dishes in dinner over the six weeks
#   dish_lunch=iii.	Average number of dishes in lunch over the six weeks
#   dish_days=avg number of dishes cooked on average in a day
#   skipped_breakfast=ii.	Total number of breakfast meals skipped over all six weeks.
#   skipped_dinner=iv.	Total number of dinner meals skipped over all six weeks.
#   skipped_lunch=iii.	Total number of lunch meals skipped over all six weeks.
#   skip_tot=i.	Total number of meals skipped over all six weeks

##integrate number of dishes with hh characteristics and covariables====
dish_meals_cov<-dish_meals%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))

#dish_breakfast====

dish_breakfast<-dish_meals_cov%>%
  dplyr::select(all_of(cov), dish_breakfast)


dish_breakfast$village_cor <- relevel(factor(dish_breakfast$village_cor), ref = "Lealui")
dish_breakfast$gender <- relevel(factor(dish_breakfast$gender), ref = "Women")
dish_breakfast$highest_grade <- relevel(factor(dish_breakfast$highest_grade), ref = "None")
dish_breakfast$aas_involvement <- relevel(factor(dish_breakfast$aas_involvement), ref = "None")
dish_breakfast$solar_stove <- relevel(factor(dish_breakfast$solar_stove), ref = "No")


model_dish_breakfast <- felm(dish_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                          data = dish_breakfast)
robust_se_dish_breakfast <- vcovCL(model_dish_breakfast, type = "HC1")

rt_dish_breakfast <- coeftest(model_dish_breakfast, vcov = robust_se_dish_breakfast)
tidy_r_dish_breakfast <- tidy(rt_dish_breakfast)%>%
  mutate(run="dish_breakfast")

#dish_dinner====
dish_dinner<-dish_meals_cov%>%
  dplyr::select(all_of(cov), dish_dinner)

dish_dinner$village_cor <- relevel(factor(dish_dinner$village_cor), ref = "Lealui")
dish_dinner$gender <- relevel(factor(dish_dinner$gender), ref = "Women")
dish_dinner$highest_grade <- relevel(factor(dish_dinner$highest_grade), ref = "None")
dish_dinner$aas_involvement <- relevel(factor(dish_dinner$aas_involvement), ref = "None")
dish_dinner$solar_stove <- relevel(factor(dish_dinner$solar_stove), ref = "No")


model_dish_dinner <- felm(dish_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                             data = dish_dinner)
robust_se_dish_dinner <- vcovCL(model_dish_dinner, type = "HC1")

rt_dish_dinner <- coeftest(model_dish_dinner, vcov = robust_se_dish_dinner)
tidy_r_dish_dinner <- tidy(rt_dish_dinner)%>%
  mutate(run="dish_dinner")

#dish_lunch====
dish_lunch<-dish_meals_cov%>%
  dplyr::select(all_of(cov), dish_lunch)

dish_lunch$village_cor <- relevel(factor(dish_lunch$village_cor), ref = "Lealui")
dish_lunch$gender <- relevel(factor(dish_lunch$gender), ref = "Women")
dish_lunch$highest_grade <- relevel(factor(dish_lunch$highest_grade), ref = "None")
dish_lunch$aas_involvement <- relevel(factor(dish_lunch$aas_involvement), ref = "None")
dish_lunch$solar_stove <- relevel(factor(dish_lunch$solar_stove), ref = "No")


model_dish_lunch <- felm(dish_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                          data = dish_lunch)
robust_se_dish_lunch <- vcovCL(model_dish_lunch, type = "HC1")

rt_dish_lunch <- coeftest(model_dish_lunch, vcov = robust_se_dish_lunch)
tidy_r_dish_lunch <- tidy(rt_dish_lunch)%>%
  mutate(run="dish_lunch")

#dish_days====
dish_days<-dish_meals_cov%>%
  dplyr::select(all_of(cov), dish_day)

dish_days$village_cor <- relevel(factor(dish_days$village_cor), ref = "Lealui")
dish_days$gender <- relevel(factor(dish_days$gender), ref = "Women")
dish_days$highest_grade <- relevel(factor(dish_days$highest_grade), ref = "None")
dish_days$aas_involvement <- relevel(factor(dish_days$aas_involvement), ref = "None")
dish_days$solar_stove <- relevel(factor(dish_days$solar_stove), ref = "No")


model_dish_days <- felm(dish_day ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                         data = dish_days)
robust_se_dish_days <- vcovCL(model_dish_days, type = "HC1")

rt_dish_days <- coeftest(model_dish_days, vcov = robust_se_dish_days)
tidy_r_dish_days <- tidy(rt_dish_days)%>%
  mutate(run="dish_days")

#skipped_breakfast====
skipped_breakfast<-dish_meals_cov%>%
  dplyr::select(all_of(cov), skipped_breakfast)

skipped_breakfast$village_cor <- relevel(factor(skipped_breakfast$village_cor), ref = "Lealui")
skipped_breakfast$gender <- relevel(factor(skipped_breakfast$gender), ref = "Women")
skipped_breakfast$highest_grade <- relevel(factor(skipped_breakfast$highest_grade), ref = "None")
skipped_breakfast$aas_involvement <- relevel(factor(skipped_breakfast$aas_involvement), ref = "None")
skipped_breakfast$solar_stove <- relevel(factor(skipped_breakfast$solar_stove), ref = "No")


model_skipped_breakfast <- felm(skipped_breakfast ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                        data = skipped_breakfast)
robust_se_skipped_breakfast <- vcovCL(model_skipped_breakfast, type = "HC1")

rt_skipped_breakfast <- coeftest(model_skipped_breakfast, vcov = robust_se_skipped_breakfast)
tidy_r_skipped_breakfast <- tidy(rt_skipped_breakfast)%>%
  mutate(run="skipped_breakfast")

#skipped_dinner====
skipped_dinner<-dish_meals_cov%>%
  dplyr::select(all_of(cov), skipped_dinner)

skipped_dinner$village_cor <- relevel(factor(skipped_dinner$village_cor), ref = "Lealui")
skipped_dinner$gender <- relevel(factor(skipped_dinner$gender), ref = "Women")
skipped_dinner$highest_grade <- relevel(factor(skipped_dinner$highest_grade), ref = "None")
skipped_dinner$aas_involvement <- relevel(factor(skipped_dinner$aas_involvement), ref = "None")
skipped_dinner$solar_stove <- relevel(factor(skipped_dinner$solar_stove), ref = "No")


model_skipped_dinner <- felm(skipped_dinner ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                data = skipped_dinner)
robust_se_skipped_dinner <- vcovCL(model_skipped_dinner, type = "HC1")

rt_skipped_dinner <- coeftest(model_skipped_dinner, vcov = robust_se_skipped_dinner)
tidy_r_skipped_dinner <- tidy(rt_skipped_dinner)%>%
  mutate(run="skipped_dinner")

#skipped_lunch====
skipped_lunch<-dish_meals_cov%>%
  dplyr::select(all_of(cov), skipped_lunch)

skipped_lunch$village_cor <- relevel(factor(skipped_lunch$village_cor), ref = "Lealui")
skipped_lunch$gender <- relevel(factor(skipped_lunch$gender), ref = "Women")
skipped_lunch$highest_grade <- relevel(factor(skipped_lunch$highest_grade), ref = "None")
skipped_lunch$aas_involvement <- relevel(factor(skipped_lunch$aas_involvement), ref = "None")
skipped_lunch$solar_stove <- relevel(factor(skipped_lunch$solar_stove), ref = "No")


model_skipped_lunch <- felm(skipped_lunch ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                             data = skipped_lunch)
robust_se_skipped_lunch <- vcovCL(model_skipped_lunch, type = "HC1")

rt_skipped_lunch <- coeftest(model_skipped_lunch, vcov = robust_se_skipped_lunch)
tidy_r_skipped_lunch <- tidy(rt_skipped_lunch)%>%
  mutate(run="skipped_lunch")

#skip_tot====
skip_tot<-dish_meals_cov%>%
  dplyr::select(all_of(cov), skip_tot)

skip_tot$village_cor <- relevel(factor(skip_tot$village_cor), ref = "Lealui")
skip_tot$gender <- relevel(factor(skip_tot$gender), ref = "Women")
skip_tot$highest_grade <- relevel(factor(skip_tot$highest_grade), ref = "None")
skip_tot$aas_involvement <- relevel(factor(skip_tot$aas_involvement), ref = "None")
skip_tot$solar_stove <- relevel(factor(skip_tot$solar_stove), ref = "No")


model_skip_tot <- felm(skip_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                            data = skip_tot)
robust_se_skip_tot <- vcovCL(model_skip_tot, type = "HC1")

rt_skip_tot <- coeftest(model_skip_tot, vcov = robust_se_skip_tot)
tidy_r_skip_tot <- tidy(rt_skip_tot)%>%
  mutate(run="skip_tot")

#merging all skipped dish/meals====
dish_meals_skipped<-rbind(tidy_r_dish_breakfast,tidy_r_dish_dinner,tidy_r_dish_lunch,tidy_r_dish_days,tidy_r_skipped_breakfast,
                          tidy_r_skipped_dinner,tidy_r_skipped_lunch,tidy_r_skip_tot)%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))

dish_meals_skipped$term <- ordered(dish_meals_skipped$term,
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


dish_meals_skipped$run <- ordered(dish_meals_skipped$run,
                                  levels = c("dish_breakfast", "skipped_breakfast", "dish_lunch", "skipped_lunch", "dish_dinner",
                                             "skipped_dinner","dish_days","skip_tot"))

dish_meals_skipped_f<-ggplot(dish_meals_skipped, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",   ncol = 2, nrow = 4)+
  theme(legend.position="bottom")


ggsave(here("figures", "dish_meals_skipped.png"),
       dish_meals_skipped_f,
       dpi =300,
       width = 7,
       height = 9,
       units ="in")


#Boiled liquids=====
liq_day<-boiled_liquids_cov%>%
  group_by(cod,week, day )%>%
  summarise(liq_fre_day=n(), liq_qty_day=mean(quantity_lt))%>%
  group_by(cod)%>%
  summarise(liq_qty_day=mean(liq_qty_day), liq_fre_day=mean(liq_fre_day))
  
liq_week<-boiled_liquids_cov%>%
  group_by(cod,week )%>%
  summarise(liq_fre_week1=n(), liq_qty_week1=mean(quantity_lt))%>%
  group_by(cod)%>%
  summarise(liq_qty_week=mean(liq_qty_week1), liq_fre_week=mean(liq_fre_week1))

liq_tot<-boiled_liquids_cov%>%
  group_by(cod )%>%
  summarise(liq_fre_tot=n(), liq_qty_tot=sum(quantity_lt))

liq<-cbind(liq_day,liq_week[,-1],liq_tot[,-1])%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))%>%
  mutate(across(c(liq_qty_day,
                  liq_fre_day,
                  liq_qty_week,
                  liq_fre_week,
                  liq_fre_tot,
                  liq_qty_tot), winsorize_column)) 

#Variable explanation
# liq_qty_day=iv.	The volume of liquid boiled in a given day
# liq_fre_day=i.	The number of times liquid was boiled in a given day
# liq_qty_week=v.	The volume of liquid boiled in a given week.
# liq_fre_week=ii.	The number of times liquid was boiled in a given week
# liq_fre_tot=iii.	The number of times liquid was boiled over all six weeks.
# liq_qty_tot=vi.	The volume of liquid boiled over all six weeks.

#liq_qty_day====
liq_qty_day<-liq%>%
  dplyr::select(all_of(cov), liq_qty_day)

liq_qty_day$village_cor <- relevel(factor(liq_qty_day$village_cor), ref = "Lealui")
liq_qty_day$gender <- relevel(factor(liq_qty_day$gender), ref = "Women")
liq_qty_day$highest_grade <- relevel(factor(liq_qty_day$highest_grade), ref = "None")
liq_qty_day$aas_involvement <- relevel(factor(liq_qty_day$aas_involvement), ref = "None")
liq_qty_day$solar_stove <- relevel(factor(liq_qty_day$solar_stove), ref = "No")


model_liq_qty_day <- felm(liq_qty_day ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                      data = liq_qty_day)
robust_se_liq_qty_day <- vcovCL(model_liq_qty_day, type = "HC1")

rt_liq_qty_day <- coeftest(model_liq_qty_day, vcov = robust_se_liq_qty_day)
tidy_r_liq_qty_day <- tidy(rt_liq_qty_day)%>%
  mutate(run="liq_qty_day")

#liq_fre_day====
liq_fre_day<-liq%>%
  dplyr::select(all_of(cov), liq_fre_day)

liq_fre_day$village_cor <- relevel(factor(liq_fre_day$village_cor), ref = "Lealui")
liq_fre_day$gender <- relevel(factor(liq_fre_day$gender), ref = "Women")
liq_fre_day$highest_grade <- relevel(factor(liq_fre_day$highest_grade), ref = "None")
liq_fre_day$aas_involvement <- relevel(factor(liq_fre_day$aas_involvement), ref = "None")
liq_fre_day$solar_stove <- relevel(factor(liq_fre_day$solar_stove), ref = "No")


model_liq_fre_day <- felm(liq_fre_day ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                          data = liq_fre_day)
robust_se_liq_fre_day <- vcovCL(model_liq_fre_day, type = "HC1")
rt_liq_fre_day <- coeftest(model_liq_fre_day, vcov = robust_se_liq_fre_day)
tidy_r_liq_fre_day <- tidy(rt_liq_fre_day)%>%
  mutate(run="liq_fre_day")

#liq_qty_week====
liq_qty_week<-liq%>%
  dplyr::select(all_of(cov), liq_qty_week)

liq_qty_week$village_cor <- relevel(factor(liq_qty_week$village_cor), ref = "Lealui")
liq_qty_week$gender <- relevel(factor(liq_qty_week$gender), ref = "Women")
liq_qty_week$highest_grade <- relevel(factor(liq_qty_week$highest_grade), ref = "None")
liq_qty_week$aas_involvement <- relevel(factor(liq_qty_week$aas_involvement), ref = "None")
liq_qty_week$solar_stove <- relevel(factor(liq_qty_week$solar_stove), ref = "No")


model_liq_qty_week <- felm(liq_qty_week ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                          data = liq_qty_week)
robust_se_liq_qty_week <- vcovCL(model_liq_qty_week, type = "HC1")
rt_liq_qty_week <- coeftest(model_liq_qty_week, vcov = robust_se_liq_qty_week)
tidy_r_liq_qty_week <- tidy(rt_liq_qty_week)%>%
  mutate(run="liq_qty_week")

#liq_fre_week====
liq_fre_week<-liq%>%
  dplyr::select(all_of(cov), liq_fre_week)

liq_fre_week$village_cor <- relevel(factor(liq_fre_week$village_cor), ref = "Lealui")
liq_fre_week$gender <- relevel(factor(liq_fre_week$gender), ref = "Women")
liq_fre_week$highest_grade <- relevel(factor(liq_fre_week$highest_grade), ref = "None")
liq_fre_week$aas_involvement <- relevel(factor(liq_fre_week$aas_involvement), ref = "None")
liq_fre_week$solar_stove <- relevel(factor(liq_fre_week$solar_stove), ref = "No")


model_liq_fre_week <- felm(liq_fre_week ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                           data = liq_fre_week)
robust_se_liq_fre_week <- vcovCL(model_liq_fre_week, type = "HC1")
rt_liq_fre_week <- coeftest(model_liq_fre_week, vcov = robust_se_liq_fre_week)
tidy_r_liq_fre_week <- tidy(rt_liq_fre_week)%>%
  mutate(run="liq_fre_week")

#liq_fre_tot====
liq_fre_tot<-liq%>%
  dplyr::select(all_of(cov), liq_fre_tot)

liq_fre_tot$village_cor <- relevel(factor(liq_fre_tot$village_cor), ref = "Lealui")
liq_fre_tot$gender <- relevel(factor(liq_fre_tot$gender), ref = "Women")
liq_fre_tot$highest_grade <- relevel(factor(liq_fre_tot$highest_grade), ref = "None")
liq_fre_tot$aas_involvement <- relevel(factor(liq_fre_tot$aas_involvement), ref = "None")
liq_fre_tot$solar_stove <- relevel(factor(liq_fre_tot$solar_stove), ref = "No")


model_liq_fre_tot <- felm(liq_fre_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                           data = liq_fre_tot)
robust_se_liq_fre_tot <- vcovCL(model_liq_fre_tot, type = "HC1")
rt_liq_fre_tot <- coeftest(model_liq_fre_tot, vcov = robust_se_liq_fre_tot)
tidy_r_liq_fre_tot <- tidy(rt_liq_fre_tot)%>%
  mutate(run="liq_fre_tot")

#liq_qty_tot====
liq_qty_tot<-liq%>%
  dplyr::select(all_of(cov), liq_qty_tot)

#setting reference levels
liq_qty_tot$village_cor <- relevel(factor(liq_qty_tot$village_cor), ref = "Lealui")
liq_qty_tot$gender <- relevel(factor(liq_qty_tot$gender), ref = "Women")
liq_qty_tot$highest_grade <- relevel(factor(liq_qty_tot$highest_grade), ref = "None")
liq_qty_tot$aas_involvement <- relevel(factor(liq_qty_tot$aas_involvement), ref = "None")
liq_qty_tot$solar_stove <- relevel(factor(liq_qty_tot$solar_stove), ref = "No")


model_liq_qty_tot <- felm(liq_qty_tot ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                          data = liq_qty_tot)
robust_se_liq_qty_tot <- vcovCL(model_liq_qty_tot, type = "HC1")
rt_liq_qty_tot <- coeftest(model_liq_qty_tot, vcov = robust_se_liq_qty_tot)
tidy_r_liq_qty_tot <- tidy(rt_liq_qty_tot)%>%
  mutate(run="liq_qty_tot")

#merging all liquid models====

liq_freq<-rbind(tidy_r_liq_qty_day,tidy_r_liq_fre_day,tidy_r_liq_qty_week,tidy_r_liq_fre_week,tidy_r_liq_fre_tot,tidy_r_liq_qty_tot)%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))



liq_freq$term <- ordered(liq_freq$term,
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



desired_order <- c("liq_qty_day","liq_fre_day","liq_qty_week","liq_fre_week","liq_qty_tot","liq_fre_tot")
liq_freq$run <- factor(liq_freq$run, levels = desired_order)

#custom_labels <- c("leg_day" = "Times legumes cooked / day", 
#                   "leg_week" = "Times legumes cooked / week", 
#                   "leg_tot" = "Times legumes cooked  / 6weeks")


liq_freq_f<-ggplot(liq_freq, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",  labeller = labeller(group = wrap_text), ncol = 2, nrow = 3)+
  theme(legend.position="bottom")


ggsave(here("figures", "liq_freq.png"),
       liq_freq_f,
       dpi =300,
       width = 7,
       height = 9,
       units ="in")

