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

#see data preparation script "data_integration_cleaning_2024.R" lines 212-2036

firewood_a<-read.csv(here("anonymized_source_files", "all_villages_firewood_cov.csv"))%>%
  mutate(across(c(tot_time,
                  tot_firewood_lb,
                  tot_firewood_ub,
                  tot_charcoal_lb,
                  tot_charcoal_ub,
                  avg_time,
                  avg_firewood_lb,
                  avg_firewood_ub,
                  avg_charcoal_lb,
                  avg_charcoal_ub,
                  tot_firewood_charcoal_costusd_lb,
                  tot_firewood_charcoal_costusd_ub,
                  avg_firewood_charcoal_costusd_lb,
                  avg_firewood_charcoal_costusd_ub), winsorize_column))  

#avg_fire_6weeks_cov%>%group_by(solar_stove)%>%summarise(sum=sum(tot_firewood_lb))
#avg_fire_6weeks_cov%>%group_by(solar_stove)%>%summarise(n_distinct(cod))
#18284/95 192.4632 no stove
#8099/61 132.7705 yes stove



#Data preparation for the linear model====

#felm model accept factor variables, so ensure to remove dummy ones and indicators not specific to each analysis
cov<-c("solar_stove" , "gender", "age_cal" , "highest_grade" ,"hh_no","tlu" , 
         "phy_assets" , "aas_involvement" ,"village_cor")


firewood_tot_kg_lb<-firewood_a%>%
  dplyr::select(all_of(cov), tot_firewood_lb)

#setting reference levels
firewood_tot_kg_lb$village_cor <- relevel(factor(firewood_tot_kg_lb$village_cor), ref = "Lealui")
firewood_tot_kg_lb$gender <- relevel(factor(firewood_tot_kg_lb$gender), ref = "Women")
firewood_tot_kg_lb$highest_grade <- relevel(factor(firewood_tot_kg_lb$highest_grade), ref = "None")
firewood_tot_kg_lb$aas_involvement <- relevel(factor(firewood_tot_kg_lb$aas_involvement), ref = "None")
firewood_tot_kg_lb$solar_stove <- relevel(factor(firewood_tot_kg_lb$solar_stove), ref = "No")

charcoal_tot_kg_lb<-firewood_a%>%
  dplyr::select(all_of(cov), tot_charcoal_lb)

#setting reference levels
charcoal_tot_kg_lb$village_cor <- relevel(factor(charcoal_tot_kg_lb$village_cor), ref = "Lealui")
charcoal_tot_kg_lb$gender <- relevel(factor(charcoal_tot_kg_lb$gender), ref = "Women")
charcoal_tot_kg_lb$highest_grade <- relevel(factor(charcoal_tot_kg_lb$highest_grade), ref = "None")
charcoal_tot_kg_lb$aas_involvement <- relevel(factor(charcoal_tot_kg_lb$aas_involvement), ref = "None")
charcoal_tot_kg_lb$solar_stove <- relevel(factor(charcoal_tot_kg_lb$solar_stove), ref = "No")

firewood_tot_time<-firewood_a%>%
  dplyr::select(all_of(cov), tot_time)

#setting reference levels
firewood_tot_time$village_cor <- relevel(factor(firewood_tot_time$village_cor), ref = "Lealui")
firewood_tot_time$gender <- relevel(factor(firewood_tot_time$gender), ref = "Women")
firewood_tot_time$highest_grade <- relevel(factor(firewood_tot_time$highest_grade), ref = "None")
firewood_tot_time$aas_involvement <- relevel(factor(firewood_tot_time$aas_involvement), ref = "None")
firewood_tot_time$solar_stove <- relevel(factor(firewood_tot_time$solar_stove), ref = "No")

firewood_charcoal_cost_tot_lb<-firewood_a%>%
  dplyr::select(all_of(cov), tot_firewood_charcoal_costusd_lb)

#setting reference levels
firewood_charcoal_cost_tot_lb$village_cor <- relevel(factor(firewood_charcoal_cost_tot_lb$village_cor), ref = "Lealui")
firewood_charcoal_cost_tot_lb$gender <- relevel(factor(firewood_charcoal_cost_tot_lb$gender), ref = "Women")
firewood_charcoal_cost_tot_lb$highest_grade <- relevel(factor(firewood_charcoal_cost_tot_lb$highest_grade), ref = "None")
firewood_charcoal_cost_tot_lb$aas_involvement <- relevel(factor(firewood_charcoal_cost_tot_lb$aas_involvement), ref = "None")
firewood_charcoal_cost_tot_lb$solar_stove <- relevel(factor(firewood_charcoal_cost_tot_lb$solar_stove), ref = "No")

firewood_avg_kg_lb<-firewood_a%>%
  dplyr::select(all_of(cov), avg_firewood_lb)

#setting reference levels
firewood_avg_kg_lb$village_cor <- relevel(factor(firewood_avg_kg_lb$village_cor), ref = "Lealui")
firewood_avg_kg_lb$gender <- relevel(factor(firewood_avg_kg_lb$gender), ref = "Women")
firewood_avg_kg_lb$highest_grade <- relevel(factor(firewood_avg_kg_lb$highest_grade), ref = "None")
firewood_avg_kg_lb$aas_involvement <- relevel(factor(firewood_avg_kg_lb$aas_involvement), ref = "None")
firewood_avg_kg_lb$solar_stove <- relevel(factor(firewood_avg_kg_lb$solar_stove), ref = "No")

charcoal_avg_kg_lb<-firewood_a%>%
  dplyr::select(all_of(cov), avg_charcoal_lb) 

#setting reference levels
charcoal_avg_kg_lb$village_cor <- relevel(factor(charcoal_avg_kg_lb$village_cor), ref = "Lealui")
charcoal_avg_kg_lb$gender <- relevel(factor(charcoal_avg_kg_lb$gender), ref = "Women")
charcoal_avg_kg_lb$highest_grade <- relevel(factor(charcoal_avg_kg_lb$highest_grade), ref = "None")
charcoal_avg_kg_lb$aas_involvement <- relevel(factor(charcoal_avg_kg_lb$aas_involvement), ref = "None")
charcoal_avg_kg_lb$solar_stove <- relevel(factor(charcoal_avg_kg_lb$solar_stove), ref = "No")

firewood_avg_time<-firewood_a%>%
  dplyr::select(all_of(cov), avg_time)

#setting reference levels
firewood_avg_time$village_cor <- relevel(factor(firewood_avg_time$village_cor), ref = "Lealui")
firewood_avg_time$gender <- relevel(factor(firewood_avg_time$gender), ref = "Women")
firewood_avg_time$highest_grade <- relevel(factor(firewood_avg_time$highest_grade), ref = "None")
firewood_avg_time$aas_involvement <- relevel(factor(firewood_avg_time$aas_involvement), ref = "None")
firewood_avg_time$solar_stove <- relevel(factor(firewood_avg_time$solar_stove), ref = "No")

firewood_charcoal_cost_avg_lb<-firewood_a%>%
  dplyr::select(all_of(cov), avg_firewood_charcoal_costusd_lb)

#setting reference levels
firewood_charcoal_cost_avg_lb$village_cor <- relevel(factor(firewood_charcoal_cost_avg_lb$village_cor), ref = "Lealui")
firewood_charcoal_cost_avg_lb$gender <- relevel(factor(firewood_charcoal_cost_avg_lb$gender), ref = "Women")
firewood_charcoal_cost_avg_lb$highest_grade <- relevel(factor(firewood_charcoal_cost_avg_lb$highest_grade), ref = "None")
firewood_charcoal_cost_avg_lb$aas_involvement <- relevel(factor(firewood_charcoal_cost_avg_lb$aas_involvement), ref = "None")
firewood_charcoal_cost_avg_lb$solar_stove <- relevel(factor(firewood_charcoal_cost_avg_lb$solar_stove), ref = "No")

#totals Eicker-Huber-White (EHW) robust standard errors
model_firewood_tot_kg_lb <- felm(tot_firewood_lb ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
              data = firewood_tot_kg_lb)
robust_se_firewood_tot_kg_lb <- vcovCL(model_firewood_tot_kg_lb, type = "HC1")
rt_firewood_tot_kg_lb <- coeftest(model_firewood_tot_kg_lb, vcov = robust_se_firewood_tot_kg_lb)
tidy_r_firewood_tot_kg_lb <- tidy(rt_firewood_tot_kg_lb)%>%
  mutate(run="firewood_tot_kg_lb")

model_charcoal_tot_kg_lb <- felm(tot_charcoal_lb ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                 data = charcoal_tot_kg_lb)
robust_se_charcoal_tot_kg_lb <- vcovCL(model_charcoal_tot_kg_lb, type = "HC1")
rt_charcoal_tot_kg_lb <- coeftest(model_charcoal_tot_kg_lb, vcov = robust_se_charcoal_tot_kg_lb)
tidy_r_rt_charcoal_tot_kg_lb <- tidy(rt_charcoal_tot_kg_lb)%>%
  mutate(run="charcoal_tot_kg_lb")

model_firewood_tot_time <- felm(tot_time ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                 data = firewood_tot_time)
robust_se_firewood_tot_time <- vcovCL(model_firewood_tot_time, type = "HC1")
rt_firewood_tot_time <- coeftest(model_firewood_tot_time, vcov = robust_se_firewood_tot_time)
tidy_r_firewood_tot_time <- tidy(rt_firewood_tot_time)%>%
  mutate(run="firewood_tot_time")

model_firewood_charcoal_cost_tot_lb <- felm(tot_firewood_charcoal_costusd_lb ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                data = firewood_charcoal_cost_tot_lb)
robust_se_firewood_charcoal_cost_tot_lb <- vcovCL(model_firewood_charcoal_cost_tot_lb, type = "HC1")
rt_firewood_charcoal_cost_tot_lb <- coeftest(model_firewood_charcoal_cost_tot_lb, vcov = robust_se_firewood_charcoal_cost_tot_lb)
tidy_r_firewood_charcoal_cost_tot_lb <- tidy(rt_firewood_charcoal_cost_tot_lb)%>%
  mutate(run="firewood_charcoal_cost_tot_lb")


#avgals Liang-Zeger cluster-robust standard errors
model_firewood_avg_kg_lb <- felm(avg_firewood_lb ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                 data = firewood_avg_kg_lb)
robust_se_firewood_avg_kg_lb <- vcovCL(model_firewood_avg_kg_lb, type = "CR")
rt_firewood_avg_kg_lb <- coeftest(model_firewood_avg_kg_lb, vcov = robust_se_firewood_avg_kg_lb)
tidy_r_firewood_avg_kg_lb <- tidy(rt_firewood_avg_kg_lb)%>%
  mutate(run="firewood_avg_kg_lb")

model_charcoal_avg_kg_lb <- felm(avg_charcoal_lb ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                 data = charcoal_avg_kg_lb)
robust_se_charcoal_avg_kg_lb <- vcovCL(model_charcoal_avg_kg_lb, type = "CR")
rt_charcoal_avg_kg_lb <- coeftest(model_charcoal_avg_kg_lb, vcov = robust_se_charcoal_avg_kg_lb)
tidy_r_rt_charcoal_avg_kg_lb <- tidy(rt_charcoal_avg_kg_lb)%>%
  mutate(run="charcoal_avg_kg_lb")

model_firewood_avg_time <- felm(avg_time ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                data = firewood_avg_time)
robust_se_firewood_avg_time <- vcovCL(model_firewood_avg_time, type = "CR")
rt_firewood_avg_time <- coeftest(model_firewood_avg_time, vcov = robust_se_firewood_avg_time)
tidy_r_firewood_avg_time <- tidy(rt_firewood_avg_time)%>%
  mutate(run="firewood_avg_time")

model_firewood_charcoal_cost_avg_lb <- felm(avg_firewood_charcoal_costusd_lb ~ solar_stove + gender + age_cal + highest_grade + hh_no + tlu + phy_assets + aas_involvement | village_cor,
                                            data = firewood_charcoal_cost_avg_lb)
robust_se_firewood_charcoal_cost_avg_lb <- vcovCL(model_firewood_charcoal_cost_avg_lb, type = "CR")
rt_firewood_charcoal_cost_avg_lb <- coeftest(model_firewood_charcoal_cost_avg_lb, vcov = robust_se_firewood_charcoal_cost_avg_lb)
tidy_r_firewood_charcoal_cost_avg_lb <- tidy(rt_firewood_charcoal_cost_avg_lb)%>%
  mutate(run="firewood_charcoal_cost_avg_lb")



energy_tot<-rbind(tidy_r_firewood_tot_kg_lb, tidy_r_rt_charcoal_tot_kg_lb, tidy_r_firewood_tot_time, tidy_r_firewood_charcoal_cost_tot_lb,
                  tidy_r_firewood_avg_kg_lb,tidy_r_rt_charcoal_avg_kg_lb,tidy_r_firewood_avg_time, tidy_r_firewood_charcoal_cost_avg_lb )%>%
  mutate(xmin=estimate - 1.96 * std.error,
         xmax=estimate + 1.96 * std.error,
         sig=if_else(xmin <= 0 & xmax >= 0, "not sig", "sig"))

#variable interpretation
# firewood_tot_kg_lb= The amount of firewood that was collected over all six weeks.
# charcoal_tot_kg_lb=The time spent collecting charcoal over all six weeks
# firewood_tot_time=The time spent collecting firewood over all six weeks.
# firewood_charcoal_cost_tot_lb=	The money spent purchasing firewood AND charcoal over all six.
# firewood_avg_kg_lb=The amount of firewood that was collected in each week.
# charcoal_avg_kg_lb=The time spent collecting charcoal in each week
# firewood_avg_time=The time spent collecting firewood in each week
# firewood_charcoal_cost_avg_lb=	The money spent purchasing firewood AND charcoal in each week


custom_labels <- c("firewood_tot_kg_lb" = "Firewood kg/hh/6weeks", 
                   "charcoal_tot_kg_lb" = "Charcoal kg/hh/6weeks", 
                   "firewood_tot_time" = "Harvesting time min/hh/6weeks",
                   "firewood_charcoal_cost_tot_lb" = "Energy cost U$/hh/6weeks",
                   "firewood_avg_kg_lb" = "Firewood kg/hh/week", 
                   "charcoal_avg_kg_lb" = "Charcoal kg/hh/week", 
                   "firewood_avg_time" = "Harvesting time min/hh/week",
                   "firewood_charcoal_cost_avg_lb" = "Energy cost U$/hh/week")

energy_tot$term <- ordered(energy_tot$term,
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

energy_tot$run <- ordered(energy_tot$run,
                           levels = c("firewood_charcoal_cost_avg_lb" ,"firewood_charcoal_cost_tot_lb",
                                      "firewood_avg_kg_lb" ,"firewood_tot_kg_lb" ,
                                      "charcoal_avg_kg_lb" , "charcoal_tot_kg_lb" ,
                                      "firewood_avg_time" , "firewood_tot_time" ))

energy_f<-ggplot(energy_tot, aes(x=estimate, y=term, color=sig)) + 
  geom_point(stat="identity") +
  geom_errorbar(aes(xmin=xmin, xmax=xmax), width=.2) +
  geom_vline(xintercept = 0, linetype="dashed")+
  scale_color_manual(values= c("sig" = "blue", "not sig" = "black"))+
  
  facet_wrap(~run, scales = "free_x",  labeller = labeller(group = wrap_text), ncol = 4, nrow = 2)+
  theme(legend.position="bottom")

ggsave(here("figures", "energy.png"),
            energy_f,
            dpi =300,
            width = 6.5,
            height = 8,
            units ="in")
