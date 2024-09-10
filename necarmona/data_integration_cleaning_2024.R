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
#settings====

#TODO:Winsorize  response variables as part of the preparation step check the package robustHD  for multiple datasets
#TODO: Explore figures i.e. forest plots to present the results. 1) use of solar stoves, 2) dietary outcomes (HDDS, wdds, SR), 3) health outcomes (skipped meals, number of dishes, boiled liquids), 4) energy sources harvesting time, use and cost (firewood and charcoal)
#TODO: Test lm and felm results (both treat covarietes differently, see effect of AAS)
#TODO: Run all the analyses stated in the action plan
#TODO: Estimate dish, meal, day Se USING Liang-Zeger cluster-robust standard errors and use Eicker-Huber-White (EHW) robust standard errors when calculating total values

encoding = "native.enc"
Sys.setlocale(locale = "English")
options(stringsAsFactors = FALSE)# avoid automatically auto-converting string to factors
set.seed(1111)#to guarantee replicable results

#call functions needed
source("Functions.R")

#Main  HH characterization collected by the Min of Health partner who visited all participants====
##data not shared: it contains sensitive information about respondents. 
main_hh_characterization<-read.csv("D:/2 Bioversity/spia/2024/runs_files_2024/source_data/all_villages_main_hh_characterization.csv")

#calculating physical and living assets
main_hh_covariates<-main_hh_characterization%>%
  mutate(
    #Physical asset factor - Conversion factors from ???
    Cellphone_a= ifelse(Cellphone=="Yes",
                        ifelse (cellphone_cond!="not working", (cellphone_no*3),0 ),0 ),
    boat_a= ifelse(boat=="Yes", #no info so the weight = moto
                   ifelse (cellphone_cond!="not working",  (boats_no*48),0 ),0 ),
    Canoe_a= ifelse(Canoe=="Yes",#no info so the weight = Bicycle
                    ifelse (cellphone_cond!="not working",  (canoe_no*6),0 ),0 ),
    moto_a= ifelse(moto=="Yes",
                   ifelse (cellphone_cond!="not working",  (moto_no*48),0 ),0 ),
    Bicycle_a= ifelse(Bicycle=="Yes",
                      ifelse (cellphone_cond!="not working",  (bycicle_no*6),0 ),0 ),
    Radio_a= ifelse(Radio=="Yes",
                    ifelse (cellphone_cond!="not working",  (radio_no*2),0 ),0 ),
    #Tropical livestock units - TLU. Conversion factors from  ???
    Cows_a= ifelse(Cows=="Yes", (caws_no*1),0 ),
    Oxen_a= ifelse(Oxen=="Yes", (oxen_no*1.42),0 ),
    Pigs_a= ifelse(Pigs=="Yes", (pigs_no*0.3),0 ),
    Goats_a= ifelse(Goats=="Yes", (goats_no*0.2),0 ),
    chicken_a= ifelse(chicken=="Yes", (chicken_no*0.04),0 ),
    Ducks_a= ifelse(Ducks=="Yes", (ducks_no*0.04),0 ),
    phy_assets=Cellphone_a+boat_a+Canoe_a+moto_a+Bicycle_a+Radio_a,
    TLU=Cows_a+Oxen_a+Pigs_a+Goats_a+chicken_a+Ducks_a,
    
    #Participation in AAS activities
    aas_involvement=if_else(Participation_in_NSL_AAS_activities=="Learning plots"|Participation_in_NSL_AAS_activities=="Learning plots SILC", "agriculture",
                            if_else(Participation_in_NSL_AAS_activities=="Nutritional clubs"|
                                      Participation_in_NSL_AAS_activities=="Nutritional clubs Cooking demonstrations"|
                                      Participation_in_NSL_AAS_activities=="Nutritional clubs Cooking demonstrations SILC"|
                                      Participation_in_NSL_AAS_activities=="Nutritional clubs SILC", "nutrition",
                                    if_else(Participation_in_NSL_AAS_activities=="Nutritional clubs Cooking demonstrations Learning plots"|
                                              Participation_in_NSL_AAS_activities=="Nutritional clubs Cooking demonstrations Learning plots SILC"|
                                              Participation_in_NSL_AAS_activities=="Nutritional clubs Learning plots SILC", "Ag_nut", "None"))),
    
    village_cor=if_else(COD<200000, "Lealui",
                                       if_else(COD<300000, "Mapungu", "Nalitoya")),
    
    highest_grade=if_else(highest_grade=="", "None", highest_grade),
    
    age_cal=as.numeric(age_cal),
    
    hh_no=as.numeric(hh_no))%>%
  
  clean_names()%>%
  #remove hh names and year of birth. 
  dplyr::select(c(cod,village_cor, gender,age_cal,highest_grade, solar_stove, hh_no, aas_involvement,tlu,phy_assets  ))%>%
  
  
  # create dummy columns for covariates 
  dummy_cols()


write.csv(main_hh_covariates, here("anonymized_source_files", "main_hh_covariates.csv"), row.names=FALSE, sep = ";")


# Daily Dietary registries with ingredient, dish, meal, method located in the sheet with the same name====
sheet_name_dish_meal <- "Data_Entry" 

# Read the same sheet from all files
dish_meals_ing_all_data <- map(excel_files , read_and_clean_sheet, sheet_name = sheet_name_dish_meal)

# Combine the data into a single data frame 
dish_meals_ing_combined <- bind_rows(dish_meals_ing_all_data)%>%
  mutate(across(c("COD"), as.numeric)) %>%

  #clean village column
  mutate(village=if_else(COD<200000, "Lealui",
                         if_else(COD<300000, "Mapungu", "Nalitoya")))%>%
  clean_names()%>%
  mutate(meal=tolower(str_trim(meal)))%>%
  
  select(-c(name_on_the_form, source_pdf,starting_date, finishing_date, who_enter_the_data ))

#Check
test<-dish_meals_ing_combined%>%distinct(cod)

# View the combined data
print(dish_meals_ing_combined)

write.csv(dish_meals_ing_combined, here("anonymized_source_files", "all_villages_ing_dish_meals.csv"), row.names=FALSE, sep = ";")

#Ingredients in the long format to calculate indicators====
#each row is an ingredient

all_villages_ing_dish_meals_l<-dish_meals_ing_combined%>%
  
  #remove boiling liquids information
  select(-c("x24",,"liquid","method","type" ,"quantity","units","note"))%>%
  filter(meal=="breakfast" |  meal=="dinner" | meal=="lunch")%>%
  pivot_longer(cols=c("ingredient1","ingredient2","ingredient3","ingredient4","ingredient5","ingredient6","ingredient7","other"),
               names_to = "ingridient_no",
               values_to="ingridient_name")%>%
  mutate(ingridient_name=tolower(str_trim(ingridient_name)),
         ingridient_name = str_replace_all(ingridient_name, "[^a-zA-Z\\s]", ""))%>%
  filter(!ingridient_name=="na" )%>%
  filter(!is.na(ingridient_name))

write.csv(all_villages_ing_dish_meals_l, here("anonymized_source_files", "all_villages_ing_dish_meals_l.csv"), row.names=FALSE, sep = ";")


#Find unique ingredients names to translate into English and assign  food groups

u_ingr<-all_villages_ing_dish_meals_l%>%distinct(ingridient_name)
write.csv(u_ingr, here("anonymized_source_files","u_ingr_all_villages.csv"), row.names=FALSE, sep = ";")


# Get all ingredients and translations reported in the questionnaires ====

sheet_name_ing <- "Ingredients_fooditems_Lozi" 

# Read the same sheet from all files but get the data from the Ingredients_fooditems_Lozi sheet
all_ingridients <- map(excel_files , read_and_clean_sheet, sheet_name = sheet_name_ing)

# Combine the data into a single data frame (optional)
l_combined_all_ingredients <- bind_rows(all_ingridients)%>%
  rename(ing_lozi="Lozi..Lico", ing_eng="English...Food")%>%
  mutate(ing_lozi_c=tolower(str_trim(ing_lozi)))%>%
  distinct(ing_lozi_c, ing_eng)

#check multiple definitions exist
test<-l_combined_all_ingredients%>%group_by(ing_lozi_c)%>%summarise(n=n())

#first round of ingredients shared my Maybin-> to integrate with the unique ingridients names obtained from "data_integration_cleaning_2024", and previous files verified by Mulele and Maybin (local MoA and MoH partners)
write.csv(l_combined_all_ingredients, here("anonymized_source_files","ingredients_tab.csv"), row.names=FALSE, sep = ";")

# the file integration and comparison (e.g. word by word in Outlook to see if translated already) was made in excel. 
#Files revised: 
#crops_fooditems_mulele_Apr09.xlsx / sheet: printcrop
#Lealui_mapungu_nalitoya_data.xlsx / sheet: dictionary
#Outlook previous emails and files shared

####
####
## The final curated file is called dictionary_2016_2024.csv
####
####


#Prepare the main file with ingredients for calculating indicators====

#Main hh characteristics 
main_hh_covariates<-read.csv(here("anonymized_source_files", "main_hh_covariates.csv"))

#All hh ingredients long format
all_villages_ing_dish_meals_l<-read.csv(here("anonymized_source_files", "all_villages_ing_dish_meals_l.csv"))

#Dictionary translating Lozi words into English and assigning the food group, household dietary diversity group and womends dietary diversity group
#Guide used-> https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/
dictionary_2016_2024<-read.csv(here("anonymized_source_files", "dictionary_2016_2024.csv"))%>%
  #for comparability ensure names are lower case and without special characters
  mutate(ingridient_name = str_replace_all(Lozi, "[^a-zA-Z\\s]", ""))

#Assign to every ingredient the translated name and food groups

all_villages_ing_dish_meals_l$ingridient_name <- stri_enc_toutf8(all_villages_ing_dish_meals_l$ingridient_name )

dictionary_2016_2024$ingridient_name <- stri_enc_toutf8(dictionary_2016_2024$ingridient_name )


all_villages_ing_dish_meals_l_fg<-all_villages_ing_dish_meals_l%>%
  stringdist_left_join(dictionary_2016_2024,by=c("ingridient_name") , ignore_case=TRUE, max_dist = 0, method="osa")

write.csv(all_villages_ing_dish_meals_l_fg, here("anonymized_source_files","all_villages_ing_dish_meals_l_fg.csv"), row.names=FALSE, sep = ";")


###
###
# FIXME: Revisit to handle separate columns
###
###

# Prepare Firewood data collected weekly ====
firewood<-read.csv("D:/2 Bioversity/spia/2024/runs_files_2024/source_data/firewood_charcoal.csv")%>%
  clean_names()%>%
  mutate(tot_firewood_lb= as.numeric(quantity_kg),
         tot_firewood_ub=as.numeric(quantity_kg*times_per_week_written),
         tot_charcoal_lb=as.numeric(quantity_clean_1),
         tot_charcoal_ub=as.numeric(quantity_clean_1*times_per_week_writen))%>%
  dplyr::select(c(cod, tot_firewood_lb, tot_firewood_ub, tot_charcoal_lb, tot_charcoal_ub,week, time_min ))%>%
  group_by(cod)%>%
  summarise(tot_time=sum(time_min, na.rm=TRUE),
            tot_firewood_lb=sum(tot_firewood_lb, na.rm=TRUE),
            tot_firewood_ub=sum(tot_firewood_ub, na.rm=TRUE),
            tot_charcoal_lb=sum(tot_charcoal_lb, na.rm=TRUE),
            tot_charcoal_ub=sum(tot_charcoal_ub, na.rm=TRUE),
            weeks=n(),
            avg_time=tot_time/weeks,
            avg_firewood_lb=tot_firewood_lb/weeks,
            avg_firewood_ub=tot_firewood_ub/weeks,
            avg_charcoal_lb=tot_charcoal_lb/weeks,
            avg_charcoal_ub=tot_charcoal_ub/weeks,
            tot_firewood_charcoal_costusd_lb=tot_firewood_lb*0.13+tot_charcoal_lb*0.12,
            tot_firewood_charcoal_costusd_ub=tot_firewood_ub*0.13+tot_charcoal_ub*0.12,
            avg_firewood_charcoal_costusd_lb=avg_firewood_lb*0.13+avg_charcoal_lb*0.12,
            avg_firewood_charcoal_costusd_ub=avg_firewood_ub*0.13+avg_charcoal_ub*0.12)


##integrate firewood with hh characteristics and covariables
firewood_cov<-firewood%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))%>%
  mutate()


write.csv(firewood_cov, here("anonymized_source_files","all_villages_firewood_cov.csv"), row.names=FALSE, sep = ";")


###Preparing Boiling liquids ====
all_villages_liquids<-dish_meals_ing_combined%>%
  filter(meal=="boiled liquid"  | meal=="boiled liquids")%>%
  filter(units=="cc" |units== "Cc" | units=="l"|units=="L"|units=="lt"|units=="lts"|units=="ly"|units=="ml")%>%
  filter(!quantity=="na")%>%
  mutate(quantity_num=as.numeric(str_replace_all(quantity, c("," = ".", "\\.\\." = ".")))) %>%
  dplyr::select(cod, week, day, liquid, method, type, quantity_num, units, note, village)%>%
  mutate(units_clean=if_else(units== "cc" |units== "Cc" , "cc",
                             if_else(units=="l"|units=="L"|units=="lt"|units=="lts"|units=="ly", "lt", 
                                     if_else(units=="ml", "ml", ""))),
         quantity_lt=if_else(units_clean=="cc" | units_clean=="ml", quantity_num*0.001, quantity_num))

##integrate boiled water with hh characteristics and covariables
boiled_liquids_cov<-all_villages_liquids%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))


write.csv(boiled_liquids_cov, here("anonymized_source_files","all_villages_boiled_liquids_cov.csv"), row.names=FALSE, sep = ";")



# legumes frequency 
legumes<-all_villages_ing_dish_meals_l_fg%>%
  filter(Food.group=="Pulses (beans, peas and lentils)")%>%
  group_by(cod, week, day, meal)%>% #to remove times when cooked two spp of pulses in one meal. 
  dplyr::summarise(freq=n_distinct(Food.group))%>%
  group_by(cod)%>%
  dplyr::summarise(freq_tot=sum(freq))
    
#variables interpretation
#freq_tot-> total number of times the hh cooked pulses


##integrate number of dishes with hh characteristics and covariables====
legumes_6w<-legumes%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))

tot_f<-ggplot(legumes_6w, aes(x=freq_tot, y=solar_stove, fill=solar_stove))+
  geom_boxplot(notch = TRUE)+
  geom_violin(trim=FALSE, fill=NA)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
  xlab("Total number of times the hh cooked pulses (6 weeks)")+
  ylab("")+
  theme(legend.position="none")


ggsave(tot_f,
       filename="pulses.png",
       path=here("runs_files_2024","figure"),
       dpi = 300,
       width = 7,
       height = 4,
       units="in")


legumes_6w_cov<-legumes_6w%>%
  dplyr::select(-c(village_cor_Lealui,village_cor_Mapungu,village_cor_Nalitoya,       
                   gender_Men,gender_Women,highest_grade_Higher,highest_grade_None,         
                   highest_grade_Primary,highest_grade_Secondary,solar_stove_No,solar_stove_Yes,           
                   aas_involvement_Ag_nut,aas_involvement_agriculture,aas_involvement_None,aas_involvement_nutrition))

#setting reference levels
legumes_6w_cov$village_cor <- relevel(factor(legumes_6w_cov$village_cor), ref = "Lealui")
legumes_6w_cov$gender <- relevel(factor(legumes_6w_cov$gender), ref = "Women")
legumes_6w_cov$highest_grade <- relevel(factor(legumes_6w_cov$highest_grade), ref = "None")
legumes_6w_cov$aas_involvement <- relevel(factor(legumes_6w_cov$aas_involvement), ref = "None")
legumes_6w_cov$solar_stove <- relevel(factor(legumes_6w_cov$solar_stove), ref = "No")


  
#species richness====

sr_avg<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod, week, day)%>%
  dplyr::summarise(no_spp=n_distinct(scientific_name))%>%
  group_by(cod)%>%
  dplyr::summarise(no_spp_mean=mean(no_spp))

sr_tot<-all_villages_ing_dish_meals_l_fg%>%
  group_by(cod)%>%
  dplyr::summarise(no_spp=n_distinct(scientific_name))

#variables interpretation
#sr_tot-> total number of unique species consumed during the 6-week period
#sr_avg-> daily average number of unique species consumed 

##integrate number of dishes with hh characteristics and covariables====
sr<-sr_avg%>%
  left_join(sr_tot, by="cod")%>%
  left_join(main_hh_covariates, by="cod")%>%
  filter(!is.na(solar_stove))

sr_mean_f<-ggplot(sr, aes(x=no_spp_mean, y=solar_stove, fill=solar_stove))+
  geom_boxplot(notch = TRUE)+
  geom_violin(trim=FALSE, fill=NA)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
  xlab("Daily average number of unique species consumed")+
  ylab("")+
  theme(legend.position="none")

sr_tot_f<-ggplot(sr, aes(x=no_spp, y=solar_stove, fill=solar_stove))+
  geom_boxplot(notch = TRUE)+
  geom_violin(trim=FALSE, fill=NA)+
  stat_summary(fun.y=mean, geom="point", shape=23, size=2)+
  xlab("Total number of unique species consumed during the 6-week period")+
  ylab("")+
  theme(legend.position="none")


SR_f<-grid.arrange(  sr_mean_f, sr_tot_f, nrow=1,ncol=2)

ggsave(SR_f,
       filename="SR_f.png",
       path=here("runs_files_2024","figure"),
       dpi = 300,
       width = 7,
       height = 4,
       units="in")


sr_mean_6W_cov<-sr%>%
  dplyr::select(-c(no_spp, village_cor_Lealui,village_cor_Mapungu,village_cor_Nalitoya,       
                   gender_Men,gender_Women,highest_grade_Higher,highest_grade_None,         
                   highest_grade_Primary,highest_grade_Secondary,solar_stove_No,solar_stove_Yes,           
                   aas_involvement_Ag_nut,aas_involvement_agriculture,aas_involvement_None,aas_involvement_nutrition))

#setting reference levels
sr_mean_6W_cov$village_cor <- relevel(factor(sr_mean_6W_cov$village_cor), ref = "Lealui")
sr_mean_6W_cov$gender <- relevel(factor(sr_mean_6W_cov$gender), ref = "Women")
sr_mean_6W_cov$highest_grade <- relevel(factor(sr_mean_6W_cov$highest_grade), ref = "None")
sr_mean_6W_cov$aas_involvement <- relevel(factor(sr_mean_6W_cov$aas_involvement), ref = "None")
sr_mean_6W_cov$solar_stove <- relevel(factor(sr_mean_6W_cov$solar_stove), ref = "No")

sr_tot_6W_cov<-sr%>%
  dplyr::select(-c(no_spp_mean, village_cor_Lealui,village_cor_Mapungu,village_cor_Nalitoya,       
                   gender_Men,gender_Women,highest_grade_Higher,highest_grade_None,         
                   highest_grade_Primary,highest_grade_Secondary,solar_stove_No,solar_stove_Yes,           
                   aas_involvement_Ag_nut,aas_involvement_agriculture,aas_involvement_None,aas_involvement_nutrition))

#setting reference levels
sr_tot_6W_cov$village_cor <- relevel(factor(sr_tot_6W_cov$village_cor), ref = "Lealui")
sr_tot_6W_cov$gender <- relevel(factor(sr_tot_6W_cov$gender), ref = "Women")
sr_tot_6W_cov$highest_grade <- relevel(factor(sr_tot_6W_cov$highest_grade), ref = "None")
sr_tot_6W_cov$aas_involvement <- relevel(factor(sr_tot_6W_cov$aas_involvement), ref = "None")
sr_tot_6W_cov$solar_stove <- relevel(factor(sr_tot_6W_cov$solar_stove), ref = "No")
