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
library(tidyverse)
#settings====


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

    #Tropical livestock units - TLU. Conversion factors from  FAO 2008
    Cows_a= ifelse(Cows=="Yes", (caws_no*0.7),0 ),
    Oxen_a= ifelse(Oxen=="Yes", (oxen_no*0.8),0 ),
    Pigs_a= ifelse(Pigs=="Yes", (pigs_no*0.2),0 ),
    Goats_a= ifelse(Goats=="Yes", (goats_no*0.1),0 ),
    chicken_a= ifelse(chicken=="Yes", (chicken_no*0.01),0 ),
    Ducks_a= ifelse(Ducks=="Yes", (ducks_no*0.01),0 ),

    tli=Cows_a+Oxen_a+Pigs_a+Goats_a+chicken_a+Ducks_a,
    
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
  dplyr::select(c(cod,village_cor, gender,age_cal,highest_grade, solar_stove, hh_no, aas_involvement,tli))%>%
  
  
  # create dummy columns for covariates 
  dummy_cols()
#Physical asset factor - Conversion factors from Filmer and Pritchett 2001. and Traisac, 2012
#account for not working material assets. 
#use the number of assets in the PCA rather than just yes/no responses in MFA
library(psych)
main_hh_covariates_asset<-main_hh_characterization%>%
  mutate(Cellphone_no_c=if_else(cellphone_cond=="not working", 0, cellphone_no),
         boat_no_c=if_else(boat_cond=="not working", 0, boats_no),
         canoe_no_c=if_else(canoe_cond=="not working", 0, canoe_no),
         moto_no_c=if_else(moto_cond=="not working", 0, moto_no),
         radio_no_c=if_else(radio_cond=="not working", 0, radio_no))%>%
  select(Cellphone_no_c,boat_no_c,canoe_no_c,moto_no_c,radio_no_c )%>%
  replace(is.na(.), 0)
         

# Perform PCA
#first pc1 explains 30.28% of the variability

pca_result <- principal(main_hh_covariates_asset, nfactors = 1, rotate = "none")

# Extract the first principal component (which is the asset index)
asset_index <- pca_result$scores[,1]

# Add the asset index to the original data
main_hh_covariates <- cbind(main_hh_covariates, main_hh_covariates_asset, asset_index)

ggplot(main_hh_covariates, aes(x = seq_along(asset_index), y = asset_index, color=village_cor)) +
  geom_point(size = 3) +
  labs(title = "Household Asset Index",
       x = "Household",
       y = "Asset Index") +
  theme_minimal()

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
## The final curated file is called dictionary_2016_2024_2.csv
####
####


#Prepare the main file with ingredients for calculating indicators====

#Main hh characteristics 
main_hh_covariates<-read.csv(here("anonymized_source_files", "main_hh_covariates.csv"))

#All hh ingredients long format
all_villages_ing_dish_meals_l<-read.csv(here("anonymized_source_files", "all_villages_ing_dish_meals_l.csv"))

#Dictionary translating Lozi words into English and assigning the food group, household dietary diversity group and womends dietary diversity group
#Guide used-> https://www.fao.org/nutrition/assessment/tools/household-dietary-diversity/en/
dictionary_2016_2024<-read.csv(here("anonymized_source_files", "dictionary_2016_2024_2.csv"))%>%
  #for comparability ensure names are lower case and without special characters
  mutate(ingridient_name = str_replace_all(Lozi, "[^a-zA-Z\\s]", ""))

#Assign to every ingredient the translated name and food groups

all_villages_ing_dish_meals_l$ingridient_name <- stri_enc_toutf8(all_villages_ing_dish_meals_l$ingridient_name )

dictionary_2016_2024$ingridient_name <- stri_enc_toutf8(dictionary_2016_2024$ingridient_name )


all_villages_ing_dish_meals_l_fg<-all_villages_ing_dish_meals_l%>%
  stringdist_left_join(dictionary_2016_2024,by=c("ingridient_name") , ignore_case=TRUE, max_dist = 0, method="osa")


separate_ing<-all_villages_ing_dish_meals_l_fg%>%
  filter(separate=="yes")%>%
  
  #separating ingredients entered as one word "saltmezi" when revised is salt and water. 
  separate(Ingridient,  into = c("Ingridient_part1","Ingridient_part2"), sep="; ", fill="right")%>%
  separate(ing_type,  into=c("ing_type_part1","ing_type_part2"), sep=";", fill="right")%>%
  separate(scientific_name,  into=c("scientific_name_part1","scientific_name_part2"), sep=";", fill="right")%>%
  separate(hdds,  into=c("hdds_part1","hdds_part2"), sep=";", fill="right")%>%
  separate(wdds,  into=c("wdds_part1","wdds_part2"), sep=";", fill="right")%>%
  separate(zfbdrfg,  into=c("zfbdrfg_part1","zfbdrfg_part2"), sep=";", fill="right")
  

  #putting it back again into one column only - pivoting from wider to longer
separate_ing_long<-separate_ing%>%
  tidyr::pivot_longer(cols=starts_with("Ingridient_part"),
               names_to = "Ingridient_parts",
               values_to = "Ingridient")%>%
    tidyr::pivot_longer(cols=starts_with("ing_type_part"),
               names_to = "ing_type_parts",
               values_to ="ing_type")%>%
  tidyr::pivot_longer(cols=starts_with("scientific_name_part"),
               names_to = "scientific_name_parts",
               values_to = "scientific_name")%>%
    tidyr::pivot_longer(cols=starts_with("hdds_part"),
               names_to = "hdds_parts",
               values_to = "hdds")%>%
    tidyr::pivot_longer(cols=starts_with("wdds_part"),
               names_to = "wdds_parts",
               values_to ="wdds" )  %>% 
    tidyr::pivot_longer(cols=starts_with("zfbdrfg_part"),
               names_to = "zfbdrfg_parts",
               values_to ="zfbdrfg" ) %>%
    distinct(cod, week, day, meal, dish, ingridient_no,Ingridient_parts, .keep_all=TRUE)%>%
  dplyr::select(-c(Ingridient_parts,ing_type_parts,scientific_name_parts,hdds_parts,hdds_parts,wdds_parts, zfbdrfg_parts ))%>%
  
  #nsuring column order is the same as all_villages_ing_dish_meals_l_fg
  dplyr::select("cod","week","day", "meal","dish","notes","cooking_method","based_on_memory","no_of_cups_of_legumes" , "notes_or_specify_if_other_units_were_used",
                "village","ingridient_no" , "ingridient_name.x","Lozi","Ing_desc"  ,"Food.group","Ingridient","ing_type",
                "scientific_name","found_2024","in_mabyn_dic_notlisted","separate","hdds","wdds","zfbdrfg","ingridient_name.y" )
 
  
all_villages_ing_dish_meals_l_fg_c<-rbind(all_villages_ing_dish_meals_l_fg, separate_ing_long)


write.csv(all_villages_ing_dish_meals_l_fg_c, here("anonymized_source_files","all_villages_ing_dish_meals_l_fg.csv"), row.names=FALSE, sep = ";")


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
