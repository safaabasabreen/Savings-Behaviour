#*******************************************************************************
#  ** ELSA **
#*******************************************************************************

#rm(list=ls())    # remove any objects in current environment
gc()
## set seed
set.seed(1)

#1) load libraries
# Describe data
library(tidyverse)# Simulations
library(dplyr)
library(lubridate)# for date
library(ggplot2)
library(foreign)
library(stargazer)
library(haven)
library(texreg)
# Data analysis
library(plm)
library(bife)#Binary Choice Models with Fixed Effects
library(pglm)
library(caret)# To find cor.
library(fixest)# For fixed effect analysis
library(AER)# For IV analysis
library(ivtools)
library(estimatr)
library(sandwich)

library(stargazer)
library(modelr)
library(texreg)
library(sjPlot)
library(jtools)# to extract a comparison table btw models
library(huxtable)# work with jtools
library(sandwich)
library(broom)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
getwd()
# open log file
# split =TRUE means that all output will be sent to current screen as well as the log file
sink(file="Elsa_output.log", split=TRUE)
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# change to working directory where raw data files are in
setwd("C:/Users/Working data/Used DataSets/English Longitudinal Study of Ageing Waves 0-9 1998-2019/UKDA-5050-stata/stata/stata13_se/")

# set up a file path to data directory
# --> CHANGE THE FILE PATH TO WHERE THE DATA IS STORED
dir <- "stata13_se/"
# --> CHANGE THE WORKING DIRECTORY TO YOUR OWN CURRENT PROJECT SPECIFIC FOLDER. 

knitr::opts_chunk$set(echo = TRUE)
knitr::opts_knit$set(root.dir = "C:/Users/Working data/Used DataSets/English Longitudinal Study of Ageing Waves 0-9 1998-2019/UKDA-5050-stata/stata/stata13_se/")
getwd()


#From 2002/3 to 2018/19  (9 waves)
#https://ifs.org.uk/sites/default/files/output_url_files/wave_1_user_guide.pdf
# 3.1 COMBINING WAVE 1 to WAVE 9 DATA INTO LONG FORMAT
#******************************************************
# selecting variables
#id_var <- c("idauniq")
#w5edqual = Highest Educational Qualification at ELSA Wave 5
#Variable = hehelf	Variable label = Self-reported general health
#Variable = wpdes	Variable label = Best description of current work situation
#Variable = dimarr	Variable label = (D) Respondent current legal marital status - combined marriage/civil partnership
#chinat   = Number of Natural Children Inside the Household
#chinhh1  = Number of children
#hhsize   = Household Size
#hhtot	= Number of people in household (computed)
#chinhh	  = Whether respondent has a child in the household
#chouthh = Whether respondent has a child outside the household
#ngrandchinhh	 = number of grandchildren living in the household
#numhhk013	Variable label = number of children in HH aged 0-13
#Variable = sclifea	Variable label = How much agrees with the statement: in most ways my life is close to my ideal
#Variable = sclifeb	Variable label = How much agrees with the statement: the conditions of my life are excellent
#Variable = sclifec	Variable label = How much agrees with the statement: I am satisfied with my life
#Variable = sclifed	Variable label = How much agrees with the statement: so far I have got the important things I want in life
#Variable = sclifee	Variable label = How much agrees with the statement: if I could live my life again, I would change almost nothing
#Variable = scworkd	Variable label = Whether feels their salary is adequate# dropped due to NA`s`
#Variable = scovsa	Variable label = How satisfied respondent is overall with their life nowadays# dropped (NA¬s)
#empinc_r_s Respondent's income from employment (individual level)#= respondent total net employment income - summary var
#eqto7nc_bu_s =	Total	income	adjusted	for	family	size	('equivalised')		
#to7nc_bu_s	=	Unadjusted	('unequivalised')	total	income	
#Variable = empinc_bu_s	Variable label = BU total net employment income - summary var
#Variable = totinc_bu_ni2	Variable label = no. of imputed components of total income (type>=2)
#*Variable = eqtotinc_bu_s	Variable label = BU equivalised total income - summary var
#Variable = sinc_bu_i	Variable label = BU wage and salary income (iasinc) - value (incl. imputed values)
#Variable = seinc_bu_s	Variable label = BU total net self-employment income - summary var
#Variable = anin_r_i	Variable label = annuity income (iaaim/iaaip) - value (incl. imputed values)
#Variable = sharesi_bu_i	Variable label = BU income from Shares (iasssi) - value (incl. imputed values)
#Variable = bondsi_bu_i	Variable label = BU income from Bonds and Gilts (iabgi) - value (incl. imputed values)
#Variable = othsav_bu_i	Variable label = BU other savings and investments (iasio) - value (incl. imputed values)
#Variable = othsavi_bu_i	Variable label = BU income from other savings (iasioi) - value (incl. imputed values)
#Variable = prdebt_bu_i	Variable label = BU private debt (iaowem) - value (incl. imputed values)
#IV
#Variable = pscedc	Variable label = Whether felt their sleep was restless during past week
#Variable = pscedh	Variable label = Whether could not get going much of the time during past week
# Let us start with Wave 1
#Elsa_w1 <- read_dta("wave_1_financial_derived_variables.dta") %>% select("idauniq", "totinc_bu_s", "save_bu_i", "savei_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>% mutate(wave = 1)#Variable = netfw_bu_s	Variable label = BU total net financial wealth - summary var
Elsa_w2 <- read_dta("wave_2_financial_derived_variables.dta") %>%
  select("idauniq","eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 2)# exclude "seinc_bu_s","eqtotinc_bu_s","sinc_bu_i", "anin_r_i",
Elsa_w3 <- read_dta("wave_3_financial_derived_variables.dta") %>%
  select("idauniq","eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 3)
Elsa_w4 <- read_dta("wave_4_financial_derived_variables.dta") %>%
  select("idauniq","eqtotinc_bu_s",  "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 4)
Elsa_w5 <- read_dta("wave_5_financial_derived_variables.dta") %>%
  select("idauniq", "eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 5)
Elsa_w6 <- read_dta("wave_6_financial_derived_variables.dta") %>%
  select("idauniq", "eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 6)
Elsa_w7 <- read_dta("wave_7_financial_derived_variables.dta") %>%
  select("idauniq","eqtotinc_bu_s",  "eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 7)
Elsa_w8 <- read_dta("wave_8_elsa_financial_dvs_eul_v1.dta") %>%
  select("idauniq", "eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 8)
Elsa_w9 <- read_dta("wave_9_financial_derived_variables.dta") %>%
  select("idauniq", "eqtotinc_bu_s", "empinc_r_s","totinc_bu_s","sharesi_bu_i","bondsi_bu_i","save_bu_i", "savei_bu_i","othsav_bu_i", "othsavi_bu_i","savings_bu_s", "invests_bu_s", "debt_bu_s", "prdebt_bu_i", "netfw_bu_s") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 9)

#**********************Rest of variables****************

#w1 <- read_dta('wave_1_core_data_v3.dta') %>%  select("idauniq","indsex", "edqual", "indobyr", "chinhh1", "hehelf", "wpdes", "sclifed", "sclifee", "scworkd", "scovsa","pscedc", "pscedh" ) %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>% mutate(wave = 1)#Column `dimarr`, sclifea, `sclifeb`,`sclifec`,`sclifed`... don't exist # Most LS are missing

w2 <- read_dta('wave_2_core_data_v4.dta') %>%
  select("idauniq","indsex", "indobyr", "chinhh1","DiMar","wpdes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee", "PScedC", "PScedH","hofood", "hooutf", "holeis", "gor", "iaomm", "HHTot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 2)#Column , "iintdatm"  don't exist.
w3 <- read_dta('wave_3_elsa_data_v4.dta') %>%
  select("idauniq","indsex", "indobyr","chinhh","w3edqual","hegenh", "dimar", "wpdes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","pscedc", "pscedh","hofood", "hooutf",  "GOR", "iaomm", "hhtot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>% mutate(wave = 3)#scovsa` don't exist
w4 <- read_dta('wave_4_elsa_data_v3.dta') %>%
  select("idauniq","indsex", "indobyr","chinhh","w4edqual", "hehelf", "dimar","wpdes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","pscedc", "pscedh","hofood", "hooutf", "holeis", "GOR", "iaomm", "hhtot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 4)# ,`scovsa`
w5 <- read_dta('wave_5_elsa_data_v4.dta') %>%
  select("idauniq","indsex","indobyr","w5edqual", "chinhh", "hehelf","dimar", "wpdes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","pscedc", "pscedh","hofood", "hooutf", "holeis",  "GOR", "iaomm", "hhtot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>% mutate(wave = 5)#scovsa`,hhsize can not find it
w6 <- read_dta('wave_6_elsa_data_v2.dta') %>%
  select("idauniq","indsex", "Indobyr", "chinhh", "Hehelf","DiMar", "WpDes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","PScedC", "PScedH","HoFood", "HoOutf", "HoLeis",   "GOR", "IaOmm", "HHTot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>% mutate(wave = 6)#,"w6edqual"
w7 <- read_dta('wave_7_elsa_data.dta') %>%
  select("idauniq","indsex", "Indobyr","chinhh", "Hehelf","DiMar", "WpDes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","PScedC", "PScedH","HoFood", "HoOutf", "HoLeis",   "gor", "IaOmm", "HHTot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 7)
w8 <- read_dta('wave_8_elsa_data_eul_v2.dta') %>%
  select("idauniq","indsex", "indobyr", "chinhh", "hehelf","dimarr", "wpdes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","pscedc", "pscedh","hofood", "hooutf", "holeis",  "gor", "iaomm", "hhtot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 8)
w9 <- read_dta('wave_9_elsa_data_eul_v1.dta') %>%
  select("idauniq","indsex", "indobyr", "chinhh", "hehelf","dimarr", "wpdes", "sclifea", "sclifeb","sclifec", "sclifed", "sclifee","pscedc", "pscedh","hofood", "hooutf", "holeis",  "GOR", "iaomm", "hhtot") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 9)


#**********************Rest of variables****************
wc2 <- read_dta("wave_2_ifs_derived_variables.dta") %>%
  select("idauniq", "ngrandchinhh", "numhhk013","wselfd_p", "wselfd", "age" ) %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 2)
wc3 <- read_dta("wave_3_ifs_derived_variables.dta") %>%
  select("idauniq", "ngrandchinhh", "numhhk013", "wselfd_p", "wselfd", "age"  ) %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 3)
wc4 <- read_dta("wave_4_ifs_derived_variables.dta") %>%
  select("idauniq",  "ngrandchinhh", "numhhk013","wselfd_p", "wselfd", "age" ) %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 4)
wc5 <- read_dta("wave_5_ifs_derived_variables.dta") %>%
  select("idauniq",  "ngrandchinhh", "numhhk013", "wselfd_p", "wselfd", "age" ) %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 5)
wc6 <- read_dta("wave_6_ifs_derived_variables.dta") %>%
  select("idauniq",  "ngrandchinhh", "numhhk013","wselfd_p", "wselfd", "age" ) %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 6)
wc7 <- read_dta("wave_7_ifs_derived_variables.dta") %>%
  select("idauniq", "ngrandchinhh", "numhhk013", "wselfd_p", "wselfd", "age") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 7)
wc8 <- read_dta("wave_8_elsa_ifs_dvs_eul_v1.dta") %>%
  select("idauniq",  "ngrandchinhh", "numhhk013","wselfd_p", "wselfd", "age" ) %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 8)
wc9 <- read_dta("wave_9_ifs_derived_variables.dta") %>%
  select("idauniq",  "ngrandchinhh", "numhhk013", "wselfd_p", "wselfd", "age") %>%
  rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 9)
#****************************************************************************
#-> Make variables names consistent by renaming
names(w6)[names(w6) == 'HoFood'] <- 'hofood'
names(w7)[names(w7) == 'HoFood'] <- 'hofood'
names(w6)[names(w6) == 'HoOutf'] <- 'hooutf'
names(w6)[names(w6) == 'HoLeis'] <- 'holeis'
names(w7)[names(w7) == 'HoOutf'] <- 'hooutf'
names(w7)[names(w7) == 'HoLeis'] <- 'holeis'
names(w6)[names(w6) == 'IaOmm'] <- 'iaomm'
names(w7)[names(w7) == 'IaOmm'] <- 'iaomm'


names(w3)[names(w3) == 'GOR'] <- 'gor'
names(w4)[names(w4) == 'GOR'] <- 'gor'
names(w5)[names(w5) == 'GOR'] <- 'gor'
names(w6)[names(w6) == 'GOR'] <- 'gor'
names(w9)[names(w9) == 'GOR'] <- 'gor'

names(w2)[names(w2) == 'HHTot'] <- 'hhtot'
names(w6)[names(w6) == 'HHTot'] <- 'hhtot'
names(w7)[names(w7) == 'HHTot'] <- 'hhtot'

names(w2) <- tolower(names(w2))
names(w2)[names(w2) == 'chinhh1'] <- 'chinhh'
names(w2)
names(w3)[names(w3) == 'w3edqual'] <- 'edqual'
names(w3)[names(w3) == 'hegenh'] <- 'hehelf'
names(w4)[names(w4) == 'w4edqual'] <- 'edqual'
names(w5)[names(w5) == 'w5edqual'] <- 'edqual'
names(w5)
names(w6) <- tolower(names(w6))
names(w6)
names(w7) <- tolower(names(w7))
names(w7)
names(w8) <- tolower(names(w8))
names(w8)[names(w8) == 'dimarr'] <- 'dimar'
names(w8)
names(w9) <- tolower(names(w9))
names(w9)[names(w9) == 'dimarr'] <- 'dimar'
names(w9)

#****************************************************************************
#****************************************************************************
# To combine these two files into long format we use the command "rbind"
#Elsa <- bind_rows(Elsa_w2, Elsa_w3, Elsa_w4,Elsa_w5, Elsa_w6, Elsa_w7, Elsa_w8, Elsa_w9, w2, w3, w4, w5, w6, w8, w9) %>%
#  select(sort(names(.))) %>%    # arrange columns in alphabetical order
#  select(idauniq, wave, everything())# %>% complete(idauniq)

Elsa1 <- bind_rows(Elsa_w2, Elsa_w3, Elsa_w4,Elsa_w5, Elsa_w6, Elsa_w7, Elsa_w8, Elsa_w9)
Elsa2 <- bind_rows(w2, w3, w4, w5, w6,w7, w8, w9)
Elsa3 <- bind_rows(wc2, wc3, wc4, wc5, wc6,wc7, wc8, wc9)

# merge data frames by ID

Elsa <- merge(Elsa1, Elsa2, by=c("idauniq","wave")) 
df <- merge(Elsa, Elsa3, by=c("idauniq","wave"))

## getting id & wave column as first
#data %>% select(department, everything())
df %>% select(sort(names(.))) %>%    # arrange columns in alphabetical order
  select(idauniq, wave, everything())  # put idauniq in front

rm(Elsa_w2, Elsa_w3, Elsa_w4,Elsa_w5, Elsa_w6, Elsa_w7, Elsa_w8, Elsa_w9, w2, w3, w4, w5, w6,w7, w8, w9, Elsa1, Elsa2, Elsa3, wc2, wc3, wc4, wc5, wc6,wc7, wc8, wc9 ) 

#****************************************************************************

dim(df)# [1] 77975    41 variables
length( unique(df$idauniq))#[1] 17,727
DataExplorer::plot_missing(df)

#replace missing values (integers -9 to -1) with NA
missval <- c(-9, -8, -4, -3, -2, -1)

for (i in 1:6) {
  df <- df %>%
    mutate(across(where(is.numeric), ~na_if(., missval[i]))) %>%
    mutate(across(where(is.character), ~na_if(., as.character(missval[i]))))
}


# add age column
df$squr_age <- (df$age * df$age)
# Filter out rows with unrealistic age values (e.g., age greater than 100)
df <- df[df$age <= 150, ]
# Verify the changes
summary(df$age)
# add location column
# Define variables
table(df$gor)
#Value = E12000004                  	Label = East Midlands#x
#Value = E12000001                  	Label = North East#x
#Value = E12000003  D                	Label = Yorkshire and The Humber
#Value = S99999999                  	Label = (pseudo) Scotland
#Value = L99999999                  	Label = (pseudo) Channel Islands#x
#Value = E12000009                  	Label = South West#x
#Value = E12000006                  	Label = East of England
#Value = E12000005                  	Label = West Midlands#x
#Value = W99999999                  	Label = (pseudo) Wales
#Value = E12000002  B                	Label = North West
#Value = E12000007  H                	Label = London
#Value = E12000008  J                	Label = South East


# First, you will need to ensure that both datasets have a common key column to join on. 

# Create location dummy variable columns using variable (df$gor_dv) when 
# 7 = London  10	= Wales  2=	North West Manchester and 0= other locations

#First, we can create a new column in the df data frame called location_dummy and initialize it to 0.
df$location_dummy <- 0
df$location_dummy <- ifelse(df$gor == "B", "North West Manchester", df$location_dummy)
df$location_dummy <- ifelse(df$gor == "D", "Yorkshire and the Humber", df$location_dummy)
df$location_dummy <- ifelse(df$gor == "E12000006", "East of England", df$location_dummy)
df$location_dummy <- ifelse(df$gor == "H", "London", df$location_dummy)
df$location_dummy <- ifelse(df$gor == "J", "South East", df$location_dummy)
df$location_dummy <- ifelse(df$gor == "W99999999", "Wales", df$location_dummy)
df$location_dummy <- ifelse(df$gor == "S99999999", "Scotland", df$location_dummy)

library(dplyr)

df <- df %>%
  mutate(location_dummy = ifelse(gor == "B", "North West Manchester",
                                 ifelse(gor == "D", "Yorkshire and the Humber",
                                        ifelse(gor == "E12000006", "East of England",
                                               ifelse(gor == "H", "London",
                                                      ifelse(gor == "J", "South East",
                                                             ifelse(gor == "W99999999", "Wales",
                                                                    ifelse(gor == "s99999999", "Scotland", 0))))))))


df$location <- df$location_dummy

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Create SWB variable by aggregating these 5 statements
#How much agrees with the statement:
#Variable = sclifea	 label =in most ways my life is close to my ideal
#Variable = sclifeb	 label = the conditions of my life are excellent
#Variable = sclifec	 label = I am satisfied with my life
#Variable = sclifed	 label = so far I have got the important things I want in life
#Variable = sclifee	 label = I could live my life again, I would change almost nothing
# Use the recode function to transform the ordinal variable values
df$SWB1 <- recode(as.numeric(df$sclifec), "7=1;6=2;5=3;4=4;3=5;2=6;1=7")
df$SWB2 <- recode(as.numeric(df$sclifeb), "7=1;6=2;5=3;4=4;3=5;2=6;1=7")
df$SWB3 <- recode(as.numeric(df$sclifee), "7=1;6=2;5=3;4=4;3=5;2=6;1=7")
df$SWB4 <- recode(as.numeric(df$sclifea), "7=1;6=2;5=3;4=4;3=5;2=6;1=7")
df$SWB5 <- recode(as.numeric(df$sclifed), "7=1;6=2;5=3;4=4;3=5;2=6;1=7")
df$SWB <- (df$SWB1 + df$SWB2 + df$SWB3 + df$SWB4 + df$SWB5)

#Add new variable risk taking scgrisk
#Only exist in one wave
w8 <- read_dta('wave_8_elsa_data_eul_v2.dta') %>%
  select("idauniq","scgrisk") %>% rename_at(vars(starts_with("_")), ~str_replace(.,"_", "")) %>%
  mutate(wave = 8)
w8$scgrisk
#replace missing values (integers -9 to -1) with NA
missval <- c(-9, -8,-4, -3, -2, -1, -7, 11)#-1 Not applicable -8 Don't know -9 Refusal 
for (i in 1:7) {
  w8 <- w8 %>%
    mutate_all(., list(~na_if(., missval[i])))}
DataExplorer::plot_missing(w8)

library(tidyr)

# Merge Elsa with w8 to add the "scgrisk" column to Elsa
Elsa_merged <- merge(df, w8[, c("idauniq", "wave", "scgrisk")], by = c("idauniq", "wave"), all.x = TRUE)

# Fill missing values in column "scgrisk" in Elsa_merged with values from w8 for wave 8
Elsa_filled <- Elsa_merged %>%
  group_by(idauniq) %>%
  fill(scgrisk, .direction = "downup") %>%
  ungroup()
df <- Elsa_filled

dim(df)# 77975    51
length( unique(df$idauniq))#[1] 17727
DataExplorer::plot_missing(df)
ELSA <- df 

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
#**Save the data file in Stata format**
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(haven)
write_dta(ELSA, "C:/Users/data/ELSA.dta")


