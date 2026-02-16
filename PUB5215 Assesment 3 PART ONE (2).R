library(tidyverse) 
library(haven) 
library(skimr) 
library(labelled) 
library(janitor) 
library(lubridate)
library(dplyr)


#Importing hospital data into R
rds <- '//shared.sydney.edu.au/research-data/PRJ-Linked_data_course'
dataloc <- file.path(rds, 'Assessment 3')

## Question 1
datafile <- file.path(dataloc, 'apdc_hipfrac25.csv')

hipfrac <- read.csv(datafile)

#Checking data
hipfrac %>% generate_dictionary()
  # episode_start_date and episode_end_date should be Date format

#Turning episode_start/end_date from chr into Date format
hipfrac <- hipfrac %>%
  mutate(episode_start_date = dmy(episode_start_date))
hipfrac <- hipfrac %>%
  mutate(episode_end_date = dmy(episode_end_date))

#Check if they turned to Date format
str(hipfrac$episode_start_date)
str(hipfrac$episode_end_date)

#Check for duplicate rows and deleting the duplicates
any(duplicated(hipfrac))
hipfrac <- hipfrac %>% distinct()

#Only keeping observations between the dates 1/7/2005 to 30/6/2014
study_period_start <- as.Date("2005-07-01") # creating a start date value
study_period_end <- as.Date("2014-06-30") # creating an end date value

hipfrac <- hipfrac %>%
  filter(episode_start_date >= study_period_start & episode_end_date <= study_period_end)
  # Excluding all data not included in the study period

#Flagging patients aged greater than or equal to 65 and hip fracture as the primary diagnosis
hipfrac <- hipfrac %>% 
  arrange(NOF_PPN) %>%
  mutate(flag_65 = (age_recode >= 65)*1) %>%
  mutate(hipflag = if_else(grepl("^S72\\.[012]", diagnosis_codeP), 1, 0))

table(hipfrac$flag_65, useNA = "ifany")

#Creating file_seq
hipfrac <- hipfrac %>%
  arrange(NOF_PPN, episode_start_date, episode_end_date) %>%
  mutate(file_seq = row_number())

hipfrac %>% select(NOF_PPN, age_recode, file_seq)
  # checking if done correctly

#Creating person_seq - counts number of episodes
hipfrac <- hipfrac %>%
  arrange(NOF_PPN, episode_start_date, episode_end_date) %>%
  group_by(NOF_PPN) %>%
  mutate(person_seq = row_number()) %>%
  ungroup() 

hipfrac %>% select(NOF_PPN, episode_start_date, episode_end_date, file_seq, person_seq)
  # checking if done correctly

hipfrac <- hipfrac %>%
  arrange(NOF_PPN, episode_start_date, episode_end_date) %>%
  group_by(NOF_PPN) %>%
  mutate(hipflag_cumcount = cumsum(hipflag)) %>% # creating a variable for cumulative observations
  mutate(post_index = (hipflag_cumcount > 0)*1) %>% # creating a flag for observations after the index record
  ungroup()

#Creating index_seq - counts number of episodes starting from hip fracture as the first episode
hipfrac <- hipfrac %>%
  group_by(NOF_PPN, post_index) %>%
  mutate(index_seq = if_else(post_index == 1,
                             row_number(),
                             as.integer(0))) %>%
  ungroup()

hipfrac %>% 
  filter(NOF_PPN == 1710) %>%
  select(NOF_PPN, hipflag, person_seq, index_seq)
  # checking if done correctly

#Creating hip_seq - counts number of episodes with hip fracture as primary diagnosis
hipfrac <- hipfrac %>%
  group_by(NOF_PPN, hipflag) %>%
  mutate(hip_seq = if_else(hipflag == 1,
                             row_number(),
                             as.integer(0))) %>%
  ungroup()

hipfrac %>% 
  filter(NOF_PPN == 1710) %>%
  select(NOF_PPN, hipflag, person_seq, index_seq, hip_seq)
  # checking if done correctly

#keeping only records of patients aged 65 and over with a hip fracture as a primary diagnosis
hipfrac <- hipfrac %>%
  filter(NOF_PPN %in% NOF_PPN[hipflag == 1 & flag_65 == 1])
  #patients meets study criteria, but keeps all of the patients records

#Calculating how many individuals are in the study cohort
hipfrac %>% group_by(NOF_PPN)

#creating a new variable for year of admission
hipfrac <- hipfrac %>%
  mutate(year = year(episode_start_date)) 

#Creating a frequency table of first hip fracture admission by year
hipfrac %>% 
  filter(hip_seq == 1) %>%
  tabyl(year)

## Question 2
#Import death data
datafile <- file.path(dataloc, 'rbdmdeaths25.csv')

deaths <- read.csv(datafile)

deaths %>% generate_dictionary()
  # death date is in chr format

#Renaming variables to match the hipfrac dataset
deaths <- deaths %>% rename(NOF_PPN = NOF_ppn)
deaths <- deaths %>% rename(age_dth = age_recode)
hipfrac <- hipfrac %>% rename(age_hosp = age_recode)

#Turning death_date into a Date format
deaths <- deaths %>%
  mutate(death_date = dmy(death_date))

deaths %>% generate_dictionary() # checking all variables are in correct format

any(duplicated(deaths))
deaths <- deaths %>% distinct()
  #deleting exact duplicates

#Finding individuals with multiple death dates
deaths %>%
  group_by(NOF_PPN) %>%
  filter(n() > 1)

#Checking each individual and comparing with hospitals admission
## NOF_PPN 2022
hipfrac %>%
  filter(NOF_PPN == 2022) %>%
  select(NOF_PPN, episode_start_date, episode_end_date)

deaths <- deaths %>%
  filter(!(NOF_PPN == 2022 & age_dth == 79)) 
  #deleting first death observation as there are episodes after the first death date and before the second one

## NOF_PPN 9432
hipfrac %>%
  filter(NOF_PPN == 9432) %>%
  select(NOF_PPN, episode_start_date, episode_end_date)
  
hipfrac <- hipfrac %>%
  filter(!(NOF_PPN == 9432))

deaths <- deaths %>%
  filter(!(NOF_PPN == 9432))
  #deleting PPN 9432 because there are no hospital admission after 2011 where the death date is stated

## NOF_PPN 10277
hipfrac %>%
  filter(NOF_PPN == 10277) %>%
  select(NOF_PPN, episode_start_date, episode_end_date) %>%
  print(n = 34)

deaths <- deaths %>%
  filter(!(NOF_PPN == 10277 & age_dth == 90)) 
  #deleting first death date as there are records after the first death date and before the second one

#Merging datasets
hipfrac <- hipfrac %>% arrange(NOF_PPN, episode_start_date, episode_end_date)
deaths <- deaths %>% arrange(NOF_PPN)

merged_data <- hipfrac %>% 
  left_join(deaths, by = "NOF_PPN")

#Individuals in merged data set
merged_data %>% group_by(NOF_PPN)


## Question 3
#Using transfers function on the merged dataset
library(devtools)
install_github('james-hedley/transfers')
library(transfers)

transfers_data <- merged_data %>% transfers(id = "NOF_PPN",
                               admdate = "episode_start_date",
                               sepdate = "episode_end_date",
                               mode = "mode_of_separation_recode")

transfers_data %>% 
  select(NOF_PPN, episode_start_date, episode_end_date, mode_of_separation_recode,
         episode, stayseq, transseq)


#Creating a frequency table for total number of episodes for first hip fracture
transfers_data %>% 
  filter(hip_seq == 1) %>%
  tabyl(transseq)
  # maximum number if transfers in the table shows 14 transfers

transfers_data %>% filter(hip_seq == 1 & transseq == 14) %>% select(NOF_PPN, transseq)
