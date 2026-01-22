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

datafile <- file.path(dataloc, 'apdc_hipfrac25.csv')

hipfrac <- read.csv(datafile)

#Checking data
hipfrac %>% generate_dictionary()

#Turning episode_start/end_date from chr into Date format
hipfrac <- hipfrac %>%
  mutate(episode_start_date = dmy(episode_start_date))
hipfrac <- hipfrac %>%
  mutate(episode_end_date = dmy(episode_end_date))

#Checking if they turned to Date format
str(hipfrac$episode_start_date)
str(hipfrac$episode_end_date)

#Check for dupicate rows and deleting and duplicates
hipfrac <- hipfrac %>% distinct()

#Only keeping observations between the dates 1/7/2005 to 30/6/2014
study_period_start <- as.Date("2005-07-01")
study_period_end <- as.Date("2014-06-30")

hipfrac <- hipfrac %>%
  filter(episode_start_date >= study_period_start & episode_end_date <= study_period_end)

#Flagging aged >= 65 and hip fracture as primary diagnosis
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

#Creating person_seq
hipfrac <- hipfrac %>%
  arrange(NOF_PPN, episode_start_date, episode_end_date) %>%
  group_by(NOF_PPN) %>%
  mutate(person_seq = row_number()) %>%
  ungroup() 

hipfrac %>% select(NOF_PPN, episode_start_date, episode_end_date, file_seq, person_seq)

#Creating index_seq
hipfrac <- hipfrac %>%
  arrange(NOF_PPN, episode_start_date, episode_end_date) %>%
  group_by(NOF_PPN) %>%
  mutate(hipflag_cumcount = cumsum(hipflag)) %>%
  mutate(post_index = (hipflag_cumcount > 0)*1) %>%
  ungroup()

hipfrac <- hipfrac %>%
  group_by(NOF_PPN, post_index) %>%
  mutate(index_seq = if_else(post_index == 1,
                             row_number(),
                             as.integer(0))) %>%
  ungroup()

hipfrac %>% 
  filter(NOF_PPN == 1710) %>%
  select(NOF_PPN, hipflag, person_seq, index_seq)

#Creating hip_seq
hipfrac <- hipfrac %>%
  group_by(NOF_PPN, hipflag) %>%
  mutate(hip_seq = if_else(hipflag == 1,
                             row_number(),
                             as.integer(0))) %>%
  ungroup()

hipfrac %>% 
  filter(NOF_PPN == 1710) %>%
  select(NOF_PPN, hipflag, person_seq, index_seq, hip_seq)

#Only keeping data that meets criteria which is if they have an observation with a hip fracture and are 65 or older
hipfrac_original <- hipfrac

hipfrac <- hipfrac %>%
  filter(NOF_PPN %in% NOF_PPN[hipflag == 1 & flag_65 == 1])

#Calculating how many individuals are in the study cohort
hipfrac %>% group_by(NOF_PPN)

#Creating a frequency table of first hip fracture admission by year
hipfrac <- hipfrac %>%
  mutate(year = year(episode_start_date)) #creating a new variable for year of admission

hipfrac %>% 
  filter(hip_seq == 1) %>%
  tabyl(year)

#Import death data
datafile <- file.path(dataloc, 'rbdmdeaths25.csv')

deaths <- read.csv(datafile)

deaths %>% generate_dictionary()

#Renaming variables
deaths <- deaths %>% rename(NOF_PPN = NOF_ppn)
deaths <- deaths %>% rename(age_dth = age_recode)
hipfrac <- hipfrac %>% rename(age_hosp = age_recode)

#Turning death_date into a Date format
deaths <- deaths %>%
  mutate(death_date = dmy(death_date))

deaths %>% generate_dictionary()

deaths <- deaths %>% distinct()
  #deleting repeated rows

#Finding individuals with multiple death dates
deaths %>%
  group_by(NOF_PPN) %>%
  filter(n() > 1)

#Checking each individual and comparing with hospitals admission
hipfrac %>%
  filter(NOF_PPN == 2022) %>%
  select(NOF_PPN, episode_start_date, episode_end_date)

deaths <- deaths %>%
  filter(!(NOF_PPN == 2022 & age_dth == 79)) #deleting first death observation

hipfrac %>%
  filter(NOF_PPN == 9432) %>%
  select(NOF_PPN, episode_start_date, episode_end_date)
  
hipfrac <- hipfrac %>%
  filter(!(NOF_PPN == 9432))

deaths <- deaths %>%
  filter(!(NOF_PPN == 9432))#deleting PPN 9432 because there are no hospital admission after 2011 where the death date is stated

hipfrac %>%
  filter(NOF_PPN == 10277) %>%
  select(NOF_PPN, episode_start_date, episode_end_date) %>%
  print(n = 34)

deaths <- deaths %>%
  filter(!(NOF_PPN == 10277 & age_dth == 90)) #deleting first death date

#Merging datasets
hipfrac <- hipfrac %>% arrange(NOF_PPN, episode_start_date, episode_end_date)
deaths <- deaths %>% arrange(NOF_PPN)

merged_data <- hipfrac %>% 
  left_join(deaths, by = "NOF_PPN")

#Individuals in merged data set
merged_data %>% group_by(NOF_PPN)

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

transfers_data %>% filter(hip_seq == 1 & transseq == 14) %>% select(NOF_PPN, transseq)
