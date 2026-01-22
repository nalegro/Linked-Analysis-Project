library(tidyverse) 
library(haven) 
library(labelled) 
library(lubridate)
library(dplyr)
install.packages("janitor")
library(janitor)
install.packages("archive")
library("archive")
library(ggplot2)

#Loading dataset
rds <- '//shared.sydney.edu.au/research-data/PRJ-Linked_data_course'
dataloc <- file.path(rds, 'Assessment 3')

cleaneddf <- read.csv(archive::archive_read(file.path(dataloc, paste0("cleaned_merged", ".zip"))
                                            , "cleaned_merged.csv", password = "R4linkedDat"))

#Creating frequency table of number of hip fracture (ignoring transfers)
cleaneddf %>% 
  filter(hf_flag == 1) %>%
  group_by(NOF_PPN) %>%
  summarise(hip_seq = max(hip_seq)) %>% #makes sure it is per person
  tabyl(hip_seq)

#Creating hip sequence accounting for transfers
cleaneddf_q4 <- cleaneddf %>%
  filter(transseq == 0, hf_flag == 1) %>%
  group_by (NOF_PPN) %>%
  mutate(hf_stays_all = row_number()) %>%
  select(NOF_PPN, stayseq, hf_stays_all) 
  #creating new data set for hip fractures and transfers

cleaneddf <- cleaneddf %>%
  left_join(cleaneddf_q4, by = c("NOF_PPN", "stayseq")) %>%
  mutate(hf_stays_all = replace_na(hf_stays_all,0))
  #merging with original data set

cleaneddf %>% 
  filter(NOF_PPN == 3399) %>%
  select(NOF_PPN, hf_flag, hip_seq, transseq, stayseq, hf_stays_all)

#Creating frequency table for number of hip fracture (accounting for transfers)
cleaneddf %>% 
  filter(hf_flag == 1) %>%
  group_by(NOF_PPN) %>%
  summarise(hf_stays_all = max(hf_stays_all)) %>% #makes sure it is per person
  tabyl(hf_stays_all)

#Frequency table for mode of separation
cleaneddf %>%
  tabyl(hip_seq, mode_of_separation_recode)

cleaneddf %>%
  tabyl(hf_stays_all, mode_of_separation_recode)

#Histogram of totlos for first hip fracture
first_hip_los <- cleaneddf %>%
  filter(hf_stays_all == 1) %>% 
  group_by(NOF_PPN, hf_stays_all)
  #creating data set for total length of stay for first hip fracture

ggplot(first_hip_los, aes(x = totlos)) +
  geom_histogram(binwidth = 1, fill = "skyblue", color = "black") +
  labs(title = "Histogram of Total LOS for First Hip Fractures",
       x = "Total Length of Stay (days)",
       y = "Frequency") +
  coord_cartesian(xlim = c(0, 112),
                  ylim = c(0, 500)) +
  scale_x_continuous(breaks = seq(0, 110, 10),
                     minor_breaks = seq(0, 110, 1),
                     expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 500, 50),
                     minor_breaks = seq(0, 500, 10),
                     expand = c(0, 0)) +
  theme_minimal()

#Boxplot of totlos for first hip fracture
ggplot(first_hip_los, aes(y = totlos)) +
  geom_boxplot(fill = "skyblue") +
  coord_cartesian(ylim = c(0, 112)) +
  labs(title = "Boxplot of Total LOS for First Hip Fractures (linked)",
       y = "Total Length of Stay (days)", x = "") +
  theme_minimal()

#Persons with most extreme total lengths of stay
cleaneddf %>% 
  group_by(NOF_PPN) %>%
  filter(hip_seq == 1) %>%
  summarise(max_los = max(totlos)) %>% #finding highest length of stay
  arrange(desc(max_los)) %>%
  select(NOF_PPN, max_los) 

cleaneddf %>% 
  arrange(desc(totlos)) %>% #highest totlos will appear first
  filter(NOF_PPN %in% c(5240, 5059, 1392, 7327, 2134) & hip_seq == 1) %>%
  select(NOF_PPN, hip_seq, totlos) 
  #Looking at top 5

#Mean, sd, meadian and quantiles of totlos for first hip fracture
cleaneddf %>% 
  filter(hip_seq == 1) %>%
  summarise(
    mean(totlos),
    sd(totlos),
    median(totlos),
    quantile(totlos, 0.25),
    quantile(totlos, 0.75))

cleaneddf_outlier <- cleaneddf %>%
  filter(!(NOF_PPN %in% c(5240, 5059, 1392, 7327, 2134) & totlos %in% c(2107, 1878, 1620, 1569, 1384) & hip_seq == 1))
  #making new data set removing top five extreme totlos from data

#Mean, sd, meadian and quantiles of totlos for first hip fracture (extremes removed)
cleaneddf_outlier %>% 
  filter(hip_seq == 1) %>%
  summarise(
    mean(totlos),
    sd(totlos),
    median(totlos),
    quantile(totlos, 0.25),
    quantile(totlos, 0.75))

#Question 6
first_hip <- cleaneddf %>%
  filter(hip_seq == 1) #new data set with only first hip fractures

first_hip <- first_hip %>%
  mutate(discharge_date = sepdate_last) 
  #making new variable for discharge date of first hip fracture

all_admission <- cleaneddf %>%
  filter(NOF_PPN %in% first_hip$NOF_PPN) %>% #only NOF_PPN in first_hip data set are kept
  arrange(NOF_PPN, episode_start_date)
  
next_admissions <- all_admission %>%
  left_join(first_hip %>% select(NOF_PPN, discharge_date), by = "NOF_PPN") %>% #merging discharge date from first admission
  filter(episode_start_date > discharge_date) %>% #admissions occuring after the discharge date
  group_by(NOF_PPN) %>%
  slice_min(episode_start_date, with_ties = FALSE) %>% #picking earliest episode start date
  rename(next_admission_date = episode_start_date) %>%
  select(NOF_PPN, next_admission_date)

first_hip <- first_hip %>%
  left_join(next_admissions, by = "NOF_PPN")
  #merging into first_hip data set

#Creating censor variable
first_hip <- first_hip %>%
  mutate(censor = is.na(next_admission_date)*1)
  #1 - readmission
  #0 - censor

study_end <- "2014-06-30" #creating a value for the study end date

first_hip <- first_hip %>%
  mutate(next_admission_date = if_else(
    is.na(next_admission_date) & death_date <= study_end,
    death_date,
    next_admission_date
  )) #replacing next admission date with death date, if death date is earlier than end of study date

first_hip <- first_hip %>%
  mutate(next_admission_date = if_else(
    is.na(next_admission_date) & death_date >= study_end,
    study_end,
    next_admission_date
  )) #replacing next admission date with study end date, if death date is later than end of study date

first_hip <- first_hip %>%
  mutate(next_admission_date = if_else(
    is.na(next_admission_date) & is.na(death_date),
    study_end,
    next_admission_date
  )) #replacing next admission date with study end date, if death date is missing value

first_hip %>% generate_dictionary() #checking formats

#Changing discharge date and next admission date into Date format
first_hip <- first_hip %>%
  mutate(next_admission_date = ymd(next_admission_date))
first_hip <- first_hip %>%
  mutate(discharge_date = ymd(discharge_date))

first_hip <- first_hip %>% 
  mutate(time_to_next = as.numeric(next_admission_date - discharge_date))
  #creating time to next admission variable

first_hip <- first_hip %>% charlson(data = first_hip) #using charlson function

#Creating categorical variables for comorbidities
first_hip <- first_hip %>%
  mutate(
    cci_group = recode(ccicat,
                       "[0,1)" = "0",
                       "[1,3)" = "1-2",
                       "[3,Inf)" = "3+"))

library(survival)
install.packages("survminer")
library(survminer)

surv_obj <- Surv(time = first_hip$time_to_next,
                 event = first_hip$censor)
  #creating survival object for time to next admission and readmission

km_fit <- survfit(surv_obj ~ cci_group, data = first_hip)
  #fitting in K-M curve by CCI groups

first_hip$cci_group <- factor(first_hip$cci_group,
                              levels = c("0","1-2","3+"),
                              labels = c("CCI 0","CCI 1–2","CCI 3+"))
  #renaming and turning cci_groups into factors with labels

#Creating K-M plot
ggsurvplot(km_fit, data = first_hip,
           pval = TRUE,           
           conf.int = TRUE,        
           legend.title = "CCI Group",
           xlab = "Days to Re-admission",
           ylab = "Survival Probability",
           xlim = c(0, 1000),
           break.time.by = 200)  

#Creating table with number of individuals
first_hip %>%
  group_by(cci_group) %>%
  summarise(
    total_patients = n(), #total patients with first hip fracture
    readmitted = sum(censor == 1), #patients who were readmitted
    censored_death = sum(censor == 0 & death_date < study_end, na.rm = TRUE), #patients censored due to death
    censored_end = sum(censor == 0 & death_date > study_end, na.rm = TRUE) #patients censored due to end of study
  ) %>%
  mutate(
    readmitted_prop = readmitted / total_patients * 100) #proportion of patients who were readmitted within the group
