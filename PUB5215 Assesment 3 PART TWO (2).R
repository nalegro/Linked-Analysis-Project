library(tidyverse) 
library(haven) 
library(labelled) #generate_dictionary()
library(lubridate) #ymd
library(dplyr)
install.packages("janitor")
library(janitor)
install.packages("archive")
library("archive")
library(ggplot2)
install.packages("gtsummary")
library(gtsummary)

#Loading dataset
rds <- '//shared.sydney.edu.au/research-data/PRJ-Linked_data_course'
dataloc <- file.path(rds, 'Assessment 3')

cleaneddf <- read.csv(archive::archive_read(file.path(dataloc, paste0("cleaned_merged", ".zip"))
                                            , "cleaned_merged.csv", password = "R4linkedDat"))

## Question 4
#Creating frequency table of number of hip fracture (ignoring transfers)
cleaneddf %>% 
  filter(hf_flag == 1) %>%
  group_by(NOF_PPN) %>%
  summarise(hip_seq = max(hip_seq)) %>% #makes sure it is per person
  tbl_summary(
    label = list(hip_seq ~ "Number of hip fracture admissions")
  ) %>%
  modify_caption("Number of hip fractures per person when we ignore transfers")

#Creating hip sequence accounting for transfers
cleaneddf_q4 <- cleaneddf %>%
  filter(transseq == 0, hf_flag == 1) %>% #includes the first episode and had a hip fracture
  group_by (NOF_PPN) %>%
  mutate(hf_stays_all = row_number()) %>% #new variable that includes all hospital stays by a patient
  select(NOF_PPN, stayseq, hf_stays_all) 
  #creating new data set for hip fractures and transfers

cleaneddf <- cleaneddf %>%
  left_join(cleaneddf_q4, by = c("NOF_PPN", "stayseq")) %>%
  mutate(hf_stays_all = replace_na(hf_stays_all,0))
  #merging with original data set

cleaneddf %>% 
  filter(NOF_PPN == 3399) %>%
  select(NOF_PPN, hf_flag, hip_seq, transseq, stayseq, hf_stays_all)
  #checking if done correctly

#Creating frequency table for number of hip fracture (accounting for transfers)
cleaneddf %>% 
  filter(hf_flag == 1) %>%
  group_by(NOF_PPN) %>%
  summarise(hf_stays_all = max(hf_stays_all)) %>% #makes sure it is per person
  tbl_summary(
    label = list(hf_stays_all ~ "Number of hip fracture admissions")
  ) %>%
  modify_caption("Number of hip fractures per person accounting for transfers")

#Frequency table for mode of separation ignoring transfers
cleaneddf %>%
  tbl_summary(
    by = mode_of_separation_recode,
    include = hip_seq,
    label = list(
      hip_seq ~ "Number of hip fracture admissions"))

#Frequency table for mode of separation accounting for transfers
cleaneddf %>%
  tbl_summary(
    by = mode_of_separation_recode,
    include = hf_stays_all,
    label = list(
      hf_stays_all ~ "Number of hip fractture admissions"))
  
## Question 5
first_hip_los <- cleaneddf %>%
  filter(hf_stays_all == 1) %>% 
  group_by(NOF_PPN, hf_stays_all)
  #creating data set for total length of stay for first hip fracture

first_hip_los <- cleaneddf %>%
  filter(hip_seq == 1) %>% 
  group_by(NOF_PPN)

#Histogram of total length of stay for first hip fracture
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

#Boxplot of total length of stay for first hip fracture
ggplot(first_hip_los, aes(y = totlos)) +
  geom_boxplot(fill = "skyblue") +
  scale_y_continuous(
    breaks = seq(0, 100, by = 5)) +
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
    #lists length of stay from highest to lowest

cleaneddf %>% 
  arrange(desc(totlos)) %>% #highest total length of stay will appear first
  filter(NOF_PPN %in% c(5240, 5059, 1392, 7327, 2134) & hip_seq == 1) %>%
  select(NOF_PPN, hip_seq, totlos) 
  #Selecting the top five patients

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

#Mean, sd, median and quantiles of totlos for first hip fracture (extremes removed)
cleaneddf_outlier %>% 
  filter(hip_seq == 1) %>%
  summarise(
    mean(totlos),
    sd(totlos),
    median(totlos),
    quantile(totlos, 0.25),
    quantile(totlos, 0.75))

## Question 6
first_hip <- cleaneddf %>%
  filter(hip_seq == 1) 
  #new data set with only first hip fractures

first_hip <- first_hip %>%
  mutate(discharge_date = sepdate_last) 
  #making new variable for discharge date of first hip fracture

all_admission <- cleaneddf %>%
  filter(NOF_PPN %in% first_hip$NOF_PPN) %>% #all rows where a NOF_PPN is in first_hip data set are kept
  arrange(NOF_PPN, episode_start_date)
  #making new variable of all the admissions of a patient with a hip fracture
  
next_admissions <- all_admission %>%
  left_join(first_hip %>% select(NOF_PPN, discharge_date), by = "NOF_PPN") %>% #merging discharge date from first admission
  filter(episode_start_date > discharge_date) %>% #admissions occurring after the discharge date
  group_by(NOF_PPN) %>%
  slice_min(episode_start_date, with_ties = FALSE) %>% #picking earliest episode start date
  rename(next_admission_date = episode_start_date) %>%
  select(NOF_PPN, next_admission_date)
  # making new variable for next admission after discharged

first_hip <- first_hip %>%
  left_join(next_admissions, by = "NOF_PPN")
  #merging next admission into the first_hip data set

#Creating censor variable
first_hip <- first_hip %>%
  mutate(event = if_else(!is.na(next_admission_date), 1,0))
  #1 - readmission
  #0 - censor

study_end <- as.Date("2014-06-30") 
  #creating a value for the study end date

first_hip %>% generate_dictionary()
  # check if all date variables are in date format

# Changing dates into Date format 
first_hip <- first_hip %>%
  mutate(
    next_admission_date = ymd(next_admission_date),
    discharge_date = ymd(discharge_date),
    death_date = ymd(death_date)
  )

first_hip <- first_hip %>%
  mutate(
    censor_date = pmin(death_date, study_end, na.rm = TRUE), #variable for the earliest date between death or end of study
    end_date = if_else(event == 1, next_admission_date, censor_date)) #if readmitted, end date is next admission date, otherwise it is censor_date

first_hip <- first_hip %>% 
  mutate(time_to_next = as.numeric(end_date - discharge_date))
  #creating time to next admission variable

# check for negative survival times
sum(first_hip$time_to_next < 0, na.rm = TRUE)
  # resulted in 0 which is ideal

library(stringr)

first_hip <- first_hip %>% charlson(data = first_hip) 
  #using charlson function for the comorbidities index

#Creating categorical variables for comorbidities
first_hip <- first_hip %>%
  mutate(
    cci_group = recode(ccicat,
                       "[0,1)" = "0",
                       "[1,3)" = "1-2",
                       "[3,Inf)" = "3+"))

library(survival)
library(survminer)

surv_obj <- Surv(time = first_hip$time_to_next,
                 event = first_hip$event)
  #creating survival object for time to next admission and readmission

first_hip$cci_group <- factor(first_hip$cci_group,
                              levels = c("0","1-2","3+"),
                              labels = c("CCI 0","CCI 1–2","CCI 3+"))
  #renaming and turning cci_groups into factors with labels

km_fit <- survfit(surv_obj ~ cci_group, data = first_hip)
  #fitting in K-M curve by CCI groups

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
    readmitted = sum(event == 1), #patients who were readmitted
    censored_death = sum(event == 0 & !is.na(death_date) & death_date <= study_end), #patients censored due to death
    censored_end = sum(event == 0 & (is.na(death_date) | death_date > study_end) #patients censored due to end of study
  )) %>%
  mutate(
    readmitted_prop = readmitted / total_patients * 100) #proportion of patients who were readmitted within the group