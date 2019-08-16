# Week 1 Examples
library(tidyverse)
library(readxl)
library(janitor)
library(tidylog)
library(DescTools)
#VDOE Cohort Data
cohort <- read_csv("http://www.doe.virginia.gov/statistics_reports/research_data/data_files/on-time-grad_cohort/2018-cohort.csv") %>%
  #clean up the variable names
  clean_names() %>%
  #create a new variable "full.cohort" that indicates "1" if all of the other identifying variables are missing/NA
  mutate(full.cohort = if_else(
    is.na(gender) & is.na(disability_flag) & is.na(lep_flag) & is.na(disadvantaged_flag) & is.na(federal_race_code) & level_code == "DIV", 1, 0)) %>%
  #keep/filter for only those that do not have subsample indications
  filter(full.cohort == 1)

dropout <- cohort %>%
  ggplot(aes(dropout_rate)) + 
  geom_histogram()
dropout

diploma <- cohort %>%
  ggplot(aes(diploma_rate)) +
  geom_histogram()
diploma

skimr::skim(cohort)
Hmisc::describe(cohort)
summarytools::freq(cohort$cohort_cnt)
