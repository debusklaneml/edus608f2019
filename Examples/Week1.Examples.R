# Week 1 Examples
library(tidyverse)
library(readxl)
library(janitor)
library(tidylog)

#VDOE Cohort Data
cohort <- read_csv("http://www.doe.virginia.gov/statistics_reports/research_data/data_files/on-time-grad_cohort/2018-cohort.csv") %>%
  clean_names() %>%
  mutate(full.cohort = if_else(
    is.na(gender) & is.na(disability_flag) & is.na(lep_flag) & is.na(disadvantaged_flag) & is.na(federal_race_code) & level_code == "DIV", 1, 0)) %>%
  filter(full.cohort == 1)

dropout <- cohort %>%
  ggplot(aes(dropout_rate)) + 
  geom_histogram()
dropout

diploma <- cohort %>%
  ggplot(aes(diploma_rate)) +
  geom_histogram()
diploma

