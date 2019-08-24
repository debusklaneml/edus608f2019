# Week 1 Examples
library(tidyverse)
library(readxl)
library(janitor)
library(tidylog)
library(DescTools)
library(fGarch)
#VDOE Cohort Data
cohort <- read_csv("http://www.doe.virginia.gov/statistics_reports/research_data/data_files/on-time-grad_cohort/2018-cohort.csv") %>%
  #clean up the variable names
  clean_names() %>%
  #create a new variable "full.cohort" that indicates "1" if all of the other identifying variables are missing/NA
  mutate(full.cohort = if_else(
    is.na(gender) & is.na(disability_flag) & is.na(lep_flag) & is.na(disadvantaged_flag) & is.na(federal_race_code) & level_code == "DIV", 1, 0)) %>%
  #keep/filter for only those that do not have subsample indications
  dplyr::filter(full.cohort == 1)

write_csv(cohort, "cohort.csv")

#histogram of dropout and diploma's granted.
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

#distributions
df <- data_frame(
  skewed_normal = rsnorm(n = 1000, mean = 0, sd = 18, xi = 130),
  exp_distrib = rexp(n = 1000, rate = .1),
  gamma_distrib = rgamma(n = 1000, shape = 2, scale = 2),
  beta_distrib = rbeta(n = 1000, shape1 = 4, shape2 = 2))

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df %>% 
  gather(key = distribution, value = value) %>% 
  dplyr::filter(distribution == "exp_distrib") %>% 
  ggplot(aes(x = value)) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  #geom_density(alpha = .2, fill="#FF6666") +
  #facet_wrap(~distribution) +
  scale_fill_manual(values = c("red", "green", "blue", "orange")) +
  scale_color_manual(values = c("red", "green", "blue", "orange")) +
  labs(title = "Histogram of random draws from a Beta distribution") +
  geom_vline(aes(xintercept = mean(value)), color = "red", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = getmode(value)), color = "blue") +
  geom_vline(aes(xintercept = median(value)), color = "blue", size = 1)


