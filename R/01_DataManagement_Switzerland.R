# ----------------------------------------------------------------------------#
# DataManagement_Switzerland.R
# 
# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
ifelse(class(source("setup.R"))!="try-error",
       print(paste("setup.R sourced.")))
# ----------------------------------------------------------------------------#
library(tidyverse)
library(lubridate)

# ----------------------------------------------------------------------------#
# input ####
# ----------------------------------------------------------------------------#
# should we move all this to a separate file that can be read in? This might 
# increase the possibility that we can write functions and decreases the need
# to repeat ourselves

# Dates ----------------------------------------------------------------------#
### Time period is different
{
  day_start =      as.Date("2020-03-01")
  day_data =       as.Date("2020-03-02")
  day_max =        as.Date("2020-04-23") #10 days before last date of data
  day_quarantine = as.Date("2020-03-16")
}

# Age distribution -----------------------------------------------------------#
### Not hardwired as China, but from csv.
  # from : data/contact_matrix/age_distribution.csv
  # from: http://data.un.org/Data.aspx?d=POP&f=tableCode%3A22
  # uses a self-defined function : age_class
excel_age_dist=read.csv("data/all_regions_pop_age_dist.csv",header=T)

age_class=function(x,min_age,age_range,max_age){
  age_lim=seq(min_age,max_age,age_range)
  return(sapply(as.list(x),function(x) sum(age_lim<=x)))
}
age_dist <-as.data.frame(excel_age_dist) %>%
  filter(Country.or.Area=="Switzerland") %>%
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group=age_class(as.numeric(levels(Age))[Age],10,10,80)) %>% 
  group_by(Sex, age_group) %>%
  summarise(n=sum(Value),.groups = "keep") %>%
  pull(n)

age_dist <- age_dist/sum(age_dist) # getting relative age distribution

pop_t = 8.545e6

### Both cases (A) & deaths (B) are from:
   # data/switzerland/agg_data.csv

cases_deaths=read.csv("data/Switzerland_cases.csv",header=T) %>%
  transmute(date=ymd(as.character(date)), new_cases=count_pos, 
            new_cases_onset=onset_dt, new_deaths=death_dt) %>%
  filter(date>=ymd(day_data),date<=ymd(day_max))

# dataset A: confirmed daily cases -------------------------------------------#
incidence_cases = pull(cases_deaths,new_cases_onset)

# dataset C: confirmed daily deaths ------------------------------------------#
incidence_deaths = pull(cases_deaths,new_deaths)
  
### Both age distributions of cases (B) and deaths(D) are from : 
  # data/switzerland/age_cases_mort.csv
  
agedistr_cases_deaths=read.csv("data/Switzerland_age_dist.csv",header=T) %>%
  select(age_class, cases, deaths)

# dataset B: age distribution of all cases ------------------------#
agedistr_cases = pull(agedistr_cases_deaths,cases)

# dataset D: age distribution of all deaths -----------------------#
agedistr_deaths = pull(agedistr_cases_deaths,deaths)

# ----------------------------------------------------------------------------#

p_underreport_cases=sum(incidence_cases)/sum(cases_deaths$new_cases)
p_underreport_cases
p_underreport_deaths=1
