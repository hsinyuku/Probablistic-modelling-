# ----------------------------------------------------------------------------#
# DataManagement_Lombardy.R
# 
# This file prepares the data that is used for modelling.
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
ifelse(class(source("setup.R"))!="try-error",
       print(paste("setup.R sourced.")))
# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# input ####
# ----------------------------------------------------------------------------#

# Dates ----------------------------------------------------------------------#
# Lockdown instituted on 24/02/2020

{
  day_start = as.Date("2020-02-10")
  day_data = as.Date("2020-02-11")
  day_max = as.Date("2020-04-25")
  day_quarantine = as.Date("2020-02-24")
}

# summarise exact ages into age groups, starting from min_age and going to
# max_age in intervals of age_range (both min_age and max_age are upper bound
# for the age groups)

age_class = function(x, min_age, age_range, max_age) {
  age_lim = seq(min_age, max_age, age_range)
  return(sapply(as.list(x), function(x) sum(age_lim <= x)))
} 

# Age distribution In Lombardy -----------------------------------------------#
age_dist <- read_csv("data/all_regions_pop_age_dist.csv")
# error messages are ok, there are footnotes that are not part of the data

age_dist <- age_dist %>% 
  filter(`Country or Area` == "Italy") %>%
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group = age_class(as.numeric(Age),10,10,80)) %>% 
  group_by(age_group) %>%
  summarise(n = sum(Value), .groups = "keep") %>%
  pull(n)
age_dist = age_dist/sum(age_dist)

## Population in Lombardy  
# (Source Eurostat 2018)

pop_t = 10.04e6

# datasets -------------------------------------------------------------------#
# all the data has been extracted from the appendix to the daily bulletin of the Italian Istituto Superiore
# di Sanità (ISS) published on 28/04:
# https://www.epicentro.iss.it/coronavirus/bollettino/Bollettino-sorveglianza-integrata-COVID-19_28-aprile-2020_appendix.pdf 

# datasets A and B: confirmed daily cases and deaths -------------------------#

lombardy_cases_deaths <- read.csv("data/Lombardy_Cases_Deaths_Confirmed.csv") %>% 
  tbl_df() %>%
  mutate(date=ymd(paste("2020",month,day,sep="-"))) %>% 
  filter(date>=day_data,date<=day_max)

# dataset A -----------------------------------------------------------------#

incidence_cases = pull(lombardy_cases_deaths,cases)
sum(incidence_cases)

# dataset C ------------------------------------------------------------------#

incidence_deaths = pull(lombardy_cases_deaths,deaths)
sum(incidence_deaths)

# dataset B and D: Age distribution of cases and deaths ----------------------#

age_distributions_cases_deaths = read.csv("data/Age_Distribution_Cases_Deaths_Lombardy.csv") %>%
  tbl_df()

# dataset B ------------------------------------------------------------------#

cases_tmax = pull(age_distributions_cases_deaths,cases)

agedistr_cases = cases_tmax
sum(cases_tmax)
prop_cases_tmax = cases_tmax / sum(cases_tmax)

# dataset D ------------------------------------------------------------------#

mort_tmax = pull(age_distributions_cases_deaths,deaths)
agedistr_deaths = mort_tmax
sum(agedistr_deaths)

prop_mort_tmax = mort_tmax / sum(mort_tmax)

# Underreporting in all of Italy ---------------------#

p_underreport_cases = sum(incidence_cases)/74346

# ----------------------------------------------------------------------------#
