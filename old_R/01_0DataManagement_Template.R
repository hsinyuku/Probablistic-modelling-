# ----------------------------------------------------------------------------#
# DataManagement_Template.R
# 
# This file prepares the data that is used for modelling, and is the
# template file with which the other files can be compared.
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
# should we move all this to a separate file that can be read in? This might 
# increase the possibility that we can write functions and decreases the need
# to repeat ourselves

# Dates ----------------------------------------------------------------------#
{
  day_start =       as.Date("2019-12-31")
  day_data =        as.Date("2020-01-01")
  day_max =         as.Date("2020-02-11")
  day_quarantine =  as.Date("2020-01-20")
}

# Age distribution -----------------------------------------------------------#
age_dist <- read_csv("data/age_distribution_total_population_all_regions.csv")
# error messages are ok, there are footnotes that are not part of the data

# Function to classify age groups --------------------------------------------#
age_class = function(x, min_age, age_range, max_age) {
  age_lim = seq(min_age, max_age, age_range)
  return(sapply(as.list(x), function(x) sum(age_lim <= x)))
} 

age_dist <- age_dist %>% 
  filter(`Country or Area` == "_____") %>% ### fill in your area
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group = age_class(as.numeric(Age),10, 10, 80)) %>% 
  # had to remove levels() around Age, because Age is clearly not a factor
  group_by(Sex, age_group) %>%
  summarise(n = sum(Value), .groups = "keep") %>%
  pull(n)

age_dist = age_dist/sum(age_dist)

# Spain has a slightly different order in which data are inputted

# dataset A: confirmed daily cases -------------------------------------------#
confirmed_cases = read_csv("data/confirmed-cases_Hubei.csv") %>%
  mutate(date = ymd(paste(year, month, day, sep="-"))) %>%
  filter(date >= day_data) %>% 
  select(-c(day, month, year))
incidence_cases = pull(confirmed_cases, confirmed_cases_hubei)
# for Spain, the data looks different, so the code to read it looks different

# dataset C: confirmed daily deaths ------------------------------------------#
# this whole block here does not work, because we can not install the package
# nCov2019, wher the data comes from; when it works, this block will generate
# the object incidence_deaths
# for Spain, data comes from .csv, code is different

# dataset B: age distribution of all cases, for China ------------------------#
# source: Chinese CDC Weekly, The epidemiological characteristics of an 
#         outbreak...
cases_tmax     = c(416, 549, 3619, 7600, 8571, 10008, 8583, 3918, 1408)
agedistr_cases = cases_tmax / sum(cases_tmax) # relative age distribution
remove(cases_tmax)
# for Spain, data comes from .csv, is not comparable

# dataset D: age distribution of all deaths, for China -----------------------#
mort_tmax       = c(0, 1, 7, 18, 38, 130, 309, 312, 208)
agedistr_deaths = mort_tmax / sum(mort_tmax) 

# ----------------------------------------------------------------------------#
