# ----------------------------------------------------------------------------#
# DataManagement_Bavaria.R
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
  day_start =       as.Date("2020-03-02")
  day_data =        as.Date("2020-03-03")
  day_max =         as.Date("2020-04-16")
  day_quarantine =  as.Date("2020-03-12")
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
  filter(`Country or Area` == "Germany") %>%
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group = age_class(as.numeric(Age), 10, 10, 80)) %>% 
  # had to remove levels() around Age, because Age is clearly not a factor
  group_by(Sex, age_group) %>%
  summarise(n = sum(Value), .groups = "keep") %>%
  pull(n)

age_dist = age_dist/sum(age_dist)

pop_t = 13.08e6

# Spain has a slightly different order in which data are inputted

# dataset A: confirmed daily cases -------------------------------------------#
confirmed_cases = read_csv("data/confirmed-cases_Hubei.csv") %>%
  mutate(date = ymd(paste(year, month, day, sep="-"))) %>%
  filter(date >= day_data) %>% 
  select(-c(day, month, year))
incidence_cases = pull(confirmed_cases, confirmed_cases_hubei)


data_germany <-  read.csv("data/cases_mort_germany.csv",header=T)
data_bavaria <-
  data_germany %>% transmute(
    date = dmy(as.character(date)),
    new_cases = cases_bavaria,
    new_deaths = c(NA, diff(deaths_bavaria))
  ) %>%
  filter(date >= ymd(day_data), date <= ymd(day_max))

incidence_cases=data_bavaria$new_cases

# dataset C: confirmed daily deaths ------------------------------------------#
incidence_deaths=data_bavaria$new_deaths

# dataset B: age distribution of all cases -----------------------------------#
age_cases = read.csv("data/age_distribution_cases_Germany.csv",header = F) %>%
  tbl_df() %>%
  transmute(inc=V2) %>%
  mutate(age=rep(seq(0,100,10),2), age2=rep(c(seq(0,80,10),80,80),2),sex=rep(c("Female","Male"),each=11),pop=age_dist100$n,
         cases=inc*pop/100000) %>%
  group_by(age2) %>%
  summarise(cases=round(sum(cases)))




# for Spain, data comes from .csv, is not comparable

# dataset D: age distribution of all deaths, for China -----------------------#
mort_tmax       = c(0, 1, 7, 18, 38, 130, 309, 312, 208)
agedistr_deaths = mort_tmax / sum(mort_tmax) 

# ----------------------------------------------------------------------------#
