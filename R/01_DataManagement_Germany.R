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
# day_max is 18.06.2020, day_start is 28.01.2020 to match the data from RKI

{
  day_start =       as.Date("2020-01-28")
  day_data =        as.Date("2020-01-28")
  day_max =         as.Date("2020-06-18")
  day_quarantine =  as.Date("2020-03-12")
}

# Age distribution -----------------------------------------------------------#
age_dist <- read_csv("data/all_regions_pop_age_dist.csv")
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

# For the following data sets, data extracted directly from RKI daily report 
# and hard-coded here.
# https://www.rki.de/DE/Content/InfAZ/N/Neuartiges_Coronavirus/Situationsberichte/2020-06-18-en.pdf?__blob=publicationFile

# dataset A: confirmed daily cases -------------------------------------------#
data_Germany <-  read.csv("data/Germany_cases_deaths_confirmed.csv", header = T) %>%
  mutate(date = ymd(gsub(" 00:00:00", "", Meldedatum))) %>%
  select(date, AnzahlFall, AnzahlTodesfall) %>%
  group_by(date) %>%
  summarise(new_cases = sum(AnzahlFall),
            new_deaths = sum(AnzahlTodesfall)) %>%
  arrange(date)
  

incidence_cases <- data_Germany$new_cases


# dataset C: confirmed daily deaths ------------------------------------------#
incidence_deaths <- data_Germany$new_deaths

# dataset B: age distribution of all cases -----------------------------------#
case_tmax <-  c(4271, 8730, 81674/3, 
                        81674/3, 81674/3, 57884*3/5, 
                        57884*2/5, 29801/3, 29801*2/3 + 5305) 

agedistr_cases <- case_tmax/(sum(case_tmax))

# for Spain, data comes from .csv, is not comparable

# dataset D: age distribution of all deaths, for China -----------------------#
mort_tmax       = c(1, 2, 9, 23, 69, 310, 842, 1994, 3938 + 1612 + 51)
agedistr_deaths = mort_tmax / sum(mort_tmax) 

# ----------------------------------------------------------------------------#
