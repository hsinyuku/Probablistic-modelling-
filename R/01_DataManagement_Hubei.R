# -----------------------------------------------------------------------------
# DataManagement_Hubei.R
# 
# This file prepares the data that is used for modelling, only for the region
# of Hubei (China). 
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# sourcing other scripts
# -----------------------------------------------------------------------------
  # this line is an attempt at making the code execution stop when there are 
  # errors in on of the files to be sourced
ifelse(class(source("setup.R"))!="try-error",
       print(paste("setup.R sourced.")))
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# hardwired input
# -----------------------------------------------------------------------------
# should we move all this to a separate file that can be read in? This might 
# increase the possibility that we can write functions and decreases the need
# to repeat ourselves

# Dates -----------------------------------------------------------------------
{
  day_start =       as.Date("2019-12-31")
  day_data =        as.Date("2020-01-01")
  day_max =         as.Date("2020-02-11")
  day_quarantine =  as.Date("2020-01-20")
}

# Age distribution ------------------------------------------------------------
age_dist <- c(
  83932437 + 86735183,
  84262751 + 82341859,
  87158167+ 97989003,
  128738970 + 100091455, 
  96274146 + 119837617,
  123445382 + 98740491,
  77514139 + 74149766,
  44949689 + 26544616,
  16181417 + 7581777 + 2305024 + 475193 + 74692)
# the last line probably sums up age groups 
age_dist <- age_dist/sum(age_dist) # getting relative age distribution

pop_t = 59020000

# dataset A: confirmed daily cases --------------------------------------------
confirmed_cases = read_csv("data/Hubei_cases_confirmed.csv") %>%
  mutate(date = ymd(paste(year, month, day, sep="-"))) %>%
  filter(date >= day_data) %>% 
  select(-c(day, month, year))

incidence_cases = pull(confirmed_cases, confirmed_cases_hubei)

# dataset C: confirmed daily deaths -------------------------------------------

chinaDeath <- readRDS("data/Hubei_deaths_confirmed.Rds")

chinaDeath <- chinaDeath$dailyHistory %>%  # Data within this table
  select(date, hubei.dead) %>%  # Extract columns
  mutate(date = as.Date(paste0("2020.",date),  
                        ("%Y.%m.%d"))) %>%  # Convert date
  filter(date <= day_max) %>%   # Filter until day_max
  mutate(dailyDeath = hubei.dead - lag(hubei.dead, 1, default = 0)) %>%  
  # Calc daily death, default = 0: to save the daily death on 20.01 instead of
  # returning NA
  mutate(dailyDeathCorrect = round(dailyDeath * 100 / 71.4, 0)) 
  # The reported deaths in this data is believed to only account for 71.4% of
  # the total confirmed deaths in this time frame. We correct that for each
  # daily number of reported deaths.
  
# Daily deaths are only recorded from 20.01.2020, so 0s are filled in for the
# time frame from day_start until 20.01.2020. It is now length 42.

incidence_deaths = c(rep(0, day_max - day_data + 1 -
                           length(chinaDeath$dailyDeathCorrect)),
                     chinaDeath$dailyDeathCorrect)

# dataset B: age distribution of all cases, for China -------------------------
# source: Chinese CDC Weekly, The epidemiological characteristics of an 
#         outbreak...
agedistr_cases  = c(416, 549, 3619, 7600, 8571, 10008, 8583, 3918, 1408)

# dataset D: age distribution of all deaths, for China ------------------------
agedistr_deaths = c(0, 1, 7, 18, 38, 130, 309, 312, 208)

# -----------------------------------------------------------------------------


# ----------------------------------------------------------------------------#
# fixed corrections and delays ####
# ----------------------------------------------------------------------------#
# Fixed corrections ----------------------#
p_report_80plus      = 1
p_underreport_deaths = 1
p_underreport_cases  = 1
# ----------------------------------------------------------------------------#

