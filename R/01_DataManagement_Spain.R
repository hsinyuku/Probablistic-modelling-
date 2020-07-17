# -----------------------------------------------------------------------------
# DataManagement_Spain.R
# 
# This file prepares the data that is used for modelling, and is the
# template file with which the other files can be compared.
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# sourcing other scripts and functions
# -----------------------------------------------------------------------------
# this line is an attempt at making the code execution stop when there are 
# errors in on of the files to be sourced
ifelse(class(source("setup.R"))!="try-error",
       print(paste("setup.R sourced.")))

# summarise exact ages into age groups, starting from min_age and going to
# max_age in intervals of age_range (both min_age and max_age are upper bound
# for the age groups)
age_class = function(x, min_age, age_range, max_age) {
  age_lim = seq(min_age, max_age, age_range)
  return(sapply(as.list(x), function(x) sum(age_lim <= x)))
} 
# -----------------------------------------------------------------------------


# -----------------------------------------------------------------------------
# hardwired input
# -----------------------------------------------------------------------------
# should we move all this to a separate file that can be read in? This might 
# increase the possibility that we can write functions and decreases the need
# to repeat ourselves

# Dates -----------------------------------------------------------------------
{
  day_start = as.Date("2020-03-01")
  day_data = as.Date("2020-03-02")
  day_max = as.Date("2020-04-16")
    # 10 days before last day of data about number of new symptomatic cases
  day_quarantine = as.Date("2020-03-09")
}

# Age distribution ------------------------------------------------------------

age_dist <- read_csv("data/all_regions_pop_age_dist.csv") 
  # error messages are ok, there are footnotes that are not part of the data

age_dist <- age_dist %>% 
  filter(`Country or Area` == "Spain") %>%
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group = age_class(as.numeric(Age),10,10,80)) %>% 
    # had to remove levels() around Age, because Age is clearly not a factor
  group_by(age_group) %>%
  summarise(n = sum(Value), .groups = "keep") %>%
  pull(n)
age_dist = age_dist/sum(age_dist)

pop_t = 46.94e6

# dataset A: confirmed daily cases --------------------------------------------
#https://www.mscbs.gob.es/profesionales/saludPublica/ccayes/alertasActual/nCov-China/documentos/Actualizacion_89_COVID-19.pdf
cases <- read_csv("data/Spain_cases_confirmed.csv", col_names = F) %>%
  # adding date and rounding values to whole integers
  transmute(date = seq(dmy("20-02-2020"), dmy("24-04-2020"), by="day"),
            new_cases = round(X2)) %>%
  filter(date >= day_data, date <= day_max)
incidence_cases = cases$new_cases
remove(cases)

# dataset C: confirmed daily deaths -------------------------------------------
# dataset loaded from https://covid19.isciii.es/ (link at the bottom left), 
# situation 28-04-2020
incidence_deaths = read_csv("data/Spain_deaths_confirmed.csv") %>%
  transmute(date = dmy(FECHA), deaths = round(Fallecidos)) %>%
  group_by(date) %>%
  summarise(deaths = sum(deaths,na.rm=T)) %>%
  mutate(new_deaths = c(NA,diff(deaths))) %>%
  filter(date >= ymd(day_data), date <= ymd(day_max)) %>% 
  pull(new_deaths)

# dataset B and D: age distribution of all cases, deaths ----------------------
agedistr = read_csv("data/Spain_cases_deaths_age_dist.csv") %>%
  mutate(age2 = c(1:9,9))  %>%
  group_by(age2) %>%
  summarise(cases = sum(cases, na.rm=T), deaths = sum(deaths, na.rm=T))

agedistr_cases  = pull(agedistr,cases)
agedistr_deaths = pull(agedistr,deaths)
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# fixed corrections and delays ####
# ----------------------------------------------------------------------------#
# Fixed corrections ----------------------#
p_underreport_deaths = 1
p_underreport_cases  = 142343/180689
# ----------------------------------------------------------------------------#