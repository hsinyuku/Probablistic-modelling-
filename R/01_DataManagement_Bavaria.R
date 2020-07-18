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
  day_start =       as.Date("2020-03-02")
  day_data =        as.Date("2020-03-03")
  day_max =         as.Date("2020-04-16")
  day_quarantine =  as.Date("2020-03-12")
}

# Age distribution --------------------------------------------------------####
pop_age <- read_csv("data/all_regions_pop_age_dist.csv")
# error messages are ok, there are footnotes that are not part of the data

# Function to classify age groups --------------------------------------------#
age_class = function(x, min_age, age_range, max_age) {
  age_lim = seq(min_age, max_age, age_range)
  return(sapply(as.list(x), function(x) sum(age_lim <= x)))
} 

# age_dist_absolute is absolute number of each age group. Here, the population
# is divided into 9 age groups like in the paper -> official one.
age_dist_absolute <- pop_age %>% 
  filter(`Country or Area` == "Germany") %>%
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group = age_class(as.numeric(Age), 10, 10, 80)) %>% 
  # had to remove levels() around Age, because Age is clearly not a factor
  group_by(Sex, age_group) %>%
  summarise(n = sum(Value), .groups = "keep") %>%
  pull(n)

# age_dist is % of population age distribution
age_dist = age_dist_absolute/sum(age_dist_absolute)

pop_t = 13.08e6

# ----------------------------------------------------------------------------#

# Gender distribution -----------------------------------------------------####
# Gender distribution data for the year 2020 is collected under 
# https://population.un.org/wpp/DataQuery/

gender_dist <- read.csv2("data/all_region_pop_gender_dist.csv")
colnames(gender_dist) <- c("location","sexRatioMto100F", "M", "F")
gender_dist <- gender_dist %>% select(location, M, F) %>% 
  pivot_longer(-location, names_to = "Sex", values_to = "Ratio") %>% 
  filter(location == "Germany") %>% pull(Ratio)

# ----------------------------------------------------------------------------#
# age_dist_absolute11 is absolute number of each age group. Here, the population
# is divided into 11 age groups for some further calculation.
age_dist_absolute11 <- pop_age %>% 
  filter(`Country or Area` == "Germany") %>%
  filter(Year == 2018) %>%
  filter(Age %in% as.character(0:120)) %>% 
  mutate(age_group = age_class(as.numeric(Age), 10, 10, 100)) %>% 
  # had to remove levels() around Age, because Age is clearly not a factor
  group_by(Sex, age_group) %>%
  summarise(n = sum(Value), .groups = "keep") %>%
  pull(n)
# ----------------------------------------------------------------------------#

# dataset A: confirmed daily cases ----------------------------------------####
# This data is taken from the dynamic Dash-Board of KRI, where new reported
# incidences are updated real-time.
# Link: https://npgeo-corona-npgeo-de.hub.arcgis.com/datasets/dd4580c810204019a7b8eb3e0b329dd6_0/data

data_Bavaria <-  read.csv("data/Germany_cases_deaths_confirmed.csv", header = T) %>%
  mutate(date = ymd(gsub(" 00:00:00", "", Meldedatum))) %>%
  filter(Bundesland == "Bayern", date >= day_data, date <= day_max) %>% 
  select(date, AnzahlFall, AnzahlTodesfall) %>%
  group_by(date) %>%
  summarise(new_cases = sum(AnzahlFall),
            new_deaths = sum(AnzahlTodesfall)) %>%
  arrange(date)
  
incidence_cases <- data_Bavaria$new_cases

p_underreport_cases <- 20547 / sum(incidence_cases) # 20547 is number of
                                                    # confirmed cases by day of
                                                    # symptoms on set, part of
                                                    # total confirmed cases that
                                                    # is underreported
# ----------------------------------------------------------------------------#

# dataset C: confirmed daily deaths ---------------------------------------####
incidence_deaths <- data_Bavaria$new_deaths

p_underreport_deaths = 1
# ----------------------------------------------------------------------------#

# dataset B: age distribution of all cases --------------------------------####
# This data is extracted from Figure 3, COVID-Situation Report 26.04.2020 of
# KRI, which is linked in the paper.
age_cases <- read.csv("data/Germany_cases_age_dist.csv",header = F) %>%
  as_tibble() %>% 
  transmute(cases=V2) %>%
  mutate(age=rep(c(seq(0,80,10),90,100),2),sex=rep(c("Female","Male"),each=11)) %>% 
  group_by(age) %>% 
  summarise(cases = sum(cases)/2) %>% 
  # Some how it counts double for female and male so cases / 2
  mutate(population = age_dist_absolute11, 
         age_dist = round(population*cases/100000, 0))
  # The data provides only numbers of cases PER 100.000 population of each age
  # group, so we need to convert those to actual number of reported cases per
  # age group, = population*cases/100000.

# age_cases now contains 11 age groups. Now, we group the last 3 age groups
# together to form 80+ age group

age_80plus <- age_cases %>% 
  filter(age >= 80) %>% 
  summarise(sum = sum(age_dist)) %>% 
  pull()
         
age_otherGroups <- age_cases %>% 
  filter(age < 80) %>% 
  pull()        
         
agedistr_cases = c(age_otherGroups, age_80plus)

# ----------------------------------------------------------------------------#

# dataset B: gender distribution of all cases -----------------------------####
# This dataset is extracted from Statista website:
# https://www.statista.com/statistics/1105480/coronavirus-covid-19-cases-by-gender-germany/
# We assume that the gender distribution of cases in Bavaria is the same as that
# in Germany.

# Percentage values are (Male, Female)
gender_cases <- read.csv("data/Germany_cases_deaths_gender_dist.csv", sep = ";") %>% 
  filter(status == "cases") %>% pull(value)

# ----------------------------------------------------------------------------#

# dataset D: age distribution of all deaths -------------------------------####

# This data is extracted from Figure 4, COVID-Situation Report 26.04.2020 of
# KRI, which is linked in the paper. In the raw data, they are 16 "bar" values,
# corresponding to age groups from 30-39 to 100+ (here we have 11 age groups)
# in the figure. Age groups 0-9, 10-19, 20-29 have no reported deaths.
# For this data, the numbers are already the actual reported deaths, not per
# 100.000 population like the cases data, so we don't need to correct that.

age_deaths <- read.csv("data/Germany_deaths_age_dist.csv",header = F) %>%
  as_tibble() %>% 
  transmute(deaths=V2) %>% mutate(age=rep(seq(30,100,10),2),
                                  sex=rep(c("Female","Male"),each=8)) %>% 
  group_by(age) %>% 
  summarise(deaths = round(sum(deaths)/2, 0))

# age_deaths now contains 8 age groups. Now, we group the last 3 age groups
# together to form 80+ age group

age_80plus <- age_deaths %>% 
  filter(age >= 80) %>% 
  summarise(sum = sum(deaths)) %>% 
  pull()

age_otherGroups <- age_deaths %>% 
  filter(age < 80) %>% 
  pull()        

# We put them together to have complete age distribution of deaths among 9 age
# groups. The first 3 age groups record no deaths, so zeros are added.

agedistr_deaths = c(0, 0, 0, age_otherGroups, age_80plus)

# ----------------------------------------------------------------------------#

# dataset D: gender distribution of all deaths ----------------------------####
# This dataset is extracted from Statista website:
# https://www.statista.com/statistics/1105512/coronavirus-covid-19-deaths-by-gender-germany/
# We assume that the gender distribution of deaths in Bavaria is the same as 
# that in Germany.

# Percentage values are (Male, Female)
gender_deaths <- read.csv("data/Germany_cases_deaths_gender_dist.csv", sep = ";") %>% 
  filter(status == "deaths") %>% pull(value)

# ----------------------------------------------------------------------------#

# ----------------------------------------------------------------------------#
# scale age distribution of cases/deaths to the total number in the state  ####
# ----------------------------------------------------------------------------#

# One assumption from the paper is that the authors take age distribution of 
# cases/deaths (in absolute numbers) of the whole Germany and apply it to
# the region (in data B and D). Since we have the total number of cases/deaths 
# in the region (through data A and C), we scale B and D down.

agedistr_cases = agedistr_cases / sum(agedistr_cases) * sum(incidence_cases)
agedistr_cases = round(agedistr_cases, 0)
  
agedistr_deaths = agedistr_deaths / sum(agedistr_deaths) * sum(incidence_deaths)
agedistr_deaths = round(agedistr_deaths, 0)

genderdistr_cases = gender_cases * sum(incidence_cases)
genderdistr_cases = round(genderdistr_cases, 0)

genderdistr_deaths = gender_deaths * sum(incidence_deaths)
genderdistr_deaths = round(genderdistr_deaths, 0)
