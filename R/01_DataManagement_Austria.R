# ----------------------------------------------------------------------------#
# DataManagement_Austria.R
# 
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
### Time period is different
{
  day_start =      as.Date("2020-03-10")
  day_data =       as.Date("2020-03-11")
  day_max =        as.Date("2020-04-14") #10 days before last date of data
  day_quarantine = as.Date("2020-03-16")
}

# Age distribution -----------------------------------------------------------#
### Not hardwired as China, but from csv.
# from : data/contact_matrix/age_distribution.csv (same for Switzerland)
# from: http://data.un.org/Data.aspx?d=POP&f=tableCode%3A22
# uses a self-defined function : age_class
excel_age_dist=read.csv("data/all_regions_pop_age_dist.csv",header=T)

age_class=function(x ,min_age,age_range,max_age){
  age_lim=seq(min_age,max_age,age_range)
  return(sapply(as.list(x),function(x) sum(age_lim<=x)))
}

age_dist = as.data.frame(excel_age_dist) %>%
    filter(Country.or.Area=="Austria") %>%
    filter(Source.Year == 2018) %>%
    filter(Age %in% as.character(0:120)) %>% 
    mutate(age_group=age_class(as.numeric(levels(Age))[Age],5,10,75)) %>% 
    group_by(age_group) %>%
    summarise(n=sum(Value),.group="keep") %>%
    pull(n)

age_dist <- age_dist/sum(age_dist) # getting relative age distribution

pop_t = 8.859e6  # (Source Eurostat 2019)

# Gender distribution -----------------------------------------------------####
# Gender distribution data for the year 2020 is collected under 
# https://population.un.org/wpp/DataQuery/

gender_dist <- read.csv2("data/all_region_pop_gender_dist.csv")
colnames(gender_dist) <- c("location","sexRatioMto100F", "M", "F")
gender_dist <- gender_dist %>% select(location, M, F) %>% 
  pivot_longer(-location, names_to = "Sex", values_to = "Ratio") %>% 
  filter(location == "Austria") %>% pull(Ratio)

# ----------------------------------------------------------------------------#

### Confirmed cases (A) & deaths (B) are from different csv.:

# dataset A: confirmed daily cases -------------------------------------------#
cases = read.table("data/Austria_cases.csv",sep=";",header=T) %>%
  transmute(date=dmy(time), new_cases=tag_Erkrankungen) %>%
  filter(date>=ymd(day_data),date<=ymd(day_max))

incidence_cases = pull(cases, new_cases)

# dataset C: confirmed daily deaths ------------------------------------------#
deaths = read.table("data/Austria_deaths.csv",sep=";",header=T) %>%
  transmute(date=dmy(time),new_deaths=c(NA,diff(Todesfalle))) %>%
  filter(date>=ymd(day_data),date<=ymd(day_max))

incidence_deaths = pull(deaths,new_deaths)

### Age distributions of cases (B) and deaths(D) are from different csv. : 

# dataset B: age distribution of all cases ------------------------#
agedistr_cases = read.table("data/Austria_cases_age_dist.csv",sep=";",header=T) %>%
  transmute(age=age, cases=number, age2=c(1:9,9)) %>%
  group_by(age2) %>%
  summarise(n=sum(cases)) %>%
  pull(n)

# dataset D: age distribution of all deaths -----------------------#
agedistr_deaths = read.table("data/Austria_deaths_age_dist.csv",sep=";",header=T) %>%
  transmute(age=age, new_deaths=number, age2=c(1:9,9)) %>%
  group_by(age2) %>%
  summarise(n=sum(new_deaths)) %>%
  pull(n)

# ----------------------------------------------------------------------------#

# dataset B & D: gender distribution of all cases and deaths --------------####
# This dataset is extracted from:
# https://info.gesundheitsministerium.at/dashboard_GenTod.html?l=de
# Percentage values are (Male, Female)
gender_cases <- read.csv("data/Austria_cases_deaths_gender_dist.csv", sep = ";") %>% 
  filter(status == "cases") %>% pull(value)
gender_deaths <- read.csv("data/Austria_cases_deaths_gender_dist.csv", sep = ";") %>% 
  filter(status == "deaths") %>% pull(value)

genderdistr_cases = gender_cases * sum(incidence_cases)
genderdistr_cases = round(genderdistr_cases, 0)

genderdistr_deaths = gender_deaths * sum(incidence_deaths)
genderdistr_deaths = round(genderdistr_deaths, 0)

# ----------------------------------------------------------------------------#

p_underreport_deaths=1
p_underreport_cases=1
