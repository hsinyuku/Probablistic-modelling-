# ----------------------------------------------------------------------------#
# ContactMatrix.R
# 
# This script produces two contact matrices and the corresponding graphs: one
# for China, the other for Europe (used in all European regions)
# ----------------------------------------------------------------------------#

# list_surveys()
# loaded from the socialmixr-package; lists available surveys

#### Get 2 CMs from zenodo ####

# ----------------------------------------------------------------------------#
# sourcing other scripts ####
# ----------------------------------------------------------------------------#

# Contact matrix function, selecting desired region and type
contact_matrix_gender_age <- function (region, type, n = 1, 
                                   counts = FALSE, symmetric = FALSE, 
                                   split = FALSE, weights = c(), 
                                   weigh.dayofweek = FALSE, 
                                   sample.all.age.groups = FALSE, 
                                   quiet = FALSE, ...) {
  # Load the correct survey according to the selected region
  if(region == "Hubei"){
    
    if (!file.exists("data/contact_matrix_shanghai.Rds")) {
      print("Downloading contact matrix:")
      survey_shanghai <- get_survey("https://doi.org/10.5281/zenodo.3366396")
      print("Saving contact matrix to data/contact_matrix_shanghai.Rds:")
      saveRDS(survey_shanghai, "data/contact_matrix_shanghai.Rds")
    } else if (!exists("survey_shanghai")) {
      print("Loading contact matrix from data/contact_matrix_shanghai.Rds")
      survey_shanghai <- readRDS("data/contact_matrix_shanghai.Rds")
      survey <- survey_shanghai
    } else {
      print("survey_shanghai already loaded")
    }
    
  } else {
  
    if (!file.exists("data/contact_matrix_POLYMOD.Rds")) {
      print("Downloading contact matrix for European regions.")
      survey_europe <- get_survey("https://doi.org/10.5281/zenodo.1043437")
      print("Saving contact matrix to data/contact_matrix_POLYMOD.Rds.")
      saveRDS(survey_europe, "data/contact_matrix_POLYMOD.Rds")
    } else if (!exists("survey_europe")) {
      print("Loading contact matrix from data/contact_matrix_POLYMOD.Rds")
      survey_europe <- readRDS("data/contact_matrix_POLYMOD.Rds")
      survey <- survey_europe
    } else {
      print("survey_europe already loaded")
    }
  }
  
  # Return the correct contact matrix according to the selected type
  
  if (type == "Gender") {
    N <- NULL
    population <- NULL
    weight <- NULL
    dayofweek <- NULL
    proportion <- NULL
    weight.cont <- NULL
    weight.part <- NULL
    id <- NULL
    sampled.weight <- NULL
    participants <- NULL
    surveys <- c("participants", "contacts")
  
    # Get_survey didn't do anything, just copied the same source
    survey <-get_survey(survey, quiet)
    
    # Checks that a survey fulfills all the requirements to work with the 
    # 'contact_matrix' function. Take some column names out, maybe the important
    # ones. In the survey we have 2 gender-related variables part_gender &
    # cnt_gender. The hope is to extract them.
    columns <- check(survey, columns = TRUE, quiet = TRUE)
    
    # Whether to sample participants and contacts randomly using a bootstrap; 
    # by default, will use bootstrap if n > 1.

    # Remove the participants with no recorded gender
    survey$participants <- survey$participants %>% filter(part_gender != "")
    
    # Remove the contact information with no recorded gender
    survey$contacts <- survey$contacts %>% filter(cnt_gender != "")
    
    # Take names of all countries in the survey
    survey.countries <- unique(survey$participants$country)
    
    # Take survey year
    country.pop <- wpp_age(survey.countries)
    country.pop.year <- unique(country.pop$year)
    survey.year <- survey$participants[, median(get(columns[["year"]]), 
                                                na.rm = TRUE)]
    survey.year <- min(country.pop.year[which.min(abs(survey.year - 
                                                        country.pop.year))])
    
    # Sum of population among age groups, of all countries in the survey,
    # in the survey year
    survey.pop <- country.pop %>% filter(year == survey.year) %>% 
      group_by(country) %>% summarize(pop = sum(population))
    
    if (!(region == "Hubei")){
      # Load in sex ratio (Male to 100 Female) of the survey countries in survey
      # year 2005. Data is extracted from UN - World Population Prospects.
      # https://population.un.org/wpp/DataQuery/
      sexRatio <- read.csv2("data/europe_contact_matrix_sex_ratio.csv")
      colnames(sexRatio) <- c("country", "sexRatio")#
      
    } else {
      
      # The sex ratio is obtained from the China government.
      sexRatio <- data.frame(country = "China", sexRatio = 106.19)
    }
    
    # Merge sexRatio data with corresponding population data of each country
    survey.pop <- merge(survey.pop, sexRatio, by = "country")
    
    # Calculate absolute number of M and F of each country based on sexRatio,
    # then sum population up according to Gender. We now have data of total 
    # population for all countries in the survey, divided by gender.
    survey.pop <- survey.pop %>% 
      mutate(F = round(pop / (sexRatio + 100) * 100,0)) %>% 
      mutate(M = pop - F) %>% select(F, M) %>% 
      pivot_longer(everything(), names_to = "Gender", values_to = "pop") %>% 
      group_by(Gender) %>% summarise(pop = sum(pop))
    
    # Set some weights
    survey$participants[, `:=`(weight, 1)]
    survey$contacts[, `:=`(weight, 1)]

    # Set weight
    survey$participants[, `:=`(weight, weight/sum(weight) * .N)]
    # Set key
    setkeyv(survey$participants, columns[["id"]])
    
    # Take id out
    participant_ids <- unique(survey$participants[[columns[["id"]]]])
    
    # Merge participant data with contact data
    survey$contacts <- merge(survey$contacts, survey$participants, 
                             by = columns[["id"]], all = F, allow.cartesian = T, 
                             suffixes = c(".cont", ".part"))
    
    # Set new weights
    survey$contacts[, `:=`(weight, weight.cont * weight.part)]
    survey$contacts[, `:=`(weight, weight/sum(weight) * 
                             .N)]
    
    # Set key
    setkeyv(survey$contacts, columns[["id"]])
    
    # Create return list
    ret <- list()
    
    sampled.contacts <- survey$contacts
    sampled.contacts[, `:=`(sampled.weight, weight)]
    sampled.participants <- survey$participants
    sampled.participants[, `:=`(sampled.weight, 
                                weight)]
  
    # KEY: create cross tabulation (raw contact matrix) for Gender
    weighted.matrix <- xtabs(data = sampled.contacts, formula = sampled.weight ~ 
                               part_gender + cnt_gender, addNA = TRUE)
    
    # Save dimensions and dimension names of the raw CM
    dims <- dim(weighted.matrix)
    dim.names <- dimnames(weighted.matrix)
    
    # Normalization: return means instead of counts
      norm.vector <- xtabs(data = sampled.participants, 
                           formula = sampled.weight ~ part_gender, addNA = TRUE)
      
      weighted.matrix <- array(apply(weighted.matrix, 2, 
                                     function(x) x/norm.vector), dim = dims, dimnames = dim.names)
  
      normalised.weighted.matrix <- diag(survey.pop$pop) %*% 
        weighted.matrix
      
      weighted.matrix <- 0.5 * diag(1/survey.pop$pop) %*% 
        (normalised.weighted.matrix + t(normalised.weighted.matrix))
      
      weighted.matrix <- structure(weighted.matrix,dim=c(2,2))
    return(c(t(weighted.matrix)))
  }
  
  if (type == "Age"){
    m <- contact_matrix(survey = survey,
                        age.limits=c(0,10,20,30,40,50,60,70,80),
                        symmetric = TRUE)
    m <- structure(m$matrix,dim=c(9,9))
    return(c(t(m)))
  }
}
