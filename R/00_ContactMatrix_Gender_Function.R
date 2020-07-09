library(data.table)

contact_matrix_gender <- function (survey, n = 1, bootstrap, 
                                   counts = FALSE, symmetric = FALSE, 
                                   split = FALSE, weights = c(), 
                                   weigh.dayofweek = FALSE, 
                                   sample.all.age.groups = FALSE, 
                                   quiet = FALSE, ...) 
  
{
  N <- NULL
  population <- NULL
  weight <- NULL
  dayofweek <- NULL
  proportion <- NULL
  weight.cont <- NULL
  weight.part <- NULL
  id <- NULL
  sampled.weight <- NULL
  bootstrap.weight <- NULL
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
    if (missing(bootstrap)) {
    bootstrap <- (n > 1)}
  
  # Remove the participants with no recorded gender
  survey$participants <- survey$participants %>% filter(part_gender != "")
  
  # Remove the contact information with no recorded gender
  survey$contacts <- survey$contacts %>% filter(cnt_gender != "")
  
  # Take names of all countries in the survey
  survey.countries <- unique(survey$participants$country)
  
  # Take survey year
  country.pop <- wpp_age(survey.countries)
  country.pop.year <- unique(country.pop$year)
  survey.year <- min(country.pop.year[which.min(abs(survey.year - 
                                                      country.pop.year))])
  
  # Sum of population among age groups, of all countries in the survey,
  # in the survey year
  survey.pop <- country.pop %>% filter(year == survey.year) %>% 
    group_by(country) %>% summarize(pop = sum(population))
  
  # Load in sex ratio (Male to 100 Female) of the survey countries in survey
  # year 2005. Data is extracted from UN - World Population Prospects.
  # https://population.un.org/wpp/DataQuery/
  sexRatio <- read.csv2("data/europe_contact_matrix_sex_ratio.csv")
  colnames(sexRatio) <- c("country", "sexRatio")
  
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
  
  # We sample only once so no need for bootstrapping
  if (n > 1) {
    if (!bootstrap) {
      warning("n > 1 does not make sense if not bootstrapping. Will return just one sample.")
      n <- 1
    }
  }
  
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
  
  matrix.gender <- weighted.matrix / sum(weighted.matrix)
  
  return(matrix.gender)
}