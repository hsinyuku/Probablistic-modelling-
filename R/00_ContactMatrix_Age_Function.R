survey <- europe_survey
symmetric <- TRUE
split <- FALSE
age.limits=c(0,10,20,30,40,50,60,70,80)
quiet = F
estimated.contact.age = "mean"
missing.participant.age = "remove"
missing.contact.age = "remove"
n = 1
weigh.dayofweek = FALSE
sample.all.age.groups = FALSE
weights = c()
countries = c()

library(data.table)
library(socialmixr)

contact_matrix_age <- function (survey, countries = c(), survey.pop, age.limits, filter, 
          n = 1, bootstrap, counts = FALSE, symmetric = FALSE, split = FALSE, 
          estimated.contact.age = c("mean", "sample", "missing"), 
          missing.participant.age = c("remove", "keep"), 
          missing.contact.age = c("remove", "sample", "keep"), 
          weights = c(), weigh.dayofweek = FALSE, sample.all.age.groups = FALSE, 
          quiet = FALSE, ...) 
  
{
  lower.age.limit <- NULL
  N <- NULL
  population <- NULL
  upper.age.limit <- NULL
  age.group <- NULL
  weight <- NULL
  dayofweek <- NULL
  contact.age.group <- NULL
  proportion <- NULL
  weight.cont <- NULL
  weight.part <- NULL
  id <- NULL
  sampled.weight <- NULL
  bootstrap.weight <- NULL
  participants <- NULL
  surveys <- c("participants", "contacts")
  dot.args <- list(...)
  unknown.args <- setdiff(names(dot.args), union(names(formals(check.survey)), 
                                                 names(formals(pop_age))))
  if (length(unknown.args) > 0) {
    stop("Unknown argument(s): ", paste(unknown.args, 
                                        sep = ", "), ".")
  }
  missing.participant.age.set <- !missing(missing.participant.age)
  missing.contact.age.set <- !missing(missing.contact.age)
  estimated.contact.age <- match.arg(estimated.contact.age)
  missing.participant.age <- match.arg(missing.participant.age)
  missing.contact.age <- match.arg(missing.contact.age)
  
  # Get_survey didn't do anything, just copied the same source
  survey <- get_survey(survey, quiet)
  
  # Checks that a survey fulfills all the requirements to work with the 
  # 'contact_matrix' function. Take some column names out, maybe the important
  # ones. In the survey we have 2 gender-related variables part_gender &
  # cnt_gender. The hope is to extract them.
  
  columns <- check(survey, columns = TRUE, quiet = TRUE)
  
  # Whether to sample participants and contacts randomly using a bootstrap; 
  # by default, will use bootstrap if n > 1. We don't touch this.
  
  if (missing(bootstrap)) 
    bootstrap <- (n > 1)
  
  # The original code from the authors sampled the contact matrix from all
  # countries in the data, so argument "country" is not given. We don't touch
  # this.
  
  if (length(countries) > 0 && columns[["country"]] %in% 
      colnames(survey$participants)) {
    survey$participants[, `:=`(paste(columns[["country"]]), 
                               countrycode(get(columns[["country"]]), "country.name", 
                                           "country.name"))]
    if (all(nchar(countries) == 2)) {
      suppressWarnings(corrected_countries <- countrycode(countries, 
                                                          "iso2c", "country.name"))
    }
    else {
      suppressWarnings(corrected_countries <- countrycode(countries, 
                                                          "country.name", "country.name"))
    }
    present_countries <- unique(as.character(survey$participants[[columns[["country"]]]]))
    missing_countries <- countries[which(is.na(corrected_countries))]
    if (length(missing_countries) > 0) {
      stop("Survey data not found for ", paste(missing_countries, 
                                               sep = ", "), ".")
    }
    countries <- corrected_countries
    survey$participants <- survey$participants[get(columns[["country"]]) %in% 
                                                 countries]
    if (nrow(survey$participants) == 0) {
      stop("No participants left after selecting countries.")
    }
  }
  
  # This is where the age windows come in. Maybe we would have to delete this
  # entire thing in the end, or disable it somehow.
  
  # If no age.limits entered, age windows = all unique ages in the survey
  max.age <- max(survey$participants[, get(columns[["participant.age"]])], 
                 na.rm = TRUE) + 1
  if (missing(age.limits)) {
    all.ages <- unique(as.integer(survey$participants[, get(columns[["participant.age"]])]))
    all.ages <- all.ages[!is.na(all.ages)]
    all.ages <- all.ages[order(all.ages)]
    age.limits <- union(0, all.ages)
  }
  
  # Otherwise just accept age limits
  age.limits <- as.integer(age.limits)
  if (any(is.na(age.limits)) || any(diff(age.limits) <= 0)) {
    stop("'age.limits' must be an increasing integer vector of lower age limits.")
  }
  
  # Any filters to apply to the data, given as list of the form 
  # (column=filter_value) - only contacts that have 'filter_value' in 'column' 
  # will be considered. We don't touch this.
  
  if (!missing(filter)) {
    missing_columns <- list()
    for (table in surveys) {
      if (nrow(survey[[table]]) > 0) {
        missing_columns <- c(missing_columns, list(setdiff(names(filter), 
                                                           colnames(survey[[table]]))))
        for (column in names(filter)) {
          if (column %in% colnames(survey[[table]])) {
            survey[[table]] <- survey[[table]][get(column) == 
                                                 filter[[column]]]
          }
        }
      }
    }
    missing_all <- do.call(intersect, missing_columns)
    if (length(missing_all) > 0) {
      warning("filter column(s) ", paste(missing_all), 
              " not found")
    }
  }
  
  
  # Missing age value: if set to "remove" (default), participants without age 
  # information are removed; if set to "keep", participants with missing age 
  # are kept and treated as a separate age group. I think this should be set
  # to ´remove´
  
  if (missing.participant.age == "remove" && nrow(survey$participants[is.na(get(columns[["participant.age"]])) | 
                                                                      get(columns[["participant.age"]]) < min(age.limits)]) > 
      0) {
    if (!quiet && !missing.participant.age.set) {
      message("Removing participants without age information. ", 
              "To change this behaviour, set the 'missing.participant.age' option")
    }
    survey$participants <- survey$participants[!is.na(get(columns[["participant.age"]])) & 
                                                 get(columns[["participant.age"]]) >= min(age.limits)]
  }
  
  # Here they just set some name variables according to the `columns` variable
  exact.column <- paste(columns[["contact.age"]], "exact", 
                        sep = "_")
  min.column <- paste(columns[["contact.age"]], "est_min", 
                      sep = "_")
  max.column <- paste(columns[["contact.age"]], "est_max", 
                      sep = "_")
  
  # In the contact data, an NA contact.age column is created
  if (!(columns[["contact.age"]] %in% colnames(survey$contacts))) {
    survey$contacts[, `:=`(paste(columns[["contact.age"]]), 
                           NA_integer_)]
    
    # Fill contact.age column with all available non-NA cnt_age_exact 
    if (exact.column %in% colnames(survey$contacts)) {
      survey$contacts[!is.na(get(exact.column)), `:=`(paste(columns[["contact.age"]]), 
                                                      get(exact.column))]
    }
  }
  
  
  # Check if the age columns are of class "factor". None of them is. We don't
  # touch this.
  for (age_column in c(columns[["contact.age"]], min.column, 
                       max.column, exact.column)) {
    if (age_column %in% colnames(survey$contacts) && class(survey$contacts[[age_column]]) == 
        "factor") {
      survey$contacts[, `:=`(paste(age_column), as.integer(levels(get(age_column)))[get(age_column)])]
    }
  }
  
  # Argument `estimated.contact.age`
  # How comes people who don't have exact age but only an age range. If set to 
  # "mean" (default), people whose ages are given as a range (in columns 
  # named "..._est_min" and "..._est_max") but not exactly (in a column named 
  # "..._exact") will have their age set to the mid-point of the range; if set 
  # to "sample", the age will be sampled from the range; if set to "missing", 
  # age ranges will be treated as missing
  
  if (min.column %in% colnames(survey$contacts) && max.column %in% 
      colnames(survey$contacts)) {
    if (estimated.contact.age == "mean") {
      survey$contacts[is.na(get(columns[["contact.age"]])) & 
                        !is.na(get(min.column)) & !is.na(get(max.column)), 
                      `:=`(paste(columns[["contact.age"]]), 
                           as.integer(rowMeans(.SD))), .SDcols = c(min.column, 
                                                                   max.column)]
    }
    else if (estimated.contact.age == "sample") {
      survey$contacts[is.na(get(columns[["contact.age"]])) & 
                        !is.na(get(min.column)) & !is.na(get(max.column)) & 
                        get(min.column) <= get(max.column), `:=`(paste(columns[["contact.age"]]), 
                                                                 as.integer(runif(.N, get(min.column), get(max.column))))]
    }
  }
  
  # age information are removed; if set to "sample", contacts without age 
  # information are sampled from all the contacts of participants of the same 
  # age group; if set to "keep", contacts with missing age are kept and treated 
  # Argument `missing.contact.age`
  # if set to "remove" (default), participants that that have contacts without 
  # as a separate age group.
  
  if (missing.contact.age == "remove" && nrow(survey$contacts[is.na(get(columns[["contact.age"]])) | 
                                                              get(columns[["contact.age"]]) < min(age.limits)]) > 
      0) {
    if (!quiet && n == 1 && !missing.contact.age.set) {
      message("Removing participants that have contacts without age information. ", 
              "To change this behaviour, set the 'missing.contact.age' option")
    }
    missing.age.id <- survey$contacts[is.na(get(columns[["contact.age"]])) | 
                                        get(columns[["contact.age"]]) < min(age.limits), 
                                      get(columns[["id"]])]
    survey$participants <- survey$participants[!(get(columns[["id"]]) %in% 
                                                   missing.age.id)]
  }
  
  # This results in TRUE (FALSE || TRUE)
  need.survey.pop <- split || symmetric
  
  # Right now we don't have survey.pop yet, missing(survey.pop) == T
  if (need.survey.pop) {
    if (missing(survey.pop) || is.character(survey.pop)) {
      survey.representative = FALSE
      
      if (!missing(survey.pop)) {
        survey.countries <- survey.pop
      }
      
      else if (!missing(countries)) {
        survey.countries <- countries
      }
      
      # We check all countries, saved names in survey.countries
      else {
        if (columns[["country"]] %in% colnames(survey$participants)) {
          survey.countries <- unique(survey$participants[, 
                                                         get(columns[["country"]])])
        }
        else {
          warning("No 'survey.pop' or 'countries' given, and no '", 
                  columns[["country"]], "' column found in the data. ", 
                  "I don't know which population this is from. ", 
                  "Assuming the survey is representative")
          survey.representative = TRUE
        }
      }
      
      
      # Get survey.year = 2006
      if (!survey.representative) {
        country.pop <- data.table(wpp_age(survey.countries))
        if (columns[["year"]] %in% colnames(survey$participants)) {
          survey.year <- survey$participants[, median(get(columns[["year"]]), 
                                                      na.rm = TRUE)]
        }
        
        else {
          survey.year <- country.pop[, max(year, na.rm = TRUE)]
          warning("No '", columns[["year"]], 
                  "' column found in the data. Will use ", 
                  survey.year, " population data.")
        }
        
        # we miss no countries so skip this
        missing.countries <- setdiff(survey.countries, 
                                     unique(country.pop$country))
        if (length(missing.countries) > 0) {
          stop("Could not find population data for ", 
               paste(missing.countries, collapse = ", "), 
               ". ", " Use wpp_countries() to get a list of country names.")
        }
        
        
        country.pop.year <- unique(country.pop[, year])
        
        survey.year <- min(country.pop.year[which.min(abs(survey.year - 
                                                            country.pop.year))])
        
        # Sum of population among age groups, of all countries in the survey,
        # in the survey year
        survey.pop <- country.pop[year == survey.year][, 
                                                       list(population = sum(population)), by = "lower.age.limit"]
      
      }
      
      if (survey.representative) {
        survey.pop <- survey$participants[, `:=`(lower.age.limit, 
                                                 reduce_agegroups(get(columns[["participant.age"]]), 
                                                                  age.limits))]
        survey.pop <- survey.pop[, list(population = .N), 
                                 by = lower.age.limit]
        survey.pop <- survey.pop[!is.na(lower.age.limit)]
        if (columns[["year"]] %in% colnames(survey$participants)) {
          survey.year <- survey$participants[, median(get(columns[["year"]]), 
                                                      na.rm = TRUE)]
        }
      }
    }
    
    # pop_age change population data, grouping population to specific age group
    # limits. We now have data of total population for all countries in the
    # survey, divided by the given age groups
    survey.pop <- data.table(pop_age(survey.pop, age.limits))
    
    # Change nothing
    survey.pop[, `:=`(lower.age.limit, reduce_agegroups(lower.age.limit, 
                                                        age.limits))]
    # Change nothing
    survey.pop <- survey.pop[, list(population = sum(population)), 
                             by = lower.age.limit]
    
    setkey(survey.pop, lower.age.limit)
    
    # Create new column lower.age.limit for all participants
    survey$participants[, `:=`(lower.age.limit, reduce_agegroups(get(columns[["participant.age"]]), 
                                                                 survey.pop$lower.age.limit))]
    
    # Write lower age limit to new var
    present.lower.age.limits <- unique(survey.pop$lower.age.limit)
    present.lower.age.limits <- present.lower.age.limits[order(present.lower.age.limits)]
    
    # Create upper age limit
    survey.pop[, `:=`(upper.age.limit, c(survey.pop$lower.age.limit[-1], 
                                         max.age))]
    
    # Same deal as above
    lower.upper.age.limits <- data.table(lower.age.limit = present.lower.age.limits, 
                                         upper.age.limit = c(present.lower.age.limits[-1], 
                                                             max.age))
    # Merging, so now in the participant data we have upper and lower age limits
    survey$participants <- merge(survey$participants, lower.upper.age.limits, 
                                 by = "lower.age.limit", all.x = TRUE)
  }
  
  
  survey$participants[, `:=`(lower.age.limit, reduce_agegroups(get(columns[["participant.age"]]), 
                                                               age.limits[age.limits < max.age]))]
  
  part.age.group.breaks <- c(age.limits[age.limits < max.age], 
                             max.age)
  
  # Create new column: age group for participants
  survey$participants[, `:=`(age.group, cut(survey$participants[, 
                                                                get(columns[["participant.age"]])], breaks = part.age.group.breaks, 
                                            right = FALSE))]
  
  # Create age groups labels for participants
  age.groups <- survey$participants[, levels(age.group)]
  
  age.groups[length(age.groups)] <- paste0(max(survey$participants$lower.age.limit, 
                                               na.rm = TRUE), "+")
  
  survey$participants[, `:=`(age.group, factor(age.group, 
                                               levels = levels(age.group), labels = age.groups))]
  
  # Set some weights
  survey$participants[, `:=`(weight, 1)]
  
  survey$contacts[, `:=`(weight, 1)]
  
  
  # Argument `weight.dayofweek`
  # whether to weigh the day of the week (weight 5 for weekdays ans 2 for weekends)

    if (weigh.dayofweek) {
      found.dayofweek <- FALSE
      for (table in surveys) {
        if ("dayofweek" %in% colnames(survey[[table]])) {
          survey[[table]][dayofweek %in% 1:5, `:=`(weight, 
                                                   5)]
          survey[[table]][!(dayofweek %in% 1:5), `:=`(weight, 
                                                      2)]
          found.dayofweek <- TRUE
        }
      }
      if (!found.dayofweek) {
        warning("'weigh.dayofweek' is TRUE, but no 'dayofweek' column in the data. ", 
                "Will ignore.")
      }
    }
  
  # Don't care about this
  if (length(weights) > 0) {
    for (i in 1:length(weights)) {
      for (table in surveys) {
        if (weights[i] %in% colnames(survey[[table]])) {
          survey[[table]][, `:=`(weight, weight * 
                                   get(weights[i]))]
        }
      }
    }
  }
  
  # We sample only once so no need for bootstrapping
  if (n > 1) {
    if (!bootstrap) {
      warning("n > 1 does not make sense if not bootstrapping. Will return just one sample.")
      n <- 1
    }
  }
  
  # Set weight
  survey$participants[, `:=`(weight, weight/sum(weight) * 
                               .N)]
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
  
  # Argument `missing.contact.age`
  # if set to "remove" (default), participants that have contacts without 
  # age information are removed; if set to "sample", contacts without age 
  # information are sampled from all the contacts of participants of the same 
  # age group; if set to "keep", contacts with missing age are kept and treated 
  # as a separate age group
  
  if (missing.contact.age == "sample" && nrow(survey$contacts[is.na(get(columns[["contact.age"]]))]) > 
      0) {
    for (this.age.group in unique(survey$contacts[is.na(get(columns[["contact.age"]])), 
                                                  age.group])) {
      if (nrow(survey$contacts[!is.na(get(columns[["contact.age"]])) & 
                               age.group == this.age.group]) > 0) {
        survey$contacts[is.na(get(columns[["contact.age"]])) & 
                          age.group == this.age.group, `:=`(paste(columns[["contact.age"]]), 
                                                            sample(survey$contacts[!is.na(get(columns[["contact.age"]])) & 
                                                                                     age.group == this.age.group, get(columns[["contact.age"]])], 
                                                                   size = .N, replace = TRUE))]
      }
      else {
        min.contact.age <- survey$contacts[, min(get(columns[["contact.age"]]), 
                                                 na.rm = TRUE)]
        max.contact.age <- survey$contacts[, max(get(columns[["contact.age"]]), 
                                                 na.rm = TRUE)]
        survey$contacts[is.na(get(columns[["contact.age"]])) & 
                          age.group == this.age.group, `:=`(paste(columns[["contact.age"]]), 
                                                            as.integer(floor(runif(.N, min = min.contact.age, 
                                                                                   max = max.contact.age + 1))))]
      }
    }
  }
  
  
  # Max contact age
  max.contact.age <- survey$contacts[, max(get(columns[["contact.age"]]), 
                                           na.rm = TRUE) + 1]
  
  
  contact.age.group.breaks <- part.age.group.breaks
  
  
  if (max.contact.age > max(contact.age.group.breaks)) {
    contact.age.group.breaks[length(contact.age.group.breaks)] <- max.contact.age
  }
  
  # New column: label age groups for contacts
  survey$contacts[, `:=`(contact.age.group, cut(get(columns[["contact.age"]]), 
                                                breaks = contact.age.group.breaks, labels = age.groups, 
                                                right = FALSE))]
  
  ret <- list()
  
  for (i in seq_len(n)) {
    
    # We don't bootstrap. So skip this part.
    if (bootstrap) {
      good.sample <- FALSE
      while (!good.sample) {
        part.sample <- sample(participant_ids, replace = T)
        part.age.limits <- unique(survey$participants[get(columns[["id"]]) %in% 
                                                        part.sample, lower.age.limit])
        good.sample <- !sample.all.age.groups || (length(setdiff(age.limits, 
                                                                 part.age.limits)) == 0)
        sample.table <- data.table(id = part.sample, 
                                   weight = 1)
        sample.table <- sample.table[, list(bootstrap.weight = sum(weight)), 
                                     by = id]
        setnames(sample.table, "id", columns[["id"]])
        setkeyv(sample.table, columns[["id"]])
        sampled.contacts <- merge(survey$contacts, sample.table)
        sampled.contacts[, `:=`(sampled.weight, 
                                weight * bootstrap.weight)]
        sampled.participants <- merge(survey$participants, 
                                      sample.table)
        sampled.participants[, `:=`(sampled.weight, 
                                    weight * bootstrap.weight)]
      }
    }
    
    # Start here.
    else {
      sampled.contacts <- survey$contacts
      sampled.contacts[, `:=`(sampled.weight, weight)]
      sampled.participants <- survey$participants
      sampled.participants[, `:=`(sampled.weight, 
                                  weight)]
    }
    weighted.matrix <- xtabs(data = sampled.contacts, formula = sampled.weight ~ 
                               age.group + contact.age.group, addNA = TRUE)
    
    dims <- dim(weighted.matrix)
    dim.names <- dimnames(weighted.matrix)
    if (!counts) {
      norm.vector <- xtabs(data = sampled.participants, 
                           formula = sampled.weight ~ age.group, addNA = TRUE)
      weighted.matrix <- array(apply(weighted.matrix, 2, 
                                     function(x) x/norm.vector), dim = dims, dimnames = dim.names)
      weighted.matrix[is.nan(weighted.matrix)] <- NA_real_
    }
    na.headers <- any(is.na(colnames(weighted.matrix))) || 
      any(is.na(rownames(weighted.matrix)))
    na.content <- any(is.na(weighted.matrix))
    na.present <- na.headers || na.content
    if (na.present) {
      warning.suggestion <- "  Consider "
      if (na.headers) {
        warning.suggestion <- paste0(warning.suggestion, 
                                     "setting ")
        suggested.options <- c()
        if (any(is.na(rownames(weighted.matrix)))) 
          suggested.options <- c(suggested.options, "'missing.participant.age'")
        if (any(is.na(colnames(weighted.matrix)))) 
          suggested.options <- c(suggested.options, "'missing.contact.age'")
        warning.suggestion <- paste0(warning.suggestion, 
                                     paste(suggested.options, collapse = " and "))
        if (na.content) {
          warning.suggestion <- paste0(warning.suggestion, 
                                       ", and ")
        }
        else {
          warning.suggestion <- paste0(warning.suggestion, 
                                       ".")
        }
      }
      if (na.content) {
        warning.suggestion <- paste0(warning.suggestion, 
                                     "adjusting the age limits.")
      }
    }
    if (symmetric & prod(dim(as.matrix(weighted.matrix))) > 
        1) {
      if (counts) {
        warning("'symmetric=TRUE' does not make sense with 'counts=TRUE'; ", 
                "will not make matrix symmetric.")
      }
      else if (na.present) {
        warning("'symmetric=TRUE' does not work with missing data; ", 
                "will not make matrix symmetric\n", warning.suggestion)
      }
      else {
        normalised.weighted.matrix <- diag(survey.pop$population) %*% 
          weighted.matrix
        weighted.matrix <- 0.5 * diag(1/survey.pop$population) %*% 
          (normalised.weighted.matrix + t(normalised.weighted.matrix))
      }
    }
    ret[[i]] <- list()
    if (split) {
      if (counts) {
        warning("'split=TRUE' does not make sense with 'counts=TRUE'; ", 
                "will not split the contact matrix.")
      }
      else if (na.present) {
        warning("'split=TRUE' does not work with missing data; ", 
                "will not split contact.matrix.\n", warning.suggestion)
        ret[[i]][["mean.contacts"]] <- NA
        ret[[i]][["normalisation"]] <- NA
        ret[[i]][["contacts"]] <- rep(NA, nrow(weighted.matrix))
      }
      else {
        weighted.matrix <- unname(weighted.matrix)
        nb.contacts <- apply(weighted.matrix, 1, sum)
        mean.contacts <- sum(survey.pop$population * 
                               nb.contacts)/sum(survey.pop$population)
        spectrum.matrix <- weighted.matrix
        spectrum.matrix[is.na(spectrum.matrix)] <- 0
        spectrum <- as.numeric(eigen(spectrum.matrix, 
                                     only.values = TRUE)$values[1])
        ret[[i]][["mean.contacts"]] <- mean.contacts
        ret[[i]][["normalisation"]] <- spectrum/mean.contacts
        age.proportions <- survey.pop$population/sum(survey.pop$population)
        weighted.matrix <- diag(1/nb.contacts) %*% weighted.matrix %*% 
          diag(1/age.proportions)
        nb.contacts <- nb.contacts/spectrum
        ret[[i]][["contacts"]] <- nb.contacts
      }
    }
    ret[[i]][["matrix"]] <- weighted.matrix
  }
  if (exists("survey.year")) {
    survey.pop[, `:=`(year, survey.year)]
    survey.pop <- merge(survey.pop, unique(survey$participants[, 
                                                               list(lower.age.limit, age.group)]))
    survey.pop <- survey.pop[, list(age.group, population, 
                                    proportion = population/sum(population), year)]
  }
  if (any(is.na(survey$participants$age.group))) {
    useNA <- "always"
  }
  else {
    useNA <- "no"
  }
  part.pop <- data.table(table(survey$participants[, age.group], 
                               useNA = useNA))
  setnames(part.pop, c("age.group", "participants"))
  part.pop[, `:=`(proportion, participants/sum(participants))]
  if (length(ret) > 1) 
    return_value <- list(matrices = ret)
  else return_value <- ret[[1]]
  if (!is.null(return_value)) {
    if (need.survey.pop) 
      return_value[["demography"]] <- survey.pop[]
    return_value[["participants"]] <- part.pop[]
  }
  return(return_value)
}
