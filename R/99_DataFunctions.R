# ----------------------------------------------------------------------------#
# ModelDiagnostics_Plotting.R
# 
# This script provides functions to plot out model diagnostics.
# 
# Note: the functions work with so-called side-effects. That means that many
# of their arguments are not explicitly provided when called, but directly
# taken from the global environment. That means that, in order for the
# functions to work at all, the objects data_list_model, controls, and some
# StanFit-object (here, usually called sampler) have to be loaded.
# ----------------------------------------------------------------------------#


groupLabels <- function(group) {
  if(group == "age")  {
    groupLabels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                     "60-69", "70-79", "80+")
  } else if(group == "gender") {
    groupLabels <- c("male", "female") }
  return(groupLabels)
}

# function to extract values from parameters as tibble -----------------------#
extractValue <- function(sample, name, printStat = c("2.5%", "97.5%", "50%")) {
  summary(sample, name)[[1]] %>%
    as_tibble() %>% mutate(metric = name) %>% 
    select(metric, printStat)
}

data_Real_Time <- function(data_list_model, metric, 
                           day_start, day_max) {
  day_start = day_start
  day_max = day_max
  data <- tibble(date = as_date(day_data:day_max))
  if(metric == "cases") {
    data <- cbind(data, n = data_list_model$incidence_cases)
  } else if(metric == "deaths") {
    data <- cbind(data, n = data_list_model$incidence_deaths)
  }
}

# simulated vs. real cases and deaths over time ---------------------#
data_SimVsReal_Time <- function(metric, controls, sample, day_max, day_data,
                                data_list_model,
                                AllCasesFill = "#00B2EE",
                                SymptCasesFill = "#66CD00",
                                RepCasesFill = "#008B8B",
                                SimDeaths = "#B22222", 
                                ResDeaths = "#FFD700") {
  # extracting values from data_list_model for easier referencing
  daysTotal = data_list_model$S
  # Generate a table with all the necessary data. Real reported symptomatic
  # cases have their own column; for the predicted data, different statistics
  # (median, mean, quantiles) are in the stat and the value columns (long data).
  if (metric == "cases") {
    dates <- as_date(day_data:day_max)
    simData <- rbind(
      cbind(extractValue(sample,
                         "predicted_reported_incidence_symptomatic_cases"),
            date = dates),
      cbind(extractValue(sample,
                         "predicted_overall_incidence_symptomatic_cases"),
            date = dates),
      cbind(extractValue(sample,
                         "predicted_overall_incidence_all_cases"),
            date = dates))  %>% 
      # the releveling is necessary to control which metric gets printed over
      # which metric ("order in which they are printed")
      mutate(metric = factor(metric),
             metric = fct_recode(
               metric,
               All = "predicted_overall_incidence_all_cases",
               Symptomatic = "predicted_overall_incidence_symptomatic_cases",
               Reported = "predicted_reported_incidence_symptomatic_cases"))
    realData  <- 
      tibble(date = dates,
             incidence = data_list_model$incidence_cases)
  } else if (metric == "deaths") {
    simData <- 
      cbind(extractValue(sample,
                         "predicted_overall_incidence_deaths"), 
            date = as_date(day_data:(day_max+data_list_model$G))) %>% 
      mutate(metric = case_when(date <= day_max ~ "Simulated Deaths",
                                date >= day_max ~ "Residual Deaths"),
             metric = fct_relevel(metric, "Simulated Deaths")) 
    realData <- 
      tibble(date = as_date(day_data:day_max),
             incidence=data_list_model$incidence_deaths)
  }
  return(list("real" = realData, "simulated" = simData))
}

# plotting simulated vs. real deaths and cases per age group -----------------#
data_SimVsReal_Group <- function(controls, data_list_model, metric, sample,
                                 AllCasesFill = "#00B2EE",
                                 SymptCasesFill = "#66CD00",
                                 RepCasesFill = "#008B8B",
                                 SimDeaths = "#B22222", 
                                 ResDeaths = "#FFD700") {
  # Preparing the real (reported) data
  if(controls$type == "Age") {
    groupLabels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                     "60-69", "70-79", "80+")
    if(metric == "deaths") {
      realData <- tibble(n = data_list_model[["agedistr_deaths"]],
                         group = groupLabels)
    } else if (metric == "cases") {
      realData <- tibble(n = data_list_model[["agedistr_cases"]],
                         group = groupLabels)
      
    }
  } else if(controls$type == "Gender") {
    groupLabels <- c("male", "female")
    if(metric == "deaths") {
      realData <- tibble(n = data_list_model[[f("genderdistr_deaths")]],
                         group = groupLabels)
    } else if (metric == "cases") {
      realData <- tibble(n = data_list_model[[f("genderdistr_cases")]],
                         group = groupLabels)
    }
  }
  # Preparing the simulated data
  if(metric == "cases") {
    # Getting names of the quantities to be extracted - this is done so both 
    # gender and age work
    predicted_total_reported_symptomatic_cases_by_group <- 
      ifelse(controls$type == "Age", 
             "predicted_total_reported_symptomatic_cases_by_age",
             "predicted_total_reported_symptomatic_cases_by_gender")
    predicted_total_overall_symptomatic_cases_by_group <- 
      ifelse(controls$type == "Age", 
             "predicted_total_overall_symptomatic_cases_by_age",
             "predicted_total_overall_symptomatic_cases_by_gender")
    predicted_total_overall_all_cases_by_group <- 
      ifelse(controls$type == "Age", 
             "predicted_total_overall_all_cases_by_age",
             "predicted_total_overall_all_cases_by_gender")
    # extracting the quantities
    simData <- rbind(
      extractValue(sample,
                   name = predicted_total_reported_symptomatic_cases_by_group),
      extractValue(sample,
                   predicted_total_overall_symptomatic_cases_by_group),
      extractValue(sample,
                   predicted_total_overall_all_cases_by_group)) %>% 
      mutate(group = rep(groupLabels, 3),
             metric = str_replace(metric, "(age)|(gender)", "group"),
             metric = factor(metric),
             metric = fct_recode(
               metric,
               All = "predicted_total_overall_all_cases_by_group",
               `Symptomatic Cases` = "predicted_total_overall_symptomatic_cases_by_group",
               `Reported Cases` = "predicted_total_reported_symptomatic_cases_by_group"))
  } else if (metric == "deaths") {
    # Getting names of the quantities to be extracted - this is done so both 
    # gender and age work
    predicted_total_overall_deaths_tmax_by_group <- 
      ifelse(controls$type == "Age", 
             "predicted_total_overall_deaths_tmax_by_age",
             "predicted_total_overall_deaths_tmax_by_gender")
    predicted_total_overall_deaths_delay_by_group <- 
      ifelse(controls$type == "Age", 
             "predicted_total_overall_deaths_delay_by_age",
             "predicted_total_overall_deaths_delay_by_gender")
    simData <- rbind(
      extractValue(sample,
                   predicted_total_overall_deaths_tmax_by_group),
      extractValue(sample,
                   predicted_total_overall_deaths_delay_by_group)) %>% 
      mutate(group = rep(groupLabels, 2),
             metric = str_replace(metric, "(age)|(gender)", "group"),
             metric = factor(metric),
             metric = fct_recode(
               metric,
               `Reported Deaths` = "predicted_total_overall_deaths_tmax_by_group",
               `Projected Deaths` = "predicted_total_overall_deaths_delay_by_group"
             ))
  }
  return(list("real" = realData, "simulated" = simData))
}

# plotting total deaths and cases --------------------------------------------#
data_SimVsReal_Total <- function(sample, metric, data_list_model, controls,
                                 AllCasesFill = "#00B2EE",
                                 SymptCasesFill = "#66CD00",
                                 RepCasesFill = "#008B8B",
                                 SimDeaths = "#B22222", 
                                 ResDeaths = "#FFD700") {
  realData <- tibble(
    sumOverGroups = 
      sum(data_list_model[[paste0(str_to_lower(controls$type),
                                  "distr_", metric)]]),
    sumOverTime = sum(data_list_model[[paste0("incidence_", metric)]]),
    metric = paste("Reported", metric)) %>% 
    pivot_longer(cols = -metric)
  if (metric == "cases") {
    simData <- rbind(
      extractValue(sample,
                   "predicted_total_reported_symptomatic_cases"),
      extractValue(sample,
                   "predicted_total_overall_symptomatic_cases"),
      extractValue(sample,
                   "predicted_total_overall_all_cases"))  %>% 
      # the releveling is necessary to control which metric gets printed over
      # which metric ("order in which they are printed")
      mutate(metric = factor(metric),
             metric = fct_recode(
               metric,
               All = "predicted_total_overall_all_cases",
               "Symptomatic Cases" = "predicted_total_overall_symptomatic_cases",
               "Reported Cases" = "predicted_total_reported_symptomatic_cases"))
  } else if (metric == "deaths") {
    simData <- 
      rbind(extractValue(sample,
                         "predicted_total_overall_deaths_tmax"),
            extractValue(sample,
                         "predicted_total_overall_deaths_delay")) %>% 
      mutate(metric = factor(metric),
             metric = fct_recode(
               metric,
               "Including Pro-\njected Deaths" = "predicted_total_overall_deaths_delay",
               "Simulated Deaths" = "predicted_total_overall_deaths_tmax"))
  }
  return(list("real" = realData, "simulated" = simData))
}
# some nomenclature:
# CFR = (reported symptomatic cases) / (reported deaths)
#   (possibly per age group or per day)
# sCFR = (symptomatic cases) / (deaths including delay)
#   (possibly per age group or per day)
# IFR = (cases) / (deaths including dealy)
#   (possibly per age group or per day)

# getting group CFR: real und simulated
data_CFR_Group <- function(controls, data_list_model, sample) {
  # this  function provides CFRs per group, together with extensive footnotes
  # that explain where certain data come from
  group <- str_to_lower(controls$type)
  reportedCases  = data_list_model[[paste0(group, "distr_cases")]]
  reportedDeaths = data_list_model[[paste0(group, "distr_deaths")]]
  realData <- 
    tibble(group = groupLabels(group),
           reportedCases, reportedDeaths) %>%
    mutate(reportedCFR = reportedDeaths / reportedCases)
  simData <- rbind(
    tibble(extractValue(sample$sample, paste0("cfr_A_symptomatic_by_", group)),
             group = groupLabels(group)) %>% 
        mutate(metric_description = "CFR (simulated)"),
    tibble(extractValue(sample$sample, paste0("cfr_D_symptomatic_by_", group)),
             group = groupLabels(group)) %>% 
        mutate(metric_description = "sCFR (simulated)"),
    tibble(extractValue(sample$sample, paste0("cfr_D_all_by_", group)),
           group = groupLabels(group)) %>% 
      mutate(metric_description = "IFR (simulated)"))
  return(list("real" = realData, "simulated" = simData))
}


# calculate CFR per day from real data:
data_CFR_Time <- function() {
  return(tibble(date = as_date(day_data:day_max),
                dailyCases = data_list_model$incidence_cases,
                dailyDeaths = data_list_model$incidence_deaths) %>% 
           mutate(realCFR = dailyDeaths / dailyCases))
}

data_CFR_Total <- function(controls, data_list_model, sample) {
  group <- str_to_lower(controls$type)
  realData <- tibble(
    casesTime = sum(data_list_model$incidence_cases),
    deathsTime = sum(data_list_model$incidence_deaths),
    casesGroups = sum(data_list_model[[paste0(group,"distr_cases")]]),
    deathsGroups = sum(data_list_model[[paste0(group, "distr_deaths")]])) %>% 
    transmute(CFRoverTime = deathsTime/casesTime,
              CFRoverGroups = deathsGroups / casesGroups) %>% 
    pivot_longer(cols = c(CFRoverTime, CFRoverGroups))
  simData <- rbind(extractValue(sample, "cfr_A_symptomatic"),
        extractValue(sample, "cfr_C_symptomatic"),
        extractValue(sample, "cfr_D_all")) %>% 
    mutate(metric_description = c("CFR (simulated)", "sCFR (simulated)",
                                  "IFR (simulated)"))
  return(list("real" = realData, "simulated" = simData))
}
