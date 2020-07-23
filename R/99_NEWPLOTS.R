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


Sys.setlocale("LC_TIME", "English") # this is necessary to get English date
# labels in the plots

# ----------------------------------------------------------------------------#
# functions already translated ####
# ----------------------------------------------------------------------------#

# Function to save plots -----------------------------------------------------#
# necessary parts of the name is taken directly from the list with the
# controls, but can also be specified manually
save_gg <- function(plot, name, region = controls["region"],
                    ind_eta = controls["ind_eta"],
                    chains = controls["chains"],
                    iterations = controls["iterations"], 
                    type = controls["type"],
                    width = 10, height = 6){
  file = paste0("Figures/", region, "_", type, "_", ind_eta, "_", iterations,
                "iteratoins_", chains, "chains.png")
  ggsave(file, units = "in",
         width = width, height = height)
  print(paste0("Saved plot to ", file))
}

# function to extract values from parameters as tibble -----------------------#
extractValue <- function(name, printStat = c("2.5%", "97.5%", "50%")) {
  rstan::summary(samples, name)[[1]] %>%
    as_tibble() %>% mutate(metric = name) %>% 
    select(metric, printStat)
}

# plotting simulated vs. real cases and deaths over time ---------------------#
plot_SimVsReal_Time <- function(metric, day_max, day_data,
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
    estimatedData <- rbind(
      cbind(extractValue("predicted_reported_incidence_symptomatic_cases"),
            date = dates),
      cbind(extractValue("predicted_overall_incidence_symptomatic_cases"),
            date = dates),
      cbind(extractValue("predicted_overall_incidence_all_cases"),
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
    estimatedData <- 
      cbind(extractValue("predicted_overall_incidence_deaths"), 
            date = as_date(day_data:(day_max+data_list_model$G))) %>% 
      mutate(metric = case_when(date <= day_max ~ "Simulated Deaths",
                                date >= day_max ~ "Residual Deaths"),
             metric = fct_relevel(metric, "Simulated Deaths")) 
    realData <- 
      tibble(date = as_date(day_data:day_max),
             incidence=data_list_model$incidence_deaths)
  }
  
  # Plotting the data
  plot <- ggplot(estimatedData, aes(x = date)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = metric),
                alpha = 1) +
    geom_line(aes(y = `50%`, linetype = metric)) +
    geom_point(data = realData, 
               aes(y = incidence), shape = 21, fill = "white")
  # Styling the plot - common stylings
  plot <- plot +
    scale_x_date(labels = scales::label_date(format = "%b %d"))
  # some stylings differ between cases and deaths:
  if (metric == "cases") {
    plot <- plot +
      coord_cartesian(xlim=c(day_data, day_max)) +
      labs(x = "Date (days)",
           y = "Cases",
           fill =  "Simulated Data \n(Median, 95% CI)",
           linetype = "Simulated Data \n(Median, 95% CI)") +
      scale_y_continuous(expand = expansion(mult=c(0,.05)),
                         labels = scales::label_number(scale = 1/1000,
                                                       accuracy = 0.1,
                                                       suffix = " K")) +
      geom_vline(xintercept = data_list_model$tswitch + day_data, linetype=2) +
      scale_fill_manual(values = c(AllCasesFill, SymptCasesFill,
                                   RepCasesFill))
  } else if (metric == "deaths") {
    plot <- plot +
      coord_cartesian(xlim = c(day_data, day_max + data_list_model$G)) +
      labs(x = "Date (days)", y = "Deaths per day", 
           fill = "Simulated Data \n(Median, 95% CI)",
           linetype = "Simulated Data \n(Median, 95% CI)") +
      geom_vline(xintercept = day_max + 0.5, linetype=2) +
      scale_y_continuous(expand = expansion(mult=c(0,.05))) +
      scale_fill_manual(values = c(ResDeaths, SimDeaths))
  }
  return(plot)
}

# plotting simulated vs. real deaths and cases per age group -----------------#
plot_SimVsReal_Group <- function(metric, AllCasesFill = "#00B2EE",
                                 SymptCasesFill = "#66CD00",
                                 RepCasesFill = "#008B8B",
                                 SimDeaths = "#B22222", 
                                 ResDeaths = "#FFD700") {
  # Preparing the real (reported) data
  if(controls$type == "age") {
    groupLabels <- c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                     "60-69", "70-79", "80+")
    if(metric == "deaths") {
      realData <- tibble(n = data_list_model[["agedistr_deaths"]],
                         group = groupLabels)
    } else if (metric == "cases") {
      realData <- tibble(n = data_list_model[["agedistr_cases"]],
                         group = groupLabels)
      
    }
  } else if(controls$type == "gender") {
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
    estimatedData <- rbind(
      extractValue("predicted_total_reported_symptomatic_cases_by_age"),
      extractValue("predicted_total_overall_symptomatic_cases_by_age"),
      extractValue("predicted_total_overall_all_cases_by_age")) %>% 
      mutate(group = rep(groupLabels, 3),
             metric = factor(metric),
             metric = fct_recode(
               metric,
               All = "predicted_total_overall_all_cases_by_age",
               `Symptomatic Cases` = "predicted_total_overall_symptomatic_cases_by_age",
               `Reported Cases` = "predicted_total_reported_symptomatic_cases_by_age"))
  } else if (metric == "deaths") {
    estimatedData <- rbind(
      extractValue("predicted_total_overall_deaths_tmax_by_age"),
      extractValue("predicted_total_overall_deaths_delay_by_age")) %>% 
      mutate(group = rep(groupLabels, 2),
             metric = factor(metric),
             metric = fct_recode(
               metric,
               `Reported Deaths` = "predicted_total_overall_deaths_tmax_by_age",
               `Projected Deaths` = "predicted_total_overall_deaths_delay_by_age"
             ))
  }
  plot <- ggplot() +
    geom_col(data = realData, aes(y = n, x = group), fill = "white",
             col = "black") +
    geom_pointrange(data = estimatedData,
                    aes(x = group, ymin = `2.5%`, y = `50%`, ymax = `97.5%`,
                        col = metric))
  # Common styling for all plots
  plot <- plot + 
    labs(col = "Simulated Data") +
    scale_y_continuous(labels = scales::label_number(scale = 1/1000,
                                                     accuracy = 0.1,
                                                     suffix = " K"),
                       expand = expansion(mult=c(0,.05)))
  # Styling for Age vs. for Gender
  if(controls$type == "age") {
    plot <- plot + 
      labs(x = "Age Group") +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  } else if(controls$type == "gender") {
  }
  if(metric == "cases") {
    print("applying styling for cases")
    plot <- plot +
      labs(y = "Number of total cases") +
      scale_colour_manual(values = c(AllCasesFill, SymptCasesFill,
                                   RepCasesFill))
  } else if(metric == "deaths") {
    print("applying styling for deaths")
    plot <- plot + 
      labs(y = "Number of deaths") +
      scale_colour_manual(values = c(ResDeaths, SimDeaths))
  }
  return(plot)
}

# plotting total deaths and cases --------------------------------------------#
plot_SimVsReal_Total <- function(metric, AllCasesFill = "#00B2EE",
                                 SymptCasesFill = "#66CD00",
                                 RepCasesFill = "#008B8B",
                                 SimDeaths = "#B22222", 
                                 ResDeaths = "#FFD700") {
  # as the total number of cases or deaths, the code currently just picks the
  # greater one of two sums: age distribution and incidences.
  accessString1 <- paste0(controls$type, "distr_", metric)
  accessString2 <- paste0("incidence_", metric)
  total <- max(sum(data_list_model[[accessString1]]),
               sum(data_list_model[[accessString2]]))
  if (metric == "cases") {
    realData <- tibble(
      n = total,
      metric = "Reported cases")
    estimatedData <- rbind(
      extractValue("predicted_total_reported_symptomatic_cases"),
      extractValue("predicted_total_overall_symptomatic_cases"),
      extractValue("predicted_total_overall_all_cases"))  %>% 
      # the releveling is necessary to control which metric gets printed over
      # which metric ("order in which they are printed")
      mutate(metric = factor(metric),
             metric = fct_recode(
               metric,
               All = "predicted_total_reported_symptomatic_cases",
               "Symptomatic Cases" = "predicted_total_overall_symptomatic_cases",
               "Reported Cases" = "predicted_total_overall_all_cases"))
  } else if (metric == "deaths") {
    realData <- tibble(
      n = total,
      metric = "Reported deaths")
    estimatedData <- 
      rbind(extractValue("predicted_total_overall_deaths_tmax"),
            extractValue("predicted_total_overall_deaths_delay")) %>% 
      mutate(metric = factor(metric),
             metric = fct_recode(
               metric,
               "Including Pro-\njected Deaths" = "predicted_total_overall_deaths_delay",
               "Simulated Deaths" = "predicted_total_overall_deaths_tmax"))
  }
  plot <- ggplot() +
    geom_col(data = realData, width = 0.5,
             aes(x = 1, y = n), fill = "white", col = "black") +
    geom_pointrange(data = estimatedData,
                    aes(x = 1, ymin = `2.5%`, y = `50%`, ymax = `97.5%`,
                        col = metric),
                    position = position_dodge2(width = 0.1, padding = 0.2))
  # some stylings
  plot <- plot +
    scale_x_continuous(breaks = NULL, labels = NULL, name = NULL) +
    scale_y_continuous(expand = expansion(mult=c(0,.05)),
                       labels = scales::label_number(scale = 1/1000,
                                                     accuracy = 0.1,
                                                     suffix = " K")) +
    coord_cartesian(xlim = c(0.5, 1.5)) +
    labs(col = "Simulated Data \n(Median, 95% CI)")
  # some stylings differ between cases and deaths:
  if (metric == "cases") {
    plot <- plot +
      labs(y = "Cases") +
      scale_colour_manual(values = c(AllCasesFill, SymptCasesFill,
                                   RepCasesFill))
  } else if (metric == "deaths") {
    plot <- plot +
      labs(y = "Deaths") +
      scale_colour_manual(values = c(ResDeaths, SimDeaths))
  }
  return(plot)
}

# plotting ascertainment ratio rho per age group -----------------------------#
plot_ascertainment <- function(AscRateFill = "#") {
  rhoData <- summary(samples, "rho")$summary %>% as_tibble()
  rhoData <- rhoData %>% mutate(ageGroup = rep(c("0-9","10-19","20-29","30-39","40-49","50-59","60-69","70-79","80+")))
  
  ggplot(rhoData, aes(x = ageGroup, y = `50%`)) +
    geom_col(width = 0.5, alpha = 0.7, fill = AscRateFill) +
    geom_errorbar(aes(ymin = `2.5%`, ymax = `97.5%`), width = 0.5) +
    labs(x = "Age group", y = "Ascertainment Rate (%) with 95% CI",
         caption = paste0("Ascertainment Rate (%): proportion of symptomatic",
                          "individuals per age group seeking care.\n",
                          "Note: $\\rho$ is fixed to 1 for age roup 80+.")) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    scale_y_continuous(labels = scales::label_percent())
}

# Plot the reduction in transmissibility for different age groups ------------#
plot_eta <- function(TransRedFill = "#8FCB9B") {
  eta_age <-
    summary(samples, "eta")$summary %>% as_tibble() %>%
    mutate(ageGroup = rep(
      c(
        "0-9",
        "10-19",
        "20-29",
        "30-39",
        "40-49",
        "50-59",
        "60-69",
        "70-79",
        "80+"
      )
    ))
  ggplot(eta_age, aes(x = ageGroup, y = `50%`)) +
    geom_col(fill = TransRedFill, width = 0.5) +
    labs(x = "Age group", y = "Reduction in transmissibility per age group, %",
         caption = paste0("$\\eta$ is the reduction in transmissibility, for",
                          " each age group, after the control measures are",
                          "fully effective.")) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    scale_y_continuous(labels = scales::label_percent())
}

# plot the number of reported cases / deaths per day -------------------------#
plot_Real_Time <- function(metric, day_start, day_max,
                           RepCasesFill = "#008B8B",
                           SimDeaths = "#B22222") {
  day_start = day_start
  day_max = day_max
  data <- tibble(date = as_date(day_data:day_max))
  if(metric == "cases") {
    data <- cbind(data, n = data_list_model$incidence_cases)
  } else if(metric == "deaths") {
    data <- cbind(data, n = data_list_model$incidence_cases)
  }
  plot <- ggplot(data, aes(x = date, y = n)) +
    labs(x = "Date (days)")
  if(metric == "cases") {
    plot <- plot +
      geom_col(col = "black", fill = RepCasesFill) +
      scale_y_continuous(expand = expansion(mult=c(0,.05)),
                         labels = scales::label_number(scale = 1/1000,
                                                       accuracy = 0.1,
                                                       suffix = " K"),
                         name = "Number of cases")
  } else if (metric == "deaths") {
    plot <- plot +
      geom_col(col = "black", fill = SimDeaths) +
      scale_y_continuous(expand = expansion(mult=c(0,.05)),
                         name = "Number of deaths")
  }
  return(plot)
}
  
# plot the proportion of reported cases / deaths per group -------------------#
plot_Real_GroupProp <- function(GenPopFill = "white",
                                RepCasesFill = "#008B8B",
                                SimDeaths = "#B22222") {
  if(controls$type == "age") {
    data <- tibble(group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                             "60-69", "70-79", "80+"),
                   agedist = data_list_model$age_dist)
    ylabtitle <- "Age Group"
  } else if (controls$type == "gender") {
    data <- tibble(group = c("male", "female"))
    ylabtitle <- "Gender"
  }
  data <- cbind(data,
                deaths = data_list_model$agedistr_deaths,
                cases = data_list_model$agedistr_cases) %>% 
    mutate_at(.vars = c("deaths", "cases"), .funs = function(x) x / sum(x)) %>%
    rename(`General Population` = agedist,
           `Reported Cases` = cases,
           `Reported Deaths` = deaths) %>% 
    pivot_longer(cols = -group)
  plot <- ggplot(data, aes(y = group, x = value, fill = name)) +
    geom_col(col = "black") +
    facet_wrap(facets = vars(name), nrow = 1) +
    scale_x_continuous(expand = expansion(mult=c(0,.05)),
                       labels = scales::label_percent(),
                       name = NULL) +
    theme(axis.text.x=element_text(angle=45,hjust=1)) +
    coord_cartesian(xlim = c(0, 0.65)) +
    scale_fill_manual(values = c(GenPopFill, RepCasesFill, SimDeaths),
                      guide = FALSE)+ 
    labs(y = ylabtitle)
  return(plot)
}
  

