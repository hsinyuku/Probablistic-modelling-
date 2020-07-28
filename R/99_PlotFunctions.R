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


# this function should be able to provide python-like fstring-functionality. -#
# Probably really slow, but will do the trick for things like plot caption. --#
# CAN NOT BE USED INSIDE FUNCTION, MUST BE CALLED DIRECTLY FROM THE GLOBAL
# ENVIRONMENT
f <- function(fstring) {
  replaceRegExp <- "\\{[[:alnum:]\\_\\]\\[]+\\}"
  # the pattern we are looking for is any variable, surrounded by curly 
  # brackets; variable names can contain letters, digits, and _ (but not 
  # other special characters)
  fstring2val <- function(fstring) {
    # writing a function to use in str_replace_all(); one object name (enclosed)
    # by curly brackets) at a time will be passed to this
    objectName <- str_sub(fstring, 2, -2)
    # remove the surrounding brackets, so the object's name remains
    if(str_detect(objectName, "\\[[[:alnum:]\\_]+\\]")) {
      # function should be able to call objects from lists, which have to be
      # indexed using [[]] and quotation marks. To deal with this, we separate
      # the two parts: the list name and the element name.
      listName = str_extract(objectName, "[[:alnum:]\\_]+(?=\\[)")
      # this is the name of the list to be accessed
      listElement = str_sub(str_extract(objectName, "\\[[[:alnum:]\\_]+\\]"), 2, -2)
      # this is the name of the element inside the list to be accessed
      return(get(listName)[listElement])
      # glue it together and call get()
    } else {
      # if the string passed was not a list, accessing is much simpler:
      return(get(objectName))
    }
  }
  # finally, search for all objects in the fstring (surrounded by curly
  # brackets), then replace them one be one.
  return(str_replace_all(fstring, replaceRegExp, fstring2val))
}


# Function to save plots -----------------------------------------------------#
# necessary parts of the name is taken directly from the list with the
# controls, but can also be specified manually
save_gg <- function(plot, name,
                    width = 10, height = 6){
  ggsave(paste0("Figures/", name, ".png"), units = "in",
         width = width, height = height)
  print(paste0("Saved plot to Figures/", name, ".png"))
}

# Functions for plot styling
scale_x_labelsRotate <- function(Angle = 45, ...) {
  theme(axis.text.x = element_text(angle = Angle, hjust=1, ...))
}

scale_y_percent <- function(labels = "percent", ...) {
  scale_y_continuous(expand = expansion(mult=c(0,.05)),
                     labels = scales::label_percent())
}

# plotting simulated vs. real cases and deaths over time ---------------------#
plot_SimVsReal_Time <- function(data, metric, day_max, day_data,
                                data_list_model,
                                AllCasesFill = "#00B2EE",
                                SymptCasesFill = "#66CD00",
                                RepCasesFill = "#008B8B",
                                SimDeaths = "#B22222", 
                                ResDeaths = "#FFD700") {
  plot <- ggplot(data$simulated, aes(x = date)) +
    geom_ribbon(aes(ymin = `2.5%`, ymax = `97.5%`, fill = metric),
                alpha = 1) +
    geom_line(aes(y = `50%`, linetype = metric)) +
    geom_point(data = data$real, 
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
plot_SimVsReal_Group <- function(data, controls, metric, AllCasesFill = "#00B2EE",
                                 SymptCasesFill = "#66CD00",
                                 RepCasesFill = "#008B8B",
                                 SimDeaths = "#B22222", 
                                 ResDeaths = "#FFD700") {
  plot <- ggplot() +
    geom_col(data = data$real, aes(y = n, x = group), fill = "white",
             col = "black") +
    geom_pointrange(data = data$simulated,
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
  if(controls$type == "Age") {
    plot <- plot + 
      labs(x = "Age Group") +
      theme(axis.text.x=element_text(angle=45,hjust=1))
  } else if(controls$type == "Gender") {
  }
  if(metric == "cases") {
    plot <- plot +
      labs(y = "Number of total cases") +
      scale_colour_manual(values = c(AllCasesFill, SymptCasesFill,
                                     RepCasesFill))
  } else if(metric == "deaths") {
    plot <- plot + 
      labs(y = "Number of deaths") +
      scale_colour_manual(values = c(ResDeaths, SimDeaths))
  }
  return(plot)
}

# plotting total deaths and cases --------------------------------------------#
plot_SimVsReal_Total <- function(data, metric,
                                 AllCasesFill = "#00B2EE",
                                 SymptCasesFill = "#66CD00",
                                 RepCasesFill = "#008B8B",
                                 SimDeaths = "#B22222", 
                                 ResDeaths = "#FFD700") {
  plot <- ggplot() +
    geom_col(data = data$real, width = 1,
             aes(x = name, y = value), fill = "white", col = "black") +
    geom_pointrange(data = data$simulated,
                    aes(x = 1.5, ymin = `2.5%`, y = `50%`, ymax = `97.5%`,
                        col = metric),
                    position = position_dodge2(width = 1, padding = 0.2))
  # some stylings
  plot <- plot +
    scale_x_labelsRotate() +
    scale_x_discrete(labels = c("Sum over\ngroups", "Sum over\ntime"),
                         name = "Calculation of total cases") +
    scale_y_continuous(expand = expansion(mult=c(0,.05)),
                       labels = scales::label_number(scale = 1/1000,
                                                     accuracy = 0.1,
                                                     suffix = " K")) +
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

# plot the number of reported cases / deaths per day -------------------------#
plot_Real_Time <- function(data, metric, RepCasesFill = "#008B8B",
                           SimDeaths = "#B22222") {
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
  if(controls$type == "Age") {
    data <- tibble(group = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59",
                             "60-69", "70-79", "80+"),
                   popdist = data_list_model$age_dist,
                   deaths = data_list_model$agedistr_deaths,
                   cases = data_list_model$agedistr_cases)
    ylabtitle <- "Age Group"
  } else if (controls$type == "Gender") {
    data <- tibble(group = c("male", "female"),
                   popdist = data_list_model$gender_dist,
                   deaths = data_list_model$genderdistr_deaths,
                   cases = data_list_model$genderdistr_cases)
    ylabtitle <- "Gender"
  }
  data <- mutate_at(data, .vars = c("deaths", "cases"),
                    .funs = function(x) x / sum(x)) %>%
    rename(`General Population` = popdist,
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

# plot CFRs -------------------------------------------------------------------

# some nomenclature:
# CFR = (reported deaths) / (reported symptomatic cases)
#   (possibly per age group or per day)
# sCFR = (deaths including delay) / (symptomatic cases)
#   (possibly per age group or per day)
# IFR = (cases) / (deaths including delay)
#   (possibly per age group or per day)

plot_CFR_Group <- function(data) {
  ggplot() +
    geom_col(data = data$real,
             aes(x = group, y = reportedCFR, fill = "white"), col = "black") +
    geom_pointrange(data = data$simulated,
                    aes(x = group, col = metric_description,
                        ymin = `2.5%`, y = `50%`, ymax = `97.5%`),
                    position = position_dodge(width = 0.5)) +
    labs(x = "Age Group", y = NULL,
         col = "Simulated Data \n(Median, 95% CI)",
         fill = "test") +
    scale_x_labelsRotate() + scale_y_percent() +
    scale_fill_manual(name = "Reported Data", values = "white",
                      labels = "CFR (reported)")
}

# this plots CFRs for one individual region
plot_CFR_Total <- function(data) {
  ggplot() +
    geom_col(data = data$real, aes(x = name, y = value),
             fill ="white", col = "black", width = 1) +
    geom_pointrange(data = data$simulated,
                    aes(x = 1.5, col = metric_description,
                        y = `50%`, ymin = `2.5%`, ymax= `97.5%`),
                    position = position_dodge(width = 1)) +
    scale_y_percent() +
    scale_x_discrete(labels = c("groups", "time"),
                     name = "CFR generated by sum over...") +
    labs(col = "Simulated")
  
}

# this plots CFRs for different regions
plot_CFR_Total_Regions <- function(data) {
  ggplot(data) +
    geom_pointrange(aes(x = name, ymin = `2.5%`, ymax = `97.5%`, y = `50%`,
                        col = parameter),
                    position = position_dodge(width = 0.3)) +
    scale_colour_discrete(
      label = c("CFR: fatality ratio among reported cases",
                # "CFR (corrected for right-censoring)",
                # "CFR (corrected for underreporting)",
                "sCFR: CFR, corrected for right-censoring and underreporting",
                # "sCFR (corrected for proportion of\nnon-symptomatic)",
                "IFR: sCFR, corrected for right-censoring and proportion of non-symptomatic"),
      name = "Simulated fatality ratios") +
    theme(legend.position = "bottom", legend.direction = "vertical") +
    scale_x_labelsRotate() +
    scale_y_percent() +
    labs(y = NULL, x = NULL)
} 


# this plots the IFR for different regions
plot_CFR_Total_Regions <- function(data) {
  ggplot(data) +
    geom_pointrange(aes(x = name, ymin = `2.5%`, ymax = `97.5%`, y = `50%`,
                        col = parameter),
                    position = position_dodge(width = 0.3)) +
    scale_colour_discrete(name = "Simulated fatality ratios") +
    theme(legend.position = "bottom", legend.direction = "horizontal") +
    scale_x_labelsRotate() +
    scale_y_percent() +
    labs(y = NULL, x = NULL)
} 

plot_IFR_Groups_Regions <- function(data, log = F) {
  minor_breaks <- c(
    seq(0.00001, 0.0001, 0.00001), seq(0.0001, 0.001, 0.0001),
    seq(0.001, 0.01, 0.001), seq(0.01, 0.1, 0.01), seq(0.1, 1, 0.1)
  )
  plot <- ggplot(data, aes(x = index, y = `50%`, col = region, group = region)) +
    geom_line() + geom_point()
  if(log) {
    plot + 
      scale_y_log10(expand = expansion(mult=c(0,.05)),
                    labels = scales::percent_format(accuracy = 0.001),
                    minor_breaks = minor_breaks,
                    breaks = c(.00003, .0001, .0003, .001, .003, .01, .03, .1, .3)) +
      labs(x = "Age Group", y = "IFR (log-scaled)", col = "Region") +
      scale_x_labelsRotate()
  } else {
    plot + 
      scale_y_percent() +
      labs(x = "Age Group", y = "IFR", col = "Region") +
      scale_x_labelsRotate()
  }
}

# plot model parameters -------------------------------------------------------

plot_Parameters <- function(sample){
  # Gather all parameters in theta
  pars <- c("beta", "epsilon","rho","pi","psi", "eta")
  subtitle = f(paste0("For {controls[chains]} chains and ",
                      "{controls[iterations]} iterations per chain."))
  title = f(paste0("Posterior Density Plots ({controls[region]})" ))
  plot <- stan_dens(sample, pars = pars, separate_chains = T, nrow = 3) +
    labs(title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(xlim =c(0,1))
  return(plot)
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

# plotting ascertainment ratio rho per age group -----------------------------#
plot_ascertainment <- function(sample, AscRateFill = "#") {
  rhoData <- summary(sample, "rho")$summary %>% as_tibble()
  if(controls$type == "Age") {
    rhoData <- rhoData %>%
      mutate(ageGroup = rep(c("0-9","10-19","20-29","30-39","40-49","50-59",
                              "60-69","70-79","80+")))
  } else if (controls$type == "Gender") {
    rhoData <- rhoData %>%
      mutate(ageGroup = rep(c("male", "female")))
  }
  
  
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
