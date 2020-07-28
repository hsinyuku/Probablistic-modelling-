# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to provide a function to run a full analysis on
# one posterior. Argument should only be the posterior's name.
# ----------------------------------------------------------------------------#

list.files("Posteriors")
remove(list = ls())
# Which posterior do you want to inspect?
posteriorName <- list.files("Posteriors")[1]

{
  print("1) sourcing setup.R")
  source("setup.R")
  print("2) generate a controls-variable from the posteriorName")
  controls <- extractPosteriorMetadata(posteriorName)
  print("3) check that the controls have been initialised correctly")
  check_controls(controls)
  print("4) extract the data from a posterior object, specified by the controls")
  sample <- initialiseSample(posteriorName, type = controls$type)
  print(paste0("5) check again the controls object to make sure that the metadata from the", 
               "posterior matches its name"))
  controls$chains == as.character(sample$metadata$chains)
  controls$iterations == as.character(sample$metadata$iterations)
  print("6) load the real data")
  source(paste0("R/01_DataManagement_", controls["region"], ".R"))
  source("R/99_ContactMatrix_Gender_Age_Function.R")
  source(paste0("R/02_PrepareModel_", controls["type"], ".R"))
  print("7) clean the environment of all unnecessary objects, then re-load functions")
  remove_except(list("data_list_model", "controls", "sample", "day_data", "day_max",
                     "posteriorName"))
  print("8) load functions for plotting")
  source("R/99_PlotFunctions.R")
}

theme_set(theme_bw())
posterior <- str_sub(posteriorName, 1, str_locate(posteriorName, "\\.")[1]-1)

# ----------------------------------------------------------------------------#
# plots for individual samples ----
# ----------------------------------------------------------------------------#
{
  # numbers of cases /deaths per day -------------------------------------------#
  plot_RealDeathsTime <- plot_Real_Time("deaths", day_data, day_max)
  plot_RealDeathsTime
  if (controls$savePlots) save_gg(plot_RealDeathsTime,
                                  paste0("RealDeathsTime_", posterior))
  plot_RealCasesTime <- plot_Real_Time("cases", day_data, day_max) 
  plot_RealCasesTime
  if (controls$savePlots) save_gg(plot_RealCasesTime,
                                  paste0("RealCasesTime_", posterior))
  # number of cases and deaths per group
  plot_RealCasesDeathsGroups <- plot_Real_GroupProp()
  plot_RealCasesDeathsGroups
  if (controls$savePlots) save_gg(plot_RealCasesDeathsGroups,
                                  paste0("RealCasesDeathsGroups_", posterior))
  # plot parameter distributions
  plot_ParametersDensity <- plot_Parameters(sample$sample)
  plot_ParametersDensity
  if (controls$savePlots) save_gg(plot_ParametersDensity,
                                  paste0("ParameterDensity", posterior))
  
  # plot parameter traces 
  plot_ParameterTrace <- stan_trace(sample$sample,
                                    pars=c("beta", "epsilon","rho","pi",
                                           "psi", "eta"))
  plot_ParameterTrace
  if (controls$savePlots) save_gg(plot_ParameterTrace,
                                  paste0("ParameterTrace", posterior))
  
  # Plot simulated cases (all, symptomatic, reported) vs real cases
  plot_RealSimDeathsTime <- plot_SimVsReal_Time(sample$sample, "deaths",
                                                day_data = day_data,
                                                day_max = day_max)
  plot_RealSimDeathsTime
  if (controls$savePlots) save_gg(plot_RealSimDeathsTime,
                                  paste0("RealDeathsTime", posterior))
  plot_RealSimCasesTime <- plot_SimVsReal_Time(sample$sample, "cases",
                                               day_data = day_data,
                                               day_max = day_max)
  plot_RealSimCasesTime
  if (controls$savePlots) save_gg(plot_RealSimCasesTime,
                                  paste0("RealSimCasesTime", posterior))
  
  # plot cases and deaths over time, simulated vs. real data
  plot_SimRealGroupCases <- plot_SimVsReal_Group(sample$sample, metric = "cases")
  plot_SimRealGroupCases
  if (controls$savePlots) save_gg(plot_SimVsReal_Group,
                                  paste0("SimRealGroupCases", posterior))
  plot_SimRealGroupDeaths <- plot_SimVsReal_Group(sample$sample, metric = "deaths")
  plot_SimRealGroupDeaths
  if (controls$savePlots) save_gg(plot_SimVsReal_Group,
                                  paste0("SimRealGroupDeaths", posterior))
  
  # plot total cases and deaths, simulated vs. real
  plot_SimRealTotalCases <- plot_SimVsReal_Total(sample$sample, "cases")
  plot_SimRealTotalCases
  if (controls$savePlots) save_gg(plot_SimRealTotalCases,
                                  paste0("SimRealTotalCases", posterior))
  plot_SimRealTotalDeaths <- plot_SimVsReal_Total(sample$sample, "deaths")
  plot_SimRealTotalDeaths
  if (controls$savePlots) save_gg(plot_SimRealTotalDeaths,
                                  paste0("SimRealTotalDeaths", posterior))
  
  # plot ascertainment rate per group
  plot_AscertainmentGroup <- plot_ascertainment(sample$sample, "#CC333F")
  plot_AscertainmentGroup
  if (controls$savePlots) save_gg(plot_AscertainmentGroup,
                                  paste0("AscertainmentGroup", posterior))
  
  # CFR
  plot_SimRealCFRGroup <- plot_SimVsReal_CFRGroup(sample$sample) +
    labs(caption = paste0("Note: simulated CFR differs from reported CFR in that",
                          " it counts deaths corrected\nfor underreporting.")) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
  plot_SimRealCFRGroup
  if (controls$savePlots) save_gg(plot_SimRealCFRGroup,
                                  paste0("SimRealCFRGroup", posterior))
}

# ----------------------------------------------------------------------------#
# data for all samples----
# ----------------------------------------------------------------------------#
# the following code can be used to extract data from all the posteriors that
# are in the respective directory. Only run this code when you want to update
# all the posteriors.
source("setup.R")
generatedQuantitiesList <- list()
for(posteriorName in list.files("Posteriors")) {
  print(posteriorName)
  remove("controls", "sample", "list")
  controls <- extractPosteriorMetadata(posteriorName)
  check_controls(controls)
  sample <- initialiseSample(posteriorName, type = controls$type)
  if(controls$chains != as.character(sample$metadata$chains) |
      controls$iterations != as.character(sample$metadata$iterations)) {
    stop(paste("Error: posterior metadata did not match its name\n",
               posteriorName))
  }
  list <- list(append(sample, controls))
  names(list) <- posteriorName
  generatedQuantitiesList <- append(generatedQuantitiesList, list)
}
saveRDS(generatedQuantitiesList, "data/00_generatedQuantities.Rds")

# this code extracts data about the generated quantities from all the 
# posteriors
{
  generatedQuantitiesList <- readRDS("data/00_generatedQuantities.Rds")
  
  generatedQuantitiesSummary <- list()
  for (parameter in c("per_day", "per_group", "per_both", "per_none")) {
    parameterSummary <- map_dfr(generatedQuantitiesList, function(x) {
      x$parameters[[parameter]] %>% 
        add_column(region = x$region, 
                   type = x$type,
                   ind_eta = x$ind_eta,
                   chains = x$chains,
                   iterations = x$iterations,
                   timestamp = x$timestamp)
    })
    generatedQuantitiesSummary <- append(generatedQuantitiesSummary,
                                         list(parameterSummary))
  }
  
  names(generatedQuantitiesSummary) <- c("per_day", "per_group",
                                         "per_both", "per_none")
  generatedQuantitiesSummary <- map_dfr(generatedQuantitiesSummary,
                                        function(x) {
    group_by(x, parameter) %>% nest()
  })
  
  quantityMetadataTable <- quantityMetadataTable(controls$type) %>% 
    mutate(parameter = str_replace(parameter, "(age)|(gender)", "group"))
  
  generatedQuantitiesSummary <- generatedQuantitiesSummary %>% 
    mutate(
      parameter = str_replace(parameter, "(age)|(gender)", "group"),
      parameter = factor(parameter,
                         levels = quantityMetadataTable$parameter))
}


# ----------------------------------------------------------------------------#
# plots for all samples ----
# ----------------------------------------------------------------------------#

# compare different fatality ratios for different regions
plot_CFR_total_regions()
  # There multiple points per region because we have multiple posterios for
  # these regions
