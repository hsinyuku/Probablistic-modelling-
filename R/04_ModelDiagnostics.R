# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to provide a function to run a full analysis on
# one posterior. Argument should only be the posterior's name.
# ----------------------------------------------------------------------------#

list.files("Posteriors")
remove(list = ls())
# Which posterior do you want to inspect?
posterior <- list.files("Posteriors")[1]

{
  source("setup.R")
  # create the controls-object from a posterior file name
  inspectPosterior(posterior, savePlots = T)
  # load the data
  source(paste0("R/01_DataManagement_", controls["region"], ".R"))
  source("R/99_ContactMatrix_Gender_Age_Function.R")
  source(paste0("R/02_PrepareModel_", controls["type"], ".R"))
  # clean the environment of all unnecessary objects, then re-load functions
  remove_except(list("data_list_model", "controls", "day_data", "day_max",
                     "posterior"))
  source("setup.R")
  source("R/99_PlotFunctions.R")
  # source the right files
  initialiseSample(posterior)
  remove(list = c("regions", "sampler_params"))
}

theme_set(theme_bw())
posterior <- str_sub(posterior, 1, str_locate(posterior, "\\.")[1]-1)

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
  plot_ParametersDensity <- plot_Parameters()
  plot_ParametersDensity
  if (controls$savePlots) save_gg(plot_ParametersDensity,
                                  paste0("ParameterDensity", posterior))
  
  # plot parameter traces 
  plot_ParameterTrace <- stan_trace(samples,
                                    pars=c("beta", "epsilon","rho","pi","psi", "eta"))
  plot_ParameterTrace
  if (controls$savePlots) save_gg(plot_ParameterTrace,
                                  paste0("ParameterTrace", posterior))
  
  # Plot simulated cases (all, symptomatic, reported) vs real cases
  plot_RealSimDeathsTime <- plot_SimVsReal_Time("deaths",
                                                day_data = day_data,
                                                day_max = day_max)
  plot_RealSimDeathsTime
  if (controls$savePlots) save_gg(plot_RealSimDeathsTime,
                                  paste0("RealDeathsTime", posterior))
  plot_RealSimCasesTime <- plot_SimVsReal_Time("cases",
                                               day_data = day_data,
                                               day_max = day_max)
  plot_RealSimCasesTime
  if (controls$savePlots) save_gg(plot_RealSimCasesTime,
                                  paste0("RealSimCasesTime", posterior))
  
  # plot cases and deaths over time, simulated vs. real data
  plot_SimRealGroupCases <- plot_SimVsReal_Group(metric = "cases")
  plot_SimRealGroupCases
  if (controls$savePlots) save_gg(plot_SimVsReal_Group,
                                  paste0("SimRealGroupCases", posterior))
  plot_SimRealGroupDeaths <- plot_SimVsReal_Group(metric = "deaths")
  plot_SimRealGroupDeaths
  if (controls$savePlots) save_gg(plot_SimVsReal_Group,
                                  paste0("SimRealGroupDeaths", posterior))
  
  # plot total cases and deaths, simulated vs. real
  plot_SimRealTotalCases <- plot_SimVsReal_Total("cases")
  plot_SimRealTotalCases
  if (controls$savePlots) save_gg(plot_SimRealTotalCases,
                                  paste0("SimRealTotalCases", posterior))
  plot_SimRealTotalDeaths <- plot_SimVsReal_Total("deaths")
  plot_SimRealTotalDeaths
  if (controls$savePlots) save_gg(plot_SimRealTotalDeaths,
                                  paste0("SimRealTotalDeaths", posterior))
  
  # plot ascertainment rate per group
  plot_AscertainmentGroup <- plot_ascertainment("#CC333F")
  plot_AscertainmentGroup
  if (controls$savePlots) save_gg(plot_AscertainmentGroup,
                                  paste0("AscertainmentGroup", posterior))
  
  # CFR
  plot_SimRealCFRGroup <- plot_SimVsReal_CFRGroup() +
    labs(caption = paste0("Note: simulated CFR differs from reported CFR in that",
                          " it counts deaths corrected\nfor underreporting.")) +
    theme(plot.caption.position = "plot",
          plot.caption = element_text(hjust = 0))
  plot_SimRealCFRGroup
  if (controls$savePlots) save_gg(plot_SimRealCFRGroup,
                                  paste0("SimRealCFRGroup", posterior))
}
