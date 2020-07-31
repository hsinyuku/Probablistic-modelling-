# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to provide a function to run a full analysis on
# one posterior. Argument should only be the posterior's name.
# ----------------------------------------------------------------------------#

list.files("Posteriors")
# remove(list = ls())
# Which posterior do you want to inspect?
posteriorName <- list.files("Posteriors")[7]

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
  source("R/99_DataFunctions.R")
}

#source("R/99_PlotFunctions.R")
controls$savePlots <-TRUE
controls$plotLegends <- FALSE

theme_set(theme_bw())
posterior <- str_sub(posteriorName, 1, str_locate(posteriorName, "\\.")[1]-1)

# ----------------------------------------------------------------------------#
# data and plots: individual samples, real and simulated ----
# ----------------------------------------------------------------------------#

# numbers of real reported symptomatic cases per day #
# -------------------------------------------------- #
realTimeCases <- data_Real_Time(data_list_model, "cases", day_data, day_max)
sum_realCases <- sum(realTimeCases["n"])
sum_realCases                     
plot_RealTimeCases <- plot_Real_Time(realTimeCases, "cases")
plot_RealTimeCases
if (controls$savePlots) save_gg(plot_RealTimeCases,
                                paste0("RealDeathsTime_", posterior))

# numbers of real reported deaths per day #
# --------------------------------------- #
realTimeDeaths <- data_Real_Time(data_list_model, "deaths", day_data, day_max)
sum_realDeaths <- sum(realTimeDeaths["n"])
sum_realDeaths
plot_RealTimeDeaths <- plot_Real_Time(realTimeDeaths, "deaths")
plot_RealTimeDeaths
if (controls$savePlots) save_gg(plot_RealTimeDeaths,
                                paste0("RealCasesTime", posterior))

# real distribution by group #
plot_Real_GroupProp()
if (controls$savePlots) save_gg(plot_Real_GroupProp,
                                paste0("Real_GroupProp", posterior))

# numbers of simulated versus real cases per group #
# ------------------------------------------------ #
simvsrealGroupCases <- data_SimVsReal_Group(controls, data_list_model,
                                            "cases", sample$sample)
simvsrealGroupCases
plot_simvsrealGroupCases <- plot_SimVsReal_Group(simvsrealGroupCases,
                                               controls, "cases")
plot_simvsrealGroupCases
if (controls$savePlots) save_gg(plot_RealSimGroupCases,
                                paste0("RealSimGroupCases", posterior))

# numbers of simulated versus real deaths per group #
# ------------------------------------------------ #
simvsrealGroupDeaths <- data_SimVsReal_Group(controls, data_list_model,
                                             "deaths", sample$sample)
plot_simvsrealGroupDeaths <- plot_SimVsReal_Group(simvsrealGroupDeaths,
                                                  controls, "deaths")
plot_simvsrealGroupDeaths
if (controls$savePlots) save_gg(plot_RealSimGroupDeaths,
                                paste0("RealSimGroupDeaths", posterior))

# numbers of simulated versus real cases per day #
# ---------------------------------------------- #
simvsrealTimeCases <- data_SimVsReal_Time("cases", controls, sample$sample,
                                          day_max, day_data, data_list_model)
plot_simvsrealTimeCases <- plot_SimVsReal_Time(simvsrealTimeCases, "cases",
                                               day_max, day_data,
                                               data_list_model)
plot_simvsrealTimeCases
if (controls$savePlots) save_gg(plot_simvsrealTimeCases,
                                paste0("SimRealCasesTime", posterior))


# numbers of simulated versus real deaths per day #
# ----------------------------------------------- #
simvsrealTimeDeaths <- data_SimVsReal_Time("deaths", controls, sample$sample,
                                          day_max, day_data, data_list_model)
plot_simvsrealTimeDeaths <- plot_SimVsReal_Time(simvsrealTimeDeaths, "deaths",
                                               day_max, day_data,
                                               data_list_model)
plot_simvsrealTimeDeaths
if (controls$savePlots) save_gg(plot_simvsrealTimeDeaths,
                                paste0("SimRealDeathsTime", posterior))

# number of simulated versus real cases, total #
# -------------------------------------------- #
simvsrealTotalCases <-  data_SimVsReal_Total(sample$sample, "cases",
                                             data_list_model, controls)
simvsrealTotalCases
plot_simvsrealTotalCases2 <- plot_SimVsReal_Total2(simvsrealTotalCases, "cases")
plot_simvsrealTotalCases2
if (controls$savePlots) save_gg(plot_simvsrealTotalCases2)
plot_simvsrealTotalCases <- plot_SimVsReal_Total(simvsrealTotalCases, 
                                                 metric = "cases",
                                                 plotSums = "time")
plot_simvsrealTotalCases
if (controls$savePlots) save_gg(plot_simvsrealTotalCases,
                                paste0("SimRealCasesTotal", posterior))

# number of simulated versus real deaths, total #
# --------------------------------------------- #
simvsrealTotalDeaths <-  data_SimVsReal_Total(sample$sample, "deaths",
                                             data_list_model, controls)
simvsrealTotalDeaths
plot_simvsrealTotalDeaths2 <- plot_SimVsReal_Total2(simvsrealTotalDeaths, "deaths")
plot_simvsrealTotalDeaths2
if (controls$savePlots) save_gg(plot_simvsrealTotalDeaths2)
plot_simvsrealTotalDeaths <- plot_SimVsReal_Total(simvsrealTotalDeaths,
                                                  metric = "deaths",
                                                  plotSums = "time")
plot_simvsrealTotalDeaths
if (controls$savePlots) save_gg(plot_simvsrealTotalDeaths,
                                paste0("SimRealDeathsTotal", posterior))
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# composite plots (single regions) ----
# ----------------------------------------------------------------------------#
library(cowplot)

legend <- get_legend(
  ggplot(data = tibble(x = factor(x = c("All Cases", "Symptomatic Cases",
                                      "Reported Cases",
                                      "Simulated Deaths", "Projected Deaths"),
                                levels = c("All Cases", "Symptomatic Cases",
                                           "Reported Cases",
                                           "Simulated Deaths", "Projected Deaths"),
                                ordered = T))) +
    geom_bar(aes(x = x, fill = x)) +
    scale_fill_manual(values = c("#00B2EE", "#66CD00", "#008B8B",
                                 "#B22222", "#FFD700"), name = NULL) +
    theme(legend.direction = "horizontal"))

plot_overview <- cowplot::plot_grid(
  # first element: six plots
  cowplot::plot_grid(
    plot_simvsrealTimeCases + guides(linetype = F, fill = F, col = F),
    plot_simvsrealTotalCases + guides(linetype = F, fill = F, col = F),
    plot_simvsrealGroupCases + guides(linetype = F, fill = F, col = F),
    plot_simvsrealTimeDeaths + guides(linetype = F, fill = F, col = F),
    plot_simvsrealTotalDeaths + guides(linetype = F, fill = F, col = F),
    plot_simvsrealGroupDeaths + guides(linetype = F, fill = F, col = F),
    nrow = 2, ncol = 3, labels = "AUTO", align = "hv", axis = "b",
    rel_widths = c(3, 1.4, 3)
  ),
  # second element: legend
  legend,
  nrow = 2, rel_heights = c(10,1)
)
if (controls$savePlots) save_gg(plot_overview,
                                paste0("Overview", posterior),
                                width = 7, height = 5)


# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# plots: model inspection ----
# ----------------------------------------------------------------------------#

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

# plot ascertainment rate per group
data_ascertainment(sample$sample)
plot_AscertainmentGroup <- plot_ascertainment(sample$sample, "#CC333F")
plot_AscertainmentGroup
if (controls$savePlots) save_gg(plot_AscertainmentGroup,
                                paste0("AscertainmentGroup", posterior))
# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# plots and data: individual CFR ----
# ----------------------------------------------------------------------------#
  
# CFR per group #
# ------------- #
CFRgroup <- data_CFR_Group(controls, data_list_model, sample)

# unique(CFRgroup[[2]]$metric_description) 

# CFR
CFRgroup[[2]] %>% 
  filter(metric_description == "CFR (simulated)")%>% 
  mutate(combo=paste((round(`50%`,4)*100),"%","[",(round(`2.5%`,4)*100),"-",(round(`97.5%`,4)*100),"]")) %>% 
  select(combo)

# sCFR
CFRgroup[[2]] %>% 
  filter(metric_description == "sCFR (simulated)") %>% 
  mutate(SFRcombo=paste((round(`50%`,5)*100),"% [",(round(`2.5%`,4)*100),"-",
                        (round(`97.5%`,4)*100),"]")) %>% 
  select(SFRcombo)

# IFR
CFRgroup[[2]] %>% 
  filter(metric_description == "IFR (simulated)")%>% 
  mutate(IFRcombo=paste((round(`50%`,5)*100),"%","[",(round(`2.5%`,4)*100),"-",
                        (round(`97.5%`,5)*100),"]")) %>% 
  select(IFRcombo)

plot_CFR_group <- plot_CFR_Group(CFRgroup)
plot_CFR_group
if (controls$savePlots) save_gg(plot_CFRgroup,
                                paste0("CFRgroup", posterior))

# total CFR #
# --------- #
CFRtotal <- data_CFR_Total(controls, data_list_model, sample$sample)
CFRtotal

plot_CFR_total <- plot_CFR_Total(CFRtotal)
plot_CFR_total
if (controls$savePlots) save_gg(plot_CFR_total,
                                paste0("CFRtotal", posterior))


