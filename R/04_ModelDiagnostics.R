# ----------------------------------------------------------------------------#
# ModelDiagnostics.R
# 
# This idea of this file is to provide a function to run a full analysis on
# one posterior. Argument should only be the posterior's name.
# ----------------------------------------------------------------------------#

list.files("Posteriors")
remove(list = ls())
# Which posterior do you want to inspect?
posteriorName <- list.files("Posteriors")[3]

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

theme_set(theme_bw())
posterior <- str_sub(posteriorName, 1, str_locate(posteriorName, "\\.")[1]-1)


# ----------------------------------------------------------------------------#
# data and plots: individual samples, real and simulated ----
# ----------------------------------------------------------------------------#

# numbers of real reported symptomatic cases per day #
# -------------------------------------------------- #
realTimeCases <- data_Real_Time(data_list_model, "cases", day_data, day_max)
plot_RealTimeCases <- plot_Real_Time(realTimeCases, "cases")
plot_RealTimeCases
if (controls$savePlots) save_gg(plot_RealTimeCases,
                                paste0("RealDeathsTime_", posterior))

# numbers of real reported deaths per day #
# --------------------------------------- #
realTimeDeaths <- data_Real_Time(data_list_model, "deaths", day_data, day_max)
plot_RealTimeDeaths <- plot_Real_Time(realTimeDeaths, "deaths")
plot_RealTimeDeaths
if (controls$savePlots) save_gg(plot_RealTimeDeaths,
                                paste0("RealCasesTime", posterior))

# numbers of simulated versus real cases per group #
# ------------------------------------------------ #
simvsrealGroupCases <- data_SimVsReal_Group(controls, data_list_model,
                                            "cases", sample$sample)
plot_simvsrealGroupCases <- plot_SimVsReal_Group(simvsrealGroupCases,
                                               controls, "cases")
plot_simvsrealGroupCases
if (controls$savePlots) save_gg(plot_RealSimDeathsTime,
                                paste0("RealDeathsGroup", posterior))

# numbers of simulated versus real deaths per group #
# ------------------------------------------------ #
simvsrealGroupDeaths <- data_SimVsReal_Group(controls, data_list_model,
                                             "deaths", sample$sample)
plot_simvsrealGroupDeaths <- plot_SimVsReal_Group(simvsrealGroupDeaths,
                                                  controls, "deaths")
plot_simvsrealGroupDeaths
if (controls$savePlots) save_gg(plot_RealSimCasesTime,
                                paste0("RealSimCasesGroup", posterior))

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
plot_simvsrealTimeCases <- plot_SimVsReal_Time(simvsrealTimeDeaths, "deaths",
                                               day_max, day_data,
                                               data_list_model)
plot_simvsrealTimeCases
if (controls$savePlots) save_gg(plot_simvsrealTimeCases,
                                paste0("SimRealDeathsTime", posterior))

# number of simulated versus real cases, total #
# -------------------------------------------- #
simvsrealTotalCases <-  data_SimVsReal_Total(sample$sample, "cases",
                                             data_list_model, controls)
plot_simvsrealTotalCases <- plot_SimVsReal_Total(simvsrealTotalCases, "cases")
plot_simvsrealTotalCases
if (controls$savePlots) save_gg(plot_simvsrealTotalCases,
                                paste0("SimRealCasesTotal", posterior))

# number of simulated versus real deaths, total #
# --------------------------------------------- #
simvsrealTotalDeaths <-  data_SimVsReal_Total(sample$sample, "deaths",
                                             data_list_model, controls)
plot_simvsrealTotalDeaths <- plot_SimVsReal_Total(simvsrealTotalDeaths, "deaths")
plot_simvsrealTotalDeaths
if (controls$savePlots) save_gg(plot_simvsrealTotalDeaths,
                                paste0("SimRealDeathsTotal", posterior))
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
plot_CFR_group <- plot_CFR_Group(CFRgroup)
plot_CFR_group
if (controls$savePlots) save_gg(plot_CFRgroup,
                                paste0("CFRgroup", posterior))

# total CFR #
# --------- #
CFRtotal <- data_CFR_Total(controls, data_list_model, sample$sample)
plot_CFR_total <- plot_CFR_Total(CFRtotal)
plot_CFR_total
if (controls$savePlots) save_gg(plot_CFR_total,
                                paste0("CFRtotal", posterior))

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
CFRtotalRegions <- filter(generatedQuantitiesSummary,
                          parameter %in% c("cfr_A_symptomatic",
                                           "cfr_B_symptomatic",
                                           "cfr_C_symptomatic",
                                           "cfr_D_symptomatic",
                                           "cfr_C_all", "cfr_D_all")) %>% 
  unnest(cols = data) %>% 
  mutate(parameter = factor(parameter, 
                            levels = c("cfr_A_symptomatic", "cfr_B_symptomatic",
                                       "cfr_C_symptomatic", "cfr_D_symptomatic",
                                       "cfr_C_all", "cfr_D_all"))) %>%
  filter(ind_eta == "CommonEta",
         parameter %in% c("cfr_A_symptomatic", "cfr_D_symptomatic",
                          "cfr_D_all")) %>% 
  mutate(region = ifelse(region == "BadenW", "Baden-\nWürttemberg", region),
         name = str_c(region, ", \n", type))
plot_CFR_Total_Regions(CFRtotalRegions)
  # There are multiple points per region because we have multiple posterios for
  # these regions

# compare the IFR for different age groups for different reasons
IFRGroupsRegions <- filter(generatedQuantitiesSummary,
                           parameter == "cfr_D_symptomatic_by_group") %>% 
  unnest(cols = data) %>% 
  filter(type == "Age", ind_eta == "CommonEta") %>% 
  mutate(region = ifelse(region == "BadenW", "Baden-\nWürttemberg", region),
         name = str_c(region, ", \n", type),
         index = factor(index))
x <- as.character(1:9)
names(x) <- groupLabels("age")
IFRGroupsRegions$index <- fct_recode(IFRGroupsRegions$index, !!!x)

plot_IFR_Groups_Regions(IFRGroupsRegions, log = T)



