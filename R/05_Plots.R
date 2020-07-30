for(posterior in list.files("Posteriors")) {
  print(posterior)
  posteriorName <- posterior
  source("R/04_ModelDiagnostics.R")
  remove(list = ls())
}
posteriorNameRegions <-
  str_sub(list.files("Posteriors"), 1,
          str_locate(list.files("Posteriors"), "\\.")[,1]-1)

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

# the following code generates all the data necessary for plotting for all the
# posteriors in the respectiv folders. The data includes:
#  - sample             sampleRegions
#  - controls           controlsRegions
#  - data_list_model    dlmRegions
#  - day_max, day_data  daysRegions
# For each of these objects, a list is created. Objects in these lists are 
# named and have the same order as the posteriors. Objects can be accessed by
# (for example) sampleRegions[[2]] to get the sample for the second posterior.
source("setup.R")
source("R/99_ContactMatrix_Gender_Age_Function.R")

sampleRegions <- list()
for(posterior in list.files("Posteriors")) {
  sample <- list(readRDS(file = paste0("Posteriors/", posterior)))
  names(sample) <- posterior
  sampleRegions <- append(sampleRegions, sample)
}
controlsRegions <- list()
daysRegions <- list()
dlmRegions <- list()
controls <- list(visualise = F)
for(posterior in list.files("Posteriors")) {
  controlsTemp <- list(extractPosteriorMetadata(posterior))
  names(controlsTemp) <- posterior
  controlsRegions <- append(controlsRegions, controlsTemp)
  source(paste0("R/01_DataManagement_", controlsTemp[[posterior]]["region"], ".R"))
  source(paste0("R/02_PrepareModel_", controlsTemp[[posterior]]["type"], ".R"))
  daysTemp <- list(list("day_data" = day_data, "day_max" = day_max))
  names(daysTemp) <- posterior
  daysRegions <- append(daysRegions, daysTemp)
  dlmtemp <- list(data_list_model)
  names(dlmtemp) <- posterior
  dlmRegions <- append(dlmRegions, dlmtemp)
}

remove_except(list("dlmRegions", "daysRegions", "controlsRegions",
                   "sampleRegions"))
source("setup.R")
source("R/99_DataFunctions.R")
source("R/99_PlotFunctions.R")

# The following code generates plot. Using pretty dumb for-loops, plots are
# generated for each posterior and then saved in plotlist. Specifiy which 
# posteriors you want to plot by changing the iterator values in the loop.
# Plots are lated plotted using cowplot's plot_grid().
plotlist <- list("time" = list(), "total" = list(), "groups" = list())
for(i in 4:6) { # choose which regions you want to plot
  simvsrealTimeCases <- data_SimVsReal_Time(metric = "cases",
                                            sample = sampleRegions[[i]],
                                            day_max = daysRegions[[i]]$day_max,
                                            day_data = daysRegions[[i]]$day_data,
                                            data_list_model = dlmRegions[[i]])
  plotlist$time <-append(plotlist$time,
                          list(plot_SimVsReal_Time(simvsrealTimeCases, "cases",
                                                   day_max, day_data,
                                                   data_list_model)))
  simvsrealTotalCases <- data_SimVsReal_Total(sample = sampleRegions[[i]],
                                              metric = "cases",
                                              data_list_model = dlmRegions[[i]],
                                              controls = controlsRegions[[i]])
  plotlist$total<- append(plotlist$total,
                          list(plot_SimVsReal_Total(simvsrealTotalCases,
                                                    metric = "cases",
                                                    plotSums = "time")))
  simvsrealGroupCases <- data_SimVsReal_Group(sample = sampleRegions[[i]],
                                              metric = "cases",
                                              data_list_model = dlmRegions[[i]],
                                              controls = controlsRegions[[i]])
  plotlist$groups <- append(plotlist$groups,
                            list(plot_SimVsReal_Group(simvsrealGroupCases,
                                                      metric = "cases",
                                                      controls = controlsRegions[[i]])))
}
legend <- get_legend(plotlist$time[[1]] + theme(legend.direction = "horizontal",
                                                legend.title = element_blank()))
comparisonCases <- cowplot::plot_grid(
  plot_grid(
    plotlist$time[[1]] + guides(linetype = F, fill = F, col = F) +
      labs(x = NULL),
    plotlist$total[[1]] + guides(linetype = F, fill = F, col = F) +
      labs(x = NULL),
    plotlist$groups[[1]] + guides(linetype = F, fill = F, col = F) +
      labs(x = NULL),
    plotlist$time[[2]] + guides(linetype = F, fill = F, col = F) +
      labs(subtitle = NULL, x = NULL),
    plotlist$total[[2]] + guides(linetype = F, fill = F, col = F) +
      labs(subtitle = NULL, x = NULL),
    plotlist$groups[[2]] + guides(linetype = F, fill = F, col = F) +
      labs(subtitle = NULL, x = NULL),
    plotlist$time[[3]] + guides(linetype = F, fill = F, col = F) +
      labs(subtitle = NULL),
    plotlist$total[[3]] + guides(linetype = F, fill = F, col = F) +
      labs(subtitle = NULL),
    plotlist$groups[[3]] + guides(linetype = F, fill = F, col = F) +
      labs(subtitle = NULL, x = "Group"),
    align = "hv", axis = "b", rel_widths = c(1, 0.4, 1),
    labels = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
    label_x = c(-.01, -.5, -.01, rep(c(-.01, -.1, -.01), 2)),
    label_y = c(1, 1, 1, rep(1.1, 9))
  ),
  legend, nrow = 2, rel_heights = c(10,1)
)
save_gg(comparisonCases,"Comparison_Spain_Cases", width = 7, height = 7.5)


plotlist <- list("time" = list(), "total" = list(), "groups" = list())
for(i in 4:6) { # choose which regions you want to plot
  simvsrealTimeDeaths <- data_SimVsReal_Time(metric = "deaths",
                                            sample = sampleRegions[[i]],
                                            day_max = daysRegions[[i]]$day_max,
                                            day_data = daysRegions[[i]]$day_data,
                                            data_list_model = dlmRegions[[i]])
  plotlist$time <-append(plotlist$time,
                         list(plot_SimVsReal_Time(simvsrealTimeDeaths, "deaths",
                                                  day_max, day_data,
                                                  data_list_model)))
  simvsrealTotalDeaths <- data_SimVsReal_Total(sample = sampleRegions[[i]],
                                              metric = "deaths",
                                              data_list_model = dlmRegions[[i]],
                                              controls = controlsRegions[[i]])
  plotlist$total<- append(plotlist$total,
                          list(plot_SimVsReal_Total(simvsrealTotalDeaths,
                                                    metric = "deaths",
                                                    plotSums = "time")))
  simvsrealGroupDeaths <- data_SimVsReal_Group(sample = sampleRegions[[i]],
                                              metric = "deaths",
                                              data_list_model = dlmRegions[[i]],
                                              controls = controlsRegions[[i]])
  plotlist$groups <- append(plotlist$groups,
                            list(plot_SimVsReal_Group(simvsrealGroupDeaths,
                                                      metric = "deaths",
                                                      controls = controlsRegions[[i]])))
}
legend <- get_legend(plotlist$time[[1]] + theme(legend.direction = "horizontal",
                                                legend.title = element_blank()))
comparisonDeaths <- cowplot::plot_grid(
  plot_grid(
    plotlist$time[[1]] + guides(linetype = F, fill = F, col = F),
    plotlist$total[[1]] + guides(linetype = F, fill = F, col = F),
    plotlist$groups[[1]] + guides(linetype = F, fill = F, col = F),
    plotlist$time[[2]] + guides(linetype = F, fill = F, col = F),
    plotlist$total[[2]] + guides(linetype = F, fill = F, col = F),
    plotlist$groups[[2]] + guides(linetype = F, fill = F, col = F),
    plotlist$time[[3]] + guides(linetype = F, fill = F, col = F),
    plotlist$total[[3]] + guides(linetype = F, fill = F, col = F),
    plotlist$groups[[3]] + guides(linetype = F, fill = F, col = F),
    align = "hv", axis = "b", rel_widths = c(1, 0.3, 1),
    labels = c("A1", "A2", "A3", "B1", "B2", "B3", "C1", "C2", "C3"),
    label_x = c(-.01, -.5, -.01, rep(c(-.01, -.1, -.01), 2)),
    label_y = c(1, 1, 1, rep(1.1, 9))
  ),
  legend, nrow = 2, rel_heights = c(10,1)
)
save_gg(comparisonCases,"Comparison_Spain_Deaths", width = 7, height = 7.5)


# this code block attempts to export tables to Latex
parRegions <- summary(sampleRegions[[1]], c("beta", "psi", "pi", "eta", "nu",
                                            "xi", "phi[1]", "phi[2]"))[[1]]
xtable::xtable(tibble("Parameter" = rownames(parRegions),
       "Posterior Median (95% CI)" = str_c(
         signif(parRegions[,"50%"], 2), " [",
         str_c(signif(parRegions[,"2.5%"], 2),
               signif(parRegions[,"97.5%"], 2), sep = " - "), "]"))) %>% 
  print(paste0("Tables/Parameters", posteriorNameRegions[1]), type = "latex")



