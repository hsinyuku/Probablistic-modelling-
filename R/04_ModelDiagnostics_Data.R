
# ----------------------------------------------------------------------------#




# ----------------------------------------------------------------------------#
# simulated vs. real data ####
# ----------------------------------------------------------------------------#

# # Plot the parameter posteriors and overlaying the chains to check for -----#
# consistence ----------------------------------------------------------------#
{
  # Gather all parameters in theta
  pars <- c("beta", "epsilon","rho","pi","psi", "eta")
  subtitle = f(paste0("For {controls[chains]} chains and ",
                      "{controls[iterations]} iterations per chain."))
  title = f(paste0("Posterior Density Plots ({controls[region]})" ))
  plot <- stan_dens(samples, pars = pars, separate_chains = T, nrow = 3) +
    labs(title = title, subtitle = subtitle) +
    scale_x_continuous(breaks = c(0, 0.5, 1), labels = c("0", ".5", "1")) +
    coord_cartesian(xlim =c(0,1))
  if (controls[["savePlots"]]) {
    save_gg(plot = plot, name = "Posterior Density")
  }
  remove(pars, subtitle, title)
  plot
}

# Plot simulated cases (all, symptomatic, reported) vs real cases ------------#
SimVsReal_Time("deaths", day_data = day_data, day_max = day_max)
SimVsReal_Time("cases", day_data = day_data, day_max = day_max)




SimVsReal_Total("deaths")
SimVsReal_Total("cases")

# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# parameter values ####
# ----------------------------------------------------------------------------#

# plot ascertainment rate per group ------------------------------------------#
plot_ascertainment("#CC333F")

# plot parameter traces ------------------------------------------------------#
stan_trace(samples, pars=c("beta", "epsilon","rho","pi","psi", "eta"))

# ----------------------------------------------------------------------------#


# ----------------------------------------------------------------------------#
# fatality ratios ####
# ----------------------------------------------------------------------------#

# plot CFR, sCFR and IFR -----------------------------------------------------#
# definition time: CFR is calculated as deaths / cases (per group), and can be
# calculated from both the simulated data and the real data

plot_SimVsReal_CFRGroup() +
  labs(caption = paste0("Note: simulated CFR differs from reported CFR in that",
                        " it counts deaths corrected\nfor underreporting.")) +
  theme(plot.caption.position = "plot",
        plot.caption = element_text(hjust = 0))

# ----------------------------------------------------------------------------#


