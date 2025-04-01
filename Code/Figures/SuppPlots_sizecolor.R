################################################################################
### Supplemental figures with body sizes color coded
################################################################################

library(dplyr); library(cowplot); library(ggplot2); library(viridisLite);library(lmodel2)

### load data

forage <- read.csv('forage_modified.csv')

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

forage <- forage %>% filter(Obs_h > 1e-6)

### calculate predicted space clearance rate

#  we will assume an energy density of prey of 5.6 kJ/g

energy_density <- 5.6

################################################################################
### predicted space clearance rates and handling times
################################################################################

forage <- forage %>% mutate(Pred_a = PredMetabolism/(PreyAbundance_10*energy_density*Mass_g),
                            Pred_h = 0.9*PreyAbundance_10*energy_density*Mass_g/(PreyAbundance_90*PredMetabolism*0.1))

### make plots of predicted and observed space clearance rates with
### points color coded by prey mass, predator mass, and their ratio

lmodel2(formula = log(Pred_a) ~ log(Obs_a), data = forage)

# prey mass

scr_preysize <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Pred_a), color = log(Mass_g))) + 
  geom_point(alpha = 0.25) + scale_colour_viridis_c(name = 'log Prey\nMass (g)') + 
  theme_cowplot() + xlab('log Observed\nSpace Clearance Rate') + 
  ylab('log Predicted\nSpace Clearance Rate') + geom_abline(intercept = 0.73, slope = 0.85, size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 1)

# predator mass 

scr_predsize <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Pred_a), color = log(PredMass_g))) + 
  geom_point(alpha = 0.25) + scale_colour_viridis_c(name = 'log Predator\nMass (g)') + 
  theme_cowplot() + xlab('log Observed\nSpace Clearance Rate') + 
  ylab('log Predicted\nSpace Clearance Rate') + geom_abline(intercept = 0.73, slope = 0.85, size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 1)

# mass ratio 

scr_sizeratio <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Pred_a), color = log(PredMass_g/Mass_g))) + 
  geom_point(alpha = 0.25) + scale_colour_viridis_c(name = 'log Predator-Prey\nMass Ratio') + 
  theme_cowplot() + xlab('log Observed\nSpace Clearance Rate') + 
  ylab('log Predicted\nSpace Clearance Rate') + geom_abline(intercept = 0.73, slope = 0.85, size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 1)

### put these plots together into a single plot

space_clearance_size_plots <- plot_grid(scr_preysize, scr_predsize, scr_sizeratio, nrow = 1, labels = 'AUTO')

save_plot(filename = 'scr_prediction_sizes.png', plot = space_clearance_size_plots,
          nrow = 0.75, ncol = 2, bg = 'white')

### plots for handling times 

lmodel2(formula = log(Pred_h) ~ log(Obs_h), data = forage)

handling_preysize <- ggplot(data = forage, aes(x = log(Obs_h), y = log(Pred_h), color = log(Mass_g))) + 
  geom_point(alpha = 0.25) + scale_colour_viridis_c(name = 'log Prey\nMass (g)') + 
  theme_cowplot() + xlab('log Observed\nHandling Time') + 
  ylab('log Predicted\nHandling Time') + geom_abline(intercept = 1.03, slope = 1.14, size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 1)

handling_predsize <- ggplot(data = forage, aes(x = log(Obs_h), y = log(Pred_h), color = log(PredMass_g))) + 
  geom_point(alpha = 0.25) + scale_colour_viridis_c(name = 'log Predator\nMass (g)') + 
  theme_cowplot() + xlab('log Observed\nHandling Time') + 
  ylab('log Predicted\nHandling Time') + geom_abline(intercept = 1.03, slope = 1.14, size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 1)

handling_sizeratio <- ggplot(data = forage, aes(x = log(Obs_h), y = log(Pred_h), color = log(PredMass_g/Mass_g))) + 
  geom_point(alpha = 0.25) + scale_colour_viridis_c(name = 'log Predator-Prey\nMass Ratio') + 
  theme_cowplot() + xlab('log Observed\nHandling Time') + 
  ylab('log Predicted\nHandling Time') + geom_abline(intercept = 1.03, slope = 1.14, size = 1) + 
  geom_abline(intercept = 0, slope = 1, linetype = 'dashed', size = 1)

handling_size_plots <- plot_grid(handling_preysize, handling_predsize, handling_sizeratio,
                                 nrow = 1, labels = 'AUTO') 

save_plot(filename = 'handling_prediction_sizes.png', plot = handling_size_plots, nrow = 0.75, ncol = 2, bg = 'white')


### size plots for relationship between half-saturation and high prey densities

forage <- forage %>%  mutate(halfsat = 1/(Obs_a*Obs_h), pop_pred = 0.1*PreyAbundance_90/0.9)

lmodel2(formula = log(pop_pred) ~ log(halfsat), data = forage)

poppred_preysize <- ggplot(data = forage, aes(x = log(halfsat), y = log(pop_pred), color = log(Mass_g))) + 
  geom_point(alpha = 0.25) + scale_color_viridis_c(name = 'log Prey\nMass (g)') + 
  theme_cowplot() + geom_abline(slope = 0.88, intercept = -0.74, size = 1) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = 'dashed') + 
  xlab(expression('log'~'1/ah')) + ylab(expression(log~"(1-"*I[S]*")"*N[high]*"/"*I[S]))

poppred_predsize <- ggplot(data = forage, aes(x = log(halfsat), y = log(pop_pred), color = log(PredMass_g))) + 
  geom_point(alpha = 0.25) + scale_color_viridis_c(name = 'log Predator\nMass (g)') + 
  theme_cowplot() + geom_abline(slope = 0.88, intercept = -0.74, size = 1) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = 'dashed') + 
  xlab(expression('log'~'1/ah')) + ylab(expression(log~"(1-"*I[S]*")"*N[high]*"/"*I[S]))

poppred_sizeratio <- ggplot(data = forage, aes(x = log(halfsat), y = log(pop_pred), color = log(PredMass_g/Mass_g))) + 
  geom_point(alpha = 0.25) + scale_color_viridis_c(name = 'log Predator-Prey\nMass Ratio') + 
  theme_cowplot() + geom_abline(slope = 0.88, intercept = -0.74, size = 1) + 
  geom_abline(slope = 1, intercept = 0, size = 1, linetype = 'dashed') + 
  xlab(expression('log'~'1/ah')) + ylab(expression(log~"(1-"*I[S]*")"*N[high]*"/"*I[S]))

poppred_plots <- plot_grid(poppred_preysize, poppred_predsize, poppred_sizeratio, 
                           nrow = 1, labels = 'AUTO')

save_plot('poppred_plots_sizes.png', poppred_plots, nrow = 0.75, ncol = 2, bg = 'white')



























