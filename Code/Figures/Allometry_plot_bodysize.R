################################################################################
### Supplementary plots with body size for allometric scaling relationships
################################################################################

library(dplyr); library(ggplot2); library(cowplot); 

### load data

forage <- read.csv('forage_modified.csv')

forage <- forage %>% filter(Fittted.h..day. > 1e-6)

### change some column names

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

### get empirical scalings 

# Low prey density

lowdensfit <- lm(log(PreyAbundance_10) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_10))) + geom_point() + 
  geom_smooth(method = 'lm') 

# High Prey Density

highdensfit <- lm(log(PreyAbundance_90) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_90), color = PreyAbundanceGroup)) + geom_point()

# Predator Metabolism

demandfit <- lm(log(PredMetabolism) ~ log(PredMass_g), data = forage)

ggplot(data = forage, aes(x = log(PredMass_g), y = log(PredMetabolism), color = PredMetabolismGroup)) + geom_point()

# Prey and Predator Masses

predtopreyfit <- lm(log(Mass_g) ~ log(PredMass_g), data = forage) 

ggplot(data = forage, aes(x = log(PredMass_g), y = log(Mass_g))) + geom_point() + 
  geom_smooth(method = 'lm') 

preytopredfit <- lm(log(PredMass_g) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PredMass_g))) + geom_point() + 
  geom_smooth(method = 'lm')

### Space Clearance rate

summary(lm(log(Obs_a) ~ log(PredMass_g), data = forage))

confint(lm(log(Obs_a) ~ log(PredMass_g), data = forage))

ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a))) + geom_point( ) + 
  geom_smooth(method = 'lm')

lm(log(Obs_a) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(Obs_a))) + geom_point() + 
  geom_smooth(method = 'lm')

### Handling Time         

ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g))) + geom_point()

lm(log(Obs_h/Mass_g) ~ log(PredMass_g), data = forage)

confint(lm(log(Obs_h/Mass_g) ~ log(PredMass_g), data = forage))

### plots with prey mass and predator-prey mass ratio 

scr_plot_preysize <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a), color = log(Mass_g))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Observed Space\nClearance Rate') + 
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = scr_lines, linewidth = 1) + 
  labs(linetype = 'Line') + scale_color_viridis_c(name = 'log Prey Mass (g)')


scr_plot_sizeratio <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a), color = log(PredMass_g/Mass_g))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Observed Space\nClearance Rate') + 
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = scr_lines, linewidth = 1) + 
  labs(linetype = 'Line') + scale_color_viridis_c(name = 'log Predator-Prey\nMass Ratio')

### put plots together and save

scr_allometry_sizeplots <- plot_grid(scr_plot_preysize, scr_plot_sizeratio,
                                     nrow = 1, labels = 'AUTO')

save_plot(filename = 'scr_allometry_sizeplots.png', plot = scr_allometry_sizeplots,
          nrow = 1, ncol = 2, bg = 'white')


### handling time plots

handling_lines <- data.frame(slopes = c(-0.85, -0.86), intercepts = c(0.24, 0.11), lines = c('Observed', 'Predicted'))

handling_allometry_preysize <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g), color = log(Mass_g))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Handling Time/Prey Mass') +
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = handling_lines, linewidth = 1) + 
  labs(linetype = 'Line') + scale_color_viridis_c(name = 'log Prey Mass (g)')

handling_allometry_sizeratio <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g), color = log(PredMass_g/Mass_g))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Handling Time/Prey Mass') +
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = handling_lines, linewidth = 1) + 
  labs(linetype = 'Line') + scale_color_viridis_c(name = 'log Predator-Prey\nMass Ratio')

### save plots

handling_size_plots <- plot_grid(handling_allometry_preysize, handling_allometry_sizeratio,
                                 nrow = 1, labels = 'AUTO')

save_plot(filename = 'handling_allometry_size_plots.png', plot = handling_size_plots,
          nrow = 1, ncol = 2, bg = 'white')









