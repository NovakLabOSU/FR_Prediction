################################################################################
### Allometric scaling predictions
################################################################################

### load packages

library(ggplot2); library(dplyr); library(cowplot); 

### set directory to Processed Data Folder

# setwd("~/GitHub/FR_Prediction/Processed Data")

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

### scaling plots for manuscript

scr_lines <- data.frame(slopes = c(0.8, 0.82), intercepts = c(-2.14, -0.42), lines = c('Observed', 'Predicted'))

scr_plot <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Observed Space\nClearance Rate') + 
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = scr_lines, linewidth = 1) + 
  labs(linetype = 'Line')
  
handling_lines <- data.frame(slopes = c(-0.85, -0.86), intercepts = c(0.24, 0.11), lines = c('Observed', 'Predicted'))

handling_plot <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g))) + geom_point(alpha = 0.25) + 
  theme_cowplot() + xlab('log Predator Mass') + ylab('log Handling Time/Prey Mass') +
  geom_abline(aes(slope = slopes, intercept = intercepts, linetype = lines), data = handling_lines, linewidth = 1) + 
  labs(linetype = 'Line')

### put plots together

together <- plot_grid(scr_plot, handling_plot, nrow = 1, labels = 'AUTO', axis = 'tblr', align = 'hv')

### set directory to Outputs folder

#setwd("~/GitHub/FR_Prediction/Outputs")

save_plot(filename = 'allometry_plot.png', plot = together, ncol = 2, nrow = 1, bg = 'white', base_asp = 1.4)





