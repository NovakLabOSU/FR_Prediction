####################################################################
### Predicting Functional Response Parameters
####################################################################

### load packages

library(ggplot2); library(cowplot); library(dplyr); library(lmodel2); library(ggpmisc)

### set directory to processed data folder

setwd("~/GitHub/FR_Prediction/Processed Data")

### load data

forage <- read.csv('forage_modified.csv')

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

forage <- forage %>% filter(Obs_h > 1e-6)

### calculate predicted space clearance rate

#  we will assume an energy density of prey of 5.6 kJ/g

energy_density <- 5.6

################################################################################
### predicted space clearance rate
################################################################################

forage <- forage %>% mutate(Pred_a = PredMetabolism/(PreyAbundance_10*energy_density*Mass_g),
                            Pred_h = 0.9*PreyAbundance_10*energy_density*Mass_g/(PreyAbundance_90*PredMetabolism*0.1))

### major axis regression of predicted scr on observed

ma_scr <- lmodel2(log(Pred_a) ~ log(Obs_a), data = forage)

cor(log(forage$Obs_a), log(forage$Pred_a))

### set up confidence bands for SCR plot

ci_scr <- predict(ma_scr, method = 'MA', interval = "confidence")

ci_scr <- cbind(forage$Obs_a, ci_scr)

colnames(ci_scr)[1] <- "Obs_a"

### plot of SCR

SCR_plot <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Pred_a))) + geom_point(alpha = 0.25) +
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + theme_cowplot() + geom_abline(slope = 0.845, intercept = 0.73, linewidth = 1) +
  xlab('log Observed Space\nClearance Rate') + ylab('log Predicted Space\nClearance Rate') + 
  geom_ribbon(data = ci_scr, aes(x = log(Obs_a), ymin = lwr, ymax = upr), inherit.aes = FALSE, alpha = 0.5)

### set directory to Outputs folder

setwd("~/GitHub/FR_Prediction/Outputs")

save_plot(filename = 'SCR_Prediction_Plot.png', plot = SCR_plot)

# save plot object to later put plots together

saveRDS(SCR_plot, file = 'SCR_Prediction_Plot.RData')

### Handling time

# linear model

ma_handling <- lmodel2(log(Pred_h) ~ log(Obs_h), data = forage)

cor(log(forage$Pred_h), log(forage$Obs_h))

### set up confidence bands for handling time plot

ci_handling <- predict(ma_handling, method = 'MA', interval = "confidence")

ci_handling <- cbind(forage$Obs_h, ci_handling)

colnames(ci_handling)[1] <- "Obs_h"

### make plot

h_plot <- ggplot(data = forage, aes(x = log(Obs_h), y = log(Pred_h))) + geom_point(alpha = 0.25) + 
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + theme_cowplot() +
  geom_abline(intercept = 1.04, slope = 1.15, linewidth = 1) +
  xlab('log Observed Handling Time') + ylab('log Predicted Handling Time') + 
  geom_ribbon(data = ci_handling, aes(x = log(Obs_h), ymin = lwr, ymax = upr), inherit.aes = FALSE, alpha = 0.5)

save_plot(filename = 'h_Prediction_Plot_constratio.png', plot = h_plot)

a_h_plots_comb <- plot_grid(SCR_plot, h_plot, nrow = 1)

save_plot(filename = 'ah_plot_comb.png', a_h_plots_comb, nrow = 1, ncol = 2, bg = 'white')

# save plot object to put together later into composite figure

saveRDS(h_plot, file = 'h_Prediction_Plot.RData')

### prediction of 1/ah and prey densities

cor(x = log(1/(forage$Obs_a*forage$Obs_h)), y = log(0.1*forage$PreyAbundance_90/0.9))

forage <- forage %>% mutate(half_sat = 1/(Obs_a*Obs_h), pop_pred = 0.1*PreyAbundance_90/0.9)

pop_fit <- lmodel2(log(pop_pred) ~ log(half_sat), data = forage) 

pop_fit

pop_plot <- ggplot(data = forage, aes(x = log(1/(Obs_a*Obs_h)), y = log(0.1*PreyAbundance_90/0.9))) + geom_point(alpha = 0.25) + 
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + theme_cowplot() + 
  geom_abline(intercept = -0.74, slope = 0.8815, size = 1) + 
  xlab(expression('log'~'1/ah')) + ylab(expression(log~"(1-"*I[S]*")"*N[high]*"/"*I[S]))

save_plot(filename = 'PopulationPredictionPlot.png', plot = pop_plot)

# save plot as object to be combined with predator-prey power law plot for figure 2

saveRDS(pop_plot, file = "PopSatPlot.RData")

### prediction of relationship between a and h

base_avh <- ggplot(data = forage, aes(x = log(Obs_h), y = log(Obs_a))) + geom_point() + 
  theme_cowplot() + xlab('') + ylab('')

avh_color <- ggplot(data = forage, aes(x = log(Obs_a), y = log(Obs_h), color = log(PreyAbundance_90))) + geom_point() + 
  theme_cowplot() + xlab('log Space Clearance Rate') + ylab('log Handling Time') + scale_color_viridis_c(name = expression('log N'['high'])) + geom_abline(slope = -1, intercept = log(9/exp(30)), color = '#fcf079', size = 1) + 
  geom_abline(slope = -1, intercept = log(9/exp(20)), color = '#72cc58', size = 1) + 
  geom_abline(slope = -1, intercept = log(9/exp(10)), color = '#299a87', size = 1) + 
  geom_abline(slope = -1, intercept = log(9), color = '#3a608b', size = 1) + 
  geom_abline(slope = -1, intercept = log(9/exp(-10)), color = '#461e65', size = 1)


save_plot(filename = 'avsh_plot_color.png', plot = avh_color, bg = 'white')

save_plot(filename = 'avh_baseplot.png', plot = base_avh)

### plot of predicted relationship between space clearance rates and handling times

avh_pred_plot <- ggplot(data = forage, aes(x = log(0.9/(0.1*PreyAbundance_90*Obs_h)), y = log(Obs_a), color = log(PreyAbundance_90))) + geom_point() + 
  geom_abline(intercept = 0, slope = 1, linewidth = 1, linetype = 'dashed') + scale_color_viridis_c(name = expression('log N'['high'])) + 
  theme_cowplot() + xlab(expression(log~I[S]*"/"*"(1-"*I[S]*")"*N[high]*"-"*log~h)) + 
  geom_abline(intercept = -1.63, slope = 1.09) + ylab('log Space Clearance Rate')


lmodel2(log(Obs_a) ~ log(0.9/(0.1*PreyAbundance_90*Obs_h)), data = forage)


### look at differences between (1-I_S)Nhigh/I_S and N_median

# if we look at log((1-I_S)Nhigh/I_SNmedian) 
# this is equal to log((1 - I_S)Nhigh/I_S) - log(Nmedian)

ggplot(data = forage, aes(x = (0.1/0.9)*PreyAbundance_90/PreyAbundance_50)) + 
  geom_histogram(bins = 15)

mean((0.1/0.9)*forage$PreyAbundance_90/forage$PreyAbundance_50)

sd((0.1/0.9)*forage$PreyAbundance_90/forage$PreyAbundance_50)

### put population plots together

together_plot <- plot_grid(pop_plot, avh_color, avh_pred_plot, nrow = 1, labels = 'AUTO')

save_plot(filename = 'Figure2_pop_avh.png', plot = together_plot, nrow = 1, ncol = 2, bg = 'white')

