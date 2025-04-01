################################################################################
### sensitivity analysis -- allometric scaling
################################################################################

### load packages

library(ggplot2); library(dplyr); library(cowplot);

### load data

forage <- read.csv('forage_modified.csv')

forage <- forage %>% filter(Fittted.h..day. > 1e-6)

### change some column names

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

### get empirical scalings 

# Low prey density

lowdensfit_10 <- lm(log(PreyAbundance_10) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_10))) + geom_point() + 
  geom_smooth(method = 'lm') 

lowdensfit_5 <- lm(log(PreyAbundance_5) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_5))) + geom_point() + 
  geom_smooth(method = 'lm') 

lowdensfit_30 <- lm(log(PreyAbundance_30) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_30))) + geom_point() + 
  geom_smooth(method = 'lm') 

# High Prey Density

highdensfit_90 <- lm(log(PreyAbundance_90) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_90))) + geom_point() + 
  geom_smooth(method = 'lm') 

highdensfit_70 <- lm(log(PreyAbundance_70) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_70))) + geom_point() + 
  geom_smooth(method = 'lm') 

highdensfit_95 <- lm(log(PreyAbundance_95) ~ log(Mass_g), data = forage)

ggplot(data = forage, aes(x = log(Mass_g), y = log(PreyAbundance_95))) + geom_point() + 
  geom_smooth(method = 'lm') 

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

### predictions for intercept and slope of the space clearance rate allometric scaling

### we need values for delta_0, delta, mu_0P, mu_P, eta_0low, eta_low, and E

delta_0 <- exp(coef(demandfit)[1])

delta <- coef(demandfit)[2]

mu_0P <- exp(coef(predtopreyfit)[1])

mu_P <- coef(predtopreyfit)[2]

eta_0low_5 <- exp(coef(lowdensfit_5)[1])

eta_low_5 <- coef(lowdensfit_5)[2]

eta_0low_10 <- exp(coef(lowdensfit_10)[1])

eta_low_10 <- coef(lowdensfit_10)[2]

eta_0low_30 <- exp(coef(lowdensfit_30)[1])

eta_low_30 <- coef(lowdensfit_30)[2]

E <- 5.6

### calculate intercept and slope of space clearance rate allometric scaling 
### for varying low prey density percentiles

### 5th percentile

scr_int_5 <- log(delta_0*mu_0P^(-eta_low_5-1)/(eta_0low_5*E))

scr_slope_5 <- delta+mu_P*(-eta_low_5-1)

### 10th percentile

scr_int_10 <- log(delta_0*mu_0P^(-eta_low_10-1)/(eta_0low_10*E))

scr_slope_10 <- delta+mu_P*(-eta_low_10-1)

### 30th percentile

scr_int_30 <- log(delta_0*mu_0P^(-eta_low_30-1)/(eta_0low_30*E))

scr_slope_30 <- delta+mu_P*(-eta_low_30-1)

### make plot with each of the different lines a different color

# make a data frame for the line information

scr_line_data <- data.frame(Low = factor(c('5th', '10th', '30th'), levels = c('5th', '10th', '30th')),
                            Intercept = c(scr_int_5, scr_int_10, scr_int_30),
                            Slope = c(scr_slope_5, scr_slope_10, scr_slope_30))

# observed relationship

summary(lm(formula = log(Obs_a) ~ log(PredMass_g), data = forage))

confint(lm(formula = log(Obs_a) ~ log(PredMass_g), data = forage))

scr_allometry_sensplot <- ggplot(data = forage, aes(x = log(PredMass_g), y = log(Obs_a))) + geom_point(alpha = 0.15) + 
  geom_abline(slope = 0.8, intercept = -2.14, linewidth = 1) + 
  geom_abline(data = scr_line_data, aes(slope = Slope, intercept = Intercept, color = Low), linewidth = 1, linetype = 'dashed') +
  scale_color_brewer(palette = 'Dark2') + guides(color = guide_legend(title = 'Low Prey Density\nPercentile')) + 
  theme_cowplot() + xlab('log Predator Mass (g)') + ylab('log Observed Space\nClearance Rate')


save_plot(filename = 'scr_allometry_sens_plot.png', plot = scr_allometry_sensplot, bg = 'white')

### predictions for intercept and slope of the handling time allometric relationship##Low# predictions for intercept and slope of the handling time allometric relationship
### here we will have 27 different combinations 

### beyond the parameters we already have, we also need IS, eta_0high, eta_high

IS_9 <- 0.9

IS_95 <- 0.95

IS_7 <- 0.7

eta_0high_90 <- exp(coef(highdensfit_90)[1])

eta_high_90 <- coef(highdensfit_90)[2]

eta_0high_95 <- exp(coef(highdensfit_95)[1])

eta_high_95 <- coef(highdensfit_95)[2]

eta_0high_70 <- exp(coef(highdensfit_70)[1])

eta_high_70 <- coef(highdensfit_70)[2]

### for each of the combinations can calculate an intercept and slope

### IS = 0.9, high = 90th percentile


hand_int_IS_9_high_9_low_10 <- log(IS_9*eta_0low_10*E/((1 - IS_9)*delta_0*eta_0high_90))
  
# check to see exponent for prey size scaling

1 + eta_low_10 - eta_high_90

# 0.96 -- close to 1

# as long as this scaling is true, predator mass just scales with metabolic rate
# so the slope is the same for everything

hand_int_IS_9_high_9_low_5 <- log(IS_9*eta_0low_5*E/((1 - IS_9)*delta_0*eta_0high_90))

# check exponent

1 + eta_low_5 - eta_high_90

# 0.95 -- close to 1

hand_int_IS_9_high_9_low_30 <- log(IS_9*eta_0low_30*E/((1 - IS_9)*delta_0*eta_0high_90))

# check exponent

1 + eta_low_30 - eta_high_90

# 0.97 -- close to 1

### IS = 0.9, high = 0.95

hand_int_IS_9_high_95_low_5 <- log(IS_9*eta_0low_5*E/((1 - IS_9)*delta_0*eta_0high_95))

# check exponent

1 + eta_low_5 - eta_high_95

# 0.94 -- close to 1

hand_int_IS_9_high_95_low_10 <- log(IS_9*eta_0low_10*E/((1 - IS_9)*delta_0*eta_0high_95))

# check exponent

1 + eta_low_10 - eta_high_95

# 0.95 -- close to 1

hand_int_IS_9_high_95_low_30 <- log(IS_9*eta_0low_30*E/((1 - IS_9)*delta_0*eta_0high_95))

# check exponent

1 + eta_low_30 - eta_high_95

# 0.96 -- close to 1

### IS = 0.9, high = 70th percentile

hand_int_IS_9_high_70_low_5 <- log(IS_9*eta_0low_5*E/((1 - IS_9)*delta_0*eta_0high_70))

# check exponent

1 + eta_low_5 - eta_high_70

# 0.96 -- close to 1

hand_int_IS_9_high_70_low_10 <- log(IS_9*eta_0low_10*E/((1 - IS_9)*delta_0*eta_0high_70))

# check exponent

1 + eta_low_10 - eta_high_70

# 0.97 -- close to 1

hand_int_IS_9_high_70_low_30 <- log(IS_9*eta_0low_30*E/((1 - IS_9)*delta_0*eta_0high_70))

# check exponent

1 + eta_low_30 - eta_high_70

# 0.98 -- close to 1

# should have all of the combinations of slopes now meaning that all of the prey scalings
# are near one and justify our approximation using the prey-mass-specific handling time


### IS = 0.95, high = 95th percentile

hand_int_IS_95_high_95_low_5 <- log(IS_95*eta_0low_5*E/((1 - IS_95)*delta_0*eta_0high_95))

hand_int_IS_95_high_95_low_10 <- log(IS_95*eta_0low_10*E/((1 - IS_95)*delta_0*eta_0high_95))

hand_int_IS_95_high_95_low_30 <- log(IS_95*eta_0low_30*E/((1 - IS_95)*delta_0*eta_0high_95))

### IS = 0.95, high = 90th percentile

hand_int_IS_95_high_90_low_5 <- log(IS_95*eta_0low_5*E/((1 - IS_95)*delta_0*eta_0high_90))

hand_int_IS_95_high_90_low_10 <- log(IS_95*eta_0low_10*E/((1 - IS_95)*delta_0*eta_0high_90))

hand_int_IS_95_high_90_low_30 <- log(IS_95*eta_0low_30*E/((1 - IS_95)*delta_0*eta_0high_90))

### IS = 0.95, high = 70th percentile

hand_int_IS_95_high_70_low_5 <- log(IS_95*eta_0low_5*E/((1 - IS_95)*delta_0*eta_0high_70))

hand_int_IS_95_high_70_low_10 <- log(IS_95*eta_0low_10*E/((1 - IS_95)*delta_0*eta_0high_70))

hand_int_IS_95_high_70_low_30 <- log(IS_95*eta_0low_30*E/((1 - IS_95)*delta_0*eta_0high_70))

### IS = 0.7, high = 95th percentile

hand_int_IS_70_high_95_low_5 <- log(IS_7*eta_0low_5*E/((1 - IS_7)*delta_0*eta_0high_95))

hand_int_IS_70_high_95_low_10 <- log(IS_7*eta_0low_10*E/((1 - IS_7)*delta_0*eta_0high_95))

hand_int_IS_70_high_95_low_30 <- log(IS_7*eta_0low_30*E/((1 - IS_7)*delta_0*eta_0high_95))

### IS = 0.7, high = 90th percentile

hand_int_IS_70_high_90_low_5 <- log(IS_7*eta_0low_5*E/((1 - IS_7)*delta_0*eta_0high_90))

hand_int_IS_70_high_90_low_10 <- log(IS_7*eta_0low_10*E/((1 - IS_7)*delta_0*eta_0high_90))

hand_int_IS_70_high_90_low_30 <- log(IS_7*eta_0low_30*E/((1 - IS_7)*delta_0*eta_0high_90))

### IS = 0.7, high = 70th percentile

hand_int_IS_70_high_70_low_5 <- log(IS_7*eta_0low_5*E/((1 - IS_7)*delta_0*eta_0high_70))

hand_int_IS_70_high_70_low_10 <- log(IS_7*eta_0low_10*E/((1 - IS_7)*delta_0*eta_0high_70))

hand_int_IS_70_high_70_low_30 <- log(IS_7*eta_0low_30*E/((1 - IS_7)*delta_0*eta_0high_70))

### that should be all of the combinations 

### want to make a plot that has lines for each combination. Similar to sensitivity 

# create a data frame to store the intercept data for the lines

hand_line_data <- data.frame(IS = rep(c('0.7','0.9','0.95'), each = 9),
                             Nhigh = rep(rep(c('70th','90th','95th'), each = 3), times = 3),
                             Nlow = rep(c('5th', '10th', '30th'), times = 9))

# add slope of line which is the same for all of them

hand_line_data$Slope <- -delta

# add intercept data

hand_line_data$Intercept <- c(hand_int_IS_70_high_70_low_5, hand_int_IS_70_high_70_low_10,
                              hand_int_IS_70_high_70_low_30, hand_int_IS_70_high_90_low_5,
                              hand_int_IS_70_high_90_low_10, hand_int_IS_70_high_90_low_30,
                              hand_int_IS_70_high_95_low_5, hand_int_IS_70_high_95_low_10,
                              hand_int_IS_70_high_95_low_30, hand_int_IS_9_high_70_low_5,
                              hand_int_IS_9_high_70_low_10, hand_int_IS_9_high_70_low_30,
                              hand_int_IS_95_high_90_low_5, hand_int_IS_9_high_9_low_10,
                              hand_int_IS_9_high_9_low_30, hand_int_IS_9_high_95_low_5,
                              hand_int_IS_9_high_95_low_10, hand_int_IS_9_high_95_low_30,
                              hand_int_IS_95_high_70_low_5, hand_int_IS_95_high_70_low_10,
                              hand_int_IS_95_high_70_low_30, hand_int_IS_95_high_90_low_5,
                              hand_int_IS_95_high_90_low_10, hand_int_IS_95_high_90_low_30,
                              hand_int_IS_95_high_95_low_5, hand_int_IS_95_high_95_low_10,
                              hand_int_IS_95_high_95_low_30)

# observed relationship

summary(lm(log(Obs_h/Mass_g) ~ log(PredMass_g), data = forage))

confint(lm(log(Obs_h/Mass_g) ~ log(PredMass_g), data = forage))

# expand forage such that there is a copy for each of the combinations of IS, Nhigh, and Nlow

forage_sens_exp <- data.frame(Obs_h = rep(forage$Obs_h, times = 27),
                              Mass_g = rep(forage$Mass_g, times = 27),
                              PredMass_g = rep(forage$PredMass_g, times = 27))


forage_sens_exp$IS <- rep(hand_line_data$IS, each = 2162)

forage_sens_exp$Nhigh <- rep(hand_line_data$Nhigh, each = 2162)

forage_sens_exp$Nlow <- rep(hand_line_data$Nlow, each = 2162)

# labels for the facets

Nlow_names <- list(
  '5th'=expression(N[low]*'='*5^th~percentile),
  '10th'=expression(N[low]*'='*10^th~percentile),
  '30th'=expression(N[low]*'='*30^th~percentile)
)

Nhigh_names = list(
  '70th'=expression(N[high]*'='*70^th~percentile),
  '90th'=expression(N[high]*'='*90^th~percentile),
  '95th'=expression(N[high]*'='*95^th~percentile)
)

facet_labeller <- function(variable,value){
  if(variable == "Nlow"){
    return(Nlow_names[value])
  } else {
    return(Nhigh_names[value])
  }
}

# make plots 

forage_sens_exp$Nlow <- factor(forage_sens_exp$Nlow, levels = c('5th', '10th', '30th'))

handling_allometry_sensplot <- ggplot(data = forage_sens_exp, aes(x = log(PredMass_g), y = log(Obs_h/Mass_g))) + geom_point(alpha = 0.05) + 
  geom_abline(slope = -0.84616, intercept = 0.243, linewidth = 1) + 
  facet_grid(rows = Nlow ~ Nhigh, labeller = facet_labeller) + 
  geom_abline(data = hand_line_data, aes(slope = Slope, intercept = Intercept, color = IS), linetype = 'dashed', linewidth = 1) + 
  theme_cowplot() + scale_color_brewer(palette = "Dark2") + guides(color = guide_legend(title = expression(I[S]))) + 
  xlab('log Predator Mass (g)') + ylab('log Handling Time/Prey Mass')

save_plot(filename = 'handling_allometry_sensplot.png', plot = handling_allometry_sensplot, nrow = 1.5, ncol = 1.5, bg = 'white')














































































