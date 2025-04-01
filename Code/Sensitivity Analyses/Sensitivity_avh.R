################################################################################
### sensitivity analysis -- a versus h
################################################################################

### load libaries

library(dplyr); library(ggplot2); library(cowplot); library(RColorBrewer); library(forcats); library(lmodel2)

### load data

forage <- read.csv('forage_modified.csv')

forage <- forage %>% filter(Fittted.h..day. > 1e-6)

### assume an energy density of 5.6 kJ/g of prey

energy_density <- 5.6

### change some column names

colnames(forage)[c(43,46)] <- c('Obs_a', 'Obs_h')

### avh relationship potentially sensitive to changes in Nhigh percentile
### and the value of IS

### set up a data frame to store the necessary data for this

sens_data <- data.frame(Obs_a = rep(forage$Obs_a, times = 9),
                        Obs_h = rep(forage$Obs_h, times = 9))

I_S <- c(0.7, 0.9, 0.95)

Nhigh <- c('70', '90', '95')

name_vec <- vector(length = 9)

I_S <- rep(I_S, each = 3)

Nhigh <- rep(Nhigh, times = 3)

name_vec <- paste(rep('IS', times = 9),
                  I_S,
                  rep('Nhigh', times = 9),
                  Nhigh, sep = '_')

sens_data$Combination <- rep(name_vec, each = 2162) 

sens_data$I_S <- rep(rep(c(0.7, 0.9, 0.95), each = 3), each = 2162)

sens_data$Nhigh <- rep(c(forage$PreyAbundance_70, 
                         forage$PreyAbundance_90,
                         forage$PreyAbundance_95), times = 3)

sens_data$h_mod <- log(I_S/((1 - I_S)*sens_data$Nhigh*sens_data$Obs_h)) 

sens_data$Nhigh_factor <- rep(rep(c('70', '90', '95'), times = 3), each = 2162)


sens_data$I_S <- as.factor(sens_data$I_S)

sens_data$Nhigh_factor <- as.factor(sens_data$Nhigh_factor)

### setup for major axis regressions and facet labels

Nhigh_names = list(
  '70'=expression(N[high]*'='*70^th~percentile),
  '90'=expression(N[high]*'='*90^th~percentile),
  '95'=expression(N[high]*'='*95^th~percentile)
)

IS_names <- list(
  '0.7' = expression(I[S]*'='*0.7),
  '0.9' = expression(I[S]*'='*0.9),
  '0.95' = expression(I[S]*'='*0.95)
)

facet_labeller <- function(variable,value){
  if(variable == "Nhigh_factor"){
    return(Nhigh_names[value])
  } else {
    return(IS_names[value])
  }
}

### major axis regressions

mareg <- sens_data %>% group_by(I_S, Nhigh_factor) %>% dplyr::summarise(Intercept = lmodel2(log(Obs_a) ~ h_mod)$regression.results$Intercept[2],
                                                                        Slope = lmodel2(log(Obs_a) ~ h_mod)$regression.results$Slope[2],
                                                                        Int_l95 = lmodel2(log(Obs_a) ~ h_mod)$confidence.intervals$`2.5%-Intercept`[2],
                                                                        Int_u95 = lmodel2(log(Obs_a) ~ h_mod)$confidence.intervals$`97.5%-Intercept`[2],
                                                                        Slope_l95 = lmodel2(log(Obs_a) ~ h_mod)$confidence.intervals$`2.5%-Slope`[2],
                                                                        Slope_u95 = lmodel2(log(Obs_a) ~ h_mod)$confidence.intervals$`97.5%-Slope`[2])


### make plot

avh_sens_plot <- ggplot(data = sens_data, aes(x = h_mod, y = log(Obs_a))) + 
  facet_grid(rows = I_S ~ Nhigh_factor, labeller = facet_labeller) + geom_point(alpha = 0.25) +
  geom_abline(slope = 1, intercept = 0, linetype = 'dashed', size = 1) + 
  geom_abline(data = mareg, aes(slope = Slope, intercept = Intercept), size = 1) + 
  theme_cowplot() + 
  xlab(expression(log~I[S]*"/"*"(1-"*I[S]*")"*N[high]*"-"*log~h)) + ylab("log Space Clearance Rate")

save_plot(filename = 'avh_sens_plot.png', plot = avh_sens_plot, bg = 'white', ncol = 1.5, nrow = 1.5)

### get correlations

sens_data %>% group_by(I_S, Nhigh_factor) %>% dplyr::summarise(correlation = cor(x = h_mod, y = log(Obs_a)))


































