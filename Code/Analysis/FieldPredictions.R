##################################################################
### Predicting FR parameters for particular systems
##################################################################

### load packages

library(dplyr); library(ggplot2); library(cowplot); library(lmodel2)

### load data

data <- read.csv('FieldPredatorPrey.csv')

### load forage to get measured a and h values

forage <- read.csv('forage_modified.csv')

colnames(forage)[1] <- 'Dataset'

### get only overlapping studies from forage

forage <- forage %>% filter(Dataset %in% data$Dataset)

### drop most variables from forage

### keep scr and h estimates

forage <- forage %>% select(Dataset, Fitted.a..median.of.BS.samples.,
                            CI.a.low, CI.a.hi, Fittted.h..day.,
                            CI.h.low, CI.h.hi)

### change names of variables in forage

colnames(forage)[2:7] <- c('Fitted_a', 'a_low', 'a_high',
                           'Fitted_h', 'h_low', 'h_high')

### combine forage and data

data <- left_join(data, forage, by = 'Dataset')

### now we need to get the predicted a's and h's from the theory

data <- data %>% mutate(Predicted_a = PredatorMetabolicRate/(PreySize_g * kJpergValue * LowPreyDensity),
                        Predicted_h = (0.9*LowPreyDensity* kJpergValue*PreySize_g)/(PredatorMetabolicRate*HighPreyDensity*(1-0.9)))

plot(log10(data$Fitted_a), log10(data$Predicted_a))
abline(a = 0, b = 1)

plot(log10(data$Fitted_h), log10(data$Predicted_h))
abline(a = 0, b = 1)

### look at each prediction in more detail

### want to make plots that have the estimates of SCR's and 
### and handling times and compare them to the predictions 
### for the DEE and Basal metabolisms. 

### Field predictions of SCR's and Handling times

field_a <- ggplot(data = data, aes(x = log(Fitted_a), y = log(Predicted_a))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + theme_cowplot() + 
  xlab('Observed\n ln Space Clearance Rate') + ylab('Predicted\n ln Space Clearance Rate')

field_h <- ggplot(data = data, aes(x = log(Fitted_h), y = log(Predicted_h))) + 
  geom_point() + 
  geom_abline(slope = 1, intercept = 0) + theme_cowplot() +
  xlab('Observed\n ln Handling Time') + ylab('Predicted\n ln Handling Time')

field_pred <- plot_grid(field_a, field_h, nrow = 1)

save_plot(filename = 'FieldPredictions.png', plot = field_pred,
          nrow = 1, ncol = 2, bg = 'white')

### plots for talk

# regressions 

Field_Attack_Rate <- ggplot(data = data, aes(x = log(Fitted_a), y = log(Predicted_a), color = System, shape = System)) + 
  geom_point(size = 3) + xlab('log Observed Space\nClearance Rate') + ylab('log Predicted Space\nClearance Rate') + theme_cowplot() + geom_abline(slope = 1, intercept = 0, linetype = 'dashed', linewidth = 1) + 
  scale_color_brewer(palette = 'Dark2') 

legend <- get_legend(Field_Attack_Rate)

Field_Attack_Rate <- Field_Attack_Rate + theme(legend.position = 'none')


Field_Handling_Time <- ggplot(data = data, aes(x = log(Fitted_h), y = log(Predicted_h), color = System, shape = System)) + 
  geom_point(size = 3) + xlab('log Observed Handling Time') + ylab('log Predicted Handling Time') + theme_cowplot() + geom_abline(slope = 1, intercept = 0, linetype = 'dashed', linewidth = 1) + 
  scale_color_brewer(palette = 'Dark2') 

Field_Handling_Time <- Field_Handling_Time + theme(legend.position = 'none')


save_plot(filename = 'FieldAttackPlot.png', plot = Field_Attack_Rate, bg = 'white')

save_plot(filename = 'FieldHandlingPlot.png', plot = Field_Handling_Time, bg = 'white')

Field_Predictions <- plot_grid(Field_Attack_Rate, Field_Handling_Time, legend, nrow = 1, rel_widths = c(1,1,0.5), axis = 'tblr', align = 'hv')

save_plot(filename = 'FieldPredictions.png', plot = Field_Predictions, bg = 'white', ncol = 2.1)

### save plots and legend to put together for figure 1

saveRDS(Field_Attack_Rate, file = 'FieldAttackRatePlot.RData')

saveRDS(Field_Handling_Time, file = 'FieldHandlingPlot.RData')

saveRDS(legend, file = 'FieldPlotLegend.RData')
