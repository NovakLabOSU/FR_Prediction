# FR_Prediction

This repository contains the code and data for Coblentz et al. 'Simple, universal rules predict trophic interaction strengths.'

The repository is structured into several folders. Below each of the folders and subfolders is given along with a description of the code contained in the folders.

## Code

### Analysis

AllometryPredictions.R -- This R file performs the regressions necessary to assess the theory's predicted allometric scaling relationships of the functional response parameters and then assesses the correspondence between the predicted and observed allometric relationships.

FieldPredictions.R -- This R file uses the FieldPredatorPrey.csv file to make space clearance rate and handling time predictions for the field studies. This file produces FieldHandlingPlot.RData, FieldAttackRatePlot.RData, and FieldPlotLegend.RData which are used by Figure1.R to produce Figure 1.

Predicting_FR_Parameters.R -- This R file does the bulk of the analyses that are presented in the main text. That is, this file predicts the values of the space clearance rates and handling times of each study, examines the predicted relationship between the half-saturation constant and high prey abundances, and examines the predicted relationship between the space clearance rates and handling times. This file also produces SCR_Prediction_Plot.RData and h_Prediction_Plot.RData that are used by Figure1.R to create Figure 1 of the manuscript.

### Data Setup

DataManipulation.R -- This R file takes the FoRAGE_db_V4_Sept_17_2023_mod.csv file and uses the metabolic scaling and mass-abundance regressions to predict predator metabolic rates and prey densities for each study in FoRAGE. The code also excludes studies as outlined in the methods of the corresponding paper. The final dataset that is used in most of the analyses is output as forage_modified.csv. 

MassAbundance_BayesianReg.R -- This R file contains the code to perform Bayesian regression analyses to separately determine the mass-abundance scaling relationships for mammals, protists, invertebrate ectotherms, vertebrate ectotherms, birds, and prokaryotes. This file also produces the RData file 'MassAbundanceScaling.RData.' This code requires cmdstan to perform the Bayesian analyses.

Metabolism_BayesianReg.R -- This R file contains the code to perform Bayesian regression analyses to separately determine metabolic scaling relationships for mammals, protists, invertebrates, vertebrate ectotherms, birds, and prokaryotes. The file also produces the RData file 'Metabolism_RData.RData'. This code requires cmdstan to perform the Bayesian analyses.

### Figures

Allometry_plot_bodysize.R -- This R file creates Figures S4.4 and S4.5

Figure1.R -- This R file contains the code to produce Figure 1 of the main text of the associated manuscript using plots created in the Predicting_FR_Parameters.R and FieldPredictions.R files and saved as RData files.

SuppPlots_sizecolor.R -- This R file contains the code to create Figures S4.1-S4.3.

### Sensitivity Analysis

Sensitivity_Analysis_AllometricScalings.R -- This R file performs a sensitivity analysis to examine the effects of different assumptions for values of I_S and the percentiles used to estimate N_{high} and N_{low} on the predictions of the allometric scaling of the space clearance rates and handling times.

Sensitivity_avh.R -- This R file performs a sensitivity analysis to examine the effects of different assumptions for values of I_S and the percentiles used to estimate N_{high} and N_{low} on the predictions of the relationship between space clearance rates and handling times.

Sensitivity_popsize_halfsaturation.R -- This R file performs a sensitivity analysis to examine the effects of different assumptions for values of I_S and the percentiles used to estimate N_{high} and N_{low} on the predictions of the relationship high prey densities and the half-saturation constant of the functional response.

Sensitivity_Analysis_Predictions.R -- This R file performs a sensitivity analysis to examine the effects of different assumptions for values of I_S and the percentiles used to estimate N_{high} and N_{low} on the predictions of space clearance rates and handling times.

## Outputs

FieldAttackRatePlot.RData -- RData containing a plot of observed versus predicted space clearance rates for field data. This file is produced by FieldPredictions.R and used by Figure1.R

FieldHandlingPlot.RData --  RData containing a plot of observed versus predicted handling times for field data. This file is produced by FieldPredictions.R and used by Figure1.R

FieldPlotLegend.RData -- RData containing the legend for the plot for field study predictions. This file is produced by FieldPredictions.R and used by Figure1.R

h_Prediction_Plot.RData -- RData containing a plot of the observed and predicted handling times across all of the data in FoRAGE. This file is produced by Predicting_FR_Parameters.R and is used by Figure1.R

MassAbundanceScaling.RData -- RData containing the Bayesian regression output for the mass abundance regressions. This file is produced by MassAbundance_BayesianReg.R and used by DataManipulation.R.

Metabolism_RData.RData -- RData containing the Bayesian regression output for the mass metabolism regressions. This file is produced by Metabolism_BayesianReg.R and used by DataManipulation.R

SCR_Prediction_Plot.RData -- RData containing a plot of the observed and predicting handling times across all of the data in FoRAGE. This file is produced by Predicting_FR_Parameters.R and is used by Figure1.R

## Processed Data

forage_modified.csv -- This csv file is created by DataManipulation.R and contains the FoRAGE database along with the predicted abundances of prey and metabolic rates of predators.

## Raw Data

AbundanceScaling.csv -- This csv file contains data from Hatton et al. (2019) on the abundances and masses of 5,985 organisms.

Metabolism.csv -- This csv file contains data from Hatton et al. (2019) on the metabolism and masses of 7,343 organisms.

FoRAGE_db_V4_Sept_27_2023_mod.csv -- This csv file contains the FoRAGE database of functional response parameters and salient covariates from the associated studies. This version has been modified to have an 'Include' column in which zeros have been placed for studies that are excluded from the analysis for having non-living prey or eggs as prey.

FieldPredatorPrey.csv -- This csv file contains system-specific estimates of the parameters necessary in the theory to predict space clearance rates and handling times for a subset of studies in FoRAGE that are field studies for which the required information could be found and which met the criteria for inclusion laid out in the methods section of the associated paper. 

### References

Hatton, I. et al. 2019. Linking scaling laws across eukaryotes. PNAS. 116:21616-21622.





