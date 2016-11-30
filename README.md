# LikAdjPS
Included is R code to conduct simulations for “Propensity Scores with Misclassified Treatment Assignment: a Likelihood-Based Adjustment”. 

sim_param.R: script to set simulation parameters 

NoPS.R: script to generate data and perform analysis under no propensity score adjustment.

Match_PS.R: script for proposed adjustment under full-matching

Strata_PS.R: script for proposed adjustment under subclassification

Weight_PS.R: script for proposed adjustment under IPTW 

BALANCE_PS.R: script to estimate balance across different models.

MERGE.R: script to merge results across 500 replications

sens.R: script to estimate sensitivity/specificity of generated data

Plots: 

PlotsFinal.R: script to generate plot of results. 

balance_plot.R: script to plot standardized bias
