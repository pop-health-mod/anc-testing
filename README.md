# anc-testing
Repository associated with the manuscript "Temporal trends and determinants of HIV testing at antenatal care in sub-Saharan Africa: a pooled analysis of population-based surveys (2005-2021)"

## Multivariate Regression and Post-stratification (MR-P)
Scripts 1-3 are used to gather, wrangle and fit the data to Hierarchical Logistic Regression models (MR).
Script 4 uses the fitted parameter to predict recent HIV testing on the complete population frame for all countries from 2005 to 2021, using the interpolated population calculated with the script `population_interpolation.R`
