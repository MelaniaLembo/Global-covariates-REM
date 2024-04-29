# Relational event models with global covariates

This repository contains the code used for the simulation study and empirical application of the paper 
_Relational event models with global covariates_.

Traditional inferential techniques for Relational Event Models,
involving Cox’s partial likelihood, can estimate the effects of covariates that are
node-specific, such as age or in-degree, or dyadic, such age difference of pairs of
nodes or reciprocity. However, the partial likelihood cannot account for global
covariates, i.e., factors that are constant for all pairs. Nevertheless,
these covariates, such as weather or time of the day, are often important in capturing and explaining the temporal nature of the studied events. This paper
addresses this challenge with the use of nested case-control sampling on a time-shifted version of the event process. This will result in a partial likelihood of a
degenerate logistic generalized additive model from which we are able to recover
effects of all kinds of covariates, including global ones.


__DIRECTORY STRUCTURE__

-simulation_study: contains scripts for the data simulation, model fitting and plotting for the simulation study in Section 4 of the paper. 
-bike_data_analysis: contains the scripts regarding data pre-processing, model fitting and plotting for the analysis of bike shares in Washington D.C presented in Section 5 of the paper
-bike_data_sim: contains scripts for a simulated realistic example in the context of bike shares (included in the Supplementary materials of this paper).

