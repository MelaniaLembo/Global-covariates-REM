## A SIMULATED BIKE-SHARING RELATED EXAMPLE

This folder contains the code for the analysis of the simulated example presented in Section 1.5. of the Supplementary materials. We consider a subset of the bike stations, analyzed in the "bike_data_analysis" folder
and simulated relational events between them over a period of two days according to particular intensity processes that involve a smooth effect of temperature and a fixed negative effect for the distance.

__DIRECTORY STRUCTURE__

- station_info.RData: contains info on the 20 bike stations used to simulate the data.
- bike_data.sim.R: simulates the relational events between the stations in "station_info.RData" (100 replications) and performs shifting and nested case-sampling on the shifted process. It then fits the GAM model and, after having corrected for the identifiability constraint using the Breslow estimator, it plots the estimated effects.

__DIRECTORY USAGE__

Run "bike_data.sim.R".
