## BIKE SHARING IN WASHINGTON D.C.

Using the proposed approach, we investigate the effects that weather variables, like temperature
and precipitation, which are global, in addition to any other node/edge-specific
covariates, such as distance between stations or repetition, have on the rate of
bike shares in D.C. occurring in the time period July 9th-31st, 2023. Weather information was taken from https://www.wunderground.com/ and distances between
stations, measured in biking minutes, were obtained from Open Street Maps
using the R package osrm. 

__DIRECTORY STRUCTURE__

- raw_data: contains the weather raw data.
- data_pre_processing:  contains the scripts to download bike sharing data, performs all the pre-processing steps described in the Supplementary material and computes distances between bike statons.
- events_ncc_gam_ncc_resbh: contains the scripts to compute the endogenous variables, performs nested case-control sampling on the shifted process and the same sampling scheme on the original process (the latter used to compute the Breslow estimator of the residual cumulative baseline hazard).
- gam_resbh_estimation: contains the scripts to fit the GAM model, computes the Breslow estimator for the identifiability correction and plotting code to obtain the plots.

__DIRECTORY USAGE__

Start with the "data_pre_processing" folder to get the data in shape for model fitting. Then move to "events_ncc_gam_ncc_resbh" to sample non-events in the shifted process. Finally use the scripts in the "gam_resbh_estimation" to fit the GAM model and create the plots. Details on how to perform the appropriate steps in each of these folders are given in a README file in each of them.
