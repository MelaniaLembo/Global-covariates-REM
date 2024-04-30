## MODEL FITTING, IDENTIFIABILITY CORRECTION AND PLOTTING

__DIRECTORY STRUCTURE__

- gam_only.R: fits the GAM model.
- c_estimation.R: computes the NCC version of the breslow estimator for the residual cumulative baseline hazard.
- cluster_gam_fit_resbh.R: it calls the 2 previous scripts and saves the fitted model in a subfolder ("results_nosl60").
- plotting.R: returns the plots of all the effects included.

__DIRECTORY USAGE__

Run "cluster_gam_fit_resbh.R" to fit the GAM model and estimate the effects. Execute "plotting.R" to get the plots.
