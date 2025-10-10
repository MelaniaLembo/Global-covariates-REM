## MODEL FITTING AND PLOTTING

__DIRECTORY STRUCTURE__

- gam_only.R: fits the GAM model.
- c_estimation.R: computes the NCC version of the breslow estimator for the residual cumulative baseline hazard.
- cluster_gam_fit_resbh.R: it calls the 2 previous scripts and saves the fitted model in a subfolder ("results_nosl60").
- gam_only_noglobal.R: fits the GAM model without global covariates.
- plotting.R: returns the plots of all the effects included.
- plotting_same_shift_mag_diff_ne_samp.R: returns the plots of all the effects for a fixed shift magnitiutde and different seeds for the non-event sampling.
- plotting_diff_shift_mag.R: returns the plots of all the effects for diffeent shift magnitiutdes.
- plotting_no_global.R: returns the plots of all the effects included in the analysis performed using traditional partial likelihood without including global covariates.

__DIRECTORY USAGE__

Run "cluster_gam_fit_resbh.R" to fit the GAM model and estimate the effects (execution time is approx. 50 mins). Execute "plotting.R" to get the plots. 
Variations to this alowign for the analysis without global coavariates or the robustenss check to non-event sampling and shifts magnitudes are possible: 
- analysis without global coavariates: in "cluster_gam_fit_resbh.R" replace "gam_only.R" with "gam_only_noglobal.R". Execute "plotting_noglobal.R" to get the plots.
- robustness checks to non-event sampling and shifts magnitudes: run "cluster_gam_fit_resbh.R" on the relevant event/non-events datasets obtained from the correspoinding seeds/shift magnitudes. Execute "plotting_same_shift_mag_diff_ne_samp.R" and/or "plotting_diff_shift_mag.R" to obtain the plots.
