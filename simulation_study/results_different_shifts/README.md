## EFFECT OF SHIFT DISTRIBUTION

__DIRECTORY STRUCTURE__

- predicting.R: script that computes values of each term of the logit model using estimated coeffiecients and saves them in the appropriate subfolder.
- plotting.R: script that creates all the figures included in both the paper and Supplementary material for this simulation scenario. It creates a "final_plots" where it saves all figures in .pdf format

__DIRECTORY USAGE__
  
After having run "wrapper_different_mean_shifts.R" in the parent folder, "mf_SOMENUMBER" subfolders are created in this folder, where fitted models are saved. To obtain the plots, first run "predicting.R" and then "plotting.R".
