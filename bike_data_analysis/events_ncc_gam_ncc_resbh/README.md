## COVARIATES COMPUTATION AND NESTED CASE-CONTROL SAMPLING

To speed it up, the code in this folder is set up to be run in parallel.

__DIRECTORY STRUCTURE__

- end_var_comp_parallel.R: compute all the endogenous variables.
- ncc_sampling parallel.R: performs nested case-control sampling on the shifted process and computes covariates for non-events.
- ncc_samp_resbh.R: performs nested case-control sampling on the original process. These non-events are used to compute the NCC version of the Breslow estimator necessary correct for the identifiability constraint.
- cluster_end_var_ncc.R: loads all the necessary intermediate data structures obtained from "data_pre_processing" folder and calls the above mentioned 3 scripts. It creates a subfolder, "datasets_nosl60", in which it saves the time, nodes and covariate values for the events and sampled non-events.

__DIRECTORY USAGE__

Run "cluster_end_var_ncc.R". 
