## COVARIATES COMPUTATION AND NESTED CASE-CONTROL SAMPLING

To speed it up, the code in this folder is set up to be run in parallel.

__DIRECTORY STRUCTURE__

- end_var_comp_parallel.R: compute all the endogenous variables.
- ncc_sampling parallel.R: performs nested case-control (NCC) sampling on the shifted process and computes covariates for non-events.
- ncc_samp_resbh.R: performs nested case-control sampling on the original process. These non-events are used to compute the NCC version of the Breslow estimator for the residual cumulative baseline hazard.
- cluster_end_var_ncc.R: loads all the necessary intermediate data structures obtained from "data_pre_processing" folder and calls the above mentioned 3 scripts. It creates a subfolder, "datasets_nosl60", in which it saves the time, nodes and covariate values for the events and sampled non-events.

__DIRECTORY USAGE__

Run "cluster_end_var_ncc.R". It should be noted that computing the endogenous variables and sampling non-events are computationally intensive operations as the require looping over the entire dataset of events. Even with the parallel implementation over 4 cores, the execution still takes hours.
