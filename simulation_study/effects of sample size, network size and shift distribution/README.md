## SIMULATION STUDY: effects of sample size, network and shift distribution

Simulation study is divided into 3 scenarios - increasing number of events, increasing number of nodes and increasing average shifts - to investigate how each of these factors influences the performance of our proposed method.

__DIRECTORY STRUCTURE__

- sim_code.R: simulation code script.
- wrapper_different_n.R: script for data simulation and model fitting for the scenario of increasing number of events. It calls sim_code.R and creates a subfolder for each value of n in the "results_different_n" folder, where fitted models are saved.
- wrapper_different_p.R: script for data simulation and model fitting for the scenario of increasing number of nodes. It calls sim_code.R and creates a subfolder for each value of p in the "results_different_p" folder, where fitted models are saved.
- wrapper_different_mean_shifts.R: script for data simulation and model fitting for the scenario of increasing average shift. It calls sim_code.R and creates a subfolder for each value of mean shift in the "results_different_shifts" folder, where fitted models are saved.

__DIRECTORY USAGE__

To obtain the estimates for each of the three scenarios, run the corresponding "wrapper_different_WHATEVER.R" script and move to the appropriate "results_different_WHATEVER" for plotting (see README file in these folders for details).
