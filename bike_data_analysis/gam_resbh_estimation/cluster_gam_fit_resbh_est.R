# packages required
library(mgcv)

# initial data necessary
load(file = "../data_pre_processing/initial_data/info_nosl60.RData")
load(file = "../data_pre_processing/initial_data/merged_data_new_nosl60.RData")
load(file = "../data_pre_processing/initial_data/starting_timepoint.RData")

new_subfolder<-"results_nosl60"
dir.create(new_subfolder)

seeds<-c(31)
for(i in seeds){

  cat("seed", i, "\n")
  
  load(file = paste("../events_ncc_gam_ncc_resbh/datasets_nosl60/events_",i,".RData", sep =""))
  load(file = paste("../events_ncc_gam_ncc_resbh/datasets_nosl60/nonevents_",i,".RData", sep = ""))
  load(file = paste("../events_ncc_gam_ncc_resbh/datasets_nosl60/nonevents_resbh_",i,".RData", sep = ""))

  # ------------------------ GAM model fitting ------------------------------------------
  source(file = "gam_only.R")
  
  save(l0.gam, file = paste("results_nosl60/gam_model_fit_",i,".RData", sep=""))
  
  if (i == seeds[1]){
    # events - reciprocity
    # hist(events$time_since_rec_pair,breaks = 500, main = "Time since last reciprocal interaction [events]")
    # hist(exp(-(events$time_since_rec_pair)),breaks = 500, main = "Exp(-(Time since last reciprocal interaction)) [events]")
    
    cat("Median of Time since last reciprocal interaction (events):", med_rec_events, "minutes. \n")
    cat("Number of events that have no reciprocal event in the past:",
        sum(is.infinite(events$time_since_rec_pair)), "\n")
    cat("Number of events that have exp(-delta_t_recip) = 0:",
        sum(exp(-(events$time_since_rec_pair))==0), "\n")
    # hist((1/(2*med_rec_events))*events$time_since_rec_pair,breaks = 500, main = "Rescaled [median] time since last reciprocal interaction [events]")
    # hist(exp(-(1/(2*med_rec_events))*(events$time_since_rec_pair)),breaks = 500, main = "Rescaled [median] exp(-(Time since last reciprocal interaction)) [events]")
    cat("Number of events that have exp(-delta_t_recip) = 0 after median rescaling:",
        sum(exp(-(1/(2*med_rec_events))*(events$time_since_rec_pair))==0), "\n")
    
    save(med_rec_events, file = "results_nosl60/median_deltat_recip_events.RData")
    
    # events - inertia
    # hist(events$time_since_same_pair,breaks = 500, main = "Time since last same interaction [events]")
    # hist(exp(-(events$time_since_same_pair)),breaks = 500, main = "Exp(-(Time since last same interaction)) [events]")
    
    cat("Median of Time since last same interaction (events):", med_rep_events, "minutes. \n")
    cat("Number of events that have no same event in the past:",
        sum(is.infinite(events$time_since_same_pair)), "\n")
    cat("Number of events that have exp(-delta_t_repet) = 0:",
        sum(exp(-(events$time_since_same_pair))==0), "\n")
    # hist((1/(2*med_rep_events))*events$time_since_same_pair,breaks = 500, main = "Rescaled [median] time since last same interaction [events]")
    # hist(exp(-(1/(2*med_rep_events))*(events$time_since_same_pair)),breaks = 500, main = "Rescaled [median] exp(-(Time since last same interaction)) [events]")
    cat("Number of events that have exp(-delta_t_repet) = 0 after median rescaling:",
        sum(exp(-(1/(2*med_rep_events))*(events$time_since_same_pair))==0), "\n")
    
    save(med_rep_events, file = "results_nosl60/median_deltat_repet_events.RData")
    
  }
  
  
  # --------------------- residual baseline hazard estimation----------------
  source(file = "c_estimation.R")
  
  save(c_est,file = paste("results_nosl60/res_basehaz_",i,".RData", sep=""))
  
  
  #------------------------------ RESULTS ---------------------------------------
  print("Summary of fitted GAM model:")
  print(summary(l0.gam))
  
  print("Summary of linear model fitted to the residual cum. baseline hazard:")
  print(summary(c_est))
  # move to plotting.R script for plots of estimates smooth terms
   
}