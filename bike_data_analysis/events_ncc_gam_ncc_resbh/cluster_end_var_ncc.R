# packages required
library(doParallel)
library(foreach)
library(data.table)
setDTthreads(1)
library(lubridate)
library(mgcv)

# initial data necessary
load(file = "../data_pre_processing/initial_data/info_nosl60.RData")
load(file = "../data_pre_processing/initial_data/merged_data_incomplete_nosl60.RData")
load(file = "../data_pre_processing/initial_data/wdt.RData")
load(file = "../data_pre_processing/initial_data/starting_timepoint.RData")
load(file = "../data_pre_processing/initial_data/before_Jul9_nosl60.RData")

new_subfolder<-"./datasets_nosl60"
dir.create(new_subfolder)

# starting from merged_data_incomplete it adds all the endogenous variables computations
source("end_var_comp_parallel.R")

# now we handle the exogenous vars using info column !!!
merged_data$Manhattan_dist<-info$Manhattan_dist[merged_data$info_ind]
merged_data$OSM_dist_min<-info$OSM_dist_min[merged_data$info_ind]
merged_data$OSM_dist_mt<-info$OSM_dist_mt[merged_data$info_ind]
merged_data$start_comp_st_min<-info$start_comp_st_min[merged_data$info_ind]
merged_data$end_comp_st_min<-info$end_comp_st_min[merged_data$info_ind]
merged_data$start_comp_st_mt<-info$start_comp_st_mt[merged_data$info_ind]
merged_data$end_comp_st_mt<-info$end_comp_st_mt[merged_data$info_ind]

# time-relevant variables
time_in_date_format<-ymd_hms(merged_data$DateTime*60 + starting_timepoint)
merged_data$tod <- hour(time_in_date_format)+
  minute(time_in_date_format)/60 + second(time_in_date_format)/3600
merged_data$dow <- wday(time_in_date_format)

merged_data$shift<-info$shift[merged_data$info_ind]

# shifted event time
merged_data$abs_te <- merged_data$DateTime + merged_data$shift
save(merged_data,file = "../data_pre_processing/initial_data/merged_data_new_nosl60.RData")


# some checks 
print("-----------------Let's check a few things on merged_data-------------------------")
cat("Any NAs ?", anyNA(merged_data))
cat("Any ties for original event time?",
    print(nrow(merged_data[duplicated(merged_data[,"DateTime"]),])), "\n")
cat("Any ties for original event time involving the same pairs?",
    print(nrow(merged_data[duplicated(merged_data[,c("DateTime", "start_station_id", "end_station_id")]),])), "\n")
cat("Any ties for shifted event time?", 
    print(nrow(merged_data[duplicated(merged_data[,"abs_te"]),])), "\n")
cat("Any ties for shifted event time involving the same pairs?", 
    print(nrow(merged_data[duplicated(merged_data[,c("abs_te", "start_station_id", "end_station_id")]),])), "\n")
print("---------------------------------------------------------------------------------")

#----------------------------------------------------------------------------------------


seeds<-c(31)

for (i in seeds) {
  set.seed(i)
  
  #------------------------ NCC sampling on the shifted process ---------------------------
  
  source(file = "ncc_sampling_parallel.R")
  
  if(anyNA(ncc_results)){
    print("Some events were dropped after NCC-sampling")
    removed<-ordered_shifted_et[which(is.na(nonevents),arr.ind = TRUE)[,1]]
    nonevents<-ncc_results[-removed,]
    events<-merged_data[-removed,1:28]
    rm("ncc_results")
  }else{
    print("No events were dropped after NCC-sampling")
    nonevents<-ncc_results
    events<-merged_data[,1:28]
    rm("ncc_results")
  }
  
  # saves informative events and corresponding nonevents
  save(events,file = paste("./datasets_nosl60/events_",i,".RData", sep =""))
  save(nonevents,file = paste("./datasets_nosl60/nonevents_",i,".RData", sep =""))
  
  #---------------------------------------------------------------------------------------
  
  #------------------------ NCC sampling on the original process ---------------------------
  # to be use for residual baseline hazard estimation
  
  # creates a set of non-events wrt to the evolution of the risk set in the original process
  # and they will be used to compute the Breslow estimator used to estimate the constant c
  source(file = "ncc_samp_resbh.R")
  save(nonevents_new,file = paste("./datasets_nosl60/nonevents_resbh_",i,".RData", sep =""))

}




