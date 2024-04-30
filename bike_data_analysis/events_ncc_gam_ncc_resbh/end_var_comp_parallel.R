# setting up parallelized code of the for loop
# used to compute endogenous variables that will end up
# in the final dataset merged_data

# Register a parallel backend with 4 cores
#cl <- makeCluster(detectCores(), type = "PSOCK")
cl <- makeCluster(4, type = "PSOCK")
registerDoParallel(cl)
 
row_indices<-1:nrow(merged_data)

merged_data_end_var_comp <- foreach(i = row_indices, .combine = 'rbind') %dopar%{
  
  s<-merged_data$start_station_id[i]
  r<-merged_data$end_station_id[i]
  t<-merged_data$DateTime[i]
  
  past<-merged_data[merged_data$DateTime < t,]
  
  in_deg_sender <- nrow(past[past$end_station_id==s,]) + 
    nrow(data_1_8[data_1_8$end_station_id ==s,])
  out_deg_sender <- nrow(past[past$start_station_id==s,]) + 
    nrow(data_1_8[data_1_8$start_station_id ==s,])
  in_deg_receiver <- nrow(past[past$end_station_id==r,]) + 
    nrow(data_1_8[data_1_8$end_station_id ==r,])
  out_deg_receiver <- nrow(past[past$start_station_id==r,]) + 
    nrow(data_1_8[data_1_8$start_station_id ==r,])
  
  rm("past")
  
  bike_avail_sender <- in_deg_sender - out_deg_sender
  bike_avail_receiver <- in_deg_receiver - out_deg_receiver
  
  
  past_rec_pair<-merged_data[merged_data$start_station_id==r & merged_data$end_station_id==s & 
                               merged_data$DateTime < t,]
  
  before_Jul9_past_rec_pair<- data_1_8[data_1_8$start_station_id==r &
                                         data_1_8$end_station_id==s,]

   if(nrow(before_Jul9_past_rec_pair)==0){
     time_since_rec_pair <- ifelse(nrow(past_rec_pair)!=0,
                                   -(max(past_rec_pair$DateTime)-t),Inf)
   }else{
     time_since_rec_pair <- ifelse(nrow(past_rec_pair)!=0,
                                   -(max(past_rec_pair$DateTime)-t),
                                   -(max(before_Jul9_past_rec_pair$started_at)-t))
   }
  
  
    
  
  past_same_pair<-merged_data[merged_data$start_station_id==s & 
                                merged_data$end_station_id==r & 
                                merged_data$DateTime < t,]
  
   before_Jul9_past_same_pair<- data_1_8[data_1_8$start_station_id==s &
                                          data_1_8$end_station_id==r,]

   if(nrow(before_Jul9_past_same_pair)==0){
     time_since_same_pair <- ifelse(nrow(past_same_pair)!=0,
                                    -(max(past_same_pair$DateTime)-t),Inf)
   }else{
     time_since_same_pair <- ifelse(nrow(past_same_pair)!=0,
                                    -(max(past_same_pair$DateTime)-t),
                                    -(max(before_Jul9_past_same_pair$started_at)-t))
   }
  
  
    
  
  # does adding abs so that it is always positive make more sense ???
  open_rec_pair <- nrow(past_rec_pair) - 
    nrow(past_same_pair)
  
  rm("past_same_pair")
  rm("past_rec_pair")
  rm("before_Jul9_past_same_pair")
  rm("before_Jul9_past_rec_pair")
  
  info_ind<-which((info$sender == s) & (info$receiver == r))
  
  return(c(in_deg_sender, out_deg_sender, in_deg_receiver, out_deg_receiver, 
           bike_avail_sender, bike_avail_receiver, open_rec_pair, 
           time_since_rec_pair,time_since_same_pair, info_ind))
  
}

dimnames(merged_data_end_var_comp)[[1]]<- row_indices
dimnames(merged_data_end_var_comp)[[2]]<- c("in_deg_sender", "out_deg_sender", "in_deg_receiver", 
                                            "out_deg_receiver", "bike_avail_sender", 
                                            "bike_avail_receiver", "open_rec_pair", 
                                            "time_since_rec_pair","time_since_same_pair", "info_ind")

# Stop the parallel backend
stopCluster(cl)
merged_data[row_indices,10:19]<-as.data.frame(merged_data_end_var_comp)
rm(list = c("row_indices","cl","merged_data_end_var_comp"))
