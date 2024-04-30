# parallel code to perform NCC sampling on original process and compute all variables for 
# sampled nonevents

# Register a parallel backend with available cores
cl <- makeCluster(4, type = "PSOCK")
registerDoParallel(cl)

row_indices<-1:nrow(merged_data)

n<-nrow(merged_data)
ordered_et<-order(merged_data$DateTime)

nonevents_new <- foreach(i = row_indices, .combine = 'rbind') %dopar%{
  which.event<-ordered_et[i]
  
  non.event_info<-sample(1:nrow(info),1) # indices in the info df of non event interac.
  non.tm <- merged_data$DateTime[which.event]
  non_sender<- info$sender[non.event_info]
  non_reciever<-info$receiver[non.event_info]
  
  line<-cbind(non_sender,non_reciever, non.tm, non.event_info,
              info[non.event_info,c("Manhattan_dist","start_comp_st_min","end_comp_st_min",
                                    "start_comp_st_mt","end_comp_st_mt",
                                    "OSM_dist_min","OSM_dist_mt")])
  colnames(line)<-c("sender", "receiver", "non_event_time", "info",
                    "Manhattan_dist","start_comp_st_min","end_comp_st_min",
                    "start_comp_st_mt","end_comp_st_mt",
                    "OSM_dist_min","OSM_dist_mt")
  
  
  # weather variables of the closest time point to non-event time
  nvw_line<-cbind(merged_data[which.event,1:6],line)
  
  rm("line")
  
  # endogenous variables
  past<-merged_data[merged_data$DateTime < non.tm,]
  
  nvw_line$ids<-nrow(past[past$end_station_id==non_sender,])+ 
    nrow(data_1_8[data_1_8$end_station_id ==non_sender,])
  nvw_line$ods<-nrow(past[past$start_station_id==non_sender,])+ 
    nrow(data_1_8[data_1_8$start_station_id ==non_sender,]) 
  nvw_line$idr<-nrow(past[past$end_station_id==non_reciever,]) + 
    nrow(data_1_8[data_1_8$end_station_id ==non_reciever,])
  nvw_line$odr<-nrow(past[past$start_station_id==non_reciever,]) +
    nrow(data_1_8[data_1_8$start_station_id ==non_reciever,]) 
  
  rm("past")
  
  nvw_line$bas<- nvw_line$ids - nvw_line$ods
  nvw_line$bar<- nvw_line$idr - nvw_line$odr
  
  
  
  #time since rec. pair, same pair and open rec. pairs
  past_same_pair<-merged_data[merged_data$start_station_id==non_sender &
                                merged_data$end_station_id==non_reciever &
                                merged_data$DateTime < non.tm,]
  
  before_Jul9_past_same_pair<- data_1_8[data_1_8$start_station_id==non_sender &
                                          data_1_8$end_station_id==non_reciever,]

  past_rec_pair<- merged_data[merged_data$start_station_id==non_reciever &
                                merged_data$end_station_id==non_sender &
                                merged_data$DateTime < non.tm,]
  
  before_Jul9_past_rec_pair<- data_1_8[data_1_8$start_station_id==non_reciever &
                                         data_1_8$end_station_id==non_sender,]

  if(nrow(before_Jul9_past_rec_pair)==0){
    nvw_line$time_since_rec_pair <- ifelse(nrow(past_rec_pair)!=0,-(max(past_rec_pair$DateTime)-non.tm),Inf)
  }else{
    nvw_line$time_since_rec_pair <- ifelse(nrow(past_rec_pair)!=0,
                                           -(max(past_rec_pair$DateTime)-non.tm),
                                           -(max(before_Jul9_past_rec_pair$started_at)-non.tm))
  }
  
  
  if(nrow(before_Jul9_past_same_pair)==0){
    nvw_line$time_since_same_pair <- ifelse(nrow(past_same_pair)!=0,-(max(past_same_pair$DateTime)-non.tm),Inf)
  }else{
    nvw_line$time_since_same_pair <- ifelse(nrow(past_same_pair)!=0,
                                            -(max(past_same_pair$DateTime)-non.tm),
                                            -(max(before_Jul9_past_same_pair$started_at)-non.tm))
  }
  
  # does adding abs so that it is always positive make more sense ???
  nvw_line$open_rec_pair <- nrow(past_rec_pair) - 
    nrow(past_same_pair)
  
  rm("past_same_pair")
  rm("past_rec_pair")
  
  nvw_line$tod<-merged_data$tod[which.event]
  nvw_line$dow<-merged_data$dow[which.event]
  
  return(nvw_line)
  
}

# Stop the parallel backend
stopCluster(cl)

rm("row_indices","cl")