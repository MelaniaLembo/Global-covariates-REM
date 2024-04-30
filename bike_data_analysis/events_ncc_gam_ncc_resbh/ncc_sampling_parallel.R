# parallel code to perform NCC sampling on shifted process and compute all variables for 
# sampled nonevents

# Register a parallel backend with available cores
#cl <- makeCluster(detectCores(), type = "PSOCK")
cl <- makeCluster(4, type = "PSOCK")
registerDoParallel(cl)

row_indices<-1:nrow(merged_data)

n<-nrow(merged_data)
ordered_shifted_et<-order(merged_data$abs_te)


ncc_results <- foreach(i = row_indices, .combine = 'rbind') %dopar%{
  which.event<-ordered_shifted_et[i]
  t.absevent<-merged_data$abs_te[which.event]
  
  # interactions that have "entered" yet and not yet "exited"
  at.risk<-setdiff(which((info$shift<t.absevent) & (info$exit>t.absevent)), 
                   merged_data$info_ind[which.event])
  
  size_rs<- length(at.risk)
  if(size_rs>0){ # we exclude the case in which the risk set is only comprised of only the event happening
    
    
    if(size_rs>1){
      non.event_info<-sample(at.risk,1) # indices in the info df of non event interac.
    }else{
      non.event_info<-at.risk
    }
    
    non.tm <- t.absevent - info$shift[non.event_info]
    non_sender<- info$sender[non.event_info]
    non_reciever<-info$receiver[non.event_info]
    
    line<-cbind(non_sender,non_reciever, non.tm, non.event_info,
                info[non.event_info,c("Manhattan_dist","start_comp_st_min","end_comp_st_min",
                                      "start_comp_st_mt","end_comp_st_mt",
                                      "OSM_dist_min","OSM_dist_mt")])
    
    colnames(line)<-c("sender", "receiver", "non_event_time", "info_ind",
                      "Manhattan_dist","start_comp_st_min","end_comp_st_min",
                      "start_comp_st_mt","end_comp_st_mt",
                      "OSM_dist_min","OSM_dist_mt")
    line<-data.table::data.table(line, key = "non_event_time")
    # weather variables of the closest time point to non-event time
    nvw_line<-wdt[line, roll = "nearest"]
    
    rm("line")
    
    past<-merged_data[merged_data$DateTime < non.tm,]

    # endogenous variables
    nvw_line$ids<-nrow(past[past$end_station_id==non_sender,]) +
      nrow(data_1_8[data_1_8$end_station_id ==non_sender,])
    nvw_line$ods<-nrow(past[past$start_station_id==non_sender,]) + 
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
    rm("before_Jul9_past_same_pair")
    rm("before_Jul9_past_rec_pair")

    nontime_in_date_format<-lubridate::ymd_hms(non.tm*60 + starting_timepoint)

    nvw_line$tod<-lubridate::hour(nontime_in_date_format)+
      lubridate::minute(nontime_in_date_format)/60 + 
      lubridate::second(nontime_in_date_format)/3600
    
    nvw_line$dow<-lubridate::wday(nontime_in_date_format)

    return(nvw_line)
    
    
  }else{
    # depending on the shifts, 
    # early events might be in a condition of having no possibility
    # of sampling a non-event.
    # we keep track of which of these events end up being non-informative (if any)
    
    no_samp<-as.data.frame(matrix(NA,1,28))
    colnames(no_samp)<-c("Temperature","Humidity","Wind.Speed","Pressure","Precip.","Condition",
                         "DateTime","sender","receiver","info_ind",
                         "Manhattan_dist","start_comp_st_min","end_comp_st_min","
                         start_comp_st_mt","end_comp_st_mt","OSM_dist_min","OSM_dist_mt",
                         "ids","ods","idr","odr","bas","bar","time_since_rec_pair",
                         "time_since_same_pair","open_rec_pair","tod","dow")
    return(no_samp)
  }
  
  
}


# Stop the parallel backend
stopCluster(cl)

rm("row_indices","cl")
