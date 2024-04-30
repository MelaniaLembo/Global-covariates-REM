# BIKE DATA PRE-PROCESSING

library(lubridate)
library(data.table)
library(osrm)
options(osrm.profile = "bike")
options(warn = 1)

new_subfolder<-"./initial_data"
dir.create(new_subfolder)

#------------------------ DATA CLEANING ----------------------------

# importing dataset of bike share in D.C: for July 2023
download.file("https://s3.amazonaws.com/capitalbikeshare-data/202307-capitalbikeshare-tripdata.zip","../raw_data/202307-capitalbikeshare-tripdata.zip")
bike_data <- read.csv(unz("../raw_data/202307-capitalbikeshare-tripdata.zip", "202307-capitalbikeshare-tripdata.csv"))
anyNA(bike_data)
print(names(bike_data))

bike_data$started_at<-as.POSIXct(bike_data$started_at)
bike_data$ended_at<-as.POSIXct(bike_data$ended_at)

anyNA(bike_data$started_at)

# some observations time travel in the past !!!
# except for one self-loop all these time travelling observations either miss a start station id or the end one
# I drop them (they are 69): I consider this discrepancy as a reason to not trust these entries
time_travel_interac<-which(difftime(bike_data$ended_at,bike_data$started_at, units="hours")<0)
View(bike_data[time_travel_interac,])
bike_data<-bike_data[-time_travel_interac,]

# some observations last less than 60 seconds, probably false starts, lets remove them
false_starts<- which(difftime(bike_data$ended_at,bike_data$started_at, units="secs")<60 &
                       bike_data$start_station_id==bike_data$end_station_id)
View(bike_data[false_starts,])
bike_data<-bike_data[-false_starts,]


# filtering events with start date in the decided interval starting on July 9th
# due to missing data in weather dataset
left_bound_date<-as.POSIXct("2023-07-09")
date_decomp<-ymd_hms(bike_data$started_at)
print(bike_data$started_at[which(is.na(date_decomp))])
bike_data_from9 <- bike_data[date(date_decomp)>=left_bound_date & !is.na(date_decomp),]
bike_data_from9<-rbind(bike_data_from9,bike_data[which(is.na(date_decomp)),])


# handling NAs in longitude/latitude coordinates:
# all coordinates for start and end are present except for 
# a few cases of lat/long of the end station.
# !!! I drop these observations !!! 
any(c(anyNA(bike_data_from9$start_lat),anyNA(bike_data_from9$start_lng)))
any(c(anyNA(bike_data_from9$end_lat),anyNA(bike_data_from9$end_lng)))

miss_end_coord<-which(is.na(bike_data_from9$end_lat))
print(all(miss_end_coord == which(is.na(bike_data_from9$end_lng))))
View(bike_data_from9[which(is.na(bike_data_from9$end_lat)),])
bike_data_from9<-bike_data_from9[-miss_end_coord,]
anyNA(bike_data_from9$end_lat)
anyNA(bike_data_from9$end_lng)

# NAs in start/end station ids
miss_start_id<-which(is.na(bike_data_from9$start_station_id))
miss_end_id<-which(is.na(bike_data_from9$end_station_id))

# all correspond to an empty start/end station name field
miss_start_name<-which(is.na(bike_data_from9$start_station_name))
miss_end_name<-which(is.na(bike_data_from9$end_station_name))

print(all(miss_start_name == miss_start_id))
print(all(miss_end_name == miss_end_id))


# dataframe of all possible stations included in the data
# for each station we save id, name and lat/long coord.
poss_station_names<-as.data.frame(unique(c(bike_data_from9[-miss_start_id,]$start_station_name, 
                             bike_data_from9[-miss_end_id,]$end_station_name)))
colnames(poss_station_names)<-c("station_name")
poss_station_names$station_id<-rep(NA,nrow(poss_station_names))

# station id - station name correspondence
for(i in 1:nrow(poss_station_names)){
  poss_id<-unique(c(bike_data_from9[setdiff(which(bike_data_from9$start_station_name==poss_station_names$station_name[i]),
                                            miss_start_id),]$start_station_id,
                    bike_data_from9[setdiff(which(bike_data_from9$end_station_name==poss_station_names$station_name[i]),
                                            miss_end_id),]$end_station_id))
  
  if(length(poss_id)>1){ # more than one id for a single name
    print("please no !!!")
  }else{
    poss_station_names$station_id[i]<-poss_id
  }
  
}

anyNA(poss_station_names)
length(unique(poss_station_names$station_id))==nrow(poss_station_names)


# handling different names (related to various spellings) for the same id
sn_sid_incons<-poss_station_names[which(duplicated(poss_station_names$station_id)),]
View(sn_sid_incons) 

# solving by taking the most frequent spelling
for (ind in sn_sid_incons$station_id) {
  ids_start<-which(bike_data_from9$start_station_id==ind)
  ids_end<-which(bike_data_from9$end_station_id==ind)
  
  final_name<- names(which.max(table(c(bike_data_from9$start_station_name[ids_start],
                                       bike_data_from9$end_station_name[ids_end]))))
  poss_station_names[poss_station_names$station_name %in% setdiff(c(bike_data_from9$start_station_name[ids_start],
                                                                    bike_data_from9$end_station_name[ids_end]),final_name),1]<-final_name
  if(length(ids_start)==0 | length(ids_end)==0){
    if(length(ids_start)==0){
      bike_data_from9[ids_end,]$end_station_name<-final_name
    }else if(length(ids_end)==0){
      bike_data_from9[ids_start,]$start_station_name<-final_name
    }
  }else{
    bike_data_from9[ids_start,]$start_station_name<-final_name
    bike_data_from9[ids_end,]$end_station_name<-final_name
  }
  
  
}

poss_station_names<-poss_station_names[-which(duplicated(poss_station_names$station_id)),]
anyNA(poss_station_names)
length(unique(poss_station_names$station_id))==nrow(poss_station_names)
length(unique(poss_station_names$station_name))==nrow(poss_station_names)

# all station names appearing in bike_data_from9 are in 1-1 correspondence with station ids
# nrow + 1, because dataset still contains some rows with empty station name field !!!
length(unique(c(bike_data_from9$start_station_name,bike_data_from9$end_station_name))) ==
  nrow(poss_station_names)+1

length(unique(c(bike_data_from9$start_station_name,bike_data_from9$end_station_name))) == 
  length(unique(c(bike_data_from9$start_station_id,bike_data_from9$end_station_id)))

poss_station_names$lat<-rep(NA,nrow(poss_station_names))
poss_station_names$lng<-rep(NA,nrow(poss_station_names))

# rounding lat/long to try and have 1-1 correspondence
# between station id and coords.
# 5 decimal digits means up to 1 mt precision
# reasonable to assume that station of coordinates that differ from the 6th decimal digit 
# are the same station
bike_data_from9$start_lat<-round(bike_data_from9$start_lat,5)
bike_data_from9$start_lng<-round(bike_data_from9$start_lng,5)
bike_data_from9$end_lat<-round(bike_data_from9$end_lat,5)
bike_data_from9$end_lng<-round(bike_data_from9$end_lng,5)

# we are uniformizing the lat/long coords
for(i in 1:nrow(poss_station_names)){
  id<-poss_station_names$station_id[i]
  ids_start<-which(bike_data_from9$start_station_id==id)
  ids_end<-which(bike_data_from9$end_station_id==id)
  
  ends_sub<-bike_data_from9[ids_end,c("end_lat", "end_lng")]
  
  if(nrow(ends_sub)==0){
    print("There is a station that never appears as an end station !!")
    break
  }
  if(nrow(unique(ends_sub))==1){
    poss_station_names$lat[i]<-as.numeric(unique(ends_sub)$end_lat)
    poss_station_names$lng[i]<-as.numeric(unique(ends_sub)$end_lng)

  }else{ # still when rounding up to 5 decimal digits there are different coords for the same station
    # we keep the most frequent coordinate pair
    count<-table(ends_sub)
    #print(id)
    #print(count)
    mc<-max(count)
    mc_ind<-which(count==mc,arr.ind = TRUE)
    poss_station_names$lat[i]<-as.numeric(row.names(count)[mc_ind[1]])
    poss_station_names$lng[i]<-as.numeric(colnames(count)[mc_ind[2]])
  }
  
  if(length(ids_start)==0 | length(ids_end)==0){
    if(length(ids_start)==0){
      bike_data_from9[ids_end,]$end_lat<-poss_station_names$lat[i]
      bike_data_from9[ids_end,]$end_lng<-poss_station_names$lng[i]
    }else if(length(ids_end)==0){
      bike_data_from9[ids_start,]$start_lat<-poss_station_names$lat[i]
      bike_data_from9[ids_start,]$start_lng<-poss_station_names$lng[i]
    }
  }else{
    bike_data_from9[ids_start,]$start_lat<-poss_station_names$lat[i]
    bike_data_from9[ids_start,]$start_lng<-poss_station_names$lng[i]
    
    bike_data_from9[ids_end,]$end_lat<-poss_station_names$lat[i]
    bike_data_from9[ids_end,]$end_lng<-poss_station_names$lng[i]
  }
  
}

anyNA(poss_station_names)

nrow(unique(poss_station_names[,c("lat","lng")]))==nrow(poss_station_names)

# handling missing data for start/end station ids:
# we infer it from lat/long. coords. by using existing id or 
# inventing a new one otherwise.


max_ex_id<-max(poss_station_names$station_id)

print(max_ex_id)

number_add_stations<-0

# let's go with start station first
for (i in miss_start_id) {
  coord<-bike_data_from9[i,c("start_lat","start_lng")]
  index<-which(poss_station_names$lat==coord$start_lat & poss_station_names$lng==coord$start_lng)
  
  if(length(index)>0){
    if(length(index)==1){
      # station id for coord already exists
      bike_data_from9$start_station_id[i]<- poss_station_names$station_id[index]
      #print("Found existing ID")
    }else{
      print("Problem !!! No 1-1 correpsondance between coords and id.")
      break
    }
  }else{
    # station id needs to be created 
    max_ex_id<-max_ex_id + 1
    number_add_stations<-number_add_stations + 1
    poss_station_names<-rbind(poss_station_names,
                              data.frame(list("station_name" = paste("new station", 
                                                                     number_add_stations), 
                                              "station_id"=max_ex_id, 
                                              "lat" = coord$start_lat, "lng" = coord$start_lng)))
    bike_data_from9$start_station_id[i]<- max_ex_id
  }
  
  
}

print(max_ex_id)
# then move on to end stations

miss_end_id_classic<-which(bike_data_from9$rideable_type=="classic_bike" & is.na(bike_data_from9$end_station_id))
View(bike_data_from9[miss_end_id_classic,])
bike_data_from9<-bike_data_from9[-miss_end_id_classic,]


miss_end_id<-which(is.na(bike_data_from9$end_station_id))
View(bike_data_from9[miss_end_id,])

for (i in miss_end_id) {
  coord<-bike_data_from9[i,c("end_lat","end_lng")]
  index<-which(poss_station_names$lat==coord$end_lat & poss_station_names$lng==coord$end_lng)
  
  if(length(index)>0){
    if(length(index)==1){
      # station id for coord already exists
      bike_data_from9$end_station_id[i]<- poss_station_names$station_id[index]
      #print("Found existing ID")
    }else{
      print("Problem !!! No 1-1 correpsondance between coords and id.")
      break
    }
  }else{
    # station id needs to be created 
    max_ex_id<-max_ex_id + 1
    number_add_stations<-number_add_stations + 1
    poss_station_names<-rbind(poss_station_names,
                              data.frame(list("station_name" = paste("new station", 
                                                                     number_add_stations), 
                                              "station_id"=max_ex_id, 
                                              "lat" = coord$end_lat, "lng" = coord$end_lng)))
    bike_data_from9$end_station_id[i]<- max_ex_id
  }
  
  
}

print(max_ex_id)

# checking that we have actually successfully removed NA for start/end stations
anyNA(bike_data_from9$start_station_id)
anyNA(bike_data_from9$end_station_id)
anyNA(bike_data_from9)

check<-bike_data_from9[,c("start_lat", "start_lng")]
colnames(check)<-c("V1","V2")
check1<-bike_data_from9[,c("end_lat", "end_lng")]
colnames(check1)<-c("V1","V2")
check_final<-rbind(check,check1)
nrow(unique(check_final)) ==
  length(unique(c(bike_data_from9$start_station_id,bike_data_from9$end_station_id)))
  

save(poss_station_names,file = "./initial_data/station_info_nosl60.RData")

# while we consider bike shares in time interval July 9th-31st
# we save and keep track  of the ones in time period July 1st-8th
# to be used to appropriately compute
# "time since last reciprocal/same event"
# later needed for the reciprocity and repetition effect
bike_data_Jul1_8 <- bike_data[date(date_decomp)<left_bound_date,]

any(c(anyNA(bike_data_Jul1_8$start_lat),anyNA(bike_data_Jul1_8$start_lng)))
any(c(anyNA(bike_data_Jul1_8$end_lat),anyNA(bike_data_Jul1_8$end_lng)))

miss_end_coord_1_8<-which(is.na(bike_data_Jul1_8$end_lat))
print(all(miss_end_coord_1_8 == which(is.na(bike_data_Jul1_8$end_lng))))
#View(bike_data_from9[which(is.na(bike_data_from9$end_lat)),])
miss_start_coord_1_8<-which(is.na(bike_data_Jul1_8$start_lat))
print(all(miss_start_coord_1_8 == which(is.na(bike_data_Jul1_8$start_lng))))

bike_data_Jul1_8<-bike_data_Jul1_8[-c(miss_end_coord_1_8,miss_start_coord_1_8),]

miss_start_id_1_8<-which(is.na(bike_data_Jul1_8$start_station_id))
miss_end_id_1_8<-which(is.na(bike_data_Jul1_8$end_station_id))

# all correspond to an empty start/end station name field
miss_start_name_1_8<-which(bike_data_Jul1_8$start_station_name == "")
miss_end_name_1_8<-which(bike_data_Jul1_8$end_station_name == "")

print(all(miss_start_name_1_8 == miss_start_id_1_8))
print(all(miss_end_name_1_8 == miss_end_id_1_8))

bike_data_Jul1_8$start_lat<-round(bike_data_Jul1_8$start_lat,4)
bike_data_Jul1_8$start_lng<-round(bike_data_Jul1_8$start_lng,4)
bike_data_Jul1_8$end_lat<-round(bike_data_Jul1_8$end_lat,4)
bike_data_Jul1_8$end_lng<-round(bike_data_Jul1_8$end_lng,4)


# even though we look at interaction between July 1st-8th
# any station appearing in this subset of data that did not appear
# in the data regarding July 9th-31st, will not be added to the risk set

tbr_start<-c()
for (ind in miss_start_id_1_8) {

  coord<-bike_data_Jul1_8[ind,c("start_lat","start_lng")]
  index<-which(poss_station_names$lat==coord$start_lat & poss_station_names$lng==coord$start_lng)
  if(length(index)==1){
    #print("found")
    bike_data_Jul1_8$start_station_id[ind]<- poss_station_names$station_id[index]

  }else{
    #print("new station not to be added to the risk set")
    tbr_start<-c(tbr_start,ind)
  }

}


tbr_end<-c()
for (ind in miss_end_id_1_8) {

  coord<-bike_data_Jul1_8[ind,c("end_lat","end_lng")]
  index<-which(poss_station_names$lat==coord$end_lat & poss_station_names$lng==coord$end_lng)
  if(length(index)==1){
    #print("found")
    bike_data_Jul1_8$end_station_id[ind]<- poss_station_names$station_id[index]

  }else{
    #print("new station not to be added to the risk set")
    tbr_end<-c(tbr_end,ind)
  }

}

bike_data_Jul1_8<-bike_data_Jul1_8[-c(tbr_start,tbr_end),]
any(anyNA(bike_data_Jul1_8$start_station_id),
    anyNA(bike_data_Jul1_8$end_station_id))


# starting to create polished dataset
data<-bike_data_from9[,-which(names(bike_data_from9) %in% 
                                c("start_station_name", 
                                  "end_station_name"))]
data$rideable_type<-as.factor(data$rideable_type)
data$member_casual<-as.factor(data$member_casual)

data_1_8<-bike_data_Jul1_8[,-which(names(bike_data_Jul1_8) %in%
                                c("start_station_name",
                                  "end_station_name","rideable_type",
                                  "member_casual"))]


# handle time: take starting time as time of occurrence of relational events
# and set the first to 0 and measure distance from there in minutes (???).
# then drop end time.

# checking if there are simultaneous starting times
length(unique(data$started_at))==nrow(data)
#View(data[duplicated(data$started_at),])
print(nrow(data[duplicated(data$started_at),]))
# there are :(. I keep them all and
# acknowledge that this introduces a negligible degree of approximation
# in going on with PL optimization by ignoring these ties.
# Method is still consistent.

# checking if among these ties
# there some that are exactly the same edges happening at the same time
#View(data[duplicated(data[c("started_at", "start_station_id", "end_station_id")]),])
print(nrow(data[duplicated(data[c("started_at", "start_station_id", "end_station_id")]),]))
# yes we have these kind of edges too (not that many)
# these events will correspond to ties also in the shifted process



# defining time
starting_timepoint<-min(data$started_at)
save(starting_timepoint,file = "./initial_data/starting_timepoint.RData")
data$started_at<-as.numeric(sapply(data$started_at,difftime,time2=starting_timepoint,units="mins"))
data_1_8$started_at<-as.numeric(sapply(data_1_8$started_at,difftime,time2=starting_timepoint,units="mins"))

# drop event occurring at time 0 
data<-data[!data$started_at==0,]
# dropping ride_id and ending time,
# member and ride type info as well
# since we can't compute for the corresponding non-events,
# and we drop lat/long coords as well
data<-data[,-which(names(data) %in% 
                     c("ride_id", "ended_at", "member_casual", "rideable_type",
                       "start_lat", "start_lng","end_lat", "end_lng"))]
data_1_8<-data_1_8[,-which(names(data_1_8) %in%
                     c("ride_id", "ended_at",
                       "start_lat", "start_lng","end_lat", "end_lng"))]

save(data_1_8,file = "./initial_data/before_Jul9_nosl60.RData")

# View(data)
# load weather data and merge it to the bike sharing
# by taking row corresponding to closest time-point
#load("weather_data.RData")
source("weather_data_processing.R")
# View(weather_Jul)
# handling time
weather_Jul$DateTime<-as.numeric(sapply(weather_Jul$DateTime,difftime,time2=starting_timepoint,
                                        units="mins"))
# merging bike share data and weather data
# for a bike share occurring at time t, the values for the weather related variables
# is the one corresponding to the time point closest to t
wdt <- data.table(weather_Jul , key = "DateTime" )
save(wdt, file = "./initial_data/wdt.RData")
bdt <- data.table( data, key = "started_at" )

merged_data <- wdt[bdt, roll = "nearest"]
# start computing and adding relevant variables for bike-sharing related data
rm("bdt")
# dataset of all possible pairs and related time-independent covariates
vertices<-poss_station_names$station_id

info<-expand.grid(vertices,vertices)
info<-cbind(info,0,0,0,0,0)
colnames(info) <- c("sender","receiver","start_lat","start_lng","end_lat","end_lng",
                    "Manhattan_dist")

coord_pick_lat<-function(x,y){
  y$lat[which(x==y$station_id)]
}

coord_pick_lng<-function(x,y){
  y$lng[which(x==y$station_id)]
}

info$start_lat<-sapply(info$sender,coord_pick_lat,poss_station_names)
anyNA(info$start_lat)
info$start_lng<-sapply(info$sender,coord_pick_lng,poss_station_names)
anyNA(info$start_lng)
info$end_lat<-sapply(info$receiver,coord_pick_lat,poss_station_names)
anyNA(info$end_lat)
info$end_lng<-sapply(info$receiver,coord_pick_lng,poss_station_names)
anyNA(info$end_lng)

info$Manhattan_dist<-apply(info[,c("start_lat","start_lng","end_lat","end_lng")],
                                  1,function(x) sum(abs(x[1]-x[3]),abs(x[2]-x[4])))

# function that compute pairwise distance between station in biking minutes
source("osrm.R")
pair_pick_dist<-function(x,y,z){
  z[which(x[1]==y$station_id),which(x[2]==y$station_id)]
}
info$OSM_dist_min<-apply(info[,c("sender","receiver")],1,pair_pick_dist,poss_station_names,dm_min)
info$OSM_dist_mt<-apply(info[,c("sender","receiver")],1,pair_pick_dist,poss_station_names,dm_mt)


# computing competition variables
info$start_comp_st_min<-rep(NA,nrow(info))
info$end_comp_st_min<-rep(NA,nrow(info))
info$start_comp_st_mt<-rep(NA,nrow(info))
info$end_comp_st_mt<-rep(NA,nrow(info))
# start/end station competition: OSM route distance from nearest station (in mins or mt)
for (j in 1:nrow(poss_station_names)) {
  i<-poss_station_names$station_id[j]
  val<-min(info[(info$sender==i|info$receiver==i)&!(info$sender==i&info$receiver==i),"OSM_dist_min"])
  val_mt<-min(info[(info$sender==i|info$receiver==i)&!(info$sender==i&info$receiver==i),"OSM_dist_mt"])
  if(val==0){
    break
  }
  info[info$sender==i,"start_comp_st_min"]<-val
  info[info$receiver==i,"end_comp_st_min"]<-val
  info[info$sender==i,"start_comp_st_mt"]<-val_mt
  info[info$receiver==i,"end_comp_st_mt"]<-val_mt
}
anyNA(info)

#------------------- constructing shifted process --------------------

set.seed(26)
# shift: one per possible interaction
info$shift<- rexp(nrow(info),1/(mean(merged_data$DateTime)))

# exit
info$exit<- max(merged_data$DateTime) + info$shift

save(info, file = "./initial_data/info_nosl60.RData")

#--------------------------------------------------------------------------
# View(head(info))

# time to compute endogenous variables !!!!
# on merged_data using info as auxiliary

# already ordered chronologically
print(all(order(merged_data$DateTime)==1:nrow(merged_data)))

# end. vars
merged_data$in_deg_sender<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$out_deg_sender<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$in_deg_receiver<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$out_deg_receiver<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$bike_avail_sender<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$bike_avail_receiver<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$open_rec_pair<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$time_since_rec_pair<-as.numeric(rep(NA,nrow(merged_data)))
merged_data$time_since_same_pair<-as.numeric(rep(NA,nrow(merged_data)))

# map between merged data and info dataset
# contains row index of info dataset corresponding to the pair occurring
merged_data$info_ind<-as.numeric(rep(NA,nrow(merged_data)))

save(merged_data,file = "./initial_data/merged_data_incomplete_nosl60.RData")
