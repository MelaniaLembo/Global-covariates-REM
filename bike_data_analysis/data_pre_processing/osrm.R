# it computes the distances between stations (both in minutes and mt, we use the former)
# as a matrix.
# a limited number of queries (100^2) can be launched with one function call
# hence the different blocks


n_st<-nrow(poss_station_names)

dm_min<-matrix(NA,n_st,n_st)
dm_mt<-matrix(NA,n_st,n_st)

frac<-100

int_times<-floor(n_st/frac)

coord_st<-poss_station_names[,c("lng","lat")]

# ------------- 10000 size blocks --------------

for (r in 1:int_times) {
  for (c in 1:int_times) {
    scr_ind<-(((r-1)*frac) + 1):(r*frac)
    dst_ind<- (((c-1)*frac) + 1):(c*frac)
    
    res<-osrmTable(src = coord_st[scr_ind,],
                   dst = coord_st[dst_ind,],
                   measure = c("duration","distance"))
    
    dm_min[scr_ind,dst_ind]<-res[["durations"]]
    
    dm_mt[scr_ind,dst_ind]<-res[["distances"]]
    
  }
  
}

#--------------------- remaining blocks --------------------

# row blocks
scr_ind<-(int_times*frac+1):n_st
for (c in 1:int_times) {
  dst_ind<- (((c-1)*frac) + 1):(c*frac)
  
  res<-osrmTable(src = coord_st[scr_ind,],
                 dst = coord_st[dst_ind,],
                 measure = c("duration","distance"))
  
  dm_min[scr_ind,dst_ind]<-res[["durations"]]
  
  dm_mt[scr_ind,dst_ind]<-res[["distances"]]
  
}

#column blocks
dst_ind<-(int_times*frac+1):n_st
for (r in 1:int_times) {
  scr_ind<- (((r-1)*frac) + 1):(r*frac)
  
  res<-osrmTable(src = coord_st[scr_ind,],
                 dst = coord_st[dst_ind,],
                 measure = c("duration","distance"))
  
  dm_min[scr_ind,dst_ind]<-res[["durations"]]
  
  dm_mt[scr_ind,dst_ind]<-res[["distances"]]
  
}

#diagonal block
scr_ind<-(int_times*frac+1):n_st
dst_ind<- (int_times*frac+1):n_st

res<-osrmTable(src = coord_st[scr_ind,],
               dst = coord_st[dst_ind,],
               measure = c("duration","distance"))

dm_min[scr_ind,dst_ind]<-res[["durations"]]

dm_mt[scr_ind,dst_ind]<-res[["distances"]]


rm(list=c("n_st","frac","int_times","coord_st","r","c","scr_ind","dst_ind","res"))