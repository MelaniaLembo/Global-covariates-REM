#------------ preparing data for gam estimation ------------------------

nn<-nrow(events)
t.mat<-cbind(rep(1,nn),rep(-1,nn))
tms<-cbind(events$DateTime,nonevents$DateTime)

temp<-cbind(events$Temperature,nonevents$Temperature)
hum<-cbind(events$Humidity,nonevents$Humidity)
wind<-cbind(events$Wind.Speed,nonevents$Wind.Speed)
pres<-cbind(events$Pressure,nonevents$Pressure)
precip<-log(cbind(events$Precip.,nonevents$Precip.)+1)


start_comp<-events$start_comp_st_min - nonevents$start_comp_st_min
end_comp<-events$end_comp_st_min - nonevents$end_comp_st_min

OSM_dist<-log(cbind(events$OSM_dist_min,nonevents$OSM_dist_min)+1)

bike_av_start<-events$bike_avail_sender - nonevents$bas
bike_av_end<-events$bike_avail_receiver - nonevents$bar


# since distribution of values for -(t_last - t) is very skewed towards 0 
# we transform these covariates by mapping the median of these values to 1/2 and redistributing all other values accordingly

med_rec_events<- median(events$time_since_rec_pair)
med_rep_events<- median(events$time_since_same_pair)


# transforming and taking exp(-())
rec<-exp(-(1/(2*med_rec_events))*(cbind(events$time_since_rec_pair, nonevents$time_since_rec_pair)))
rep<-exp(-(1/(2*med_rep_events))*(cbind(events$time_since_same_pair, nonevents$time_since_same_pair)))

tod<-cbind(events$tod, nonevents$tod)


y<-rep(1,nn)




l0.gam<-gam(y~ -1+ s(tms,by=t.mat) +
              s(temp, by = t.mat)  +  # weather vars
              s(precip, by = t.mat) + # weather vars (cont.)
              start_comp + end_comp + s(OSM_dist, by = t.mat)  +  # exogenous vars - distances -
               s(rec,by = t.mat, k = 20) + s(rep, by = t.mat, k = 20) # endogenous vars - bike avail, reciprocity and inertia -
              + s(tod, by = t.mat, bs = "cc")  # time-related global variables
              , family=binomial) 








