library(mgcv)
library(igraph)
options(warn=1)

set.seed(25)

# delta t for sub.interval division
dt <- 0.001

# mean shift
mf<-1

# dataset of all possible pairs
load(file = "initial_data/info_nosl60.RData")
info<-info[,-(14:15)]

#reducing stations involved to the ones with data (no new stations !!!)
load(file = "initial_data/station_info_nosl60.RData")
stations<- sample(poss_station_names$station_id[1:743],20)
info<-info[info$sender %in% stations &
             info$receiver %in% stations, ]
info$tot_event_counts<-0
hist(info$OSM_dist_min)
hist(log(info$OSM_dist_min+1))

# true smooth temperature effect
gc_temp<-function(temp_t){
  return(-(temp_t-23)^2/20)
}

# temp function as time passes
f_temp_t<-function(t){
  return(0.3*(t+3*pi) + 5*sin(2*pi/24*(t+3*pi)) +15)
}


# number of replications
n_sim <-100

end<-48

# true regression parameter for dist between stations
beta_dy<- -0.5


tp<-seq(0,48,length.out=2000)
pdf("final_plots/temp_t_func_bike_sim.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(tp,f_temp_t(tp), xlab="Hours", ylab ="Temperature", xaxt="n", type="l"
     ,cex.lab=2.5, cex.axis=2.5, cex.main=3, lwd = 2)
axis(1,seq(0,48,by=6),cex.lab=2.5, cex.axis=2.5)
dev.off()

# for saving and plotting
min_bh_t <- NULL
max_bh_t <- NULL
c_o<-NULL
pairwise_ev_cnt_o<-matrix(NA,nrow(info),n_sim)

model_est<-list()
c_model_est<-list()


for(it in 1:n_sim){
  print(paste("-------",it,"-----------"))
  
  info$shift <- NULL
  info$exit <- NULL
  
  simdat<-NULL # will contain events in chronological order
  
  simdat$shift<-NULL
  simdat$abs_te<-NULL
  
  #-------------------event simulation: original non-shifted process------------
  
  event_count<-0
  tl<-0
  while (tl < end) {
    mid_t<- tl + dt/2 # mid-point
    tu<- tl + dt # right extreme of sub-interval
    
    # computing intensity
    temp_mid_t<-f_temp_t(mid_t)
    l_sr <- exp(gc_temp(temp_mid_t)+ log(info$OSM_dist_min+1)*beta_dy)
    # inter-arrival time
    tm<-rexp(1,sum(l_sr))
    
    # "residual" values used to check if more than one event falls in the
    # same sub-interval
    dt_r <-dt
    tl_r<-tl
    
    while(tm < dt_r & (tl<end)){
      event_count<-event_count+1
      # which interaction
      link <- rmultinom(1,1, prob = l_sr/sum(l_sr))
      id <- which(link==1)
      info$tot_event_counts[id]<- info$tot_event_counts[id]+1
      st <- tl_r+tm # update event time
      temp_st<-f_temp_t(st)
      simdat <- rbind(simdat,c(unlist(unname(info[id,c(1:2,8)])),temp_st,st,id)) # save info about event
      
      
      # check if multiple events fall in the same sub-interval
      tl_r <- st # new left extreme
      dt_r <- tu - tl_r # delta_t for the remainder of the interval
      
      mid_t_r<- tl_r + dt_r/2 # updating mid-point
      temp_mid_t_r<-f_temp_t(mid_t_r)
      l_sr_r <- exp(gc_temp(temp_mid_t_r) + log(info$OSM_dist_min+1)*beta_dy)
      # inter-arrival time
      tm<-rexp(1,sum(l_sr_r))
    }
    
    tl<-tu # moving to the next sub-interval
  }
  
  simdat<-as.data.frame(simdat)
  colnames(simdat) <- c("sender","receiver","dist","temp","event_time","info")
  hist(simdat$event_time, main = "Event time distr. (original process)")
  
  #------------------- constructing shifted process --------------------
  
  # shift: one per possible interaction
  info$shift<- rexp(nrow(info),1/(mf*mean(simdat$event_time)))
  
  # exit
  info$exit<- max(simdat$event_time) + info$shift
  
  simdat$shift <- rep(NA, nrow(simdat))
  for(i in 1:nrow(simdat)){
    simdat$shift[i] <- info$shift[which((info$sender == simdat$sender[i]) &
                                          (info$receiver == simdat$receiver[i] ))]
  }
  # absolute event time
  simdat$abs_te <- simdat$event_time + simdat$shift 
  
  #------------------------ NCC sampling on the shifted process ------------------
  
  events<-NULL
  nonevents<-NULL
  removed<-NULL # non-informative observations
  
  n<-nrow(simdat)
    
  for (i in 1:n){
    which.event<-order(simdat$abs_te)[i]
    t.absevent<-simdat$abs_te[which.event]
    
    # interactions that have "entered" yet and not yet "exited"
    at.risk<-setdiff(which((info$shift<t.absevent) & (info$exit>t.absevent)), 
                     simdat$info[which.event])
    
    size_rs<-length(at.risk)
    if(size_rs>0){ # we exclude the case in which the risk set is only comprised of only the event happening
      
      ev<-simdat[which.event,1:5]
      events<-rbind(events, ev)
      
      #sampled non-event for NCC case (sampling 1 control per event)
      if(size_rs>1){
        non.event_info<-sample(at.risk,1) # indices in the info df of non event interac.
        
      }else{ 
        print("only one element in the risk set")
        non.event_info<- at.risk
      }
      
      non.tm <- t.absevent - info$shift[non.event_info]
     
      #temp_non_tm<-wdt$Temperature[which.min(abs(non.tm-wdt$DateTime))]
      temp_non_tm<-f_temp_t(non.tm)
      line<-cbind(info[non.event_info,c(1:2,8)],temp_non_tm,non.tm)
      colnames(line)<- c("sender","receiver","dist","temp","event_time")
      nonevents<-rbind(nonevents,line)
      
    }else{
      # depending on the shifts, 
      # early events might be in a condition of having no possibility
      # of sampling a non-event.
      # we keep track of which of these events end up being non-informative 
      removed<-c(removed,which.event)
    }
    
  }
  
  cat("number of events dropped", length(removed), "\n")
  
  #------------ preparing data for gam estimation ------------------------
  
  nn<-nrow(events)
  t.mat<-cbind(rep(1,nn),rep(-1,nn))
  temp<-cbind(events$temp,nonevents$temp)
  x_dy_diff <- log(events$dist+1) - log(nonevents$dist+1)
  hist(x_dy_diff)
  y<-rep(1,nn)
  
  l0.gam<-gam(y~ -1+ s(temp,by=t.mat) + x_dy_diff,
              family=binomial)
  model_est[[it]]<- l0.gam
  print(summary(l0.gam))
  #plot(l0.gam)
  
  #--------------------- C estimation ----------------------------
  # using predict to get the term of the linear predictor related to the smooth term
  
  # full and NCC estimators do not coincide
  # we will compute the NCC one
  
  # # estimated regression coefs.
  bhat_x_dy<-l0.gam$coefficients[1]

  
  newdat_c<- list(temp = cbind(simdat$temp, rep(0,n)), t.mat = cbind(rep(1,n),rep(0,n)),y = rep(1,n),
                  x_dy_diff =rep(0,n))
  pred_c<-predict(l0.gam, type = "terms", newdata = newdat_c)  #[,2] # we are only taking x_bh*beta_bh
  pred_c <-pred_c[order(simdat$event_time)]
  
  # sampling non-events in the original time scale
  # to compute the NCC-Breslow estimator
  nonevents_new<-NULL
  
  for (i in 1:n){
    which.event<-order(simdat$event_time)[i]
    non.event_info<-sample(1:nrow(info),1) # indices in the info df of non event interac.
    non.tm<-simdat$event_time[which.event] # non-event time is the actual event time from the original process
    
    temp_non_tm<-f_temp_t(non.tm)
    line<-cbind(info[non.event_info,c(1:2,8)],temp_non_tm,non.tm)
    colnames(line)<- c("sender","receiver","dist","temp","event_time")
    nonevents_new<-rbind(nonevents_new,line)
    
  }
  
  
  # denominator of breslow estimator: variable part for event term
  ev_p<- log(simdat$dist+1)*bhat_x_dy
  # denominator of breslow estimator: variable part for non-event term
  nonev_p<- log(nonevents_new$dist+1)*bhat_x_dy 
  # denominator of breslow estimator: global covariate part
  gc_p<-exp(pred_c)
  # multiplying factor size_risk_set / 2
  s_rs_h <-nrow(info)/2
  # estimator
  L0<-cumsum(1/(s_rs_h*gc_p*(exp(ev_p) + exp(nonev_p))))
  # c
  c_model<-lm(L0 ~ -1 + sort(simdat$event_time))
  c_model_est[[it]]<- c_model
  c_est<-c_model$coef
  c_o<-c(c_o,c_est)
  plot(sort(simdat$event_time),L0,type="l", main = "Cum. baseline haz. for original process (breslow) and lm fit")
  abline(0,c_est,col = "red")

  pairwise_ev_cnt_o[,it]<-info$tot_event_counts
  
  min_bh_t <- c(min_bh_t,min(simdat$event_time))
  max_bh_t <- c(max_bh_t,max(simdat$event_time))  
  
  
}



min_t_o<-max(min_bh_t)
max_t_o<-min(max_bh_t)

# ---------- predicting for plotting -------

np<-2000
rr_utp<-sapply(seq(min_t_o,max_t_o,length.out = np),f_temp_t)
temp_rr<-cbind(rr_utp,rep(0,np))

newdata_rr<-list(temp = temp_rr, t.mat = cbind(rep(1,np),rep(0,np)), y= rep(1,np),
                 x_dy_diff = rep(0,np))

logbh_rr_o<-NULL
for(i in 1:n_sim){
  m<-model_est[[i]]
  
  pred_rr_mat<-predict(m, type = "terms", newdata = newdata_rr,se.fit = TRUE) 
  pred_rr_se<-pred_rr_mat$se.fit[,2]
  pred_rr<-pred_rr_mat$fit[,2]  # we are only taking x_bh*beta_bh
  logbh_rr_o<-cbind(logbh_rr_o,pred_rr + log(c_o[i])) # c correction
  
}


# ------------- plotting temperature smooth effect ---------------

# plots in original scale 

pdf("final_plots/temp_bike_sim.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(rr_utp, logbh_rr_o[,1], t='l', ylim = c(-5,1),xlab ="Temperature", ylab = "Global effect"
     ,cex.lab=2.5, cex.axis=2.5, cex.main=3,col = "grey") 

for(i in 2:n_sim){
  
  lines(rr_utp, logbh_rr_o[,i], t='l', col = "grey")
  
}
lines(rr_utp,sapply(rr_utp,gc_temp), col = 'black',lwd = 3)
legend("bottomleft",c("Estimate", "True"),lty = c(1,1),lwd = c(1,2), col = c("grey","black"), cex=2.5, bty = "n")
dev.off()

# ------------------- boxplots for fixed effects ----------------------


beta_dist<-matrix(NA,n_sim,1)

for(i in 1:n_sim){
  m<-model_est[[i]]
  beta_dist[i]<-m$coefficients[1]
}
beta_dist<-as.data.frame(beta_dist)

pdf("final_plots/dyadic_bike_sim.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
boxplot(beta_dist, outline=FALSE, names="Distance between stations",show.names = T,
        cex.lab=2.5, cex.axis=2.5, cex.main=2, ylab = "Dyadic effect", ylim=c(-0.6,-0.4))
abline(beta_dy,0,lty = 2)
dev.off()
par(mfrow = c(1,1))


#-------------------descriptive plot showing distance/counts relationship----------------

data_graph<-info[,c(1:2,8)]
colnames(data_graph)<-c("from","to","dist")
data_graph$ev_cnt<-rowMeans(pairwise_ev_cnt_o)
data_graph<-data_graph[-which(data_graph$from==data_graph$to),]
g <- graph_from_data_frame(data_graph, directed=F, vertices=stations)

pdf("final_plots/station_dist_g_bike_sim.pdf", height = 8, width = 10)

edge_cols<-NULL
for (alpha in data_graph$ev_cnt) {
  edge_cols<-c(edge_cols,adjustcolor("red", alpha.f=alpha/max(data_graph$ev_cnt)))
}

ew = scale(data_graph$ev_cnt)

coords_ll<-as.data.frame(matrix(NA,20,2))
colnames(coords_ll)<-c("lng","lat")
for (i in 1:20) {
  coords_ll[i,]<- poss_station_names[poss_station_names$station_id==stations[i],c(4,3)]
}
coords_ll<-as.matrix(coords_ll)

plot(g, edge.arrow.size=0.1,
     vertex.size = 3, edge.width = ew*4,
     vertex.label.cex = 1.2, edge.color = edge_cols,
     vertex.label.color ="black",vertex.label = "",
     layout = coords_ll, vertex.color = "black")

dev.off()
