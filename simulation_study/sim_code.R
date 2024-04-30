# REM multiple simulation with specific baseline hazard (Weibull type -> polyn. bh.)
# and 4 additional covariates (one time-dependent global covariate, one individual according to sender, one dyadic,
# one endogenous and time dependent). All covariates are parametric with fixed effects
# except for the global time effect
# the end. var. is taken as a 0/1 indicating repetition


for(it in 1:n_sim){
  print(paste("-------",it,"-----------"))
  
  info$shift <- NULL
  info$exit <- NULL
  info$x_end<-0
  
  simdat<-NULL # will contain events in chronological order
  
  simdat$shift<-NULL
  simdat$abs_te<-NULL
  
  #-------------------event simulation: original non-shifted process------------
  
  
  event_count<-0
  tl<-0
  while (event_count < n) {
    mid_t<- tl + dt/2 # mid-point
    tu<- tl + dt # right extreme of sub-interval
    
    # computing intensity
    l_sr <- l0(mid_t)*exp(info$x*beta_x+ 
                                info$x_dy*beta_x_dy +
                                gc(mid_t)*beta_gc + info$x_end*beta_x_end)
    # inter-arrival time
    tm<-rexp(1,sum(l_sr))
    
    # "residual" values used to check if more than one event falls in the
    # same sub-interval
    dt_r <-dt
    tl_r<-tl
    
    while(tm < dt_r & event_count<n){
      event_count <- event_count + 1 # update event counter
      # which interaction
      link <- rmultinom(1,1, prob = l_sr/sum(l_sr))
      id <- which(link==1)
      st <- tl_r+tm # update event time
      simdat <- rbind(simdat,c(unlist(unname(info[id,])),gc(st),st,id)) # save info about event
      
      
      # update end. var (0/1 repetition)
      r <- info[id,]$receiver 
      s<- info[id,]$sender
      
      
      if(info$x_end[id]==0){
        info$x_end[info$receiver ==r & info$sender==s]<-1
      }
      
      
      # check if multiple events fall in the same sub-interval
      tl_r <- st # new left extreme
      dt_r <- tu - tl_r # delta_t for the remainder of the interval
      
      mid_t_r<- tl_r + dt_r/2 # updating mid-point
      
      l_sr_r <- l0(mid_t_r)*exp(info$x*beta_x + 
                                      info$x_dy*beta_x_dy + 
                                      gc(mid_t_r)*beta_gc + info$x_end*beta_x_end)
      # inter-arrival time
      tm<-rexp(1,sum(l_sr_r))
    }
    
    tl<-tu # moving to the next sub-interval
  }
  
  simdat<-as.data.frame(simdat)
  colnames(simdat) <- c("sender","receiver","x","x_dy","x_end","gc_x","event_time","info")
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
  removed<-NULL
  for (i in 1:n){
    which.event<-order(simdat$abs_te)[i]
    t.absevent<-simdat$abs_te[which.event]
    
    # interactions that have "entered" yet and not yet "exited"
    at.risk<-setdiff(which((info$shift<t.absevent) & (info$exit>t.absevent)), 
                     simdat$info[which.event])
    
    size_rs<-length(at.risk)
    if(size_rs>0){ # we exclude the case in which the risk set is only comprised of only the event happening
      
      ev<-simdat[which.event,1:7]
      events<-rbind(events, ev)
      
      #sampled non-event for NCC case (sampling 1 control per event)
      if(size_rs>1){
        non.event_info<-sample(at.risk,1) # indices in the info df of non event interac.
        
      }else{
        print("only one element in the risk set")
        non.event_info<- at.risk
      }
      
      non.tm <- t.absevent - info$shift[non.event_info]
      
      past_tw<-simdat[simdat$event_time < non.tm & simdat$sender==info[non.event_info,]$sender &
                        simdat$receiver == info[non.event_info,]$receiver,]

      
      if(nrow(past_tw)!=0){
        x_end_ne<-1
      }else{
        x_end_ne<-0
      }
    
      
      line<-cbind(info[non.event_info,1:4],x_end_ne,gc(non.tm),non.tm)
      colnames(line)<- c("sender","receiver","x","x_dy","x_end","gc_x","event_time")
      nonevents<-rbind(nonevents,line)
      
    }else{
      # depending on the shifts, 
      # early events might be in a condition of having no possibility
      # of sampling a non-event.
      # we keep track of which of these events end up being non-informative (if any)
      removed<-c(removed,which.event)
    }
    
  }
  
  cat("number of events dropped", length(removed), "\n")
  
  #------------ preparing data for gam estimation ------------------------
  
  nn<-nrow(events)
  t.mat<-cbind(rep(1,nn),rep(-1,nn))
  tms<-cbind(events$event_time,nonevents$event_time)
  x_diff <- events$x - nonevents$x
  x_dy_diff <- events$x_dy - nonevents$x_dy
  x_end_diff<- events$x_end - nonevents$x_end
  gc_x_diff <- events$gc_x - nonevents$gc_x
  hist(gc_x_diff)
  y<-rep(1,nn)
  
  l0.gam<-gam(y~ -1+ s(tms,by=t.mat, k = k_val) + x_diff + x_dy_diff+
                 gc_x_diff +  x_end_diff, family=binomial)
  model_est[[it]]<- l0.gam
  print(summary(l0.gam))
  
  #--------------------- C estimation ----------------------------
  # using predict to get the term of the linear predict regarding the smooth term
  
  # estimated regression coefs.
  bhat_x<-l0.gam$coefficients[1]
  bhat_x_dy<-l0.gam$coefficients[2]
  bhat_gc_x<-l0.gam$coefficients[3]
  bhat_x_end<-l0.gam$coefficients[4]
  
  newdat_c<- list(tms = cbind(simdat$event_time, rep(0,n)), t.mat = cbind(rep(1,n),rep(0,n)),y = rep(1,n),
                  x_diff = rep(0,n), x_dy_diff = rep(0,n), gc_x_diff=rep(0,n),
                  x_end_diff = rep(0,n))
  pred_c<-predict(l0.gam, type = "terms", newdata = newdat_c)[,5] # we are only taking x_bh*beta_bh
  pred_c <-pred_c[order(simdat$event_time)]
  
  # sampling non-events in the original time scale
  # to compute the NCC Breslow estimator
  
  nonevents_new<-NULL
  
  for (i in 1:n){
    which.event<-order(simdat$event_time)[i]
    non.event_info<-sample(1:nrow(info),1) # indices in the info df of non event interac.
    non.tm<-simdat$event_time[which.event] # non-event time is the actual event time from the original process
    
    past_tw<-simdat[simdat$event_time < non.tm
                    & simdat$sender==info[non.event_info,]$sender &
                      simdat$receiver == info[non.event_info,]$receiver,]

    
    if(nrow(past_tw)!=0){
      x_end_ne<-1
    }else{
      x_end_ne<-0
    }
    
    line<-cbind(info[non.event_info,1:4],x_end_ne, gc(non.tm),non.tm)
    colnames(line)<- c("sender","receiver","x","x_dy", "x_end" ,"gc_x","event_time")
    nonevents_new<-rbind(nonevents_new,line)
    
  }
  
  
  # denominator of breslow estimator: variable part for event term
  ev_p<- simdat$x*bhat_x + simdat$x_dy*bhat_x_dy + simdat$x_end*bhat_x_end
  # denominator of breslow estimator: variable part for non-event term
  nonev_p<- nonevents_new$x*bhat_x + nonevents_new$x_dy*bhat_x_dy + nonevents_new$x_end*bhat_x_end
  # denominator of breslow estimator: global covariate part
  gc_p<-exp(simdat$gc_x*bhat_gc_x)
  # denominator of breslow estimator: baseline haz.
  bh_p<- unname(exp(pred_c)) 
  # multiplying factor size_risk_set / 2
  s_rs_h <-nrow(info)/2
  # estimator
  L0<-cumsum(1/(s_rs_h*bh_p*gc_p*(exp(ev_p) + exp(nonev_p))))
  # c
  c_model<-lm(L0 ~ -1 + sort(simdat$event_time))
  c_model_est[[it]]<- c_model
  c_const<-c_model$coef
  c_o<-c(c_o,c_const)
  
  res_cum_bh<- cbind(res_cum_bh,L0)
  sim_et<-cbind(sim_et,simdat$event_time)
  plot(sort(simdat$event_time),L0,type="l", main = "Cum. baseline haz. for original process (breslow) and lm fit")
  abline(0,c_const,col = "red")

  min_bh_t <- c(min_bh_t,min(simdat$event_time))
  max_bh_t <- c(max_bh_t,max(simdat$event_time))  
  
  
}


