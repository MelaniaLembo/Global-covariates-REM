#--------------------- C estimation ----------------------------
# using predict to get the term of the linear predictor regaring the  
# smooth covariates (both global and pair dependent) and the global time effect

# full and NCC estimators do not coincide
# we will compute the NCC one: more efficient computationally

# estimated regression coefs. for parametric covariates
bhat_start_comp<-l0.gam$coefficients[1]
bhat_end_comp<-l0.gam$coefficients[2]


n<-nrow(merged_data)
newdat_c_events<- list(tms = cbind(merged_data$DateTime, rep(0,n)), t.mat = cbind(rep(1,n),rep(0,n)),y = rep(1,n),
                       temp = cbind(merged_data$Temperature, rep(0,n)),
                       precip = log(cbind(merged_data$Precip., rep(0,n))+1), start_comp = rep(0,n),end_comp = rep(0,n),
                       OSM_dist = log(cbind(merged_data$OSM_dist_min, rep(0,n))+1), rec = exp(-(1/(2*med_rec_events))*cbind(merged_data$time_since_rec_pair, rep(0,n))),
                       rep = exp(-(1/(2*med_rep_events))*cbind(merged_data$time_since_same_pair, rep(0,n))),
                       tod = cbind(merged_data$tod,rep(0,n)))



pred_c_events<-predict(l0.gam, type = "terms", newdata = newdat_c_events)[,3:9]# we are only taking smooth terms
pred_c_events <-pred_c_events[order(merged_data$DateTime),]


# denominator of breslow estimator: parametric variable part for event term
ev_p<- merged_data$start_comp_st_min*bhat_start_comp + merged_data$end_comp_st_min*bhat_end_comp 
 
# denominator of breslow estimator: parametric variable part for non-event term
nonev_p<- nonevents_new$start_comp_st_min*bhat_start_comp + nonevents_new$end_comp_st_min*bhat_end_comp 
  
# denominator of breslow estimator: global time effect (same for events and nonevents)
bh_p<- unname(exp(pred_c_events[,1]))
# denominator of breslow estimator: smooth global covariates. (same for events and nonevents)
sgc_p<- unname(pred_c_events[,c(2:3,7)])


# denominator of breslow estimator: smooth (s,r)-dependent covariates
# we need terms predicted values for non-event as well
newdat_c_nonevents<- list(tms = cbind(merged_data$DateTime, rep(0,n)), t.mat = cbind(rep(1,n),rep(0,n)),y = rep(1,n),
                          temp = cbind(merged_data$Temperature, rep(0,n)),
                          precip = log(cbind(merged_data$Precip., rep(0,n))+1), start_comp = rep(0,n),end_comp = rep(0,n),
                          OSM_dist = log(cbind(nonevents_new$OSM_dist_min, rep(0,n))+1), rec = exp(-(1/(2*med_rec_events))*cbind(nonevents_new$time_since_rec_pair, rep(0,n))),
                          rep = exp(-(1/(2*med_rep_events))*cbind(nonevents_new$time_since_same_pair, rep(0,n))),
                          tod = cbind(merged_data$tod,rep(0,n)))

pred_c_nonevents<-predict(l0.gam, type = "terms", newdata = newdat_c_nonevents)[,6:8]# we are only taking smooth terms that depend on s,r

ssr_p_ev<-apply(pred_c_events[,4:6],1,sum)
ssr_p_nonev<-apply(pred_c_nonevents,1,sum)
# multiplying factor size_risk_set / 2
s_rs_h <-nrow(info)/2
# estimator
gc_term<-exp(apply(sgc_p,1,sum))
L0<-cumsum(1/(s_rs_h*bh_p*gc_term*(exp(ev_p + ssr_p_ev) + exp(nonev_p + ssr_p_nonev))))
# c
c_est<-lm(L0~ -1 + sort(merged_data$DateTime))




