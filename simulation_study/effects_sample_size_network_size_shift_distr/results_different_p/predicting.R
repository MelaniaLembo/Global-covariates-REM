library(mgcv)
options(warn=1)


# number of nodes
sample_sizes<-c(5,15,45)

n_sim<-100

np<-200



for(p in sample_sizes){
  cat("######## p = ", p, " ########", "\n")
  
  load(file = paste("./p_",p,"/c_model_est.RData", sep = ""))
  
  load(file = paste("./p_",p,"/model_est.RData", sep = ""))
  load(file = paste("./p_",p,"/sim_et.RData", sep = ""))
  
  n<-nrow(sim_et)
  
  
  #--------------- predicting global time effect from gam results ---------------------
  
  logbh_rr_o_simulated_tp<-NULL
  
  for(i in 1:n_sim){
    m<-model_est[[i]]
    c_o_i<-unname(c_model_est[[i]]$coefficients)
    
    newdata_rr_simulated_tp<-list(tms = cbind(sim_et[,i],rep(0,n)) , t.mat = cbind(rep(1,n),rep(0,n)), y= rep(1,n),
                                  x_diff = rep(0,n), x_dy_diff = rep(0,n), gc_x_diff=rep(0,n),
                                  x_end_diff = rep(0,n))
    
    pred_rr_mat_simulated_tp<-predict(m, type = "terms", newdata = newdata_rr_simulated_tp ,se.fit = TRUE)
    logbh_rr_o_simulated_tp<-cbind(logbh_rr_o_simulated_tp,pred_rr_mat_simulated_tp$fit[,5]+ log(c_o_i))
    
  }
  
  save(logbh_rr_o_simulated_tp, file = paste("./p_",p,"/logbh_rr_o_simulated_tp.RData", sep = ""))
  
}
