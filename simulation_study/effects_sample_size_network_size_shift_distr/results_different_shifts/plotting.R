library(mgcv)
options(warn=1)

load(file = "../l0.RData")
load(file = "../beta_x.RData")
load(file = "../beta_x_dy.RData")
load(file = "../beta_x_end.RData")
load(file = "../beta_gc.RData")

dir.create("final_plots")

# proportionality constant for the average shifts
sample_sizes<-c(0.001,0.01,0.1,1,10,100,1000)


n_sim<-100


l2_norm_exp_per_mf_simulated_tp<-NULL

for(mf in sample_sizes){
  cat("######## mf = ", mf, " ########", "\n")
  
  load(file = paste("./mf_",mf,"/sim_et.RData", sep = ""))
  
  load(file = paste("./mf_",mf,"/logbh_rr_o_simulated_tp.RData", sep = ""))
  
  l2_norm_exp_per_repl_simulated_tp<-NULL
  
  for (i in 1:n_sim) {
    
    rr_utp_simulated_tp <-sim_et[,i]
    n<-length(rr_utp_simulated_tp)
    
    left_bounds_exp_simulated_tp<-(log(sapply(rr_utp_simulated_tp[-n],l0))-logbh_rr_o_simulated_tp[-n,i])^2
    right_bounds_exp_simulated_tp<- (log(sapply(rr_utp_simulated_tp[-1],l0))-logbh_rr_o_simulated_tp[-1,i])^2
    
    height_simulated_tp<-diff(rr_utp_simulated_tp)
    
    l2_norm_exp_per_repl_simulated_tp<-c(l2_norm_exp_per_repl_simulated_tp, sum((1/(max(rr_utp_simulated_tp)-min(rr_utp_simulated_tp)))*((height_simulated_tp/2)*(right_bounds_exp_simulated_tp+left_bounds_exp_simulated_tp))))
    
  }
  
  l2_norm_exp_per_mf_simulated_tp <- cbind(l2_norm_exp_per_mf_simulated_tp,sqrt(l2_norm_exp_per_repl_simulated_tp))
  
  
  # ---------plots in log scale for the smooth term -----------------
  
  
  par(mfrow = c(1,1))
  
  plot(sim_et[,1], logbh_rr_o_simulated_tp[,1], t='l',ylim=c(-3,3), xlab ='Time', ylab = "Global time effect (original scale)", col = "grey") 
  
  for(i in 2:n_sim){
    
    lines(sim_et[,i], logbh_rr_o_simulated_tp[,i], t='l', col = "grey")
    
  }
 
  lines(rr_utp_simulated_tp,log(l0(rr_utp_simulated_tp)), col = 'red',lwd = 2)
  legend("topleft",c("Estimate", "True"),lty = c(1,1),lwd = c(1,2), col = c("grey","red"), cex=0.7)
  
  
 
  
}


# ---------- checking regression parameter estimates--------



# comparing actor-level exogenous var across different mean shifts

beta_xs_s<-matrix(NA,n_sim,7)
for(mf in sample_sizes){
  cat("######## mf = ", mf, " ########", "\n")
  
  load(file = paste("./mf_",mf,"/model_est.RData", sep = ""))
  
  for(i in 1:n_sim){
    m<-model_est[[i]]
    beta_xs_s[i,which(sample_sizes==mf)]<-m$coefficients[1]
  }
  beta_xs_s<-as.data.frame(beta_xs_s)
  colnames(beta_xs_s)<-c("mf_0.001", "mf_0.01", "mf_0.1", "mf_1","mf_10", "mf_100","mf_1000")

}
  pdf("final_plots/different_mf_node_ex.pdf", height = 8, width = 12)
  par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
  boxplot(beta_xs_s, outline=FALSE, names = sample_sizes, show.names=TRUE, xlab=expression(nu), ylab = "Node-specific exogenous covariate effect", 
          cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
  abline(beta_x,0,lty = 2)
  #,ylim = c(0.3,0.7)
  dev.off()
  par(mfrow = c(1,1))
  
  
  
  # comparing dyadic exogenous var across different mean shifts
  
  beta_xs_sr<-matrix(NA,n_sim,7)
  for(mf in sample_sizes){
    cat("######## mf = ", mf, " ########", "\n")
    
    load(file = paste("./mf_",mf,"/model_est.RData", sep = ""))
    
    for(i in 1:n_sim){
      m<-model_est[[i]]
      beta_xs_sr[i,which(sample_sizes==mf)]<-m$coefficients[2]
    }
    beta_xs_sr<-as.data.frame(beta_xs_sr)
    colnames(beta_xs_sr)<-c("mf_0.001", "mf_0.01","mf_0.1", "mf_1","mf_10", "mf_100","mf_1000")

  }
  
  pdf("final_plots/different_mf_dy_ex.pdf", height = 8, width = 12)
  par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
  boxplot(beta_xs_sr, outline=FALSE, names = sample_sizes, show.names=TRUE, xlab=expression(nu),ylab = "Dyadic exogenous covariate effect", 
          cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
  abline(beta_x_dy,0,lty = 2)
  #,ylim = c(-1.25,-0.75)
  dev.off()
  par(mfrow = c(1,1))
  
  
  
  
  # comparing global var across different mean shifts
  
  beta_xs_gc<-matrix(NA,n_sim,7)
  for(mf in sample_sizes){
    cat("######## mf = ", mf, " ########", "\n")
    
    load(file = paste("./mf_",mf,"/model_est.RData", sep = ""))
    
    for(i in 1:n_sim){
      m<-model_est[[i]]
      beta_xs_gc[i,which(sample_sizes==mf)]<-m$coefficients[3]
    }
    beta_xs_gc<-as.data.frame(beta_xs_gc)
    colnames(beta_xs_gc)<-c("mf_0.001", "mf_0.01","mf_0.1", "mf_1","mf_10", "mf_100","mf_1000")

  }
  
  
  pdf("final_plots/different_mf_gc.pdf", height = 8, width = 12)
  par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
  boxplot(beta_xs_gc, outline=FALSE, names = sample_sizes, show.names=TRUE,
         xlab=expression(nu), ylab = "Global covariate effect", 
          cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
  abline(beta_gc,0,lty = 2)
  #, ylim = c(-1,-0.5)
  dev.off()
  
  par(mfrow = c(1,1))
  
  # comparing endogenous var across different mean shifts
  
  beta_xs_end<-matrix(NA,n_sim,7)
  for(mf in sample_sizes){
    cat("######## mf = ", mf, " ########", "\n")
    
    load(file = paste("./mf_",mf,"/model_est.RData", sep = ""))
    
    for(i in 1:n_sim){
      m<-model_est[[i]]
      beta_xs_end[i,which(sample_sizes==mf)]<-m$coefficients[4]
    }
    beta_xs_end<-as.data.frame(beta_xs_end)
    colnames(beta_xs_end)<-c("mf_0.001", "mf_0.01","mf_0.1", "mf_1","mf_10", "mf_100","mf_1000")

  }
  
  pdf("final_plots/different_mf_dy_end.pdf", height = 8, width = 12)
  par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
  boxplot(beta_xs_end, outline=FALSE, names = sample_sizes, show.names=TRUE, xlab=expression(nu), ylab = "Dyadic endogenous covariate effect", 
          cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
  abline(beta_x_end,0,lty = 2)
  #, ylim = c(0.75,2.3)
  dev.off()
  par(mfrow = c(1,1))
  
  # l2-norm values for global time effect evaluation for different mean shifts
  
  pdf("final_plots/different_mf_l2_gte.pdf", height = 8, width = 12)
  par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
  boxplot(log(l2_norm_exp_per_mf_simulated_tp), outline=FALSE, names = sample_sizes, show.names=TRUE , xlab=expression(nu), ylab = "log(L2)", 
          cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
  #, ylim = c(-4.5,1)
  dev.off()
  
  pdf("final_plots/different_mf_l2_gte_no_smallest.pdf", height = 8, width = 12)
  par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
  boxplot(log(l2_norm_exp_per_mf_simulated_tp)[,-c(1)], outline=FALSE, names = sample_sizes[-c(1)], show.names=TRUE , xlab=expression(nu), ylab = "log(L2)", 
          cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
  #, ylim = c(-4.5,1)
  dev.off()
  
  # plots for residual cumulative baseline hazard
  for(mf in sample_sizes){
    load(file = paste("./mf_",mf,"/c_model_est.RData", sep = ""))
    
    par(mfrow=c(1,1))
    pdf(paste("final_plots/sim_res_cum_bh_mf_",mf,".pdf",sep = ""), height = 8, width = 12)
    par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
    plot(c_model_est[[1]]$model$`sort(simdat$event_time)`,c_model_est[[1]]$model$L0,type="l", 
         cex.main = 3,cex.axis = 2.5, cex.lab = 2.5,  ylab = "Breslow estimator",
         xlab = "Time", main = bquote(paste( nu ," = ",.(mf), sep = "")), col = "grey", lwd = 3)
    abline(0,c_model_est[[1]]$coefficients,lwd = 1.5)
    legend("bottomright",c("Estimate", "Fitted line"),lty = c(1,1),lwd = c(3,1.5), col = c("grey","black"), cex=2.5,bty = "n")
    dev.off()
    
  }
# 
