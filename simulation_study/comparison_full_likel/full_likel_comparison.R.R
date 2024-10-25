###################################################################

library(brglm2)
set.seed(26)

# inverse cumulative distribution function for a weibull
# distributed X with scale = 1 and some shape parameter
# conditioned onf X > t0
Finv<-function(p,t0=0,k=shape){ (t0^k-log(1-p))^(1/k)}

# simulates the data
# and performn NCC on the shifted process
simdat<-function(n,p,shape,tm=0.01){
  dat<-NULL
  n_noninf_evs <- 0
  tms<-Finv(runif(p^2),t0=tm)
  for (i in 1:n){
    mintm<-min(tms)
    which.pair<-which.min(tms)
    event<-c(1,log(tm),log(mintm-tm),1,which.pair)
    nonevent<-c(0,log(tm),log(mintm-tm),p^2-1,NA)
    tm<-mintm
    newtm<-Finv(runif(1),tm)
    tms[tms==mintm]<-newtm
    dat<-rbind(dat,event,nonevent)
  }
  colnames(dat)<-c("y","x", "offset","weight", "event_pair_index")
  dat<-as.data.frame(dat)
  
  
  shifts<-rexp(p^2,1/(mf*mean(exp(dat[dat$y==1,]$x))))
  exits<-shifts + max(exp(dat[dat$y==1,]$x))
  
  shifted_tms<- rep(NA, nrow(dat))
  sampled_nonev_pair_index<- rep(NA, nrow(dat))
  nonev_tms<- rep(NA, nrow(dat))
  dat<-cbind(dat,shifted_tms)
  dat<-cbind(dat,sampled_nonev_pair_index)
  dat<-cbind(dat,nonev_tms)
  
  dat$shifted_tms[dat$y==1]<- exp(dat[dat$y==1,]$x) + shifts[dat[dat$y==1,]$event_pair_index]
  
  # NCC sampling in the shifted process
  for(i in 1:n){
    stm<-order(dat$shifted_tms)[i]
    ev_ind<-dat$event_pair_index[stm]
    risk_set<-setdiff(which(shifts<dat$shifted_tms[stm] & exits>dat$shifted_tms[stm])
                      ,ev_ind)
    if(length(risk_set)>1){
      nonev_ind<-sample(risk_set,1)
      dat$sampled_nonev_pair_index[stm]<-nonev_ind
      dat$nonev_tms[stm]<-dat$shifted_tms[stm]-shifts[nonev_ind]
    }else if (length(risk_set)==1){
      nonev_ind<-risk_set
      dat$sampled_nonev_pair_index[stm]<-nonev_ind
      dat$nonev_tms[stm]<-dat$shifted_tms[stm]-shifts[nonev_ind]
    } else{
      print("Non-event could NOT be sampled !")
      n_noninf_evs<- n_noninf_evs + 1
    }
    
  }
  
  return(list(dat,n_noninf_evs))
}

mf<-1 # mean shift = average event time 
p<-5 # number of nodes
# shape param
shape<-0.1
# bias corrections type
corr_type = "correction"
n<-c(100,150,250,375,500,750,1000,2500,5000) # sample size range

n.rep<-5000 # number of replications

bc_slp_full<-matrix(ncol=length(n),nrow=n.rep)
bc_slp_full.s<-matrix(ncol=length(n),nrow=n.rep)

bc_slp_ss_partial<-matrix(ncol=length(n),nrow=n.rep)
bc_slp_ss_partial.s<-matrix(ncol=length(n),nrow=n.rep)

n_noninf_evs_all<-matrix(ncol=length(n),nrow=n.rep)

for (j in 1:length(n)){
  for (i in 1:n.rep){
    res <- simdat(n[j],p,shape)
    dat<-res[[1]]
    n_noninf_evs_all[i,j]<-res[[2]]
    
    ######## BIAS-CORRECTED FULL LIKELIHOOD ###############
    bc_fit<-glm(y~1 + x, offset = offset, family = poisson,weights = weight, data=dat,
                method = "brglmFit", type = corr_type)
    bc_coefs<-summary(bc_fit)$coef
    bc_slp_full[i,j]<-bc_coefs[2,1]
    bc_slp_full.s[i,j]<-bc_coefs[2,2]
    
    
    ######## BIAS-CORRECTED SHIFTED SAMPLED PARTIAL LIKELIHOOD ###############
    
    # defining covariates 
    indices_inf_evs<-which(dat$y==1 & !(is.na(dat$sampled_nonev_pair_index)))
    n_inf_evs<-length(indices_inf_evs)
    y_dl<-rep(1,n_inf_evs)
    t.mat<-cbind(rep(1,n_inf_evs),rep(-1,n_inf_evs))
    tms<-cbind(dat$x[indices_inf_evs],log(dat$nonev_tms[indices_inf_evs]))
    tms_diff<-tms[,1]-tms[,2]
    
    bc_slp_ss_partial_fit<-glm(y_dl ~ - 1 + tms_diff, family = binomial,
                        method = "brglmFit", type = corr_type)
    bc_slp_ss_partial[i,j]<- summary( bc_slp_ss_partial_fit)$coef[1]
    bc_slp_ss_partial.s[i,j]<- summary( bc_slp_ss_partial_fit)$coef[2]
    
  }
}

save(list=ls(), file="res_shape_final_0.1.rda")

bp<-function(y,x,...){
  y.vec<-c(y)
  x.vec<-rep(x,each=nrow(y))
  boxplot(y.vec~x.vec,...)
}

# PLOTS: LOG-SCALE

pdf("DEF_shape_0.1_slope_comp_logscale.pdf", height = 8, width = 12)
par(mar=c(5.5,6,4.5,1)+1, mgp=c(5,2,0))
plot(n,colMeans(bc_slp_full),type="l", ylim =c(-0.92,-0.86), ylab = "Slope", xlab = "Sample size"
     ,cex.axis = 2.5, cex.lab = 2.5,log = "x")
abline(shape-1,0,col="darkorange",lwd=2)
lines(n,colMeans(bc_slp_full) + 2*colMeans(bc_slp_full.s)/sqrt(n.rep), lty = 2)
lines(n,colMeans(bc_slp_full) - 2*colMeans(bc_slp_full.s)/sqrt(n.rep), lty = 2)

lines(n,colMeans(bc_slp_ss_partial), col = "blue")
lines(n,colMeans(bc_slp_ss_partial) + 2*colMeans(bc_slp_ss_partial.s)/sqrt(n.rep), lty = 2, col = "blue")
lines(n,colMeans(bc_slp_ss_partial) - 2*colMeans(bc_slp_ss_partial.s)/sqrt(n.rep), lty = 2, col = "blue")

legend("topright", c("Full likelihood", "Partial likelihood", "True"),lty = c(1,1,1,1,1), 
       col = c("black","blue", "darkorange"), cex=2,bty = "n")
dev.off()

