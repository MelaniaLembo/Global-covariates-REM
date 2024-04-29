library(mgcv)
options(warn=1)

set.seed(14)


# true global time effect
l0<-function(t){
    exp(t)
}

save(l0, file = "l0.RData")


# delta t for sub.interval division
dt <- 0.00001

# number of nodes
# same set for senders and receivers
p <- 15

# number of events 
n<-3000

# dataset of all possible pairs
info <- cbind(rep(1:p,each=p),rep(1:p,p),0,0,0) # all possible interactions
sl <- which(info[,1] == info[,2]) 
info <- info[-sl,] # removing self-loops
info <- as.data.frame(info)
colnames(info) <- c("sender","receiver","x","x_dy","x_end")

# variables
x <- rnorm(p,5) # individual variable
info$x <- x[info[,1]] # value for each interaction according to sender
info$x_dy <- abs(x[info[,1]] - x[info[,2]]) # dyadic variable (absolute difference between sender and receiver)

# global covariate definition
ncp<-400
max_t<-1
cutpoints<-seq(0,max_t,length.out=ncp)
nlevels<-100
extreme_value<-1
possible_values<-seq(-extreme_value,extreme_value, length.out=nlevels)
pv<-sample(possible_values, ncp-1, replace = TRUE)
vs<-rep(rep(1/setdiff(seq(-4,4),0),each=3), length.out = ncp-1)
gc<-function(t){
  for(i in 1:(ncp-1)){
    if(t%%max_t>=cutpoints[i] && t%%max_t<cutpoints[i+1]){
      return(pv[i]+vs[i])
    }
  }
}


times<-seq(0,max_t, length.out=2*ncp)
plot(times, sapply(times, gc),type = "s", xlab = "Time", ylab = "Global covariate")

hist(info$x, xlab = "Actor-level (sender) exogenous covariate values", main="")
hist(info$x_dy, xlab = "Dyadic exogenous covariate values", main ="")

# true value of regression parameters
beta_x <- 0.5
save(beta_x,file = "beta_x.RData")
beta_x_dy <- -1
save(beta_x_dy,file = "beta_x_dy.RData")
beta_gc<- -0.7
save(beta_gc,file = "beta_gc.RData")
beta_x_end<- 1.5
save(beta_x_end,file = "beta_x_end.RData")

# number of replications
n_sim <-100


# proportionality constant for the average mean shift
sample_sizes<-c(0.001,0.01,0.1,1,10,100,1000)

for(mf in sample_sizes){
  cat("######## mf = ", mf, " ########", "\n")
  set.seed(17)
  
  # for saving and plotting
  min_bh_t <- NULL
  max_bh_t <- NULL
  c_o<-NULL
  res_cum_bh<-NULL
  sim_et<-NULL
  model_est<-list()
  c_model_est<-list()
  
  k_val<-10
  source(file ="weibull_a_2_b_0.5_sim.R")
  save(c_o, file = paste("results_different_shifts/mf_",mf,"/c_o.RData", sep = ""))
  save(model_est, file = paste("results_different_shifts/mf_",mf,"/model_est.RData", sep = ""))
  save(c_model_est, file = paste("results_different_shifts/mf_",mf,"/c_model_est.RData", sep = ""))
  save(res_cum_bh, file = paste("results_different_shifts/mf_",mf,"/res_cum_bh.RData", sep = ""))
  save(sim_et, file = paste("results_different_shifts/mf_",mf,"/sim_et.RData", sep = ""))
  
  min_t_o<-max(min_bh_t)
  save(min_t_o, file = paste("results_different_shifts/mf_",mf,"/min_t_o.RData", sep = ""))
  max_t_o<-min(max_bh_t)
  save(max_t_o, file = paste("results_different_shifts/mf_",mf,"/max_t_o.RData", sep = ""))
  
  
}




