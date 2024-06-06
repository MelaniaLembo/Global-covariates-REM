library(mgcv)
options(warn=1)

load(file = "../l0.RData")

# number of events 
sample_sizes<-c(9000)
i<-17 # selecting one replicate for illustration purposes

for(n in sample_sizes){
  cat("######## n = ", n, " ########", "\n")
  
  
  load(file = paste("./n_",n,"/logbh_rr_o_simulated_tp.RData", sep = ""))
  
  
  load(file = paste("./n_",n,"/sim_et.RData", sep = ""))
  
  # ---------plots in log scale of the smooth term --------
  
  par(mfrow = c(1,1))
  

  # Figure 1 in the paper
  pdf("final_plots/single_sim_example.pdf", height = 8, width = 12)
  par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
  
  plot(sim_et[,i], logbh_rr_o_simulated_tp[,i], t='l',ylim=c(-0.65,0.55), xlab ='Time', ylab = "Global smooth effect", lwd = 4, 
       cex.main = 3,cex.axis = 2.5, cex.lab = 2.5, lty = 2) 
  
  
  lines(sim_et[,i],log(l0(sim_et[,i])),lwd = 5)
  polygon(c(sim_et[,i], rev(sim_et[,i])),c(log(l0(sim_et[,i])),rev(logbh_rr_o_simulated_tp[,i])), col = "grey", border = NA)
  legend("bottomright",c("Estimate", "True"),lty = c(2,1),lwd = c(4,5), col = c("black","black"), cex=2.5,bty = "n")

  dev.off()
  
  
  
}
