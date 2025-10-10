# add plots for random effects visualization
Sys.setlocale("LC_TIME", "English")
library(mgcv) 

seed_option<-31
#seed_option<-as.integer(readline(prompt = "Enter wanted seed (to be chosen among {31,17,14}): "))

load(file = "../data_pre_processing/initial_data/starting_timepoint.RData")
load(file = "../data_pre_processing/initial_data/merged_data_new_nosl60.RData")
load(file = paste("results_nosl60/gam_model_fit_no_global_",seed_option,".RData", sep=""))
load(file = paste("results_nosl60/res_basehaz_",seed_option,"_no_global.RData", sep=""))
load(file = "results_nosl60/median_deltat_recip_events.RData")
load(file = "results_nosl60/median_deltat_repet_events.RData")

new_subfolder <- "final_plots_noglobal"
dir.create(new_subfolder, showWarnings = FALSE)
# ---------------------plots of estimated smooths ---------------
plt <- {
  pdf(NULL)
  res <- plot(l0.gam_no_global)
  invisible(dev.off())
  res
}



pdf("final_plots_noglobal/bike_data_dist_noglobal.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(exp(plt[[1]]$x)-1,plt[[1]]$fit, type = "l", xlab = "Route distance (mins.)", 
     ylab = "Distance smooth effect", 
     ylim = c(min(plt[[1]]$fit  - plt[[1]]$se)+10,
              max(plt[[1]]$fit  + plt[[1]]$se)), 
     xlim = c(0,160),
     cex.main = 2,cex.axis = 2.3, cex.lab = 2.5)
lines(exp(plt[[1]]$x)-1,plt[[1]]$fit  + plt[[1]]$se, lty=2)
lines(exp(plt[[1]]$x)-1,plt[[1]]$fit  - plt[[1]]$se, lty=2)
legend("bottomleft", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
#par(mfrow=c(1,1))
dev.off()

# #pdf("final_plots/bike_data_precipitation.pdf", height = 8, width = 12)
# plot(plt[[3]]$x,plt[[3]]$fit, type = "l", xlab = "Precipitation", 
#      ylab = "Precipitation smooth effect", 
#      ylim = c(min(plt[[3]]$fit  - plt[[3]]$se),
#               max(plt[[3]]$fit  + plt[[3]]$se)))
# lines(plt[[3]]$x,plt[[3]]$fit  + plt[[3]]$se, lty=2)
# lines(plt[[3]]$x,plt[[3]]$fit  - plt[[3]]$se, lty=2)
# legend("bottomleft", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
# #dev.off()

# endogenous smooth terms (reciprocity and inertia)

#par(mfrow=c(2,1))
pdf("final_plots_noglobal/bike_data_recip_noglobal.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot((-(2*med_rec_events)*log(plt[[2]]$x))/60,plt[[2]]$fit, type = "l",
     xlab = "Time since last reciprocal event (hrs.)", ylab = "Reciprocity", xaxt="n", 
     ylim = c(min(plt[[2]]$fit  - plt[[2]]$se)+1,
              max(plt[[2]]$fit  + plt[[2]]$se)),
     xlim = c(0,24),
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines((-(2*med_rec_events)*log(plt[[2]]$x))/60,plt[[2]]$fit  + plt[[2]]$se, lty=2)
lines((-(2*med_rec_events)*log(plt[[2]]$x))/60,plt[[2]]$fit  - plt[[2]]$se, lty=2)
abline(v=9,lty = 3)
axis(1, at = seq(0,600, by = 12),cex.axis = 2.5, cex.lab = 2.5)
legend("topright", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()

pdf("final_plots_noglobal/bike_data_repet_noglobal.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot((-(2*med_rep_events)*log(plt[[3]]$x))/60,plt[[3]]$fit, type = "l",
     xlab = "Time since last same event (hrs.)", ylab = "Repetition", xaxt="n",
     ylim = c(min(plt[[3]]$fit  - plt[[3]]$se)+0.75,
              max(plt[[3]]$fit  + plt[[3]]$se)),
     xlim = c(0,84),
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines((-(2*med_rep_events)*log(plt[[3]]$x))/60,plt[[3]]$fit  + plt[[3]]$se, lty=2)
lines((-(2*med_rep_events)*log(plt[[3]]$x))/60,plt[[3]]$fit  - plt[[3]]$se, lty=2)
#abline(v=10)
abline(v=9, lty = 3)
abline(v=24, lty = 3)
abline(v = 48,lty = 3)
abline(v = 72,lty = 3)
axis(1, c(at = c(9,seq(0,600, by = 24))),cex.axis = 2.5, cex.lab = 2)
legend("topright", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()
#par(mfrow=c(1,1))




# residual baseline hazard 
par(mfrow=c(1,1))
pdf("final_plots_noglobal/bike_data_res_cum_bh_noglobal.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(sort(merged_data$DateTime*60 + starting_timepoint),c_est_noglobal$model$L0,type="l", xaxt="n", 
     cex.main = 2,cex.axis = 2, cex.lab = 2,  ylab = "Breslow estimator",
     xlab = "Time", col = "grey", lwd = 2)
r <- as.POSIXct(round(range(merged_data$DateTime*60 + starting_timepoint), "days"))
axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format = "%d %b",cex.axis = 2, cex.lab = 2)
# calendar day on the x-axis
lines(sort(merged_data$DateTime*60 + starting_timepoint),sort(merged_data$DateTime)*c_est_noglobal$coefficients,col = "black", lwd = 1.5)
legend("bottomright",c("Estimate", "Fitted line"),lty = c(1,1),lwd = c(3,1.5), col = c("grey","black"), cex=1.5,bty = "n")
dev.off()

c_const<-c_est_noglobal$coefficients
cat("Slope of line fitted to residual cum. baseline hazard", c_const)

summary(l0.gam_no_global)
