# add plots for random effects visualization
Sys.setlocale("LC_TIME", "English")
library(mgcv) 

seed_option<-31
#seed_option<-as.integer(readline(prompt = "Enter wanted seed (to be chosen among {31,17,14}): "))

load(file = "../data_pre_processing/initial_data/starting_timepoint.RData")
load(file = "../data_pre_processing/initial_data/merged_data_new_nosl60.RData")
load(file = paste("results_nosl60/gam_model_fit_",seed_option,".RData", sep=""))
load(file = paste("results_nosl60/res_basehaz_",seed_option,".RData", sep=""))
load(file = "results_nosl60/median_deltat_recip_events.RData")
load(file = "results_nosl60/median_deltat_repet_events.RData")

new_subfolder<-"final_plots"
dir.create(new_subfolder)

# ---------------------plots of estimated smooths ---------------
plt <- {
  pdf(NULL)
  res <- plot(l0.gam)
  invisible(dev.off())
  res
}


# global time effect
#par(mfrow=c(2,1))
pdf("final_plots/bike_data_bh.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(plt[[1]]$x*60 + starting_timepoint,plt[[1]]$fit, type = "l", xlab = "Time ", 
     ylab = "Global time effect (log-scale)", xaxt="n", 
     ylim = c(min(plt[[1]]$fit  - plt[[1]]$se),
              max(plt[[1]]$fit  + plt[[1]]$se)), 
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
r <- as.POSIXct(round(range(plt[[1]]$x*60 + starting_timepoint), "days"))
# calendar day on the x-axis
axis.POSIXct(1, at=seq(r[1], r[2], by="days"), format = "%d %b",cex.axis = 2.5, cex.lab = 2.5)
lines(plt[[1]]$x*60 + starting_timepoint,plt[[1]]$fit  + plt[[1]]$se, lty=2)
lines(plt[[1]]$x*60 + starting_timepoint,plt[[1]]$fit  - plt[[1]]$se, lty=2)
legend("bottomleft", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()


# wrt day of the week instead
plot(plt[[1]]$x*60 + starting_timepoint,plt[[1]]$fit, type = "l", xlab = "Time ", 
     ylab = "Global time effect (log-scale)", xaxt="n", 
     ylim = c(min(plt[[1]]$fit  - plt[[1]]$se),
              max(plt[[1]]$fit  + plt[[1]]$se)))
r <- as.POSIXct(round(range(plt[[1]]$x*60 + starting_timepoint), "days"))
t<-seq(r[1], r[2], by="days")
axis.POSIXct(1, at=t, labels = weekdays(t,T))
lines(plt[[1]]$x*60 + starting_timepoint,plt[[1]]$fit  + plt[[1]]$se, lty=2)
lines(plt[[1]]$x*60 + starting_timepoint,plt[[1]]$fit  - plt[[1]]$se, lty=2)
legend("topright", c("Estimate", "Confidence inteval"), lty = 1:2, cex=2.5,bty = "n")
#par(mfrow=c(1,1))


# weather-related smooth terms
#par(mfrow=c(3,1))
pdf("final_plots/bike_data_temp.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(plt[[2]]$x,plt[[2]]$fit, type = "l", xlab = "Temperature (°C)", 
     ylab = "Temperature smooth effect", 
     ylim = c(min(plt[[2]]$fit  - plt[[2]]$se),
              max(plt[[2]]$fit  + plt[[2]]$se)), 
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines(plt[[2]]$x,plt[[2]]$fit  + plt[[2]]$se, lty=2)
lines(plt[[2]]$x,plt[[2]]$fit  - plt[[2]]$se, lty=2)
legend("bottomright", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()

pdf("final_plots/bike_data_prec.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(exp(plt[[3]]$x)-1,plt[[3]]$fit, type = "l", xlab = "Precipitation (mm)", 
     ylab = "Precipitiation smooth effect", 
     ylim = c(min(plt[[3]]$fit  - plt[[3]]$se),
              max(plt[[3]]$fit  + plt[[3]]$se)), 
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
#abline(v=2.5)
lines(exp(plt[[3]]$x)-1,plt[[3]]$fit  + plt[[3]]$se, lty=2)
lines(exp(plt[[3]]$x)-1,plt[[3]]$fit  - plt[[3]]$se, lty=2)
legend("bottomleft", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()

pdf("final_plots/bike_data_dist.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(exp(plt[[4]]$x)-1,plt[[4]]$fit, type = "l", xlab = "Route distance (mins.)", 
     ylab = "Distance smooth effect", 
     ylim = c(min(plt[[4]]$fit  - plt[[4]]$se)+10,
              max(plt[[4]]$fit  + plt[[4]]$se)), 
     xlim = c(0,160),
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines(exp(plt[[4]]$x)-1,plt[[4]]$fit  + plt[[4]]$se, lty=2)
lines(exp(plt[[4]]$x)-1,plt[[4]]$fit  - plt[[4]]$se, lty=2)
legend("bottomleft", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
#par(mfrow=c(1,1))
dev.off()

#pdf("final_plots/bike_data_precipitation.pdf", height = 8, width = 12)
plot(plt[[3]]$x,plt[[3]]$fit, type = "l", xlab = "Precipitation", 
     ylab = "Precipitation smooth effect", 
     ylim = c(min(plt[[3]]$fit  - plt[[3]]$se),
              max(plt[[3]]$fit  + plt[[3]]$se)))
lines(plt[[3]]$x,plt[[3]]$fit  + plt[[3]]$se, lty=2)
lines(plt[[3]]$x,plt[[3]]$fit  - plt[[3]]$se, lty=2)
legend("bottomleft", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
#dev.off()

# endogenous smooth terms (reciprocity and inertia)

#par(mfrow=c(2,1))
pdf("final_plots/bike_data_recip.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot((-(2*med_rec_events)*log(plt[[5]]$x))/60,plt[[5]]$fit, type = "l",
     xlab = "Time since last reciprocal event (hrs.)", ylab = "Reciprocity", xaxt="n", 
     ylim = c(min(plt[[5]]$fit  - plt[[5]]$se)+1,
              max(plt[[5]]$fit  + plt[[5]]$se)),
     xlim = c(0,24),
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines((-(2*med_rec_events)*log(plt[[5]]$x))/60,plt[[5]]$fit  + plt[[5]]$se, lty=2)
lines((-(2*med_rec_events)*log(plt[[5]]$x))/60,plt[[5]]$fit  - plt[[5]]$se, lty=2)
abline(v=12,lty = 3)
axis(1, at = seq(0,600, by = 12),cex.axis = 2.5, cex.lab = 2.5)
legend("topright", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()

pdf("final_plots/bike_data_repet.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot((-(2*med_rep_events)*log(plt[[6]]$x))/60,plt[[6]]$fit, type = "l",
     xlab = "Time since last same event (hrs.)", ylab = "Repetition", xaxt="n",
     ylim = c(min(plt[[6]]$fit  - plt[[6]]$se)+0.75,
              max(plt[[6]]$fit  + plt[[6]]$se)),
     xlim = c(0,84),
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines((-(2*med_rep_events)*log(plt[[6]]$x))/60,plt[[6]]$fit  + plt[[6]]$se, lty=2)
lines((-(2*med_rep_events)*log(plt[[6]]$x))/60,plt[[6]]$fit  - plt[[6]]$se, lty=2)
#abline(v=10)
abline(v=24, lty = 3)
abline(v = 48,lty = 3)
abline(v = 72,lty = 3)
abline(v = 9,lty = 3)
axis(1, c(9,at = seq(0,600, by = 12)),cex.axis = 2.5, cex.lab = 2.5)
legend("topright", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
dev.off()
#par(mfrow=c(1,1))

# time-related smooth terms

pdf("final_plots/bike_data_time_of_day.pdf", height = 8, width = 12)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(plt[[7]]$x,plt[[7]]$fit, type = "l",
     xlab = "Time of day (hrs)", ylab = "Time of day smooth effect", xaxt="n", 
     ylim = c(min(plt[[7]]$fit  - plt[[7]]$se),
              max(plt[[7]]$fit  + plt[[7]]$se)), 
     cex.main = 2,cex.axis = 2.5, cex.lab = 2.5)
lines(plt[[7]]$x,plt[[7]]$fit  + plt[[7]]$se, lty=2)
lines(plt[[7]]$x,plt[[7]]$fit  - plt[[7]]$se, lty=2)
legend("bottomright", c("Estimate", "Confidence interval"), lty = 1:2,cex=2.5,bty = "n")
abline(v = 9,lty = 3)
abline(v = 18,lty = 3)
axis(1,c(9,18,seq(0,24,by = 4)),cex.axis = 2.5, cex.lab = 2.5)
dev.off()



# residual baseline hazard 
par(mfrow=c(1,1))
pdf("final_plots/bike_data_res_cum_bh.pdf", height = 7, width = 11)
par(mar=c(5,6,4,1)+1, mgp=c(5,2,0))
plot(sort(merged_data$DateTime),c_est$model$L0,type="l", 
     cex.main = 2,cex.axis = 2, cex.lab = 2,  ylab = "Breslow estimator",
     xlab = "Time")
abline(0,c_est$coefficients,col = "red")
dev.off()

c_const<-c_est$coefficients
cat("Slope of line fitted to residual cum. baseline hazard", c_const)

summary(l0.gam)
