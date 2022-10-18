# Saving all figures of the RS SP500 2015 figures
# loading in all the data of a specific market

# Clear environment 
rm(list = ls())

# name of index
name = "RS SP500 2015" 

# load in .RData
load(paste0("RData/",name,".RData"))

# png saved
png(paste0("Images/",name,"/log fundamental.png"), width = 500, height = 500) 
plot(log_realprice, main = "Monthly log real price of SP500 from 
01/1871-12/2015 from Robert Shiller data", 
     ylab = "log real price", lwd = 2)
lines(ts(log(FV_10), start = c(1871,1), frequency = 12), col = "blue", lwd = 2)
lines(ts(log(FV_20), start = c(1871,1), frequency = 12), col = "red", lwd = 2)
lines(ts(log(FV_30), start = c(1871,1), frequency = 12), col = "yellow", lwd = 2)
lines(ts(log(FV_min), start = c(1871,1), frequency = 12), col = "green", lwd = 2)
legend("topleft", legend = c("log SP500(1871-2015)", 
                             "log FV with g of 10 years",
                             "log FV with g of 20 years",
                             "log FV with g of 30 years",
                             "log FV with g of min obj"),
       col = c("black", "blue", "red", "yellow", "green"), 
       lty = c(1,1,1,1), bty = "n", lwd = c(2,2,2,2))
dev.off()

png(paste0("Images/",name,"/distortion.png"), width = 500, height = 500) 
plot(dis_10, main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data", 
     ylab = "Distortion", lwd = 2, ylim = c(-2,2), col = "blue")
lines(dis_20, col = "red", lwd = 2)
lines(dis_30, col = "yellow", lwd = 2)
lines(dis_min, col = "green", lwd = 2)
legend("topleft", legend = c("distortion using g of 10 years",
                             "distortion using g of 20 years",
                             "distortion using g of 30 years",
                             "distortion using g of min obj"),
       col = c("blue", "red", "yellow", "green"), 
       lty = c(1,1,1,1), bty = "n", lwd = c(2,2,2,2))
abline(h=0, col = 1, lty = 2)
dev.off()

png(paste0("Images/",name,"/log fundamental uni.png"), width = 500, height = 500)
plot(simulations[[1]], main = "Monthly log real price of SP500 from 01/1871-12/2015 
     Robert Shiller data", 
     ylab = "log real price", lwd = 2, ylim = c(4, 9))
for (i in 1:1000) {
  lines(simulations[[i]], col = i, lwd = 2)
}
lines(log_realprice, col = "black", lwd = 2)
legend("topleft", legend = c("log real SP500"), lwd = 2, col = "black", bty = "n")
dev.off()

png(paste0("Images/",name,"/dist uni.png"), width = 500, height = 500)
plot(simulations_dist[[1]], main = "Distortion of SP500 from 01/1871-12/2015 
     Robert Shiller data", 
     ylab = "log real price", lwd = 2, ylim = c(-3,2)) 
for (i in 1:1000) {
  lines(simulations_dist[[i]], col = i, lwd = 2)
}
lines(dis_10, col = "blue", lwd = 2)
legend("topleft", legend = c("distortion using g of 10 years"), lwd = 2, 
       col = "blue", bty = "n")
abline(h=0, col="black", lty = 2, lwd = 2)
dev.off()

png(paste0("Images/",name,"/hist using g = 10.png"), width = 500, height = 500) 
hist(dis_10, breaks = 100, main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of last 10 years")
dev.off()
png(paste0("Images/",name,"/hist using g = 20.png"), width = 500, height = 500) 
hist(dis_20, breaks = 100, main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of last 20 years")
dev.off()
png(paste0("Images/",name,"/hist using g = 30.png"), width = 500, height = 500) 
hist(dis_30, breaks = 100, main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of last 30 years")
dev.off()
png(paste0("Images/",name,"/hist using g = min.png"), width = 500, height = 500) 
hist(dis_min, breaks = 100, main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of min obj")
dev.off()

# plot the smoothed histogram of the distortion.
png(paste0("Images/",name,"/smooth using g = 10.png"), width = 500, height = 500) 
plot(density(dis_10), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of last 10 years", ylim = c(0,1.3))
dev.off()
png(paste0("Images/",name,"/smooth using g = 20.png"), width = 500, height = 500) 
plot(density(dis_20), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of last 20 years", ylim = c(0,1.3))
dev.off()
png(paste0("Images/",name,"/smooth using g = 30.png"), width = 500, height = 500) 
plot(density(dis_30), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of last 30 years", ylim = c(0,1.3))
dev.off()
png(paste0("Images/",name,"/smooth using g = min.png"), width = 500, height = 500) 
plot(density(dis_min), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "SP500 distortion from 01/1871-12/2015 from 
     Robert Shiller data using g of min obj", ylim = c(0,1.3))
dev.off()

png(paste0("Images/",name,"/bw1.png"), width = 500, height = 500) 
densities.plot(x1, modes = 1:4)
dev.off()
png(paste0("Images/",name,"/bw2.png"), width = 500, height = 500) 
densities.plot(x3, modes = 1:4)
dev.off()

png(paste0("Images/",name,"/fit1 arima(4,1,1).png"), width = 500, height = 500)
plot(fit1_sim[[1]], ylab = "Distortion", 
     main = "ARMA(4,1,1) simulations for 1740 monthly distortions
     from the Robert Shiller data set from 01/1871 - 12/2015 
     using g of last 10 years", lwd = 2, ylim = c(-8,8))
for (i in 2:1000) {
  lines(fit1_sim[[i]], col = i, lwd = 2)
}
lines(density(x1), lwd = 2, col = "blue")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit1 arima(4,1,1) den.png"), width = 500, height = 500)
plot(density(fit1_sim[[1]]), xlab = "Distortion", main = "Distortion of ARMA(4,1,1)",
     xlim = c(-8,8), 
     ylim=c(0,1.5))
for (i in 2:1000) {
  lines(density(fit1_sim[[i]]), col = i, lwd = 2)
}
lines(density(x1), lwd=2, col = "blue")
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit2 arima(2,0,2).png"), width = 500, height = 500)
plot(fit2_sim[[1]], ylab = "Distortion", 
     main = "ARMA(2,2) simulations for 1740 monthly distortions
     from the Robert Shiller data set from 01/1871 - 12/2015 
     using g of last 10 years", lwd = 2, ylim = c(-2,2))
for (i in 2:1000) {
  lines(fit2_sim[[i]], col = i, lwd = 2)
}
lines(density(x1), lwd = 2, col = "blue")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit2 arima(2,0,2) den.png"), width = 500, height = 500)
plot(density(fit2_sim[[1]]), xlab = "Distortion", main = "Distortion of ARMA(2,2)",
     xlim = c(-2,2), 
     ylim=c(0,1.5))
for (i in 2:1000) {
  lines(density(fit2_sim[[i]]), col = i, lwd = 2)
}
lines(density(x1), lwd=2, col = "blue")
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit3 arima(1,0,1).png"), width = 500, height = 500)
plot(fit3_sim[[1]], ylab = "Distortion", 
     main = "ARMA(1,1) simulations for 1740 monthly distortions
     from the Robert Shiller data set from 01/1871 - 12/2015 
     using g of last 10 years", lwd = 2, ylim = c(-2,2))
for (i in 2:1000) {
  lines(fit3_sim[[i]], col = i, lwd = 2)
}
lines(density(x1), lwd = 2, col = "blue")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit3 arima(1,0,1) den.png"), width = 500, height = 500)
plot(density(fit3_sim[[1]]), xlab = "Distortion", main = "Distortion of ARMA(1,1)",
     xlim = c(-2,2), 
     ylim=c(0,1.5))
for (i in 2:1000) {
  lines(density(fit3_sim[[i]]), col = i, lwd = 2)
}
lines(density(x1), lwd=2, col = "blue")
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()


