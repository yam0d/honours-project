# Saving all figures of the TOP40
# loading in all the data of a specific market

# Clear environment 
rm(list = ls())

# name of index
name = "TOP40" 

# load in .RData
load(paste0("RData/",name,".RData"))

# png saved
png(paste0("Images/",name,"/log fundamental.png"), width = 500, height = 500)
plot(log_realprice, main = "Monthly log real price of TOP40 from 10/2002-12/2021 from 
     Bloomberg data", 
     ylab = "log real price", lwd = 2, ylim = c(9.5,11.5))
lines(ts(log(FV_10), start = c(2002,10), frequency = 12), col = "blue", lwd = 2)
lines(ts(log(FV_15), start = c(2002,10), frequency = 12), col = "purple", lwd = 2)
lines(ts(log(FV_w), start = c(2002,10), frequency = 12), col = "orange", lwd = 2)
lines(ts(log(FV_min), start = c(2002,10), frequency = 12), col = "green", lwd = 2)
legend("topleft", legend = c("log TOP40 (10/2002-12/2021)", 
                             "log FV with g of 10 years",
                             "log FV with g of 15 years",
                             "log FV with g over whole dataset",
                             "log FV with g of min obj"),
       col = c("black", "blue", "purple", "orange", "green"), 
       lty = c(1,1,1,1), bty = "n", lwd = c(2,2,2,2))
dev.off()

# png saved
png(paste0("Images/",name,"/log fundamental uni.png"), width = 500, height = 500)
plot(simulations[[1]], main = "Monthly log real price of TOP40 from 10/2002-12/2021 from 
     Bloomberg data", 
     ylab = "log real price", lwd = 2, ylim = c(9.8, 12))
for (i in 2:1000) {
  lines(simulations[[i]], col = i, lwd = 2)
}
lines(log_realprice, lwd = 2)
legend("topleft", legend = c("log TOP40"), lwd = 2, col = "black", bty = "n")
dev.off()

# png saved
png(paste0("Images/",name,"/distortion.png"), width = 500, height = 500)
plot(dis_10, main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data", 
     ylab = "Distortion", lwd = 2, col = "blue", ylim = c(-0.5,2))
lines(dis_15, col = "purple", lwd = 2)
lines(dis_min, col = "green", lwd = 2)
lines(dis_w, col = "orange", lwd = 2)
legend("topleft", legend = c("using g of 10 years",
                             "using g of 15 years",
                             "using g over whole dataset",
                             "using g of min obj"),
       col = c("blue", "purple", "orange", "green"), 
       lty = c(1,1,1,1), bty = "n", lwd = c(2,2,2,2))
abline(h=0, col = 1, lty = 2, lwd = 2)
dev.off()

# png saved
png(paste0("Images/",name,"/distortion uni.png"), width = 500, height = 500)
plot(simulations_dist[[1]], main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data", 
     ylab = "Distortion", lwd = 2, ylim = c(-5,1))
for (i in 2:1000) {
  lines(simulations_dist[[i]], col = i, lwd = 2)
}
abline(h=0, col = 1, lty = 2, lwd = 2)
dev.off()

png(paste0("Images/",name,"/hist using g = 10.png"), width = 500, height = 500) 
hist(dis_10, breaks = 100, main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g of last 10 years")
dev.off()
png(paste0("Images/",name,"/hist using g = 15.png"), width = 500, height = 500) 
hist(dis_15, breaks = 100, main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g of last 15 years")
dev.off()
png(paste0("Images/",name,"/hist using g = w.png"), width = 500, height = 500) 
hist(dis_w, breaks = 100, main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g over whole dataset exluding 2021")
dev.off()
png(paste0("Images/",name,"/hist using g = min.png"), width = 500, height = 500) 
hist(dis_min, breaks = 100, main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g of minimising the objective function")
dev.off()

png(paste0("Images/",name,"/smooth using g = 10.png"), width = 500, height = 500) 
plot(density(dis_10), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g of last 10 years", ylim = c(0,2.5))
dev.off()
png(paste0("Images/",name,"/smooth using g = 15.png"), width = 500, height = 500) 
plot(density(dis_15), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g of last 15 years", ylim = c(0,2.8))
dev.off()
png(paste0("Images/",name,"/smooth using g = w.png"), width = 500, height = 500) 
plot(density(dis_min), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g of min obj", ylim = c(0,2.5))
dev.off()
png(paste0("Images/",name,"/smooth using g = min.png"), width = 500, height = 500) 
plot(density(dis_w), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g over whole dataset", ylim = c(0,2.7))
dev.off()

png(paste0("Images/",name,"/smooth using g = uni.png"), width = 500, height = 500) 
plot(density(simulations_dist[[1]]), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = "TOP40 distortion from 10/2002-12/2021 from 
     Bloomberg data using g over whole dataset", ylim = c(0,3), xlim = c(-4,1.2))
for (i in 2:1000) {
  lines(density(simulations_dist[[i]]), col = i, lwd = 2)
}
dev.off()


png(paste0("Images/",name,"/bw1.png"), width = 500, height = 500) 
densities.plot(x1, modes = 1:4)
dev.off()
png(paste0("Images/",name,"/bw2.png"), width = 500, height = 500) 
densities.plot(x2, modes = 1:4)
dev.off()
png(paste0("Images/",name,"/bw3.png"), width = 500, height = 500) 
densities.plot(x3, modes = 1:4)
dev.off()
png(paste0("Images/",name,"/bw4.png"), width = 500, height = 500) 
densities.plot(x4, modes = 1:4)
dev.off()

png(paste0("Images/",name,"/fit1 arima(0,1,0).png"), width = 500, height = 500)
plot(fit1_sim[[1]], ylab = "Distortion", 
     main = "1000 distortions paths simulated from ARIMA(0,1,0)
     from Bloomberg TOP40  data set from 10/2002 - 12/2021 
     using g of last 10 years", lwd = 2, ylim = c(-2.5,2.5))
for (i in 2:1000) {
  lines(fit1_sim[[i]], col = i, lwd = 2)
}
lines(as.vector(dis_10), lwd = 2, col = "blue")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit1 arima(0,1,0) den.png"), width = 500, height = 500)
plot(density(fit1_sim[[1]]), xlab = "Distortion", 
     main = "1000 distortion paths of ARMA(0,1,0)", 
     lwd=2, xlim = c(-4,4), ylim = c(0,5))
for (i in 2:1000) {
  lines(density(fit1_sim[[i]]), col = i, lwd = 2)
}
lines(density(dis_10), lwd = 2, col = "blue")
legend("topleft", legend = c("using g of 10 years"),
       col = c("blue"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit2 arima(0,1,0).png"), width = 500, height = 500)
plot(fit2_sim[[1]], ylab = "Distortion", 
     main = "1000 distortions paths simulated from ARIMA(0,1,0)
     from Bloomberg TOP40  data set from 10/2002 - 12/2021 
     using g of last 15 years", lwd = 2, ylim = c(-2.5,2.5))
for (i in 2:1000) {
  lines(fit2_sim[[i]], col = i, lwd = 2)
}
lines(as.vector(dis_15), lwd = 2, col = "purple")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g of 15 years"),
       col = c("purple"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit2 arima(0,1,0) den.png"), width = 500, height = 500)
plot(density(fit2_sim[[1]]), xlab = "Distortion", 
     main = "1000 distortion paths of ARMA(0,1,0)", 
     lwd=2, xlim = c(-4,4), ylim = c(0,5))
for (i in 2:1000) {
  lines(density(fit2_sim[[i]]), col = i, lwd = 2)
}
lines(density(dis_15), lwd = 2, col = "purple")
legend("topleft", legend = c("using g of 15 years"),
       col = c("purple"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()


png(paste0("Images/",name,"/fit3 arima(0,1,0).png"), width = 500, height = 500)
plot(fit3_sim[[1]], ylab = "Distortion", 
     main = "1000 distortions paths simulated from ARIMA(0,1,0)
     from Bloomberg TOP40  data set from 10/2002 - 12/2021 
     using g over whole dataset", lwd = 2, ylim = c(-2.5,2.5))
for (i in 2:1000) {
  lines(fit3_sim[[i]], col = i, lwd = 2)
}
lines(as.vector(dis_w), lwd = 2, col = "orange")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g over whole dataset"),
       col = c("orange"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit3 arima(0,1,0) den.png"), width = 500, height = 500)
plot(density(fit3_sim[[1]]), xlab = "Distortion", 
     main = "1000 distortion paths of ARIMA(0,1,0)", 
     lwd=2, xlim = c(-4,4), ylim = c(0,5))
for (i in 2:1000) {
  lines(density(fit3_sim[[i]]), col = i, lwd = 2)
}
lines(density(dis_w), lwd = 2, col = "orange")
legend("topleft", legend = c("using g over whole dataset"),
       col = c("orange"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit4 arima(0,1,0).png"), width = 500, height = 500)
plot(fit4_sim[[1]], ylab = "Distortion", 
     main = "1000 distortions paths simulated from ARIMA(0,1,0)
     from Bloomberg TOP40  data set from 10/2002 - 12/2021 
     using g of min obj", lwd = 2, ylim = c(-2.5,2.5))
for (i in 2:1000) {
  lines(fit4_sim[[i]], col = i, lwd = 2)
}
lines(as.vector(dis_min), lwd = 2, col = "green")
abline(h=0, col="black", lty = 2, lwd = 2)
legend("topleft", legend = c("using g of min obj"),
       col = c("green"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

png(paste0("Images/",name,"/fit4 arima(0,1,0) den.png"), width = 500, height = 500)
plot(density(fit4_sim[[1]]), xlab = "Distortion", 
     main = "1000 distortion paths of ARMA(0,1,0)", 
     lwd=2, xlim = c(-4,4), ylim = c(0,5))
for (i in 2:1000) {
  lines(density(fit4_sim[[i]]), col = i, lwd = 2)
}
lines(density(dis_min), lwd = 2, col = "green")
legend("topleft", legend = c("using g of min obj"),
       col = c("green"), 
       lty = 1 , bty = "n", lwd = 2)
dev.off()

# contour plot for the fundamental value
png(paste0("Images/",name,"/contour fv.png"), width = 500, height = 500)
ggplot(data.frame(len = rep(1:231,1000), 
                  data= unlist(simulations)), 
       aes(x = len, y = data)) +
  stat_density2d_filled() + geom_line(data = data.frame(len = rep(1:231,1),
                                                        log = as.vector(log_realprice)), 
                                      aes(x = len, y = log), col = "white") +
  coord_cartesian(ylim=c(9.5,11.3),xlim = c(1,231)) + 
expand_limits(x = 1, y = 0) + 
  ggtitle("Contour plot of log fundamental value paths") + 
  labs(y = "Log real price", x = "Time")
dev.off()

# contour plot for the distortion
png(paste0("Images/",name,"/contour dist.png"), width = 500, height = 500)
ggplot(data.frame(len = rep(1:231,1000), 
                  data= unlist(simulations_dist)), 
       aes(x = len, y = data)) +
  stat_density2d_filled() +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1) +
  geom_line(data = data.frame(len = rep(1:231,1),log = as.vector(dis_10)), 
                                       aes(x = len, y = log), col = "white") +
  coord_cartesian(ylim=c(-1,1), xlim = c(1,231)) +
  geom_line(data = data.frame(len = rep(1:231,1),
                           log = as.vector(log_realprice)), 
              aes(x = len, y = log)) +
  expand_limits(x = 1, y = 0) + 
  ggtitle("Contour plot of distortion paths") + 
  labs(y = "Distortion", x = "Time") 
dev.off()

# contout plot for arima simulations when using a g_10
png(paste0("Images/",name,"/contour fit1.png"), width = 500, height = 500)
ggplot(data.frame(len = rep(1:232,1000), 
                  data= unlist(fit1_sim)), 
       aes(x = len, y = data)) +
  stat_density2d_filled() +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1) +
  geom_line(data = data.frame(len = rep(1:231,1),log = as.vector(dis_10)), 
            aes(x = len, y = log), col = "white", size = 1) +
  coord_cartesian(ylim=c(-1,1),
                  xlim = c(1,232)) + 
  expand_limits(x = 1, y = 0) + 
  ggtitle("Contour plot of 1000 ARIMA(0,1,0) simulations from a distortion using a 
growth rate of the last 10 years") + 
  labs(y = "Distortion", x = "Time") 
dev.off()

# contout plot for arima simulations when using a g_15
png(paste0("Images/",name,"/contour fit2.png"), width = 500, height = 500)
ggplot(data.frame(len = rep(1:232,1000), 
                  data= unlist(fit2_sim)), 
       aes(x = len, y = data)) +
  stat_density2d_filled() +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1) +
  geom_line(data = data.frame(len = rep(1:231,1),log = as.vector(dis_15)), 
            aes(x = len, y = log), col = "white", size = 1) +
coord_cartesian(ylim=c(-1,1), xlim = c(1,232)) + 
  expand_limits(x = 1, y = 0) + 
  ggtitle("Contour plot of 1000 ARIMA(0,1,0) simulations from a distortion using a
growth rate of the last 15 years") + 
  labs(y = "Distortion", x = "Time") 
dev.off()

# contout plot for arima simulations when using a g_w
png(paste0("Images/",name,"/contour fit3.png"), width = 500, height = 500)
ggplot(data.frame(len = rep(1:232,1000), 
                  data= unlist(fit3_sim)), 
       aes(x = len, y = data))  + 
  stat_density2d_filled() +  
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1) +
  geom_line(data = data.frame(len = rep(1:231,1),log = as.vector(dis_w)), 
            aes(x = len, y = log), col = "white", size = 1) +
coord_cartesian(ylim=c(-1,1),
                xlim = c(1,232)) + 
  expand_limits(x = 1, y = 0) + 
  ggtitle("Contour plot of 1000 ARIMA(0,1,0) simulations from a distortion using a
growth rate over the whole dataset") + 
  labs(y = "Distortion", x = "Time") 
dev.off()

# contour plot for arima simulations when using a g_min
png(paste0("Images/",name,"/contour fit4.png"), width = 500, height = 500)
ggplot(data.frame(len = rep(1:232,1000), 
                  data= unlist(fit4_sim)), 
       aes(x = len, y = data)) +  
  stat_density2d_filled() +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size = 1) +
  geom_line(data = data.frame(len = rep(1:231,1),log = as.vector(dis_min)), 
            aes(x = len, y = log), col = "white", size = 1) +
coord_cartesian(ylim=c(-1,1),
                xlim = c(1,232)) + 
  expand_limits(x = 1, y = 0) + 
  ggtitle("Contour plot of 1000 ARIMA(0,1,0) simulations from a distortion using a
growth rate minimising the objective function") + 
  labs(y = "Distortion", x = "Time")
dev.off()

png(paste0("Images/",name,"/g = 10 residuals.png"), width = 500, height = 500) 
checkresiduals(arma1)
dev.off()

png(paste0("Images/",name,"/g = min residuals.png"), width = 500, height = 500) 
checkresiduals(arma4)
dev.off()