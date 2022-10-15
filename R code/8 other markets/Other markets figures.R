# Saving all figures of a market
# loading in all the data of a specific market

# Clear environment 
rm(list = ls())

# cheat sheet of names:
# SP500, NIKKEI, NIFTY50, HANG SENG, FTSE100, DAX, BOVEPSA, ASE
name = "SP500"      # enter different name for different index

# load in .RData
load(paste0("RData/",name,".RData"))

# png saved
png(paste0("Images/",name,"/log fundamental.png"), width = 500, height = 500) 
plot(log_realprice, main = paste0("Monthly log real price of 
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"), 
     ylab = "log real price", lwd = 2)
lines(ts(log(FV_10), start = c(y,m), frequency = 12), col = "blue", lwd = 2)
lines(ts(log(FV_min), start = c(y,m), frequency = 12), col = "green", lwd = 2)
legend("topleft", legend = c(paste0("log ",name,"(",m,"/",y,"-12/2021)"), 
                              "log FV with g of 10 years",
                              "log FV with g of min obj"),
       col = c("black", "blue", "green"), 
       lty = c(1,1,1), bty = "n", lwd = c(2,2,2))
dev.off()

# png saved
png(paste0("Images/",name,"/distortion.png"), width = 500, height = 500) 
plot(dis_min, main = paste0("Distortion of 
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"), 
     ylab = "log real price", lwd = 2, col = "green")
lines(dis_10, col = "blue", lwd = 2)
legend("topleft", legend = c("distortion using g of 10 years",
                             "distortion using g of min obj"),
       col = c("blue", "green"), 
       lty = c(1,1), bty = "n", lwd = c(2,2))
abline(h=0, col = 1, lty = 2)
dev.off()

# png saved
# produces a error for name = "NIKKEI"
png(paste0("Images/",name,"/hist using g = 10.png"), width = 500, height = 500) 
hist(dis_10, breaks = 100, main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"))
dev.off()

# png saved
png(paste0("Images/",name,"/hist using g = min.png"), width = 500, height = 500) 
hist(dis_min, breaks = 100, main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"))
dev.off()

# png saved
# produces a error for name = "NIKKEI"
png(paste0("Images/",name,"/smooth using g = 10.png"), width = 500, height = 500) 
plot(density(dis_10), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data 
using g of last 10 years"))
dev.off()

# png saved
png(paste0("Images/",name,"/smooth using g = min.png"), width = 500, height = 500) 
plot(density(dis_min), lwd = 2, yaxs = "i",xlab ="Distortion",
     main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data 
using g of minimsing the objective function"))
dev.off()

# png saved
png(paste0("Images/",name,"/g = 10 residuals.png"), width = 500, height = 500) 
checkresiduals(fit1)
dev.off()

png(paste0("Images/",name,"/g = min residuals.png"), width = 500, height = 500) 
checkresiduals(fit2)
dev.off()

# special case SP500
if(name == "SP500"){
  png(paste0("Images/",name,"/log fundamental all.png"), width = 500, height = 500) 
  plot(log_realprice, main = paste0("Monthly log real price of 
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"), 
       ylab = "log real price", lwd = 2)
  lines(ts(log(FV_10), start = c(y,m), frequency = 12), col = "blue", lwd = 2)
  lines(ts(log(FV_20), start = c(y,m), frequency = 12), col = "red", lwd = 2)
  lines(ts(log(FV_30), start = c(y,m), frequency = 12), col = "yellow", lwd = 2)
  lines(ts(log(FV_min), start = c(y,m), frequency = 12), col = "green", lwd = 2)
  legend("topleft", legend = c(paste0("log ",name,"(",m,"/",y,"-12/2021)"), 
                               "log FV with g of 10 years",
                               "log FV with g of 20 years",
                               "log FV with g of 30 years",
                               "log FV with g of min obj"),
         col = c("black", "blue", "red", "yellow", "green"), 
         lty = c(1,1,1,1,1), bty = "n", lwd = c(2,2,2,2,2))
  dev.off()

  png(paste0("Images/",name,"/distortion all.png"), width = 500, height = 500) 
  plot(dis_min, main = paste0("Distortion of 
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"), 
       ylab = "log real price", lwd = 2, col = "green")
  lines(dis_10, col = "blue", lwd = 2)
  lines(dis_20, col = "red", lwd = 2)
  lines(dis_30, col = "yellow", lwd = 2)
  legend("topleft", legend = c("distortion using g of 10 years",
                               "distortion using g of 20 years",
                               "distortion using g of 30 years",
                               "distortion using g of min obj"),
         col = c("blue", "red", "yellow", "green"), 
         lty = c(1,1,1,1), bty = "n", lwd = c(2,2,2,2))
  abline(h=0, col = 1, lty = 2)
  dev.off()
  
  png(paste0("Images/",name,"/hist using g = 20.png"), width = 500, height = 500) 
  hist(dis_20, breaks = 100, main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"))
  dev.off()
  png(paste0("Images/",name,"/hist using g = 30.png"), width = 500, height = 500) 
  hist(dis_30, breaks = 100, main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data"))
  dev.off()
  png(paste0("Images/",name,"/smooth using g = 20.png"), width = 500, height = 500) 
  plot(density(dis_20), lwd = 2, yaxs = "i",xlab ="Distortion",
       main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data 
using g of last 20 years"))
  dev.off()
  png(paste0("Images/",name,"/smooth using g = 30.png"), width = 500, height = 500) 
  plot(density(dis_30), lwd = 2, yaxs = "i",xlab ="Distortion",
       main = paste0("Distribution of distortion
",name,"(",m,"/",y,"-12/2021) from Bloomberg data 
using g of last 30 years"))
  dev.off()

  }

