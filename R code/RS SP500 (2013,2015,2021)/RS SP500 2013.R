# Reading "RS SP500 2013.RData" in and producing results
# Data runs from 01/1871 to 12/2013 
# Check file configuration in "RData" folder for naming convention

# Clear environment 
rm(list = ls())

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

# RS SP500 2013 dataset
name = "RS SP500 2013"      

# load in .RData
load(paste0("RData/",name,".RData"))

# remove 2014 data - remove last 6 months from real_dividend and real_price
real_price = real_price[-c(length(real_price):(length(real_price)-5))]
real_dividend = real_dividend[-c(length(real_dividend):(length(real_dividend)-5))]

# Calculate the historical real average return using the GAR and the 
# using the GAR for the average dividend growth rate for the last 10, 20
# and 30 years.
index = real_dividend[which(Data == "2003-01-31"):which(Data == "2012-12-31")]
return_dividend = diff(index) / index[-length(index)] + 1 # 10 years
g_10 = prod(return_dividend)^(1/length(return_dividend)) - 1

index = real_dividend[which(Data == "1993-01-29"):which(Data == "2012-12-31")]
return_dividend = diff(index) / index[length(index)] + 1 # 20 years
g_20 = prod(return_dividend)^(1/length(return_dividend)) - 1

index = real_dividend[which(Data == "1983-01-31"):which(Data == "2012-12-31")]
return_dividend = diff(index) / index[length(index)] + 1 # 30 years
g_30 = prod(return_dividend)^(1/length(return_dividend)) - 1

# historical geometric average
# real monthly return on the market from 01/1871 to 12/2013
TR = NA
TR[1] = real_price[1]
for (i in 2:length(real_price)){
  TR[i] = TR[i-1]*(real_price[i] + real_dividend[i])/real_price[i-1]
}
return = diff(TR)/TR[-length(TR)] + 1
r = prod(return)^(1/length(return)) - 1

# Apply the fundamental value formula

FV = function(real_dividend, len, r, g){
  pres_div = NA
  for(k in 1:length(real_dividend)){
    real_div = real_dividend[k:length(real_dividend)]
    len = length(real_div)
    store = NA
    f_div = NA 
    
    for (i in 1:len){
      store[i] = real_div[i]/(1 + r)^(i)
    }
    
    f_div = sum(store) + ((real_div[len]*(1 + g))/(r-g))/((1 + r)^(len))
    pres_div[k] = f_div
  }
  return(pres_div)
}  

# min obj sub. to g
objective = function(P, real_dividend, r, g){
  return(sum(abs(P-FV(real_dividend, length(real_dividend), r, g))^2))
}

g_min = optim(fn = objective, par = c(0.001), r = r, P = real_price, real_dividend = real_dividend, 
             method = "Brent",lower = 0.001, upper = r-0.0001)$par # min obj.

# fundamental value for different growth rates
FV_10 = FV(real_dividend, length(real_dividend), r, g=g_10)
FV_20 = FV(real_dividend, length(real_dividend), r, g=g_20)
FV_30 = FV(real_dividend, length(real_dividend), r, g=g_30)
FV_min = FV(real_dividend, length(real_dividend), r, g=g_min)

# creating ts object for log of real price
log_realprice = ts(log(real_price), start = c(1871,1), frequency = 12)

# distortions using different growth rates
dis_10 = ts(log_realprice - log(FV_10), start = c(1871,1), frequency = 12)
dis_20 = ts(log_realprice - log(FV_20), start = c(1871,1), frequency = 12)
dis_30 = ts(log_realprice - log(FV_30), start = c(1871,1), frequency = 12)
dis_min = ts(log_realprice - log(FV_min), start = c(1871,1), frequency = 12)

# update loaded "RS SP500 2013.RData" file 
save.image(paste0("RData/",name,".RData"))
