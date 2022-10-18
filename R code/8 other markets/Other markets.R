# Reading .RData for each markets in and producing results
# Check file configuration in "RData" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(forecast)

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

# cheat sheet of names:
# SP500, NIKKEI, NIFTY50, HANG SENG, FTSE100, DAX, BOVEPSA, ASE
name = "ASE"      # enter different name for different index

# load in .RData
load(paste0("RData/",name,".RData"))

# Calculating the historical real average return using the GAR and the 
# using the GAR for the average dividend growth rate for the last 10
# and a g obtained when min obj.

# choosing the time period index of the last 10 years.
# for name = "SP500" check special case at end of code.
index = real_dividend[which(Data == "2011-01-31"):which(Data == "2020-12-31")]

# geometric average monthy growth rate of real dividends
growth = diff(index) / index[length(index)] + 1
# growth rate of last 10 years
g_10 = prod(growth)^(1/length(growth)) - 1 

# historical return on the market
# geometric average monthly real return
TR = NA
TR[1] = real_price[1]
for (i in 2:length(real_price)){
  TR[i] = TR[i-1]*(real_price[i] + real_dividend[i])/real_price[i-1]
}
return = diff(TR)/TR[-length(TR)] + 1
# discount rate
r = prod(return)^(1/length(return)) - 1

# Apply the fundamental value formula
FV = function(real_dividend, len, r, g){
  pres_div = NA
  for(k in 1:length(real_dividend)){
    real_div = real_dividend[k:length(real_dividend)]
    len = length(real_dividend[k:length(real_dividend)])
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

# Function to minimize subject to a growth rate with a fixed discount rate 
objective = function(P, real_dividend, r, g){
  return(sum(abs(P-FV(real_dividend, length(real_dividend), r, g))^2))
}

# growth rate when minimising the objective function
g_min = optim(fn = objective, par = c(0.001), r = r, 
             P = real_price, 
             real_dividend = real_dividend, 
             method = "Brent",lower = -1, upper = r-0.0001)$par 

FV_10 = FV(real_dividend, length(real_dividend), r, g=g_10)
FV_min = FV(real_dividend, length(real_dividend), r, g=g_min)

# extracting year from earliest date
y = as.numeric(format(Data$Date[1], format = "%Y"))
# extracting the month of the earliest year
m = as.numeric(format(Data$Date[1], format = "%m"))
# creating a log price time series object
log_realprice = ts(log(real_price), start = c(y,m), frequency = 12)

# creating ts objects for the distortion using different growth rates
dis_10 = ts(log_realprice - log(FV_10), start = c(y,m), frequency = 12)
dis_min = ts(log_realprice - log(FV_min), start = c(y,m), frequency = 12)

# changing ts objects to vectors
x1 = as.vector(dis_10)
x2 = as.vector(dis_min)


# Will generate error when name = "NIKKEI" since g>d
# overwrite error using try()
# fitting ARMA(1,1) to the data
fit1 = try(Arima(x1, order = c(1,0,1)))
# Confidence intervals of parameters
fit1_ci = try(confint(fit1))

# fitting ARMA(1,1) to the data
fit2 = Arima(x2, order = c(1,0,1))
# Confidence intervals of parameters
fit2_ci = confint(fit2)

# special case SP500
if(name == "SP500"){
  index = real_dividend[which(Data == "2001-01-31"):which(Data == "2020-12-31")]
  # geometric average monthy growth rate of real dividends
  growth = diff(index) / index[length(index)] + 1
  # growth rate of last 20 years
  g_20 = prod(growth)^(1/length(growth)) - 1 
  
  index = real_dividend[which(Data == "1991-01-31"):which(Data == "2020-12-31")]
  # geometric average monthy growth rate of real dividends
  growth = diff(index) / index[length(index)] + 1
  # growth rate of last 20 years
  g_30 = prod(growth)^(1/length(growth)) - 1 
  
  
  FV_20 = FV(real_dividend, length(real_dividend), r, g=g_20)
  FV_30 = FV(real_dividend, length(real_dividend), r, g=g_30)
  
  # creating a log price time series object
  log_realprice = ts(log(real_price), start = c(y,m), frequency = 12)
  
  dis_20 = ts(log_realprice - log(FV_20), start = c(y,m), frequency = 12)
  dis_30 = ts(log_realprice - log(FV_30), start = c(y,m), frequency = 12)
}

# update loaded ".RData" file 
# save each market in "RData" folder to be called later
save.image(paste0("RData/",name,".RData"))

