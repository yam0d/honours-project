# Reading "TOP40.RData" in and producing results
# Check file configuration in "RData" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(devtools)
library(silvermantest)
library(forecast)
library(ggplot2)

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

# TOP40 index from bloomberg
name = "TOP40"      

# load in .RData
load(paste0("RData/",name,".RData"))

time = system.time({
  
  # Calculate the historical real average return using the GAR and the 
  # using the GAR for the average dividend growth rate for the 10
  # and 15 years and g over the whole dataset.
  
  index = real_dividend[which(Data == "2011-01-31"):which(Data == "2020-12-31")]
  return_dividend = diff(index) / index[length(index)] + 1 
  # growth rate of last 10 years
  g_10 = prod(return_dividend)^(1/length(return_dividend)) - 1
  
  index = real_dividend[which(Data == "2006-01-31"):which(Data == "2020-12-31")]
  return_dividend = diff(index) / index[length(index)] + 1
  # growth rate of last 15 years
  g_15 = prod(return_dividend)^(1/length(return_dividend)) - 1
  
  index = real_dividend[which(Data == "2002-10-31"):which(Data == "2020-12-31")]
  return_dividend = diff(index) / index[length(index)] + 1 
  # growth rate over whole dataset
  g_w = prod(return_dividend)^(1/length(return_dividend)) - 1
  
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
  FV_15 = FV(real_dividend, length(real_dividend), r, g=g_15)
  FV_w = FV(real_dividend, length(real_dividend), r, g=g_w)
  FV_min = FV(real_dividend, length(real_dividend), r, g=g_min)
  
  # extracting year from earliest date
  y = as.numeric(format(Data$Date[1], format = "%Y"))
  # extracting the month of the earliest year
  m = as.numeric(format(Data$Date[1], format = "%m"))
  # creating a log price time series object
  log_realprice = ts(log(real_price), start = c(y,m), frequency = 12)
  
  # creating ts objects for the distortion using different growth rates
  dis_10 = ts(log_realprice - log(FV_10), start = c(y,m), frequency = 12)
  dis_15 = ts(log_realprice - log(FV_15), start = c(y,m), frequency = 12)
  dis_w = ts(log_realprice - log(FV_w), start = c(y,m), frequency = 12)
  dis_min = ts(log_realprice - log(FV_min), start = c(y,m), frequency = 12)
  
  # generating growth rates from a random uniform distribution with a set seed
  # then calculating the respective FV and storing the distortion in a list
  set.seed(2022)
  g_uni = sort(runif(1000, min = -r, max = r))
  # list for FV's of all growth rates generated
  simulations = list()
  for (i in 1:1000) {
    simulations[[i]] = ts(log(FV(real_dividend, length(real_dividend), 
                                 r, g=g_uni[i])), 
                          start = c(y,m), frequency = 12)}
  # list for distortions of all growth rates generated
  simulations_dist = list()
  for (i in 1:1000) {
    simulations_dist[[i]] = ts(log_realprice - simulations[[i]], 
                               start = c(y,m), frequency = 12)}
  
  # changing ts objects to vectors
  x1 = as.vector(dis_10)
  x2 = as.vector(dis_15)
  x3 = as.vector(dis_w)
  x4 = as.vector(dis_min)
  
  # creating a function that counts the number of densities that are 
  # non-significant relative to a threshold of 0.05 using the silverman test.
  # y - list containing vectors
  # k - number of modes to test
  
  count_mode = function(y, k){
    p = NA    # initiate p-value
    for (i in 1:length(y)){
      test = silverman.test(density(y[[i]])$y, k = k, R = 999, digits = 6)
      p[i] = test@p_value
    }
    return(list(exclude = which(p>0.05), n = length(which(p>0.05))))
  }
  
  # Time series fitting section
  
  # fitting ARMA(1,1) to the data
  arma1 = try(Arima(x1, order = c(1,0,1)))
  arma1$aic; arma1$bic; arma1$aicc
  # Confidence intervals of parameters
  arma1_ci = try(confint(arma1))
  
  # fitting ARMA(1,1) to the data
  arma2 = Arima(x2, order = c(1,0,1))
  arma2$aic; arma2$bic; arma2$aicc
  # Confidence intervals of parameters
  arma2_ci = confint(arma2)
  
  # fitting ARMA(1,1) to the data
  arma3 = Arima(x3, order = c(1,0,1))
  arma3$aic; arma3$bic; arma3$aicc
  # Confidence intervals of parameters
  arma3_ci = confint(arma3)
  
  # fitting ARMA(1,1) to the data
  arma4 = Arima(x4, order = c(1,0,1))
  arma4$aic; arma4$bic; arma4$aicc
  # Confidence intervals of parameters
  arma4_ci = confint(arma4)
  
  # fitting distortion using growth rate of the last 10 years using 
  # different selection criteria
  fit1_AIC = auto.arima(x1, trace=TRUE, ic = "aic")
  fit1_AICC = auto.arima(x1, trace=TRUE, ic = "aicc")
  fit1_AICC$aic; fit1_AICC$bic; fit1_AICC$aicc
  fit1_BIC = auto.arima(x1, trace=TRUE, ic = "bic")
  
  # 95% confidence interval 
  fit1_ci = confint(fit1_AICC)
  
  # creating 1000 paths from the time series models
  fit1_sim = list()
  for (i in 1:1000) {
    fit1_sim[[i]] = arima.sim(n = 231, list(order = c(0,1,0)),
                          sd = sqrt(fit1_AICC$sigma2))
  }
  
  # function used to calculate the number of modes with no bootstrapping
  fit1_mode = NA
  for (i in 1:1000) {
    fit1_mode[i] = nr.modes(density(fit1_sim[[i]])$y)
  }
  table(fit1_mode)
  
  # using the silverman test to calculate the number of modes with for each of
  # 1000 simulated paths
  fit1_uni = count_mode(y = fit1_sim, k = 1)
  fit1_bi = count_mode(y = fit1_sim[-fit1_uni$exclude], k = 2)
  fit1_tri = count_mode(y = fit1_sim[-fit1_uni$exclude][-fit1_bi$exclude], k = 3)
  
  # fitting distortion using growth rate of the last 15 years using 
  # different selection criteria
  fit2_AIC = auto.arima(x2, trace=TRUE, ic = "aic")
  fit2_AICC = auto.arima(x2, trace=TRUE, ic = "aicc")
  fit2_AICC$aic; fit2_AICC$bic; fit2_AICC$aicc
  fit2_BIC = auto.arima(x2, trace=TRUE, ic = "bic")
  
  # 95% confidence interval 
  fit2_ci = confint(fit2_AICC)
  
  # creating 1000 paths from the time series models
  fit2_sim = list()
  for (i in 1:1000) {
    fit2_sim[[i]] = arima.sim(n = 231, list(order = c(0,1,0)),
                              sd = sqrt(fit2_AICC$sigma2))
  }
  
  # function used to calculate the number of modes with no bootstrapping
  fit2_mode = NA
  for (i in 1:1000) {
    fit2_mode[i] = nr.modes(density(fit2_sim[[i]])$y)
  }
  table(fit2_mode)
  
  # using the silverman test to calculate the number of modes with for each of
  # 1000 simulated paths
  fit2_uni = count_mode(y = fit2_sim, k = 1)
  fit2_bi = count_mode(y = fit2_sim[-fit2_uni$exclude], k = 2)
  fit2_tri = count_mode(y = fit2_sim[-fit2_uni$exclude][-fit2_bi$exclude], k = 3)
  
  # fitting distortion using growth rate over the whole dataset 
  # different selection criteria
  fit3_AIC = auto.arima(x3, trace=TRUE, ic = "aic")
  fit3_AICC = auto.arima(x3, trace=TRUE, ic = "aicc")
  fit3_AICC$aic; fit3_AICC$bic; fit3_AICC$aicc
  fit3_BIC = auto.arima(x3, trace=TRUE, ic = "bic")
  
  # 95% confidence interval 
  fit3_ci = confint(fit3_AICC)
  
  # creating 1000 paths from the time series models
  fit3_sim = list()
  for (i in 1:1000) {
    fit3_sim[[i]] = arima.sim(n = 231, list(order = c(0,1,0)),
                              sd = sqrt(fit3_AICC$sigma2))
  }
  
  # function used to calculate the number of modes with no bootstrapping
  fit3_mode = NA
  for (i in 1:1000) {
    fit3_mode[i] = nr.modes(density(fit3_sim[[i]])$y)
  }
  table(fit3_mode)
  
  # using the silverman test to calculate the number of modes with for each of
  # 1000 simulated paths
  fit3_uni = count_mode(y = fit3_sim, k = 1)
  fit3_bi = count_mode(y = fit3_sim[-fit3_uni$exclude], k = 2)
  fit3_tri = count_mode(y = fit3_sim[-fit3_uni$exclude][-fit3_bi$exclude], k = 3)
  
  # fitting distortion using growth rate minimising the objective function 
  # different selection criteria
  fit4_AIC = auto.arima(x4, trace=TRUE, ic = "aic")
  fit4_AICC = auto.arima(x4, trace=TRUE, ic = "aicc")
  fit4_AICC$aic; fit4_AICC$bic; fit4_AICC$aicc
  fit4_BIC = auto.arima(x4, trace=TRUE, ic = "bic")
  
  # 95% confidence interval 
  fit4_ci = confint(fit4_AICC)
  
  # creating 1000 paths from the time series models
  fit4_sim = list()
  for (i in 1:1000) {
    fit4_sim[[i]] = arima.sim(n = 231, list(order = c(0,1,0)),
                              sd = sqrt(fit4_AICC$sigma2))
  }
  
  # function used to calculate the number of modes with no bootstrapping
  fit4_mode = NA
  for (i in 1:1000) {
    fit4_mode[i] = nr.modes(density(fit4_sim[[i]])$y)
  }
  table(fit4_mode)
  
  # using the silverman test to calculate the number of modes with for each of
  # 1000 simulated paths
  fit4_uni = count_mode(y = fit4_sim, k = 1)
  fit4_bi = count_mode(y = fit4_sim[-fit4_uni$exclude], k = 2)
  fit4_tri = count_mode(y = fit4_sim[-fit4_uni$exclude][-fit4_bi$exclude], k = 3)
  
  # Silverman test section
  
  # using g of 10 years
  
  # the H0: number of modes <= 1 
  test_x1 = list(silverman.test(density(x1)$y, k=1, R = 10000, digits = 6))
  
  # using g of 15 years
  # the H0: number of modes <= 1 
  # the H0: number of modes <= 2 
  test_x2 = list(silverman.test(density(x2)$y, k=1, R = 10000, digits = 6),
                 silverman.test(density(x2)$y, k=2, R = 10000, digits = 6))
  
  # using g over whole dataset
  # the H0: number of modes <= 1
  # the H0: number of modes <= 2
  test_x3 = list(silverman.test(density(x3)$y, k=1, R = 10000, digits = 6),
                 silverman.test(density(x3)$y, k=2, R = 10000, digits = 6))
  
  # using g that min obj
  # the H0: number of modes <= 1
  # the H0: number of modes <= 2
  # the H0: number of modes <= 3
  # the H0: number of modes <= 4
  test_x4 = list(silverman.test(density(x4)$y, k=1, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=2, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=3, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=4, R = 10000, digits = 6))
  
  # function used to calculate the number of modes with no bootstrapping
  mode1 = nr.modes(density(x1)$y)
  mode2 = nr.modes(density(x2)$y)
  mode3 = nr.modes(density(x3)$y)
  mode4 = nr.modes(density(x4)$y)
  
  # calculating the number of modes for the distortions using a growth rate
  # simulated from a random uniform distribution
  mode = NA
  for (i in 1:1000) {
    mode[i] = nr.modes(density(simulations_dist[[i]])$y)
  }
  table(mode)
  
  # using the silverman test to calculate the number of modes with for each of
  # the distortions using a growth rate simulated from a 
  # random uniform distribution
  uni = count_mode(y = simulations_dist, k = 1)
  # calculating how much distortions have 2 modes excluding the distortions
  # that have 1 mode
  bi = count_mode(y = simulations_dist[-uni$exclude], k = 2)
  # calculating how much distortions have 2 modes excluding the distortions
  # that have 2 mode
  tri = count_mode(y = simulations_dist[-uni$exclude][-bi$exclude], k = 3)

  # getting the smallest bandwith value so that the gaussian kernel 
  # density estimate of the given data x has k mode where k = 1,2,3,4
  # for each of the growth rates.
  h1 = NA;h2 = NA;h3 = NA;h4 = NA
  for (i in 1:4) {
    h1[i] = h.crit(x = x1, k = i, prec = 6)
    h2[i] = h.crit(x = x2, k = i, prec = 6)
    h3[i] = h.crit(x = x3, k = i, prec = 6)
    h4[i] = h.crit(x = x4, k = i, prec = 6)
  }
    
})

# check time elapsed in seconds
time
# roughly 5 hours to run

# update loaded "TOP40.RData" file 
save.image(paste0("RData/",name,".RData"))
