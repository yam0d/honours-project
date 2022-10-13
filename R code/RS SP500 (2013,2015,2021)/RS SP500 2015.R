# Reading "RS SP500 2015.RData" in and producing results
# Check file configuration in "RData" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(devtools)
library(silvermantest)
library(forecast)

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

# Robert Shiller data set used in Schmitt and Westerhoff (2017)
name = "RS SP500 2015"      

# load in .RData
load(paste0("RData/",name,".RData"))

time = system.time({
  
  # Calculate the historical real average return using the GAR and the 
  # using the GAR for the average dividend growth rate for the 10
  # and 15 years and g over the whole dataset.
  
  index = real_dividend[which(Data == "2005-01-31"):which(Data == "2014-12-31")]
  return_dividend = diff(index) / index[length(index)] + 1 
  # growth rate of last 10 years
  g_10 = prod(return_dividend)^(1/length(return_dividend)) - 1
  
  index = real_dividend[which(Data == "1995-01-31"):which(Data == "2014-12-31")]
  return_dividend = diff(index) / index[length(index)] + 1
  # growth rate of last 20 years
  g_20 = prod(return_dividend)^(1/length(return_dividend)) - 1
  
  index = real_dividend[which(Data == "1985-01-31"):which(Data == "2014-12-31")]
  return_dividend = diff(index) / index[length(index)] + 1 
  # growth rate of last 30 years
  g_30 = prod(return_dividend)^(1/length(return_dividend)) - 1
  
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
  FV_20 = FV(real_dividend, length(real_dividend), r, g=g_20)
  FV_30 = FV(real_dividend, length(real_dividend), r, g=g_30)
  FV_min = FV(real_dividend, length(real_dividend), r, g=g_min)
  
  # creating a log price time series object
  log_realprice = ts(log(real_price), start = c(1871,1), frequency = 12)
  
  # creating ts objects for the distortion using different growth rates
  dis_10 = ts(log_realprice - log(FV_10), start = c(1871,1), frequency = 12)
  dis_20 = ts(log_realprice - log(FV_20), start = c(1871,1), frequency = 12)
  dis_30 = ts(log_realprice - log(FV_30), start = c(1871,1), frequency = 12)
  dis_min = ts(log_realprice - log(FV_min), start = c(1871,1), frequency = 12)
  
  # generating growth rates from a random uniform distribution with a set seed
  # then calculating the respective FV and storing the distortion in a list
  set.seed(2022)
  g_uni = sort(runif(1000, min = -r, max = r))
  # list for FV's of all growth rates generated
  simulations = list()
  for (i in 1:1000) {
    simulations[[i]] = ts(log(FV(real_dividend, length(real_dividend), 
                                 r, g=g_uni[i])), 
                          start = c(1871,1), frequency = 12)}
  # list for distortions of all growth rates generated
  simulations_dist = list()
  for (i in 1:1000) {
    simulations_dist[[i]] = ts(log_realprice - simulations[[i]], 
                               start = c(1871,1), frequency = 12)}
  
  # changing ts objects to vectors
  x1 = as.vector(dis_10)
  x2 = as.vector(dis_20)
  x3 = as.vector(dis_30)
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
  
  # Use auto.arima with ic = "aicc"
  # fitting distortion using a  g = 10 years
  # fitting a arma(1,1), arma(2,2) and using auto.arima()
  
  fit1 = auto.arima(x1, trace=TRUE)
  fit1$aic; fit1$bic; fit1$aicc
  fit1_ci = confint(fit1)
  
  fit2 = Arima(x1, order = c(2,0,2))
  fit2$aic; fit2$bic; fit2$aicc
  fit2_ci = confint(fit2)
  
  fit3 = Arima(x1, order = c(1,0,1))
  fit3$aic; fit3$bic; fit3$aicc
  fit3_ci = confint(fit3)
  
  # 1000 simulations from each of the models
  
  fit1_sim = list()
  for (i in 1:1000) {
    fit1_sim[[i]] = arima.sim(n = 1740, list(order = c(4,1,1),
                                             ar = c(fit1$coef[1], fit1$coef[2], fit1$coef[3], fit1$coef[4]), 
                                         ma = c(fit1$coef[5])),
                          sd = sqrt(fit1$sigma2))
  }
  fit2_sim = list()
  for (i in 1:1000) {
    fit2_sim[[i]] = arima.sim(n = 1740, list(ar = c(fit2$coef[1], fit2$coef[2]), 
                                         ma = c(fit2$coef[3], fit2$coef[4])),
                          sd = sqrt(fit2$sigma2))
  }
  
  fit3_sim = list()
  for (i in 1:1000) {
    fit3_sim[[i]] = arima.sim(n = 1740, list(ar = c(fit3$coef[1]), 
                                         ma = c(fit3$coef[2])),
                          sd = sqrt(fit3$sigma2))
  }
  
  # Using the silverman test to classify the number of modes of the 1000
  # simulated paths for each model
  
  fit1_uni = count_mode(y = fit1_sim, k = 1)
  fit1_bi = count_mode(y = fit1_sim[-fit1_uni$exclude], k = 2)
  
  fit2_uni = count_mode(y = fit2_sim, k = 1)
  fit2_bi = count_mode(y = fit2_sim[-fit2_uni$exclude], k = 2)
  
  fit3_uni = count_mode(y = fit3_sim, k = 1)
  fit3_bi = count_mode(y = fit3_sim[-fit3_uni$exclude], k = 2)
  
  # Silverman test section
  # test number of modes = 1,2,3,4 on all distortions using a growth rate 
  # of the last 10,20,30 years and minimising the objective function.
  # when running if the p-value > 0.05 for a specific distortion then ignore
  # further testing of that distortion.
  
  # the H0: number of modes <= 1 
  test_k1 = list(silverman.test(density(x1)$y, k=1, R = 10000, digits = 6),
                 silverman.test(density(x2)$y, k=1, R = 10000, digits = 6),
                 silverman.test(density(x3)$y, k=1, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=1, R = 10000, digits = 6))
  
  # the H0: number of modes <= 2 
  test_k2 = list(silverman.test(density(x1)$y, k=2, R = 10000, digits = 6),
                 silverman.test(density(x2)$y, k=2, R = 10000, digits = 6),
                 silverman.test(density(x3)$y, k=2, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=2, R = 10000, digits = 6))
  
  # the H0: number of modes <= 3 
  test_k3 = list(silverman.test(density(x1)$y, k=3, R = 10000, digits = 6),
                 silverman.test(density(x2)$y, k=3, R = 10000, digits = 6),
                 silverman.test(density(x3)$y, k=3, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=3, R = 10000, digits = 6))
  
  #the H0: number of modes <= 4 
  test_k4 = list(silverman.test(density(x1)$y, k=4, R = 10000, digits = 6),
                 silverman.test(density(x2)$y, k=4, R = 10000, digits = 6),
                 silverman.test(density(x3)$y, k=4, R = 10000, digits = 6),
                 silverman.test(density(x4)$y, k=4, R = 10000, digits = 6))
  
  uni = count_mode(y = simulations_dist, k = 1)
  bi = count_mode(y = simulations_dist[-uni$exclude], k = 2)
  
  # check how many times the silverman test classifies the density of the
  # the distortion when using a growth rate of the last 30 years as bimodal
  # vs multimodal.
  p_test = list()
  for (i in 1:1000) {
    p_test[[i]] = silverman.test(density(x3)$y, k=2, R = 10000, digits = 6)
  }
  
  p_total = c()
  for (i in 1:1000) {
    p_total[i] = p_test[[i]]@p_value>0.05
  }
  sum(p_total)
  
  # getting the smallest bandwith value so that the gaussian kernel 
  # density estimate of the given data x has k mode where k = 1,2,3,4
  # for each of the growth rates.
  h1 = 0
  h2 = 0 
  for (i in 1:4) {
    h1[i] = h.crit(x = x1, k = i, prec = 6)
    h2[i] = h.crit(x = x3, k = i, prec = 6)
  }
  
})

# check time elapsed in seconds
time
# roughly 5 hours to run

# update loaded "RS SP500 2015.RData" file 
save.image(paste0("RData/",name,".RData"))
