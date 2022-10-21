rm(list = ls())
set.seed(2022)

# RUN THESE PARAMETERS, THEN RUN FROM THE FUNCTIONS ONWARDS
# parameters from Gaunersdorfer and Hommes (2007)

ybar = 1
sigma_delta = 0
r = 0.001
nu = 1
g = 1.89
sigma_sq = 1
sigma_epsilon = 10
eta = 0
alpha = 2000
beta = 2
a = 1/sigma_sq

##############################################

# p* is the fundamental value of the asset
p_star = ybar/r

######################################################
# RUN FROM HERE ONWARDS

# get chartists forecast for next time period

expected_chartists = function(p_tminus1, p_tminus2, g){
  exp = p_tminus1 + g * (p_tminus1 - p_tminus2)
  return(exp)
}

# get fundamental forecast for next time period

expected_fundamental = function(p_star, p_tminus1, nu){
  exp = p_star + nu * (p_tminus1 - p_star)
  return(exp)
}

# price equation at time period t

price = function(ybar, r, n_ch_t, n_fun_t, exp_ch_tplus1, exp_fun_tplus1){
  epsilon_t = rnorm(1, 0, sigma_epsilon)
  p_t = (1/(1 + r)) * ((n_ch_t * exp_ch_tplus1) + 
                         (n_fun_t * exp_fun_tplus1) + 
                         ybar) + epsilon_t
  return(p_t)
}

# number of chartists

market_share_chartists = function(p_star, alpha, beta, 
                                  U_tminus1_ch, U_tminus1_fun, p_tminus1){
  n_ch = (1/(1 + exp(beta * (U_tminus1_fun - U_tminus1_ch)))) * exp(-((p_star - p_tminus1)^2)/alpha)
  return(n_ch)
}

# accumulated realised profits by speculator - chartists

utility_chartists = function(r, eta, p_t, p_tminus1, 
                             y_t, z_tminus1_ch, U_tminus1_ch){
  U_t = (p_t + y_t - (1 + r) * p_tminus1) * z_tminus1_ch + (eta * U_tminus1_ch)
  return(U_t)
}

# accumulated realised profits by speculator - fundamentalists

utility_fundamentalists = function(r, eta, p_t, p_tminus1, 
                                   y_t, z_tminus1_fun, U_tminus1_fun){
  U_t = (p_t + y_t - (1 + r) * p_tminus1) * z_tminus1_fun + (eta * U_tminus1_fun)
  return(U_t)
}

# demand for asset - chartists

z_ch = function(sigma_sq, a, p_t, exp_ch_tplus1, y_tplus1){
  z_t = (exp_ch_tplus1 + y_tplus1 - (1 + r) * p_t)/(a * sigma_sq)
  return(z_t)
}

# demand for asset - fundamentalists

z_fun = function(sigma_sq, a, p_t, exp_fun_tplus1, y_tplus1){
  z_t = (exp_fun_tplus1 + y_tplus1 - (1 + r) * p_t)/(a * sigma_sq)
  return(z_t)
}


# for loop for simulation

n = 37800
y_t = matrix(0, nrow = n, ncol = 1)
p_t = matrix(0, nrow = n, ncol = 1)

exp_t_ch = matrix(0, nrow = n, ncol = 1)
exp_t_fun = matrix(0, nrow = n, ncol = 1)

z_t_ch = matrix(0, nrow = n, ncol = 1)
z_t_fun = matrix(0, nrow = n, ncol = 1)

U_t_ch = matrix(0, nrow = n, ncol = 1)
U_t_fun = matrix(0, nrow = n, ncol = 1)

n_t_ch = matrix(0, nrow = n, ncol = 1)
n_t_fun = matrix(0, nrow = n, ncol = 1)

y_t[1] = ybar
y_t[2] = ybar

p_t[1] = 1000
p_t[2] = 1000

exp_t_fun[1] = 1000
exp_t_fun[2] = 1000

exp_t_ch[1] = 1000
exp_t_ch[2] = 1000

# 3 to n
for (i in 3:n){
  
  delta_tplus1 = rnorm(1, 0, sigma_delta)
  y_t[i] = ybar + delta_tplus1
  
  exp_t_ch[i] = expected_chartists(p_t[i-1], p_t[i-2], g)
  exp_t_fun[i] = expected_fundamental(p_star, p_t[i-1], nu)
  
  n_t_ch[i] = market_share_chartists(p_star, alpha, beta, U_t_ch[i-1], 
                                     U_t_fun[i-1], p_t[i-1])
  
  # n_t_ch[i] = 0.25
  
  n_t_fun[i] = 1 - n_t_ch[i]
  
  p_t[i] = price(ybar, r, n_t_ch[i], n_t_fun[i], exp_t_ch[i], exp_t_fun[i])
  
  z_t_ch[i] = z_ch(sigma_sq, a, p_t[i], exp_t_ch[i], y_t[i])
  z_t_fun[i] = z_fun(sigma_sq, a, p_t[i], exp_t_fun[i], y_t[i])
  
  U_t_ch[i] = utility_chartists(r, eta, p_t[i], p_t[i-1], 
                                y_t[i], z_t_ch[i-1], U_t_ch[i-1])
  
  U_t_fun[i] = utility_fundamentalists(r, eta, p_t[i], p_t[i-1], 
                                       y_t[i], z_t_fun[i-1], U_t_fun[i-1])
  
}

dist = p_t - p_star
hist(dist, breaks = 100)

chart = matrix(0, nrow = 1800, ncol = 1)
mon_price = matrix(0, nrow = 1800, ncol = 1)
for(i in 1:1800){
  pos = i * 21
  mon_price[i] = p_t[pos]
  chart[i] = n_t_ch[pos]
}

png("Images/ABM/G&H2007_ts.png")
plot(x = c(1:1800), y = mon_price, type = 'l', ylim = c(200, 1800),
     xlab = 'Time', ylab = 'Price', lwd = 2)
axis(side = 2, at = c(250, 750, 1250, 1750))
axis(side = 1, at = c(250, 750, 1250, 1750))
abline(h = 1000, col = 'red', lwd = 3)
dev.off()

png("Images/ABM/G&H2007_hist.png")
dist = log(mon_price) - log(p_star)
hist(dist, breaks = 100,
     xlab = 'Distortion', main = '')
box(lty = 'solid', col = 'black')
dev.off()

png("Images/ABM/G&H2007_smooth_hist.png")
plot(density(dist), xlab = 'Distortion', main = '', lwd = 2)
dev.off()

png("Images/ABM/G&H2007_fracChart.png")
plot(chart, mon_price, type = 'l', ylim = c(400, 1600), yaxt = 'n',
     xlab = 'Fraction of Chartists', ylab = 'Price', lwd = 0.5)
axis(side = 2, at = c(500, 750, 1000, 1250, 1500))
dev.off()



