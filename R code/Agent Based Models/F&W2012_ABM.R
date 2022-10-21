rm(list = ls())
set.seed(2022)

# RUN THESE PARAMETERS, THEN RUN FROM THE FUNCTIONS ONWARDS
# parameters from Franke and Westerhoff(2012)

mu = 0.01
beta = 1
p_star = 0
chi = 1.5
phi = 0.12
sigma_C = 2.087
sigma_F = 0.758
alpha_0 = -0.327
alpha_N = 1.79
alpha_P = 18.43

##############################################

price = function(p_tminus1, mu, n_C, n_F, d_C, d_F){
  p_t = p_tminus1 + mu * (n_C * d_C + n_F * d_F)
  return(p_t)
}

orders_chartists = function(chi, p_tminus1, p_t, error_C){
  d_C = chi * (p_t - p_tminus1) + error_C
  return(d_C)
}

orders_fundamentalists = function(phi, p_star, p_t, error_F){
  d_F = phi * (p_star - p_t) + error_F
  return(d_F)
}

market_share_chartists = function(beta, a_tminus1){
  n_t = 1/(1 + exp(beta * a_tminus1))
  return(n_t)
}

relativefitness_t = function(alpha_0, alpha_N, alpha_P, n_C, n_F, p_t, p_star){
  a_t = alpha_0 + alpha_N * (n_F - n_C) + alpha_P * (p_t - p_star)^2
  return(a_t)
}

n = 37800
p_t = matrix(0, nrow = n, ncol = 1)

d_t_ch = matrix(0, nrow = n, ncol = 1)
d_t_fun = matrix(0, nrow = n, ncol = 1)

n_t_ch = matrix(0, nrow = n, ncol = 1)
n_t_fun = matrix(0, nrow = n, ncol = 1)

a_t = matrix(0, nrow = n, ncol = 1)

p_t[1] = 0

for (i in 2:n){
  p_t[i] = price(p_t[i-1], mu, n_t_ch[i-1], n_t_fun[i-1], d_t_ch[i-1], d_t_fun[i-1])
  error_C = rnorm(1, 0, sigma_C)
  d_t_ch[i] = orders_chartists(chi, p_t[i-1], p_t[i], error_C)
  error_F = rnorm(1, 0, sigma_F)
  d_t_fun[i] = orders_fundamentalists(phi, p_star, p_t[i], error_F)
  n_t_ch[i] = market_share_chartists(beta, a_t[i-1])
  n_t_fun[i] = 1 - n_t_ch[i]
  a_t[i] = relativefitness_t(alpha_0, alpha_N, alpha_P, n_t_ch[i], n_t_fun[i], p_t[i], p_star)
}

mon_price = matrix(0, nrow = 1800, ncol = 1)
chart = matrix(0, nrow = 1800, ncol = 1)
for(i in 1:1800){
  pos = i * 21
  mon_price[i] = p_t[pos]
  chart[i] = n_t_ch[pos]
}

png("Images/ABM/F&W2012_ts.png")
plot(x = c(1:1800), y = mon_price, type = 'l', 
     ylab = 'Log Price', xlab = 'Time', lwd = 2, 
     ylim = c(-0.7, 0.7))
abline(h = 0, col = 'red', lwd = 3)
dev.off()

png("Images/ABM/F&W2012_hist.png")
dist = mon_price - p_star
hist(dist, breaks = 100,
     xlab = 'Distortion', main = '')
box(lty = 'solid', col = 'black')
dev.off()

png("Images/ABM/F&W2012_smooth_hist.png")
plot(density(dist), xlab = 'Distortion', main = '', lwd = 2)
dev.off()

png("Images/ABM/F&W2012_fracChart.png")
plot(chart, mon_price, type = 'l', ylim = c(-0.5, 0.5), xlim = c(0, 1), 
     yaxt = 'n',
     xlab = 'Fraction of Chartists', ylab = 'Log Price', lwd = 0.5)
axis(side = 2, at = c(-0.5, -0.25, 0, 0.25, 0.5))
dev.off()




