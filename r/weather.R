library(Rcpp)
library(mvtnorm)
library(tidyverse)
sourceCpp("M52Sigma_2D.cpp")

# read in data and a few quick plots
weather = read.csv('../data/weather.csv')

ggplot(weather) + geom_point(aes(lon, lat, color=temperature), size=3) +  scale_colour_gradient2()
ggplot(weather) + geom_point(aes(lon, lat, color=pressure), size=3) +  scale_colour_gradient2()


# observed lat/lon pairs
x_obs = cbind(weather$lon, weather$lat)

# quick sanity check of the covariance function
kappa0 = exp(c(0,0,0))
C = M52Sigma_2D(x_obs, x_obs, c(kappa0, 1e-3))


####
# informal unit test of Rcpp function
####

# test values of hyperpar and test points
kappa_test = c(1.5, 1.1, 0.4)
test1 = matrix(c(-124.4, 41.900), nrow=1)
test2 = matrix(c(-124.9, 41.200), nrow=1)

# calculate covariance by hand
d2 = ((test1[1,1] - test2[1,1])/kappa_test[1])^2 + ((test1[1,2] - test2[1,2])/kappa_test[2])^2
c_byhand = kappa_test[3]*{1 + sqrt(5)*sqrt(d2) + (5/3)*d2}*exp(-sqrt(5)*sqrt(d2))

# Check for equality with the result from M52Sigma_2D
all.equal(c_byhand, M52Sigma_2D(test1, test2, c(kappa_test, 0)) %>% as.numeric)



####
# nmll_matern52
# the negative log marginal likelihood function to be minimized
# @theta: vector [log b_lon, log b_lat, log tau2_1]
# @x: matrix of (x1, x2) spatial coordinates for observed data points
# @y: vector of observations corresponding to the lat/lon locations
nmll_matern52 = function(theta, x, y, nugget = 1e-6) {
	require(mvtnorm)
	kappa = c(exp(theta), nugget)
	Sigma = M52Sigma_2D(x, x, kappa)
	-dmvnorm(y, sigma=Sigma, log=TRUE)
}


# single run of Nelder-Mead for temperature
out_temp = optim(par=c(0, 0, 0), fn=nmll_matern52, method='Nelder-Mead',
	x = x_obs, y=weather$temperature, nugget = 0.01)
out_temp

# try to beat it with multiple runs
for(i in 1:20) {
	out_try = optim(par=rnorm(3, 0, 2), fn=nmll_matern52, method='Nelder-Mead',
	x = x_obs, y=weather$temperature, nugget = 0.01)
	if(out_try$value < out_temp$value) {
		out_temp = out_try
	}
}

# empirical Bayes solution and evaluate covariance function at this par
kappa_temp = exp(out_temp$par)
C_temp = M52Sigma_2D(x_obs, x_obs, c(kappa_temp, 1e-2))
C_temp_inv = solve(C_temp)

# grid of points for plotting
x1_grid = seq(min(weather$lon), max(weather$lon), length=100)
x2_grid = seq(min(weather$lat), max(weather$lat), length=100)
x_grid = expand.grid(x1=x1_grid, x2= x2_grid) %>% as.matrix

# Compute relevant chunks of the joint covariance matrix
C_grid_temp = M52Sigma_2D(x_grid, x_grid, c(kappa_temp, 1e-2))
C_cross_temp = M52Sigma_2D(x_obs, x_grid, c(kappa_temp, 1e-2))

# posterior moments for f(x) at grid points
Sig_grid_temp = C_grid_temp - t(C_cross_temp) %*% C_temp_inv %*% C_cross_temp
mu_grid_temp = t(C_cross_temp) %*% C_temp_inv %*% weather$temp


#### Pressure

# single run of Nelder-Mead for temperature
# note larger nugget that is still negligible on the pressure scale
out_pre = optim(par=c(0, 0, 0), fn=nmll_matern52, method='Nelder-Mead',
	x = x_obs, y=weather$pressure, nugget = 10)

# try to beat it with multiple runs
for(i in 1:20) {
	out_try = optim(par=rnorm(3, 0, 3), fn=nmll_matern52, method='Nelder-Mead',
	x = x_obs, y=weather$pressure, nugget = 10)
	if(out_try$value < out_pre$value) {
		out_pre = out_try
	}
}

# empirical Bayes solution and evaluate covariance function at this par
kappa_pre = exp(out_pre$par)
C_pre = M52Sigma_2D(x_obs, x_obs, c(kappa_pre, 10))
C_pre_inv = solve(C_pre)


# Compute relevant chunks of the joint covariance matrix
C_grid_pre = M52Sigma_2D(x_grid, x_grid, c(kappa_pre, 1e-2))
C_cross_pre = M52Sigma_2D(x_obs, x_grid, c(kappa_pre, 1e-2))

# posterior moments for f(x) at grid points
Sig_grid_pre = C_grid_pre - t(C_cross_pre) %*% C_pre_inv %*% C_cross_pre
mu_grid_pre = t(C_cross_pre) %*% C_pre_inv %*% weather$pressure


# gather up posterior mean in a data frame for plotting
D_plot = data.frame(x_grid, temp_hat = mu_grid_temp, pre_hat = mu_grid_pre)




ggplot(weather) + geom_point(aes(lon, lat, color=temperature), size=3) +  scale_colour_gradient2()

# heatmap style
ggplot(D_plot, aes(x1, x2, fill=temp_hat)) + geom_tile()  + scale_fill_gradient2()

# contour plot
ggplot(D_plot, aes(x1, x2, z=temp_hat)) +
	stat_contour(bins=15)
	 
# combined
ggplot(D_plot, aes(x1, x2, z=temp_hat)) +
	stat_contour(geom='polygon',aes(fill=..level..)) + 
	 scale_fill_gradient2() + 
	 geom_tile(aes(fill = temp_hat)) + 
	 stat_contour(bins=15, color='grey') +
	labs(title="Temperature forecasting error (degrees C)")



# now pressure
ggplot(D_plot, aes(x1, x2, z=pre_hat)) +
	stat_contour(geom='polygon',aes(fill=..level..)) + 
	 scale_fill_gradient2() + 
	 geom_tile(aes(fill = pre_hat)) + 
	 stat_contour(bins=15, color='grey') +
	labs(title="Pressure forecasting error (Pascals)")

ggplot(weather) + geom_point(aes(lon, lat, color=pressure), size=3) +  scale_colour_gradient2()


# Big question: what source of information does this analysis ignore?
# hint? try cor(weather$temperature, weather$pressure)