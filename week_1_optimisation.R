# Machine learning and Statistical Learning
# Master 2 EBDS
# by Ewen Gallic
# December 2022

# 1. Gradient Descent -----------------------------------------------------


# 1.2 A First Example in Dimension 1 --------------------------------------

library(tidyverse)

x <- seq(-3, 3, by = .1)
f <- function(x) (x+3)*(x-2)^2*(x+1)
y <- f(x)
df <- tibble(x = x, y = y)
df

ggplot(data = df, aes(x=x, y=y)) +
  geom_line() +
  geom_segment(
    data = tibble(x = -1-sqrt(3/2), xend = -1-sqrt(3/2),
                  y = -Inf, yend = f(-1-sqrt(3/2))),
    mapping = aes(x = x, y=y, xend=xend, yend = yend),
    colour = "red", linetype = "dashed") +
  geom_point(x=-1-sqrt(3/2), y = f(-1-sqrt(3/2)), colour = "red") +
  labs(title = "Minimising a more complex loss function.")


# Starting value
starting_value <- -.5
f(starting_value)


ggplot(data = df, aes(x=x, y=y)) +
  geom_line() +
  geom_point(x=starting_value, y = f(starting_value), colour = "red") +
  labs(title = "Start at a random point")


library(numDeriv)
grad <- grad(func = f, x = c(starting_value))
grad


(intercept <- -grad*starting_value + f(starting_value))

ggplot(data = df, aes(x=x, y=y)) +
  geom_line() +
  geom_point(x=starting_value, y = f(starting_value), colour = "red") +
  geom_abline(slope = grad, intercept = intercept, colour = "blue") +
  labs(title = "Compute the derivative of the function at that point")


# Setting the learning rate
learning_rate <- 10^-2

# Update the parameter
(x_1 <- starting_value - learning_rate * grad)


ggplot(data = df, aes(x=x, y=y)) +
  geom_line() +
  geom_point(x=starting_value, y = f(starting_value), colour = "red") +
  geom_point(x=x_1, y = f(x_1), colour = "green") +
  labs(title = "Second iteration")


# Maximum number of iteration
nb_max_iter <- 100
tolerance <- 10^-5

x_1 <- -.5
# To keep track of the values through the iterations
x_1_values <- x_1
y_1_values <- f(x_1)
gradient_values <- NULL
intercept_values <- NULL

for(i in 1:nb_max_iter){
  # Steepest ascent:
  grad <- grad(func = f, x = c(x_1))
  
  intercept_value <- -grad*x_1 + f(x_1)
  # Keeping track
  gradient_values <- c(gradient_values, grad)
  intercept_values <- c(intercept_values, intercept_value)
  
  # Updating the value
  x_1 <- x_1 - learning_rate * grad
  y_1 <- f(x_1)
  
  # Keeping track
  x_1_values <- c(x_1_values, x_1)
  y_1_values <- c(y_1_values, y_1)
  
  # Stopping if no improvement (decrease of the cost function too small)
  if(abs(y_1_values[i] - y_1) < tolerance) break
  
}

# The algorithm stopped at iteration:
i
ifelse(i < nb_max_iter,
       "The algorithm converged.",
       "The algorithm did not converge.")


# Preparing data for plot
df_plot <- 
  tibble(x_1 = x_1_values[-length(x_1_values)],
         y = f(x_1),
         gradient = gradient_values,
         intercept = intercept_values
  )
df_plot

# Looking at the iterative process
ggplot() +
  geom_line(data = df, aes(x = x, y = y)) +
  geom_point(data = df_plot, aes(x = x_1, y= f(x_1)), colour = "red", size = 2) +
  coord_cartesian(ylim = c(-20, 20)) +
  labs(title = str_c("Step ", i))


# Same algorithm but with a larger learning rate
# and a higher number of max iterations
learning_rate <- 0.05
nb_max_iter <- 1000
tolerance <- 10^-5
# Starting value
x_1 <- -.5
# To keep track of the values through the iterations
x_1_values <- x_1
y_1_values <- f(x_1)
gradient_values <- NULL
intercept_values <- NULL

for(i in 1:nb_max_iter){
  # Steepest ascent:
  grad <- grad(func = f, x = c(x_1))
  
  intercept_value <- -grad*x_1 + f(x_1)
  # Keeping track
  gradient_values <- c(gradient_values, grad)
  intercept_values <- c(intercept_values, intercept_value)
  
  # Updating the value
  x_1 <- x_1 - learning_rate * grad
  y_1 <- f(x_1)
  
  # Keeping track
  x_1_values <- c(x_1_values, x_1)
  y_1_values <- c(y_1_values, y_1)
  
  # Stopping if no improvement (decrease of the cost function too small)
  if(abs(y_1_values[i] - y_1) < tolerance) break
  
}

i
ifelse(i < nb_max_iter,
       "The algorithm converged.",
       "The algorithm did not converge.")

df_plot <- 
  tibble(x_1 = x_1_values[-length(x_1_values)],
         y = f(x_1),
         gradient = gradient_values,
         intercept = intercept_values
  )

df_plot

ggplot() +
  geom_line(data = df, aes(x = x, y = y)) +
  geom_point(data = df_plot, aes(x = x_1, y= f(x_1)), colour = "red", size = .2) +
  coord_cartesian(ylim = c(-20, 20)) +
  labs(title = str_c("Step ", i))


# To create an animated GIF, uncomment the following instructions.
# library(animation)
# saveGIF({
#
#   for(i in c(rep(1,5), 2:14, rep(15, 10))){
#     p <-
#       ggplot() +
#       geom_line(data = df, aes(x = x, y = y)) +
#       geom_point(data = df_plot %>% slice(i),
#                  mapping = aes(x = x_1, y= f(x_1)), colour = "red", size = 2) +
#       geom_abline(data = df_plot %>% slice(i),
#                   aes(slope = gradient, intercept = intercept), colour = "blue") +
#       coord_cartesian(ylim = c(-20, 20)) +
#       labs(title = str_c("Step ", i))
#     print(p)
#   }
#
# }, movie.name = "example_single_var_bounce.gif", interval = 0.5,
# ani.width = 720, ani.height = 480)


# Let us change the starting point

learning_rate <- 0.01
nb_max_iter <- 1000
tolerance <- 10^-5
# Starting value
x_1 <- .5
# To keep track of the values through the iterations
x_1_values <- x_1
y_1_values <- f(x_1)
gradient_values <- NULL
intercept_values <- NULL

for(i in 1:nb_max_iter){
  # Steepest ascent:
  grad <- grad(func = f, x = c(x_1))
  
  intercept_value <- -grad*x_1 + f(x_1)
  # Keeping track
  gradient_values <- c(gradient_values, grad)
  intercept_values <- c(intercept_values, intercept_value)
  
  # Updating the value
  x_1 <- x_1 - learning_rate * grad
  y_1 <- f(x_1)
  
  # Keeping track
  x_1_values <- c(x_1_values, x_1)
  y_1_values <- c(y_1_values, y_1)
  
  # Stopping if no improvement (decrease of the cost function too small)
  if(abs(y_1_values[i] - y_1) < tolerance) break
  
}

i
ifelse(i < nb_max_iter,
       "The algorithm converged.",
       "The algorithm did not converge.")


df_plot <- 
  tibble(x_1 = x_1_values[-length(x_1_values)],
         y = f(x_1),
         gradient = gradient_values,
         intercept = intercept_values
  )

df_plot

ggplot() +
  geom_line(data = df, aes(x = x, y = y)) +
  geom_point(data = df_plot, aes(x = x_1, y= f(x_1)), colour = "red") +
  coord_cartesian(ylim = c(-20, 20)) +
  labs(title = str_c("Step ", i))



# 1.3 Moving to Higher Dimensions Optimisation Problems -------------------


x_1 <- x_2 <- seq(-2, 2, by = 0.3)
z_f <- function(x_1,x_2) x_1^2+x_2^2
z <- outer(x_1, x_2, z_f)

library(plot3D)
par(mar = c(1, 1, 1, 1))
flip <- 1 # 1 or 2
th = c(-300,120)[flip]
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)

# Starting point
theta <- c(x_1 = 1.5, x_2 = 1.5)


# Plotting this point on a 3D surface plot
par(mar = c(1, 1, 1, 1))
flip <- 1 # 1 or 2
th = c(-300,120)[flip]
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)
zz <- z_f(theta[["x_1"]], theta[["x_2"]])
new_point <- trans3d(theta[["x_1"]], theta[["x_2"]], zz, pmat = pmat)
points(new_point,pch = 20,col = "red", cex=2)



# Function to be optimised
z_f_to_optim <- function(theta){
  x_1 <- theta[["x_1"]]
  x_2 <- theta[["x_2"]]
  x_1^2 + x_2^2
}


# Learning rate:
learning_rate <- 10^-2


# Steepest ascent
grad <- grad(func = z_f_to_optim, x = theta)
grad

# Updating the values
updated_x_1 <- theta[["x_1"]] - learning_rate * grad[1]
updated_x_2  <- theta[["x_2"]] - learning_rate * grad[2]
updated_theta <- c(x_1 = updated_x_1, x_2 = updated_x_2)
updated_theta


# Plotting the new value
par(mar = c(1, 1, 1, 1))
flip <- 1 # 1 or 2
th = c(-300,120)[flip]
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)
updated_zz <- z_f(updated_theta[["x_1"]], updated_theta[["x_2"]])
new_point_2 <- trans3d(updated_theta[["x_1"]], updated_theta[["x_2"]], updated_zz,
                       pmat = pmat)
points(new_point,pch = 20,col = "red", cex=2)
points(new_point_2,pch = 20,col = "darkgreen", cex=2)


# Preparing the iterative process
learning_rate <- 10^-1
nb_max_iter <- 100
tolerance <- 10^-5

# Starting values
theta <- c(x_1 = 1.5, x_2 = 1.5)

# To keep track of what happens at each iteration
theta_values <- list(theta)
y_values <- z_f_to_optim(theta)

for(i in 1:nb_max_iter){
  # Steepest ascent
  grad <- grad(func = z_f_to_optim, x = theta)
  
  # Updating the parameters
  updated_x_1 <- theta[["x_1"]] - learning_rate * grad[1]
  updated_x_2  <- theta[["x_2"]] - learning_rate * grad[2]
  theta <- c(x_1 = updated_x_1, x_2 = updated_x_2)
  
  # Keeping track
  theta_values <- c(theta_values, list(theta))
  
  # Checking for improvement
  y_updated <- z_f_to_optim(theta)
  y_values <- c(y_values, y_updated)
  if(abs(y_values[i] - y_updated) < tolerance) break
  
}

# The algorithm stopped at iteration:
i
ifelse(i < nb_max_iter,
       "The algorithm converged.",
       "The algorithm did not converge.")


par(mar = c(1, 1, 1, 1))
flip <- 1 # 1 or 2
th = c(-300,120)[flip]
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)

xx <- map_dbl(theta_values, "x_1")
yy <- map_dbl(theta_values, "x_2")
zz <- y_values
new_point <- trans3d(xx,yy,zz,pmat = pmat)
lines(new_point,pch = 20,col = "red", cex=2, lwd=2)
points(new_point,pch = 20,col = "red", cex=2)


# For an animated GIF, uncomment the following instructions
# saveGIF({
#
#   for(j in c(rep(1,5), 2:(i-1), rep(i, 10))){
#
#     par(mar = c(1, 1, 1, 1))
#     flip <- 1 # 1 or 2
#     th = c(-300,120)[flip]
#     pmat <-
#       persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
#               asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
#               d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)
#
#     xx <- map_dbl(theta_values, "x_1")[1:j]
#     yy <- map_dbl(theta_values, "x_2")[1:j]
#     zz <- y_values[1:j]
#     new_point <- trans3d(xx,yy,zz,pmat = pmat)
#     lines(new_point,pch = 20,col = "red", cex=2, lwd=2)
#     points(new_point,pch = 20,col = "red", cex=2)
#   }
#
# }, movie.name = "descent_2D_sphere.gif", interval = 0.01,
# ani.width = 720, ani.height = 480)

# Another example with a more complexe surface

x_1 <- seq(-6.5, 0, by = 0.3)
x_2 <- seq(-10, 0, by = 0.3)
z_f <- function(x_1,x_2){
  sin(x_2)*exp(1-cos(x_1))^2 + cos(x_1)*exp(1-sin(x_2))^2 + (x_1-x_2)^2
}
z <- outer(x_1, x_2, z_f)


par(mar = c(1, 1, 1, 1))
flip <- 2 # 1 or 2
theta = c(-300,120)[flip]
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)


# The function to optimize
z_f_to_optim <- function(theta){
  x_1 <- theta[1]
  x_2 <- theta[2]
  sin(x_2) * exp(1-cos(x_1))^2 + cos(x_1) * exp(1-sin(x_2))^2 + (x_1-x_2)^2
}


#' A version of the Gradient Descent algorithm
#' @param par Initial values for the parameters to be optimized over.
#' @param fn A function to be minimized, with first argument the vector 
#'           of parameters over which minimisation is to take place.
#'           It should return a scalar result.
#' @param learning_rate Learning rate.
#' @param nb_max_iter The maximum number of iterations (default to 100).
#' @param tolerance The absolute convergence tolerance (default to 10^-5).
gradient_descent <- function(par, fn, learning_rate, 
                             nb_max_iter = 100, tolerance = 10^-5){
  
  # To keep track of what happens at each iteration
  par_values <- list(par)
  y_values <- fn(par)
  
  for(i in 1:nb_max_iter){
    # Steepest ascent
    grad <- grad(func = fn, x = par)
    
    # Updating the parameters
    par <- par - learning_rate * grad
    
    # Keeping track
    par_values <- c(par_values, list(par))
    
    # Checking for improvement
    y_updated <- fn(par)
    y_values <- c(y_values, y_updated)
    rel_diff <- abs(y_values[i] - y_updated)
    if(rel_diff < tolerance) break
    
  }
  
  # Has the algorithm converged?
  convergence <- i < nb_max_iter | (rel_diff < tolerance)
  
  structure(
    list(
      par = par,
      value = y_updated,
      pars = do.call("rbind", par_values),
      values = y_values,
      convergence = convergence,
      nb_iter = i,
      nb_max_iter = nb_max_iter,
      tolerance = tolerance
    ))
  
}

# Running the Gradient Descent Algorithm
res_optim <- 
  gradient_descent(par = c(-6, -2), fn = z_f_to_optim,
                   learning_rate = 10^-2,
                   nb_max_iter = 100,
                   tolerance = 10^-5)


res_optim$convergence
res_optim$nb_iter


res_optim$par
res_optim$value


par(mar = c(1, 1, 1, 1))
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = 120, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)

xx <- res_optim$pars[,1]
yy <- res_optim$pars[,2]
zz <- res_optim$values
new_point <- trans3d(xx,yy,zz,pmat = pmat)
lines(new_point,pch = 20,col = "red", cex=2, lwd=2)
points(new_point,pch = 20,col = "red", cex=2)


# For an animated graph, uncomment the folllowing instruction.
# saveGIF({
#   for(j in c(rep(1,5), 2:(res_optim$nb_iter-1), rep(res_optim$nb_iter, 10))){
#
#     par(mar = c(1, 1, 1, 1))
#     pmat <-
#       persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
#               asp = 1, phi = 30, theta = 120, border = "grey10", alpha=.4,
#               d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1,
#               main = str_c("Step ", j))
#
#     xx <- res_optim$pars[1:j,1]
#     yy <- res_optim$pars[1:j,2]
#     zz <- res_optim$values[1:j]
#     new_point <- trans3d(xx,yy,zz,pmat = pmat)
#     lines(new_point,pch = 20,col = "red", cex=2, lwd=2)
#     points(new_point,pch = 20,col = "red", cex=2)
#   }
#
# }, movie.name = "descent_3D_Mishra.gif", interval = 0.01,
# ani.width = 720, ani.height = 480)


# 2d representation of the iterative process
d <- tibble(x = res_optim$pars[,1],
            y = res_optim$pars[,2],
            z = res_optim$values)
contour2D(x=x_1, y=x_2, z=z, colkey=F, main="Contour plot",
          xlab="x_1", ylab="x_2")
points(x=d$x, y=d$y, t="p", pch=19, col = "red")
for(k in 2:(nrow(d+1))) 
  segments(x0 = d$x[k-1], y0 = d$y[k-1], x1 = d$x[k], y1 = d$y[k], col= 'red')


# New run with another starting point
res_optim <- 
  gradient_descent(par = c(-6, -4), fn = z_f_to_optim,
                   learning_rate = 10^-2,
                   nb_max_iter = 1000,
                   tolerance = 10^-5)

res_optim$convergence
res_optim$nb_iter


par(mar = c(1, 1, 1, 1))
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 30, theta = 120, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)

xx <- res_optim$pars[,1]
yy <- res_optim$pars[,2]
zz <- res_optim$values
new_point <- trans3d(xx,yy,zz,pmat = pmat)
lines(new_point,pch = 20,col = "red", cex=2, lwd=2)
points(new_point,pch = 20,col = "red", cex=2)


d <- tibble(x = res_optim$pars[,1],
            y = res_optim$pars[,2],
            z = res_optim$values)
contour2D(x=x_1, y=x_2, z=z, colkey=F, main="Contour plot", 
          xlab="x_1", ylab="x_2")
points(x=d$x, y=d$y, t="p", pch=19, col = "red")
for(k in 2:(nrow(d+1))) 
  segments(x0 = d$x[k-1], y0 = d$y[k-1], x1 = d$x[k], y1 = d$y[k], col= 'red')

# 1.4 Case Study: Linear Regression ---------------------------------------

set.seed(123)
# Number of observations
n <- 50
# x randomly drawn from a continuous uniform distribution with bounds [0,10]
x <- runif(min = 0, max = 10, n = n)
# Error term from Normal distribution with zero mean and variance 4
error <- rnorm(n = n, mean = 0, sd = 2)
# Response variable
beta_0 <- 3
beta_1 <- -2
y <- beta_0*x + beta_1 + error


df <- tibble(x = x, y = y)
df


ggplot(data = df, aes(x = x, y = y)) + geom_point() +
  geom_abline(slope = 3, intercept = -2) +
  labs(title = "y = f(x)")

# The function to be optimised: MSE
obj_function <- function(theta, y){
  y_pred <- theta[1] + theta[2]*x
  mean((y - y_pred)^2)
}


# Starting values
beta <- c(0, 0)

# To keep track of the changes over the iterations
beta_values <- beta
mse_values  <- NULL

# Setting the learning rate
learning_rate <- 10^-2
# And the maximum number of iterations
nb_max_iter <- 1000
# We may want to stop the iterative process earlier:
# if there has been no improvement
abstol <- 10^-5

for(i in 1:nb_max_iter){
  # Predctions with the current values:
  y_pred <- beta[1] + beta[2]*x
  
  # Just for keeping track
  mse <- mean((y - y_pred)^2)
  mse_values <- c(mse_values, mse)
  
  gradient <- grad(func = obj_function, x = beta, y=y)
  
  # We could also use the exact expression here:
  # deriv_loss_beta <- -2/n * sum( y - y_pred )
  # deriv_loss_beta_0 <- -2/n * sum( x*(y - y_pred) )
  
  
  # Updating the value
  beta <- beta - learning_rate * gradient
  
  # Keeping track of the changes
  beta_values <- rbind(beta_values, beta)
  
  
  if(i>1){
    rel_diff <- abs(mse_values[i] - mse_values[i-1])
    if(rel_diff < abstol) break
  }
  
}

print(str_c("Number of iterations: ", i))
convergence <- i < nb_max_iter | (rel_diff < abstol)
convergence


# Estimated value
beta

# Comparing with OLS
lm(y~x)


ggplot(data = tibble(mse = mse_values) %>% 
         mutate(iteration = row_number()) %>% 
         filter(iteration > 1),
       aes(x = iteration, y = mse)) +
  geom_line()


as_tibble(beta_values, .name_repair = "minimal") %>% 
  magrittr::set_colnames(c("beta_0", "beta_1")) %>% 
  mutate(iteration = row_number()) %>% 
  ggplot(data = ., aes(x = beta_0, y = beta_1)) +
  geom_point(aes(colour = iteration), size = .1)


# 2. Variants of the Gradient Descent Algorithm ---------------------------



# 2.1 Frequency of Updates & Samples Used ---------------------------------


# 2.1.1 Stochastic Gradient Descent ---------------------------------------


set.seed(123)
# Number of observations
n <- 1000
# x randomly drawn from a continuous uniform distribution with bounds [0,10]
x_1 <- runif(min = 0, max = 10, n = n)
x_2 <- runif(min = 0, max = 10, n = n)
# Error term from Normal distribution with zero mean and variance 4
error <- rnorm(n = n, mean = 0, sd = 2)
# Response variable
beta_0 <- 3
beta_1 <- -2
beta_2 <- .5
true_beta <- c(beta_0=beta_0, beta_1=beta_1, beta_2=beta_2)
y <- beta_0 + beta_1*x_1 + beta_2*x_2 + error

# The objective function to me minimised
obj_function <- function(theta, y, X){
  y_pred <- X%*%theta
  mean((y - y_pred)^2)
}

# Data in a matrix
X <- cbind(rep(1, n), x_1, x_2)
colnames(X) <- c("Intercept", "x_1", "x_2")
head(X)

# Starting values
beta <- c(1,1,1)

# Setting the learning rate
learning_rate <- 10^-2
# And the number of epoch
nb_epoch <- 20


# To keep track of the iterative process
mse_values  <- NULL

pb <- txtProgressBar(min = 1, max=nb_epoch, style=3)
for(i_epoch in 1:nb_epoch){
  cat("\n----------\nEpoch: ", i_epoch, "\n")
  
  # Shuffle the order of observations
  index <- sample(1:n, size = n, replace=TRUE)
  
  for(i in 1:n){
    # The gradient is estimated using a single observation: the ith
    gradient <- grad(func = obj_function, x=beta, y=y[index[i]], X = X[index[i],])
    # Updating the value
    beta <- beta - learning_rate * gradient
  }
  
  # Just for keeping track (not necessary to run the algorithm)
  # (Significantly slows down the algorith)
  cost <- obj_function(beta, y, X)
  cat("MSE : ", cost, "\n")
  mse_values <- c(mse_values, cost)
  # End of keeping track
  
  
  setTxtProgressBar(pb, i_epoch)
}


# True values:
true_beta
# Estimates values:
(beta_sgd <- beta)


ggplot(data = tibble(epoch = 1:length(mse_values),
                     MSE = mse_values)) +
  geom_line(mapping = aes(x = epoch, y = MSE)) +
  labs(title = "Singular Gradient Descent")



#' Performs Stochastic Gradient Descent for a Linear Model.
#' @param par Initial values for the parameters.
#' @param fn A function to be minimized, with first argument the vector of 
#'           parameters over which minimisation is to take place.
#'           It should return a scalar result.
#' @param y Target variable.
#' @param X Matrix of predictors.
#' @param learning_rate Learning rate.
#' @param nb_epoch Number of epochs.
#' @param silent If TRUE (default), progress info. not printed in the console.
f_sgd <- function(par, fn, y, X, learning_rate=10^-2, nb_epoch=10, silent=TRUE){
  mse_values <- NULL
  for(i_epoch in 1:nb_epoch){
    if(!silent) cat("\n----------\nEpoch: ", i_epoch, "\n")
    n <- nrow(X)
    index <- sample(1:n, size = n, replace=TRUE)
    for(i in 1:n){
      gradient <- grad(func = fn, x=par, y=y[index[i]], X = X[index[i],])
      # Updating the value
      par <- par - learning_rate * gradient
    }
    
    # Just for keeping track (not necessary to run the algorithm)
    # Significantly slows down the algorith
    cost <- fn(par, y, X)
    if(!silent) cat("MSE : ", cost, "\n")
    mse_values <- c(mse_values, cost)
    # End of keeping track
  }
  structure(list(par = par, mse_values = mse_values,
                 nb_epoch = nb_epoch,
                 learning_rate = learning_rate))
}


start_time_sgd <- Sys.time()
estim_sgd <- f_sgd(par = c(1,1,1), fn = obj_function, y = y, X = X, 
                   silent=TRUE, nb_epoch = 20)
end_time_sgd <- Sys.time()

# Time elapsed
end_time_sgd-start_time_sgd

estim_sgd



# 2.1.2 Batch Gradient Descent --------------------------------------------

# Starting values
beta <- c(1,1,1)

# Setting the learning rate
learning_rate <- 10^-2
# And the number of epochs
nb_epoch <- 20

# To keep track of the MSE along the iterative process
mse_values  <- NULL


pb <- txtProgressBar(min = 1, max=nb_epoch, style=3)
for(i_epoch in 1:nb_epoch){
  cat("\n----------\nEpoch: ", i_epoch, "\n")
  # For each observation, we need to compute the gradient
  gradients <- rep(0, ncol(X))
  for(i in 1:n){
    gradient_current <- grad(func = obj_function, x=beta, y=y[i], X = X[i,])
    gradients <- gradients+gradient_current
  }
  # Then we divide by the number of observations to get the average
  avg_gradients <- gradients/n
  
  # Updating the value
  beta <- beta - learning_rate * avg_gradients
  
  # Just for keeping track (not necessary to run the algorithm)
  # Significantly slows down the algorith
  cost <- obj_function(beta, y, X)
  cat("MSE : ", cost, "\n")
  mse_values <- c(mse_values, cost)
  # End of keeping track
  
  
  setTxtProgressBar(pb, i_epoch)
}

# True values:
true_beta
# Estimated values:
(beta_batch <- beta)


#' Performs Batch Gradient Descent for a Linear Model
#' @param par Initial values for the parameters.
#' @param fn A function to be minimized, with first argument the vector of 
#'           parameters over which minimisation is to take place.
#'           It should return a scalar result.
#' @param y Target variable.
#' @param X Matrix of predictors.
#' @param learning_rate Learning rate.
#' @param nb_epoch Number of epochs.
#' @param silent If TRUE (default), progress info. not printed in the console.
batch_gd <- function(par, fn, y, X, learning_rate=10^-2, 
                     nb_epoch=10, silent=TRUE){
  mse_values  <- NULL
  n <- nrow(X)
  for(i_epoch in 1:nb_epoch){
    if(!silent) cat("\n----------\nEpoch: ", i_epoch, "\n----------")
    # For each observation in the batch, we need to compute the gradient
    gradients <- rep(0, ncol(X))
    for(i in 1:n){
      gradient_current <- grad(func = fn, x=par, y=y[i], X = X[i,])
      gradients <- gradients+gradient_current
    }
    # Then we divide by the number of observations to get the average
    avg_gradients <- gradients/n
    
    # Updating the value
    par <- par - learning_rate * avg_gradients
    
    # Just for keeping track (not necessary to run the algorithm)
    # Significantly slows down the algorithm
    cost <- fn(par, y, X)
    if(!silent) cat("MSE : ", cost, "\n")
    mse_values <- c(mse_values, cost)
    # End of keeping track
  }
  structure(list(par = par, mse_values = mse_values,
                 nb_epoch = nb_epoch,
                 learning_rate = learning_rate))
}


# To keep time
start_time_batch <- Sys.time()
estim_batch <- 
  batch_gd(par = c(1,1,1), fn = obj_function, y = y, X = X, silent=TRUE,
           nb_epoch = 20)
end_time_batch <- Sys.time()


end_time_batch-start_time_batch


tibble(MSE = estim_batch$mse_values, epoch = 1:estim_batch$nb_epoch) %>% 
  ggplot(data = ., mapping = aes(x=epoch, y = MSE)) +
  geom_line() +
  labs(x = "Epoch", y = "MSE", title = "Batch Gradient Descent")



# 2.1.3 Mini-Batch Gradient Descent ---------------------------------------

# Starting values
beta <- c(1,1,1)

# Setting the learning rate
learning_rate <- 10^-2
# And the number of epochs
nb_epoch <- 20
# The batch size also needs to be set
batch_size <- 250

# To keep track of the MSE along the iterations
mse_values  <- NULL


pb <- txtProgressBar(min = 1, max=nb_epoch, style=3)
for(i_epoch in 1:nb_epoch){
  cat("\n----------\nEpoch: ", i_epoch, "\n")
  # Randomly draw a batch
  index <- sample(1:n, size = batch_size, replace=TRUE)
  
  # For each observation in the batch, we need to compute the gradient
  gradients_batch <- rep(0, ncol(X))
  for(i in 1:batch_size){
    gradient_current <- 
      grad(func = obj_function, x=beta, y=y[index[i]], X = X[index[i],])
    gradients_batch <- gradients_batch+gradient_current
  }
  # Then we divide by the number of observations to get the average
  avg_gradients_batch <- gradients_batch/batch_size
  
  # Updating the value
  beta <- beta - learning_rate * avg_gradients_batch
  
  # Just for keeping track (not necessary to run the algorithm)
  # Significantly slows down the algorithm
  cost <- obj_function(beta, y, X)
  mse_values <- c(mse_values, cost)
  # End of keeping track
  
  setTxtProgressBar(pb, i_epoch)
}


# True values:
true_beta
# Estimated values
(beta_batch <- beta)


#' Performs Mini-Batch Gradient Descent for a Linear Model
#' @param par Initial values for the parameters.
#' @param fn A function to be minimized, with first argument the vector
#'           of parameters over which minimisation is to take place.
#'           It should return a scalar result.
#' @param y Target variable.
#' @param X Matrix of predictors.
#' @param learning_rate Learning rate.
#' @param nb_epoch Number of epochs.
#' @param batch_size Batch size.
#' @param silent If TRUE (default), progress info. not printed in the console.
mini_batch_gd <- function(par, fn, y, X, learning_rate=10^-2, nb_epoch=10,
                          batch_size = 128, silent=TRUE){
  mse_values  <- NULL
  n <- nrow(X)
  for(i_epoch in 1:nb_epoch){
    if(!silent) cat("\n----------\nEpoch: ", i_epoch, "\n----------")
    # Randomly draw a batch
    index <- sample(1:n, size = batch_size, replace=TRUE)
    # For each observation in the batch, we need to compute the gradient
    gradients_batch <- rep(0, ncol(X))
    for(i in 1:batch_size){
      gradient_current <- 
        grad(func = fn, x=par, y=y[index[i]], X = X[index[i],])
      gradients_batch <- gradients_batch+gradient_current
    }
    # Then we divide by the number of observations to get the average
    avg_gradients_batch <- gradients_batch/batch_size
    
    # Updating the value
    par <- par - learning_rate * avg_gradients_batch
    
    # Just for keeping track (not necessary to run the algorithm)
    # Significantly slows down the algorithm
    cost <- fn(par, y, X)
    if(!silent) cat("MSE : ", cost, "\n")
    mse_values <- c(mse_values, cost)
    # End of keeping track
  }
  structure(list(par = par, mse_values = mse_values,
                 nb_epoch = nb_epoch,
                 learning_rate = learning_rate,
                 batch_size = batch_size))
}


# With 32 obs per mini-batch
start_time_mini_batch_32 <- Sys.time()
mini_batch_32 <- mini_batch_gd(par = c(1,1,1), fn = obj_function, y = y, X = X, 
                               silent=TRUE, nb_epoch = 20, batch_size = 32)
end_time_mini_batch_32 <- Sys.time()



# With 64 obs per mini-batch
start_time_mini_batch_64 <- Sys.time()
mini_batch_64 <- mini_batch_gd(par = c(1,1,1), fn = obj_function,
                               y = y, X = X, silent=TRUE, 
                               nb_epoch = 20, batch_size = 64)
end_time_mini_batch_64 <- Sys.time()


# With 128 obs per mini-batch
start_time_mini_batch_128 <- Sys.time()
mini_batch_128 <- mini_batch_gd(par = c(1,1,1), fn = obj_function,
                                y = y, X = X, silent=TRUE, 
                                nb_epoch = 20, batch_size = 128)
end_time_mini_batch_128 <- Sys.time()


# With 256 obs per mini-batch
start_time_mini_batch_256 <- Sys.time()
mini_batch_256 <- mini_batch_gd(par = c(1,1,1), fn = obj_function,
                                y = y, X = X, silent=TRUE, 
                                nb_epoch = 20, batch_size = 256)
end_time_mini_batch_256 <- Sys.time()


# True values:
beta
# Estimated values:
mini_batch_32$par
mini_batch_64$par
mini_batch_128$par
mini_batch_256$par

# Time taken by each run:
end_time_mini_batch_32-start_time_mini_batch_32
end_time_mini_batch_64-start_time_mini_batch_64
end_time_mini_batch_128-start_time_mini_batch_128
end_time_mini_batch_256-start_time_mini_batch_256



df_plot <- 
  map_df(list(mini_batch_32, mini_batch_64, mini_batch_128, mini_batch_256), 
         ~tibble(MSE = .$mse_values, 
                 batch_size = .$batch_size, 
                 epoch = 1:(.$nb_epoch)))

ggplot(data = df_plot, mapping = aes(x=epoch, y = MSE)) +
  geom_line(aes(colour = as.factor(batch_size))) +
  labs(x = "Epoch", y = "MSE", title = "Mini-Batch Gradient Descent") +
  scale_colour_discrete("Mini-Batch Size")


df_plot <- 
  tibble(MSE = estim_sgd$mse_values, epoch = 1:estim_sgd$nb_epoch,
         type = "Stochastic Gradient Descent") %>% 
  bind_rows(
    tibble(MSE = estim_batch$mse_values, epoch = 1:estim_batch$nb_epoch,
           type = "Batch Gradient Descent")
  ) %>% 
  bind_rows(
    map_df(list(mini_batch_32, mini_batch_64, mini_batch_128, mini_batch_256), 
           ~tibble(MSE = .$mse_values, 
                   epoch = 1:(.$nb_epoch),
                   type = .$batch_size,)) %>% 
      mutate(type = str_c("Mini-Batch Gradient Descent (size=", type, ")"))
  )

ggplot(data = df_plot, mapping = aes(x=epoch, y = MSE)) +
  geom_line(mapping = aes(colour = type)) +
  labs(x = "Epoch", y = "MSE", title = "Optimisation") +
  scale_colour_manual(values = c(
    "Stochastic Gradient Descent" = "#DDCC77",
    "Batch Gradient Descent" = "#117733",
    "Mini-Batch Gradient Descent (size=32)" = "#44AA99",
    "Mini-Batch Gradient Descent (size=64)" = "#88CCEE",
    "Mini-Batch Gradient Descent (size=128)" = "#DC267F",
    "Mini-Batch Gradient Descent (size=256)" = "#882255"
  )) +
  theme(legend.position = "bottom")


# 3 Other Algorithms ------------------------------------------------------


# 3.1 Newton's Method -----------------------------------------------------

x_1 <- seq(-10, 10, by = 0.3)
x_2 <- seq(-10, 10, by = 0.3)
z_f <- function(x_1,x_2) (x_1-x_2)^4 + 2*x_1^2 + x_2^2 - x_1 + 2*x_2

# Function to be optimised
z_f_to_optim <- function(theta){
  x_1 <- theta[1]
  x_2 <- theta[2]
  (x_1-x_2)^4 + 2*x_1^2 + x_2^2 - x_1 + 2*x_2
}
z <- outer(x_1, x_2, z_f)


par(mar = c(1, 1, 1, 1))
th = 150
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          asp = 1, phi = 40, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)


# Starting values
theta <- c(-9, 9)

# Maximum number of the iterations
nb_max_iter <- 20

# Absolute tolerance to stop the process earlier
abstol <- 10^-5

# Value of f at the starting point
(current_obj <- z_f_to_optim(theta))


# To keep track of the updates
theta_values <- NULL

for(i in 1:nb_max_iter){
  gradient <- grad(func = z_f_to_optim, x = theta)
  H <- hessian(func = z_f_to_optim, x = theta)
  # Updating the parameters
  theta <- theta - t(solve(H) %*% gradient)
  new_obj <- z_f_to_optim(theta)
  # Keeping track
  theta_values <- rbind(theta_values, theta)
  
  if(abs(current_obj - new_obj) < abstol){
    break
  }else{
    current_obj <- new_obj
  }
}

# The algorithm stopped at iteration:
i

# Estimation of the point that minimise the function:
theta

par(mar = c(1, 1, 1, 1))
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
                asp = 1, phi = 40, theta = th, border = "grey10", alpha=.4,
                d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)

xx <- theta_values[,1]
yy <- theta_values[,2]
zz <- z_f(xx,yy)
new_point <- trans3d(xx,yy,zz,pmat = pmat)
lines(new_point,pch = 20,col = "red", cex=2)
points(new_point,pch = 20,col = "red", cex=2)
points(map(new_point, last),pch = 20,col = "green", cex=1.5)


# With a 2d graphical representation:
contour2D(x=x_1, y=x_2, z=z, colkey=F, main="Contour plot", xlab="x_1", ylab="x_2")
for(i in 1:(nrow(theta_values)-1)){
  segments(x0 = theta_values[i, 1], x1 = theta_values[i+1, 1],
           y0 = theta_values[i, 2], y1 = theta_values[i+1, 2], col = "red", lwd=2)
}
points(x=theta_values[,1], y=theta_values[,2], t="p", pch=19, col = "red")
points(x=theta_values[nrow(theta_values),1], 
       y=theta_values[nrow(theta_values),2], t="p", pch=19, col = "green")



# 3.2 Coordinate Descent Algorithm ----------------------------------------


library(plot3D)
library(numDeriv)
n <- 40
x_1 <- x_2 <- seq(-3, 3, length.out=n)
z_f <- function(x_1, x_2) x_1^2 + x_2^2 + x_1*x_2
z_f_to_optim <- function(theta) theta[1]^2 + theta[2]^2 + theta[1]*theta[2]
z <- outer(x_1, x_2, z_f)


op <- par()
par(mar = c(1, 1, 1, 1))
flip <- 1
th <- 200
pmat <- 
  persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
          xlab = "x_1", ylab = "x_2", zlab = "f(x_1, x_2)",
          asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
          d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1)


# Starting values
theta <- c(2, 2.2)


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
points(theta[1], theta[2], pch = 19, cex = 2, col = "red")


# Dimension we want to optimise
dim_i <- 1


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
abline(h = theta[2], lty=2, col = "blue", lwd=2)
points(theta[1], theta[2], pch = 19, cex = 2, col = "red")


# First derivative of the function with respect to the first parameter
derivative_wrt_x1 <- function(theta){
  2*theta[1] + theta[2]
}

# First-order derivative at the point of interest
(grad_i <- derivative_wrt_x1(theta))


# Setting the learning rate
learning_rate <- 10^-1


# Then updating the value of the point in the ith dimension
theta_update <- theta
theta_update[dim_i] <- theta_update[dim_i] - learning_rate * grad_i
theta_update


# Keeping track
theta_values <- rbind(theta, theta_update)
theta_values


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
abline(h = theta[2], lty=2, col = "blue", lwd = 2)
points(theta[1], theta[2], pch = 19, cex = 2, col = "red")
segments(x0 = theta[1], x1 = theta_update[1],
         y0 = theta[2], y1 = theta_update[2], col = "red", lwd=2)
points(theta_update[1], theta_update[2], pch = 19, cex = 2, col = "red")


# Beginning of the second step of the iteration. Looking at another dimension.
dim_i <- 2


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
abline(v = theta_update[1], lty=2, col = "blue", lwd = 2)
points(theta[1], theta[2], pch = 19, cex = 2, col = "red")
segments(x0 = theta[1], x1 = theta_update[1],
         y0 = theta[2], y1 = theta_update[2], col = "red", lwd=2)
points(theta_update[1], theta_update[2], pch = 19, cex = 2, col = "red")


# First-order derivative of the function with respect to the ith parameter
# (here, the second)
derivative_wrt_x2 <- function(theta){
  2*theta[2] + theta[1]
}

# First-order derivative wrt the 2nd parameter, estimated at the current point
(grad_i <- derivative_wrt_x2(theta_update))


# Updating the ith parameter accordingly to the rule
theta_update[dim_i] <- theta_update[dim_i] - learning_rate * grad_i
theta_update


# Keeping track of the changes
theta_values <- rbind(theta_values, theta_update)
theta_values


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
abline(v = theta_update[1], lty=2, col = "blue", lwd = 2)
for(i in 1:(nrow(theta_values)-1)){
  segments(x0 = theta_values[i, 1], x1 = theta_values[i+1, 1],
           y0 = theta_values[i, 2], y1 = theta_values[i+1, 2], col = "red", lwd=2)
}
points(theta_values[,1], theta_values[,2], pch = 19, cex = 2, col = "red")

# Then the third iteration can begin.
#Instead of doing it with a long code, let us use a loop, and start back from the beginning.


# Starting values
theta <- c(2, 2.2)
learning_rate <- 10^-1
abstol <- 10^-5
nb_max_iter <- 100
z_current <- z_f_to_optim(theta)
# To keep track of what happens at each iteration
theta_values <- list(theta)
dims <- NULL

for(i in 1:nb_max_iter){
  
  nb_dim <- length(theta)
  # Cyclic rule to pick the dimension
  dim_i <- (i-1) %% nb_dim + 1
  
  # With uniform sampling
  # dim_i <- sample(x = seq_len(nb_dim), size = 1)
  # Steepest ascent
  if(dim_i == 1){
    grad_i <- derivative_wrt_x1(theta)
  }else{
    grad_i <- derivative_wrt_x2(theta)
  }
  
  # Updating the parameters
  theta_update <- theta
  theta_update[dim_i] <- theta_update[dim_i] - learning_rate * grad_i
  theta <- theta_update
  # To keep track of the changes
  theta_values <- c(theta_values, list(theta))
  dims <- c(dims, dim_i)
  
  # Checking for improvement
  z_updated <- z_f_to_optim(theta_update)
  if(abs(z_updated - z_current) < abstol) break
  z_current <- z_updated
}

# The process stopped at iteration:
i

# Successives values for the parameters in a matrix:
theta_values <- do.call("rbind", theta_values)


# Ending point:
theta


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
abline(v = theta_update[1], lty=2, col = "blue", lwd = 2)
for(i in 1:(nrow(theta_values)-1)){
  segments(x0 = theta_values[i, 1], x1 = theta_values[i+1, 1],
           y0 = theta_values[i, 2], y1 = theta_values[i+1, 2], col = "red", lwd=2)
}
points(theta_values[,1], theta_values[,2], pch = 19, cex = .8, col = "red")


# For an animated GIF, uncomment the follwing instruction.
# saveGIF({
#
#   for(j in c(rep(1,5), 2:(nrow(theta_values)-1), rep(nrow(theta_values), 10))){
#
#     par(mar = c(1, 1, 1, 1))
#     flip <- 1
#     th <- 200
#     pmat <-
#       persp3D(x = x_1, y = x_2, z = z, colkey=F, contour=T, ticktype = "detailed",
#               asp = 1, phi = 30, theta = th, border = "grey10", alpha=.4,
#               d = .8,r = 2.8,expand = .6,shade = .2,axes = T,box = T,cex = .1,
#               main = paste0("Step ", j))
#
#     xx <- theta_values[1:j,1]
#     yy <- theta_values[1:j,2]
#     zz <- z_f(xx,yy)
#     new_point <- trans3d(xx,yy,zz,pmat = pmat)
#     lines(new_point,pch = 20,col = "red", cex=2, lwd=2)
#     points(new_point,pch = 20,col = "red", cex=2)
#   }
#
# }, movie.name = "coordinate_descent_contour_3D.gif", interval = 0.01,
# ani.width = 720, ani.height = 480)


# 3.2.1 When the Function to Optimize is not Differentiable in all --------


n <- 25
x_1 <- x_2 <- seq(-3, 3, length.out=n)
z_f <- function(x_1, x_2) x_1^2+x_2^2 + abs(x_1) + abs(x_2)
z_f_to_optim <- function(theta) theta[1]^2+theta[2]^2 + abs(theta[1])+abs(theta[2])
z <- outer(x_1, x_2, z_f)


#' Numerical partial first-order derivative of a function
#' @param par Initial values for the parameters
#' @param fn A function to be derived. It should return a scalar result.
#' @param dim Direction for the derivative (1 to compute the first derivative
#' with respect to the first parameter, 2 to compute the first derivative
#' with respect to the second parameter, etc.)
#' @param nb_dim number of dimensions
num_first_deriv <- function(par, fn, dim, nb_dim){
  h <- par[dim]*sqrt(10^-12)
  e <- rep(0, nb_dim) ; e[dim_i] <- 1
  (fn(par+h*e) - fn(par))/h
}


# Starting values
theta <- c(2, 2.2)
learning_rate <- 10^-1
abstol <- 10^-6
nb_max_iter <- 500
z_current <- z_f_to_optim(theta)
# To keep track of what happens at each iteration
theta_values <- list(theta)
dims <- NULL

for(i in 1:nb_max_iter){
  
  nb_dim <- length(theta)
  # Cyclic rule to pick the dimension
  dim_i <- (i-1) %% nb_dim + 1
  
  # Partial derivative wrt to the dim_i axis
  grad_i <- 
    num_first_deriv(par = theta, fn = z_f_to_optim, dim = dim_i, nb_dim = nb_dim)
  
  # Updating the parameters
  theta_update <- theta
  theta_update[dim_i] <- theta_update[dim_i] - learning_rate * grad_i
  theta <- theta_update
  # To keep track of the changes
  theta_values <- c(theta_values, list(theta))
  dims <- c(dims, dim_i)
  
  # Checking for improvement
  z_updated <- z_f_to_optim(theta_update)
  if(abs(z_updated - z_current) < abstol) break
  z_current <- z_updated
}

theta

theta_values <- do.call("rbind", theta_values)


contour(x_1, x_1, z, nlevels = 20, xlab = "x_1", ylab = "x_2")
for(i in 1:(nrow(theta_values)-1)){
  segments(x0 = theta_values[i, 1], x1 = theta_values[i+1, 1],
           y0 = theta_values[i, 2], y1 = theta_values[i+1, 2], col = "red", lwd=2)
}
points(theta_values[,1], theta_values[,2], pch = 19, cex = .8, col = "red")

