### ISLR Chapter 3 Linear Regression



### pp. 61-62
library(tidyverse)

obs_df <- data.frame(x=c(50, 125, 200, 270),
                     y=c(10, 13.7, 14, 21))

x_bar <- mean(obs_df$x)
y_bar <- mean(obs_df$y)

b1_hat <- sum((obs_df$x - x_bar)*(obs_df$y - y_bar))/sum((obs_df$x - x_bar)^2)
b0_hat <- y_bar - b1_hat*x_bar

obs_df <- obs_df %>% mutate(y_hat = b0_hat + b1_hat*x,
                  e = y-y_hat)

obs_df %>% ggplot(aes(x,y)) +
    geom_abline(slope=b1_hat, intercept=b0_hat, color="grey") +
    geom_segment(aes(x=x,xend=x,y=y_hat,yend=y), color="grey") +
    geom_point() +
    geom_point(aes(x=x_bar, y=y_bar), colour="red")
    
linearMod <- lm(y ~ x, data=obs_df)
summary(linearMod)
sum(resid(linearMod)^2)
sum(obs_df$e^2)



### pp. 63-64, Figure 3.3
library(tidyverse)
### Plot population regression line, least squares line and data points
### First create 100 random sample points in a data frame
x <- runif(100,-2,2)
y_base <- 2 + 3*x
error <- rnorm(n=length(y_base),sd=2)
y <- y_base + error
obs_df <- data.frame(x=x, y=y)

linreg <- lm(y ~ x, data=obs_df)
int_coef <- coef(linreg)[1]   ## intercept coefficient
slope_coef <-coef(linreg)[2]  ## slope coefficient

obs_df %>% ggplot(aes(x,y)) +
    geom_abline(slope=slope_coef, intercept=int_coef, color="blue") +
    geom_abline(slope=3, intercept=2, color="red") +
    geom_point(shape=1, color="darkgrey")

### Plot population regression line and least squares lines
### for 20 different samples
coefs <- replicate(20,{
    x <- runif(100,-2,2)
    y_base <- 2 + 3*x
    error <- rnorm(n=length(y_base),sd=2)
    y <- y_base + error
    obs_df <- data.frame(x=x, y=y)
    linreg <- lm(y ~ x, data=obs_df)
    int_coef <- coef(linreg)[1]
    slope_coef <-coef(linreg)[2]
    c(int_coef,slope_coef)
})
int_coefs <- coefs[1,]
slope_coefs <- coefs[2,]
coef_df <- data.frame(slope=slope_coefs, intercept=int_coefs)

obs_df <- data.frame(x=c(-2,2), y=c(-5,10)) ## set the corners for the plot
obs_df %>% ggplot(aes(x,y)) +
    geom_blank() + 
    geom_abline(slope=coef_df$slope,
                intercept=coef_df$intercept, color="lightblue") +
    geom_abline(slope=3, intercept=2, color="red")
