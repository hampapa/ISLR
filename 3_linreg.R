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

###
library(tidyverse)

x <- runif(100,-2,2)
y_base <- 2 + 3*x
error <- rnorm(n=length(y_base),sd=2)
y <- y_base + error
obs_df <- data.frame(x=x, y=y)

linreg <- lm(y ~ x, data=obs_df)
int_coef <- summary(linreg)$coefficients[1,1]
slope_coef <-summary(linreg)$coefficients[2,1]

plot <- obs_df %>% ggplot(aes(x,y)) +
    geom_abline(slope=slope_coef, intercept=int_coef, color="blue") +
    geom_abline(slope=3, intercept=2, color="red") +
    geom_point(shape=1, color="darkgrey")

coefs <- replicate(20,{
    x <- runif(100,-2,2)
    y_base <- 2 + 3*x
    error <- rnorm(n=length(y_base),sd=2)
    y <- y_base + error
    obs_df <- data.frame(x=x, y=y)
    linreg <- lm(y ~ x, data=obs_df)
    int_coef <- summary(linreg)$coefficients[1,1]
    slope_coef <-summary(linreg)$coefficients[2,1]
    c(int_coef,slope_coef)
})
