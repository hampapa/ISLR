### Chapter 2 Statistical Learning

### pp. 39-42 Classification, KNN
library(tidyverse)
### observations, 0<=x1<=10, 0<=x2<=10
x1 <- c(2,3,1,7,3,6)
x2 <- c(5,7,9,2,4,4)
classifier <- as.factor(c(0,1,1,0,0,1))
obs_df <- data.frame(x1=x1,x2=x2,cl=classifier)
### arbitrary estimated function f_hat: a horizontal line
f_hat <- function(x){
    return(6.25)
}
### calculate y_hat
obs_df <- obs_df %>%
    mutate(y_hat=ifelse(x2>f_hat(x1),1,0))
### training error rate
sum((as.numeric(obs_df$cl)-1)!=obs_df$y_hat)/length(obs_df$x1)

grid_x1 <- seq(from=0, to=10, by=0.5)
grid_x2 <- seq(from=0, to=10, by=0.5)
points_grid <- as.matrix(expand.grid(grid_x1, grid_x2))
grid_df <- data.frame(gx1=points_grid[,1], gx2=points_grid[,2])
grid_df <- grid_df %>%
    mutate(gcl=as.factor(ifelse(gx2>f_hat(gx1),1,0)))

obs_df %>% ggplot(aes(x=x1, y=x2, color=cl)) +
    geom_point(data=grid_df,
               aes(x=gx1, y=gx2, color=gcl), size=0.5) +
    geom_point(size=3) +
    geom_abline(intercept=6.25, slope=0, color="black", size=0.8) +
    theme_bw()

### another arbitrary estimated function f_hat: a sloped line
f2_hat <- function(x){
    return(2+0.5*x)
}
grid_df <- grid_df %>%
    mutate(gcl2=as.factor(ifelse(gx2>f2_hat(gx1),1,0)))
### calculate y2_hat
obs_df <- obs_df %>%
    mutate(y2_hat=ifelse(x2>f2_hat(x1),1,0))
sum((as.numeric(obs_df$cl)-1)!=obs_df$y2_hat)/length(obs_df$x1)

obs_df %>% ggplot(aes(x=x1, y=x2, color=cl)) +
    geom_point(data=grid_df,
               aes(x=gx1, y=gx2, color=gcl2), size=0.5) +
    geom_point(size=3) +
    geom_abline(intercept=2, slope=0.5, color="black", size=0.8) +
    theme_bw()

### Bayes classifier
### https://brilliant.org/wiki/bayes-theorem/
### https://brilliant.org/wiki/conditional-probability-distribution/
