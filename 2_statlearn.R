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

grid_x1 <- seq(from=0, to=10, by=0.25)
grid_x2 <- seq(from=0, to=10, by=0.25)
points_grid <- as.matrix(expand.grid(grid_x1, grid_x2))
grid_df <- data.frame(gx1=points_grid[,1], gx2=points_grid[,2])
grid_df <- grid_df %>%
    mutate(gcl=as.factor(ifelse(gx2>f_hat(gx1),1,0)))

obs_df %>% ggplot(aes(x=x1, y=x2, color=cl)) +
    geom_point(data=grid_df,
               aes(x=gx1, y=gx2, color=gcl), size=0.3) +
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
               aes(x=gx1, y=gx2, color=gcl2), size=0.4) +
    geom_point(size=3) +
    geom_abline(intercept=2, slope=0.5, color="black", size=0.6) +
    theme_bw()

### Bayes classifier
### https://www.geeksforgeeks.org/naive-bayes-classifiers/

outlook <- c("rainy","rainy","overcast","sunny","sunny","sunny","overcast",
             "rainy","rainy","sunny","rainy","overcast","overcast","sunny")
temperature <- c("hot","hot","hot","mild","cool","cool","cool","mild","cool",
                 "mild","mild","mild","hot","mild")
humidity <- c("high","high","high","high","normal","normal","normal","high",
              "normal","normal","normal","high","normal","high")
windy <- c("false","true","false","false","false","true","true","false",
           "false","false","true","true","false","true")
play <- c("no","no","yes","yes","yes","no","yes","no","yes","yes","yes",
               "yes","yes","no")
df <- data.frame(outlook=outlook, temperature=temperature, humidity=humidity,
                 windy=windy, play=play)
P_y_no <- df

library(MASS)  
Sigma <- matrix(c(1,0,0,1),nrow = 2, ncol = 2)
means_1 <- mvrnorm(n = 10, mu = c(1,0), Sigma)

### https://stackoverflow.com/questions/24052643/how-to-plot-non-linear-decision-boundaries-with-a-grid-in-r
### https://stats.stackexchange.com/questions/21572/how-to-plot-decision-boundary-of-a-k-nearest-neighbor-classifier-from-elements-o
### https://web.stanford.edu/~hastie/ElemStatLearn/datasets/mixture.example.info.txt

