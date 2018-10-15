### ISLR Chapter 3 Linear Regression



### pp. 61-62
library(tidyverse)

obs_df <- data.frame(x=c(50, 125, 150, 200, 270),
                     y=c(10, 13.7, 14, 14.2, 21))

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

### Compare manual calculation to R lm() function
linearMod <- lm(y ~ x, data=obs_df)
summary(linearMod)
### Check if residual sum of squares (RSS) are the same between manual
### calculation and the R lm() function
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



### pp.65-66 - RSE (residual standard error)
obs_df <- data.frame(x=c(50, 125, 150, 200, 270),
                     y=c(10, 13.7, 14, 14.2, 21))

x_bar <- mean(obs_df$x)
y_bar <- mean(obs_df$y)

b1_hat <- sum((obs_df$x - x_bar)*(obs_df$y - y_bar))/sum((obs_df$x - x_bar)^2)
b0_hat <- y_bar - b1_hat*x_bar

obs_df <- obs_df %>% mutate(y_hat = b0_hat + b1_hat*x,
                  e = y-y_hat)

n <- length(obs_df$x)
RSS <- sum(obs_df$e^2)  ## residual sum of squares
RSE <- sqrt(RSS/(n-2))  ## residual standard error

linearMod <- lm(y ~ x, data=obs_df)
summary(linearMod)$sigma
RSE

summary(linearMod)
SE_b0 = sqrt((RSS/(n-2))*(1/n+(x_bar^2/sum((obs_df$x-x_bar)^2))))
SE_b0
SE_b1 =sqrt((RSS/(n-2))/sum((obs_df$x-x_bar)^2))
SE_b1


### calculating the 95% confidence interval
percentile <- 0.95  ## 95% percentile
q <- 1-(1-percentile)/2
ci_b1 <- b1_hat + c(-qnorm(q),qnorm(q))*SE_b1  ## might need t-distribution
ci_b0 <- b0_hat + c(-qnorm(q),qnorm(q))*SE_b0  ## instead of normal dist


### pp. 67 - 68
### H_0: b1 = 0 -> no relationship between X and Y
### use t-statistic, which measures the number of standard deviations b1_hat
### is away from 0
t_stat <- (b1_hat-0)/SE_b1 
t_stat
summary(linearMod)
### Pr(>|t|) 
pt(-t_stat,df=n-2)+1-pt(t_stat,df=n-2)



### pp. 68-70 - Assessing the Accuracy of the Model
### R^2 proportion of variance explained
TSS <- sum((obs_df$y-y_bar)^2)
R2 <- 1-(RSS/TSS)
R2


### pp. 71-74
### http://www-bcf.usc.edu/~gareth/ISL/
### https://rafalab.github.io/dsbook/
library(tidyverse)
url <- "http://www-bcf.usc.edu/~gareth/ISL/Advertising.csv"
download.file(url, "data/Advertising.csv")
dat <- readr::read_csv("data/Advertising.csv")

lm_TV <- lm(sales ~ TV, data=dat)
summary(lm_TV)
lm_radio <- lm(sales ~ radio, data=dat)
summary(lm_radio)
lm_newspaper <- lm(sales ~ newspaper, data=dat)
summary(lm_newspaper)
lm_all <- lm(sales ~ TV+radio+newspaper, data=dat)
summary(lm_all)
dat %>% select(TV, radio, newspaper, sales) %>% cor()

p_TV <- dat %>% ggplot(aes(x=TV, y=sales)) +
    geom_point(shape=1, color="red") +
    geom_abline(intercept=coef(lm_TV)[1],
                slope=coef(lm_TV)[2], color="blue", size=0.2) +
    scale_y_discrete(name="Sales (units)", limits=c(seq(5,25,5))) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(color="black",size=0.2),
          axis.line=element_line(color="black",size=0.2))

p_radio <- dat %>% ggplot(aes(x=radio, y=sales)) +
    geom_point(shape=1, color="red") +
    geom_abline(intercept=coef(lm_radio)[1],
                slope=coef(lm_radio)[2], color="blue", size=0.2) +
    scale_y_discrete(name="Sales (units)", limits=c(seq(5,25,5))) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(color="black",size=0.2),
          axis.line=element_line(color="black",size=0.2))

p_newspaper <- dat %>% ggplot(aes(x=newspaper, y=sales)) +
    geom_point(shape=1, color="red") +
    geom_abline(intercept=coef(lm_newspaper)[1],
                slope=coef(lm_newspaper)[2], color="blue", size=0.2) +
    scale_y_discrete(name="Sales (units)", limits=c(seq(5,25,5))) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(color="black",size=0.2),
          axis.line=element_line(color="black",size=0.2))

gridExtra::grid.arrange(p_TV, p_radio, p_newspaper, ncol=3)



### pp. 84-85
library(ISLR)
library(GGally)
data(Credit)

### https://gastonsanchez.wordpress.com/2012/08/27/scatterplot-matrices-with-ggplot/
pairs(Credit[,c(12,2:7)])

ggpairs(Credit[,c(12,2:7)])  ## intall.packages("GGally")

lm_gender <- lm(Balance ~ Gender, data=Credit)
summary(lm_gender)

Credit %>% ggplot(aes(x=Age,y=Balance, color=Gender)) +
    geom_point(shape=1) +
    geom_hline(yintercept=coef(lm_gender)[1], color="Blue", size=0.5) +
    geom_hline(yintercept=coef(lm_gender)[1]+coef(lm_gender)[2],
               color="Green", size=0.5) +
    theme_bw()

### recode Gender factor (female,male) into +1,-1
alt_credit <- Credit %>% mutate(gen=ifelse(Gender=="Female",1,-1))
lm_gen <- lm(Balance ~ gen, data=alt_credit)
summary(lm_gen)
### Predictors w/more than 2 levels
lm_ethn <- lm(Balance ~ Ethnicity, data=Credit)
summary(lm_ethn)

alt2_credit <- Credit %>%
    mutate(et_as=ifelse(Ethnicity=="Asian",1,0),
           et_ca=ifelse(Ethnicity=="Caucasian",1,0))
lm2_ethn <- lm(Balance ~ et_as + et_ca, data=alt2_credit)
summary(lm2_ethn)



### pp. 87-88
### Interaction terms: Y = b0 + b1*X1 + b2*X2 + b3*X1*X2 + e
dat <- readr::read_csv("data/Advertising.csv")

lm_main_effect <- lm(sales ~TV + radio, data=dat)
summary(lm_main_effect)
lm_interact <- lm(sales ~ TV + radio + TV*radio, data=dat)
summary(lm_interact)

### pp. 89-90
library(tidyverse)
library(ISLR)
data(Credit)

lm_student <- lm(Balance ~ Income + Student, data=Credit)
summary(lm_student)
### without interaction between Income and Student we have 2 parallel
### regression lines (red = StudentYes)
p1 <- Credit %>% ggplot(aes(x=Income,y=Balance,color=Student)) +
    geom_point(show.legend=FALSE) +
    geom_abline(intercept=coef(lm_student)[1],
                slope=coef(lm_student)[2], color="black") +
    geom_abline(intercept=coef(lm_student)[1]+coef(lm_student)[3],
                slope=coef(lm_student)[2], color="red") +
    theme_bw()
### this linear model includes an interaction variable of Income
lm2_student <- lm(Balance ~ Income + Student + Income*Student, data=Credit)
summary(lm2_student)

p2 <- Credit %>% ggplot(aes(x=Income,y=Balance,color=Student)) +
    geom_point() +
    geom_abline(intercept=coef(lm2_student)[1],
                slope=coef(lm2_student)[2], color="black") +
    geom_abline(intercept=coef(lm2_student)[1]+coef(lm2_student)[3],
                slope=coef(lm2_student)[2]+coef(lm2_student)[4], color="red") +
    theme_bw()

gridExtra::grid.arrange(p1, p2, ncol=2)



### pp. 91-92
### Polynomial Regression
library(tidyverse)
library(ISLR)
data(Auto)

lm_mpg <- lm(mpg ~ horsepower, data=Auto)
summary(lm_mpg)
lm2_mpg <- lm(mpg ~ horsepower + I(horsepower^2), data=Auto)
summary(lm2_mpg)
lm3_mpg <- lm(mpg ~ horsepower + I(horsepower^2) +
                  I(horsepower^3) + I(horsepower^4) +
            I(horsepower^5), data=Auto)
summary(lm3_mpg)

Auto %>% ggplot(aes(x=horsepower, y=mpg)) +
    geom_point(shape=1, color="grey") +
    geom_abline(intercept=coef(lm_mpg)[1], slope=coef(lm_mpg)[2],
                color="orange", size=0.3) +
    stat_smooth(method="lm", se=FALSE,
                formula=y~x+I(x^2), size=0.3,
                color="blue", fullrange=T) +
    stat_smooth(method="lm", se=FALSE,
                formula=y~x+I(x^2)+I(x^3)+I(x^4)+I(x^5), size=0.3,
                color="green", fullrange=T) +
    theme_bw() +
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_rect(color="black",size=0.2),
          axis.line=element_line(color="black",size=0.2))

