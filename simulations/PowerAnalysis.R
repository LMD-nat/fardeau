#---
#title: "Power Analysis: Logistic Regression"
#output: html_notebook
#---

#```{r Set-up}
rm(list=ls())
set.seed(123)
#```

#```{r Libraries}
library(WebPower)
#```

#Demidenko, E. (2007). Sample size determination for logistic regression revisited. Statistics in medicine, 26(18), 3385-3397.

#Zhang, Z., & Yuan, K.-H. (2018). Practical Statistical Power Analysis Using Webpower and R (Eds). Granger, IN: ISDSA Press.

#```{r Calculate observed power, normal distribution}
# Assuming a prevalence of around 13 to 15%
# p0	
# Prob(Y=1|X=0): the probability of observing 1 for the outcome variable Y when the predictor X equals 0. (The intercept)
#p1	
#Prob(Y=1|X=1): the probability of observing 1 for the outcome variable Y when the predictor X equals 1.
# If we assume ZBI scores follow a normal distribution
calculate_p <- function(intercept, slope, X) {
  p <- 1 / (1 + exp(-(intercept + slope * X)))
  return(p)
}

intercept <- 1
slope <- 0.2

p0 <- calculate_p(intercept, slope, 0)
p1 <- calculate_p(intercept, slope, 1)

a <- wp.logistic(n = 1409, p0 = p0, p1 = p1, alpha = 0.05,
            power = NULL, family = "normal")
#```

#```{r Lognormal}
b <- wp.logistic(n = 1409, p0 = p0, p1 = p1, alpha = 0.05,
            power = NULL, family = "lognormal")
#```

#```{r Generate a power curve given a sequence of sample sizes}
plot(wp.logistic(n = seq(500,1400,100), p0 = p0, p1 = p1, alpha = 0.05,
                power = NULL, family = "lognormal", parameter = c(0,1))) +
  abline(h = 0.8, col = "blue", lty = 2)
plot(wp.logistic(n = seq(500,1400,100), p0 = p0, p1 = p1, alpha = 0.05,
                power = NULL, family = "normal", parameter = c(0,1))) +
  abline(h = 0.8, col = "blue", lty = 2)
#```

#This next section performs a series of simulations to assess a-priori statistical power for a logistic regression analysis with multiple predictors.

#It makes two assumptions:

#1.  That the predictors are orthogonal.
#2.  That there is no co-linearity.

#In real-world data these two assumptions are rarely fully met, but for the real analysis I will mitigate these two possible violations of assumptions by testing for interaction and mediation effects at each step of the regression analysis and I will test for multicoliniearity once the main effects of interest have been identified in preliminary analyses.

#A (log) odds ratio from logistic regression coefficients is unstandardized, as they depend on the scale of the predictor. The coefficient returned by a logistic regression in R is a logit, or the log of the odds. To convert a logit to an odds ratio, it can be exponentiated. To convert an odds ratio to logit, it can be log-transformed.

#```{r}
OR = 1.2
log(OR) # logit
exp(log(OR)) # odds ratio
#```

#If we expect a variable to have a very small odds ratio (1.5) according to Chen, Cohen, and Chen (2010) and Cohen (1988), we would need a beta1 value of at least 0.405. I think it will be even smaller, so I'll simulate for a slope of 0.2.

#I have made the assumption that each predictor variable will have a very small effect size. This might not be the case, but because psycho-social variables tend to have low effect sizes, and because I will likely dummy-code some categorical variables, I want to see how many of these predictor variables I can put into a regression model without putting statistical power into jeopardy.

#```{r Specify the constants}
n = seq(1000, 1500, by=100) ## sample size
B = c(1, .2) ## coefficients, the first is the intercept (beta0), the second is the value of beta1 (logit)
r <- runif(20, min = -0.2, max = 0.6) # random coefficient values with very small effect sizes
s = (sqrt(3))/pi ## the "noise", the standard deviation of the logistic distribution
S = 1000 ## number of cells/simulations
alpha = 0.05
p2_ <- 0.15
output = data.frame(n=NA, s=NA, beta=NA, pval=NA)
#```

#```{r Run simulation with only the ZBI}
for(n_ in n) {
  for(i in 1:S) {
  
    zbi_z=rnorm(n_, 0, 1) # simulate ZBI scores
    y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s) # simulate the DV
    p = exp(y)/(1+exp(y)) # dichotomize the DV into ones and zeroes
    y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
    ## Run regression
    mod = glm(y01 ~ zbi_z, family='binomial')
    ## Store results
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
  #plot(zbi_z, y, xlab="ZBI Scores", ylab="LogOR of ED return")
}
#```

#```{r View results}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```

#The first row above demonstrates the sample size, and the value in the second row represents the expected statistical power associated with that sample size according to the mean p-value from 1000 simulations.

#```{r 1 IVs}
for(n_ in n) {
  for(i in 1:S) {
zbi_z=rnorm(n_, 0, 1)
v1 = rbinom(n=n_, size=1, prob=p2_)
y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s)
p = exp(y)/(1+exp(y))
y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
mod = glm(y01 ~ zbi_z + v1 + zbi_z:v1, family='binomial')
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```

#```{r 2 IVs}
for(n_ in n) {
  for(i in 1:S) {
zbi_z=rnorm(n_, 0, 1)
v1 = rbinom(n=n_, size=1, prob=p2_)
v2 = rbinom(n=n_, size=1, prob=p2_)
y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s)
p = exp(y)/(1+exp(y))
y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
mod = glm(y01 ~ zbi_z + v1 + v2 + zbi_z:v1 + zbi_z:v2, family='binomial')
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```

#```{r 3 IVs}
for(n_ in n) {
  for(i in 1:S) {
zbi_z=rnorm(n_, 0, 1)
v1 = rbinom(n=n_, size=1, prob=p2_)
v2 = rbinom(n=n_, size=1, prob=p2_)
v3 = rbinom(n=n_, size=1, prob=p2_)
y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s)
p = exp(y)/(1+exp(y))
y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
mod = glm(y01 ~ zbi_z + v1 + v2 + v3 + zbi_z:v1 + zbi_z:v2 + zbi_z:v3, family='binomial')
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```

#```{r 4 IVs}
for(n_ in n) {
  for(i in 1:S) {
zbi_z=rnorm(n_, 0, 1)
v1=rbinom(n=n_, size=1, prob=p2_)
v2=rbinom(n=n_, size=1, prob=p2_)
v3=rbinom(n=n_, size=1, prob=p2_)
v4=rbinom(n=n_, size=1, prob=p2_)
y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s)
p = exp(y)/(1+exp(y))
y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
mod = glm(y01 ~ zbi_z + v1 + v2 + v3 + zbi_z:v1 + zbi_z:v2 + zbi_z:v3 + zbi_z:v4, family='binomial')
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```

#```{r 5 IVs}
for(n_ in n) {
  for(i in 1:S) {
zbi_z=rnorm(n_, 0, 1)
v1=rbinom(n=n_, size=1, prob=p2_)
v2=rbinom(n=n_, size=1, prob=p2_)
v3=rbinom(n=n_, size=1, prob=p2_)
v4=rbinom(n=n_, size=1, prob=p2_)
v5=rbinom(n=n_, size=1, prob=p2_)
y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s)
p = exp(y)/(1+exp(y))
y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
mod = glm(y01 ~ zbi_z + v1 + v2 + v3 + v4 + v5 + zbi_z:v1 + zbi_z:v2 + zbi_z:v3 + zbi_z:v4 + zbi_z:v5, family='binomial')
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```

#```{r 6 IVs}
for(n_ in n) {
  for(i in 1:S) {
zbi_z=rnorm(n_, 0, 1)
v1=rbinom(n=n_, size=1, prob=p2_)
v2=rbinom(n=n_, size=1, prob=p2_)
v3=rbinom(n=n_, size=1, prob=p2_)
v4=rbinom(n=n_, size=1, prob=p2_)
v5=rbinom(n=n_, size=1, prob=p2_)
v6=rbinom(n=n_, size=1, prob=p2_)
y = B[1] + 0.2*zbi_z + rnorm(n_, 0, s)
p = exp(y)/(1+exp(y))
y01 = sapply(p, function(x) sample(c(1,0), size=1, prob=c((1 - x), x)))
mod = glm(y01 ~ zbi_z + v1 + v2 + v3 + v4 + v5 + v6 + zbi_z:v1 + zbi_z:v2 + zbi_z:v3 + zbi_z:v4 + zbi_z:v5 + zbi_z:v6, family='binomial')
    these_results = data.frame(n=n_, s=i, beta=mod$coefficients[2], pval=summary(mod)$coefficients[2,'Pr(>|z|)'])
    rownames(these_results)=NULL
    output = rbind(output, these_results)
  }
}
tapply(output$pval, output$n, function(p) mean(p<alpha))
#```
