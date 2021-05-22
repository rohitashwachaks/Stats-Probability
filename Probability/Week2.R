#install.packages('dslabs')
library(gtools)
library(tidyverse)
library(dslabs)


#=======================================

#        WEEK - 2 ; SECTION - 1        #

#=======================================


# Heights of Male Student
x <- heights %>% filter(sex=="Male") %>% pull(height)

# P(Height of Male student > 70)
p_70 = mean(sapply(x,function(a){a>as.double(70)}))

#CDF
cdf <- function(A){
  #x <- heights %>% filter(sex=="Male") %>% pull(height)
  mean(sapply(x,function(a){a<as.double(A)}))
}

hist(x) #Histogram
lines(seq(50,85,1),result,col="red") #Plot Cummulative frequency distribution

#Since x follows normal distribution, rough approximation is
cdf(70)
pnorm(70,mean(x),sd(x))

plot(table(x)) # Plot frequency distribution

plot(prop.table(table(x)), xlab="height(in)",ylab="Prob") #Plot Probability
lines(seq(50,85,1),dnorm(seq(50,85,1),mean(x),sd(x)),col="red")# Normal Dist


#=======================================

#        WEEK - 2 ;  ASSIGNMENT        #

#=======================================


# Question 1

set.seed(16, sample.kind = "Rounding")
avgACT <- 20.9
sdACT <- 5.7
act_scores <- rnorm(10000,avgACT,sdACT)
sd(act_scores)   # 5.673527
table(act_scores >= as.double(36))

sum(act_scores >= as.double(30))/length(act_scores) # 0.0537
sum(act_scores <= as.double(10))/length(act_scores) #  0.0287


# Question 2

x <- 1:36
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x,type = 'l')


# Question 3

z_act <- sapply(act_scores, function(a){(a-mean(act_scores))/sd(act_scores)})
mean_z = mean(z_act)
sd_z = sd(z_act)

sum(z_act>2)/length(z_act) #0.0233
(2*sd(act_scores))+mean(act_scores) #32.1906
qnorm(0.975,mean(act_scores),sd(act_scores)) #31.96338


# Question 4

cdf <- function(A){
  #x <- heights %>% filter(sex=="Male") %>% pull(height)
  paste(A,mean(sapply(act_scores,function(a){a<as.double(A)})))
}

result <- sapply(seq(20,36,1),cdf) # above 95% -> 31

qnorm(0.95,avgACT,sdACT) #0.27567

p <- seq(0.01, 0.99, 0.01)

sample_quantiles <- quantile(act_scores,probs=p)
sample_quantiles[sample_quantiles<=26]

theoretical_scores <- sapply(p,function(a){qnorm(a,avgACT,sdACT)})
theoretical_quantiles <- quantile(theoretical_scores,p)
qqplot(theoretical_quantiles,sample_quantiles)
