library(gtools)
library(tidyverse)
library(dslabs)


#=======================================

#        WEEK - 3 ; SECTION - 1        #

#=======================================


# Visualising Probability distribution for a Roullette Wheel

set.seed(1)
plays <- 1000
B <- 10000

#Monte Carlo Sim for ROullette Wheel

S <- replicate(B,{  # S because Random Variable
  sum(sample(c(-1,1), plays, replace = TRUE, prob = c(9/19,10/19)))
  # -1 -> Casino loose bet ; 1 -> Casino win bet
})
avg = mean(S); sd = sd(S)


# Frequency Plot of Random Variable of R. Wheel outcome
plot(prop.table(table(S)), xlab = "Net Profit", ylab = "Probability", type = "h")

base <- seq(min(S),max(S),length = B)
pdf = data.frame(profit = base, probability = dnorm(base,avg,sd))

# ??? WHY not overlapping?

lines(pdf)
title("-- Outcome of Roullette Games --")

# ??? Lecture Plot Overlapping

data.frame (S = S) %>%    # make data frame of S for histogram
  ggplot(aes(S, ..density..)) +
  geom_histogram(color = "black", binwidth = 10) +
  ylab("Probability") +
  geom_line(data = pdf, mapping = aes(profit, probability), color = "blue")


#=======================================

#        WEEK - 3 ; SECTION - 2        #

#=======================================


# Question -1

# correct guess = 1 point; Wrong = -0.25 point
p_correct = 0.2
p_wrong = 1-p_correct
n = 44
avg = n*(p_correct-(0.25*p_wrong))
se = sqrt(n)*1.25*sqrt(p_correct*p_wrong)
1 - pnorm(8,avg,se)

set.seed(21, sample.kind = "Rounding")
B <- 10000
S<- replicate(B,{
  sum(sample(c(1,-0.25),n,replace = TRUE, prob = c(p_correct,p_wrong)))
})

#students scoring more than 8 point
mean(S>8)


# Question-2

# correct guess = 1 point; Wrong = -0.25 point
p_correct = 0.25
p_wrong = 1-p_correct
n = 44
avg = n*(p_correct-(0*p_wrong))
se = sqrt(n)*1.25*sqrt(p_correct*p_wrong)

#Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) 
#representing a range of student skills.

#What is the lowest p such that the probability of scoring 
#over 35 exceeds 80%?

min_score <- 35
min_pass <- 0.8 # 80%
prob <- function(a){ # probably of scoring over 35 on test
  p_c <- a
  p_w <- 1-a
  avg <- n*p_c
  std <- sqrt(n)*sqrt(p_c*p_w)
  paste(a, ((1 - pnorm(min_score,avg,std))>min_pass))
}

p <- seq(0.25, 0.95, 0.05) 

ans <- sapply(p, prob)
ans


# Question - 3

p_success = 5/38
p_fail = 1-p_success
n <- 500

ex <- ((6*p_success)+(-1*p_fail))*n
sd <- 7*sqrt(p_success*p_fail)*sqrt(n)

pnorm(0,ex,sd)
S<- sum(sample(c(6,-1),n,replace = TRUE, prob = c(p_success,p_fail)))