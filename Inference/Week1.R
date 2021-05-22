library(tidyverse)
library(dslabs)
take_poll(10000)

#Assesment

N <-25
p <- seq(0,1, length = 100)

se <- sapply(p,function(x){sqrt(p*(1-p)/N)})

plot(p,se)