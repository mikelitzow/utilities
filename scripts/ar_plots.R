# plot time series with variable levels of first-order autocorrelation

library(tidyverse)

# set ar levels

ar <- c(0, 0.7, 0.9)


output <- data.frame()


for(i in 1:3){
  
  y <- 1 # set first value of timeseries
  
  for(j in 2:200){
    
    y[j] <- y[(j-1)]*ar[i] + rnorm(n = 1, mean = 0, sd = 1) # random time series with different AR(1) values
    
    
    
  }

  output <- rbind(output,
                  data.frame(ar = as.factor(as.character(ar[i])),
                  time = 1:200,
                  y = y))  
  
}


ggplot(output, aes(time, y)) +
  geom_line() +
  facet_wrap(~ar)
