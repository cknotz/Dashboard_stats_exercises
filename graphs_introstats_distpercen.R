
# Intro to stats graphs, distributions & percentiles
####################################################

library(tidyverse)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# Normal with median
ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(-4, 0)) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(0, 4)) +
  labs(y = "Density", x = "",
       title = "Median/50th percentile") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  ggsave("dist_median.pdf")
  
  
# 35%
ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(.35))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(.35), 4)) +
  labs(y = "Density", x = "",
       title = "35th percentile") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("dist_35.pdf") 

# 60%
ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(.6))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(.6), 4)) +
  labs(y = "Density", x = "",
       title = "60th percentile") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("dist_60.pdf") 
  
  
# 5% 
ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(.05))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(.05), 4)) +
  labs(y = "Density", x = "",
       title = "5th percentile") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("dist_05.pdf")  

# 95% 
ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(.95))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(.95), 4)) +
  labs(y = "Density", x = "",
       title = "95th percentile") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
ggsave("dist_95.pdf")

# 5%, two-tailed
ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(.025))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(.025), qnorm(.975))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(.975), 4)) +
  labs(y = "Density", x = "",
       title = "2.5th & 97.5th percentiles") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  ggsave("dist_5twotail.pdf")





