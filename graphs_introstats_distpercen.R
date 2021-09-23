
# Intro to stats graphs, distributions & percentiles
####################################################

library(tidyverse)
library(essurvey)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# Empty graph for tennis sampling distribution example
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
    scale_x_continuous(limits = c(50,200),
                     breaks = seq(50,200,50)) +
    scale_y_continuous(limits = c(0,400),
                       breaks = seq(0,400,50)) +
    #geom_vline(xintercept = 120, linetype="dashed") +
    labs(x = "Serve speed (km/h)", y = "Number of serves") +
    theme_bw()
  ggsave("ex_tennis-serve.pdf",
         width=15,
         height = 10,
         units = "cm")
    

## Distributions & percentiles
##############################

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
zscore <- .025

ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(zscore))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(zscore), qnorm(1-zscore))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(1-zscore), 4)) +
  labs(y = "Density", x = "",
       title = "2.5th & 97.5th percentiles") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())
  ggsave("dist_5twotail.pdf")
  
# 90%
zscore <- 0.05

ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(zscore))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(zscore), qnorm(1-zscore))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(1-zscore), 4)) +
  labs(y = "Density", x = "",
       title = "5th & 95th percentiles") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank())  
  ggsave("dist_10twotail.pdf")

# 99%
zscore <- 0.005

ggplot(NULL, aes(c(-2,2))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-4, qnorm(zscore))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(zscore), qnorm(1-zscore))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(1-zscore), 4)) +
  labs(y = "Density", x = "",
       title = "0.5th & 99.5th percentiles") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank()) 
  ggsave("dist_1twotail.pdf")

# Percentiles, with z-scores
############################
zlev <- .025  
  
ggplot(NULL, aes(c(-3,3))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-3, qnorm(zlev))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(zlev), qnorm(1-zlev))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(1-zlev), 3)) +
  geom_vline(xintercept = qnorm(1-zlev), color = "#d3d3d3", linetype = "dashed", size=1.5) +
  geom_vline(xintercept = qnorm(zlev), color = "#d3d3d3", linetype = "dashed", size=1.5) +
  annotate("segment", x = qnorm(zlev), xend = qnorm(1-zlev), y = dnorm(qnorm(zlev)), yend = dnorm(-qnorm(zlev)), arrow = arrow(ends='both'),
           size = 1.5, color = "#d3d3d3") +
  annotate("text", x=0, y=dnorm(qnorm(zlev))+.015,label = paste0(100*(1-2*zlev),"% of data"), color="#d3d3d3", fontface = "bold") +
  labs(y = "Density", x = "",
       title = paste0("+/-",round(qnorm(1-zlev),digits = 3)," stand. deviations around the mean")) +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "bold"))
  ggsave(paste0("zscores_",100*(1-2*zlev),".pdf"))
  
zlev <- .005  

ggplot(NULL, aes(c(-3,3))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-3, qnorm(zlev))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(zlev), qnorm(1-zlev))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(1-zlev), 3)) +
  geom_vline(xintercept = qnorm(1-zlev), color = "#d3d3d3", linetype = "dashed", size=1.5) +
  geom_vline(xintercept = qnorm(zlev), color = "#d3d3d3", linetype = "dashed", size=1.5) +
  annotate("segment", x = qnorm(zlev), xend = qnorm(1-zlev), y = dnorm(qnorm(zlev)), yend = dnorm(-qnorm(zlev)), arrow = arrow(ends='both'),
           size = 1.5, color = "#d3d3d3") +
  annotate("text", x=0, y=dnorm(qnorm(zlev))+.015,label = paste0(100*(1-2*zlev),"% of data"), color="#d3d3d3", fontface = "bold") +
  labs(y = "Density", x = "",
       title = paste0("+/-",round(qnorm(1-zlev),digits = 3)," stand. deviations around the mean")) +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "bold"))
ggsave(paste0("zscores_",100*(1-2*zlev),".pdf"))
  
zlev <- .05  

ggplot(NULL, aes(c(-3,3))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-3, qnorm(zlev))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(zlev), qnorm(1-zlev))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(1-zlev), 3)) +
  geom_vline(xintercept = qnorm(1-zlev), color = "#d3d3d3", linetype = "dashed", size=1.5) +
  geom_vline(xintercept = qnorm(zlev), color = "#d3d3d3", linetype = "dashed", size=1.5) +
  annotate("segment", x = qnorm(zlev), xend = qnorm(1-zlev), y = dnorm(qnorm(zlev)), yend = dnorm(-qnorm(zlev)), arrow = arrow(ends='both'),
           size = 1, color = "#d3d3d3") +
  annotate("text", x=0, y=dnorm(qnorm(zlev))+.015,label = paste0(100*(1-2*zlev),"% of data"), color="#d3d3d3", fontface = "bold") +
  labs(y = "Density", x = "",
       title = paste0("+/-",round(qnorm(1-zlev),digits = 3)," stand. deviations around the mean")) +
  scale_x_continuous(breaks = seq(-3,3,1)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        plot.title = element_text(face = "bold"))
ggsave(paste0("zscores_",100*(1-2*zlev),".pdf"))  
  





