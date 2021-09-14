
# Graphs & Tables, Kvantitativ forskningsmetode
###############################################

# Lecture 7: t-test & correlation

library(tidyverse)
library(viridis)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")


# Graph of t-distribution
ggplot(NULL, aes(c(-4,4))) + 
  geom_line(stat = "function", fun = dt,args = list(df=1), aes(color="df=1"), size=1.5) +
  geom_line(stat = "function", fun = dt,args = list(df=05), aes(color="df=5"), size=1.5) +
  geom_line(stat = "function", fun = dt,args = list(df=Inf), aes(color="df=Infinity (=Normal dist.)"), size=1.5) +
  labs(y = "Density", x = "x") +
  #scale_color_viridis(discrete = T, option = "plasma") +
  scale_color_manual(values = c("#F5793A",
                                "#A95AA1",
                                "#85C0F9",
                                "#33a02c")) +
  theme_bw() +
  theme(axis.text = element_text(size=14),
        legend.position = "bottom",
        legend.title = element_blank())
  ggsave("t-dist.pdf")

  
# Comparison of 95 percentiles, normal vs. t
#############################################
  
# Normal:
ggplot(NULL, aes(c(-5,5))) + 
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(-5, qnorm(.025))) +
  geom_area(stat = "function", fun = dnorm, fill = "grey30", xlim = c(qnorm(.025), qnorm(.975))) +
  geom_area(stat = "function", fun = dnorm, fill = "#d95f02", xlim = c(qnorm(.975), 5)) +
  scale_y_continuous(limits = c(0,.4)) +
  labs(y = "Density", x = "",
       title = paste0("2.5 & 97.5 percentiles: ",round(qnorm(0.025),digits = 3),"; ",round(qnorm(0.975),digits = 3))) +
  theme_bw() +
    theme(axis.text = element_text(size=12))
  ggsave("normal_95per.pdf")
  
ggplot(NULL, aes(c(-5,5))) + 
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
            xlim = c(-5, qt(.025, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "grey30", 
            xlim = c(qt(.025, df=3), qt(.975, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
            xlim = c(qt(.975, df=3), 5)) +
  scale_y_continuous(limits = c(0,.4)) +
  labs(y = "Density", x = "",
       title = paste0("2.5 & 97.5 percentiles: ",round(qt(0.025, df=3),digits = 3),"; ",round(qt(0.975, df=3),digits = 3))) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
ggsave("tdist_95per.pdf")
  
# Distributions around equal group means
########################################

set.seed(42)
mean_1 <- 30
mean_2 <- 40

# Equal & small sd
sd_1 <- 2.5
sd_2 <- 2.5

data.frame(group = c(rep(1,20),rep(2,20)),
           score = c(rnorm(n=20,mean=mean_1,sd=sd_1),rnorm(n=20,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
    geom_point(shape=21,size=6, alpha = .5) +
    geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
    geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
    scale_fill_manual(values = c("#ffae42","#007ba7")) +
    scale_y_continuous(limits = c(0,100)) +
    scale_x_continuous(limits = c(.5,2.5),
                       breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
  ggsave("grdist_case1.pdf")
  
  
# Equal & large sd
sd_1 <- 15
sd_2 <- 15

data.frame(group = c(rep(1,20),rep(2,20)),
           score = c(rnorm(n=20,mean=mean_1,sd=sd_1),rnorm(n=20,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("grdist_case2.pdf")  


# Unequal sd
sd_1 <- 5
sd_2 <- 25

data.frame(group = c(rep(1,20),rep(2,20)),
           score = c(rnorm(n=20,mean=mean_1,sd=sd_1),rnorm(n=20,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("grdist_case3.pdf")

