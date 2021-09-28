
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
N_1 <- 20
N_2 <- 20

# Equal & small sd
sd_1 <- 2.5
sd_2 <- 2.5

data.frame(group = c(rep(1,N_1),rep(2,N_2)),
           score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),rnorm(n=N_2,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
    geom_point(shape=21,size=6, alpha = .5) +
    geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
    geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
    scale_fill_manual(values = c("#ffae42","#007ba7")) +
    scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
    scale_x_continuous(limits = c(.5,2.5),
                       breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
  ggsave("grdist_case1.pdf")
  
  
# Equal & large sd
sd_1 <- 15
sd_2 <- 15

data.frame(group = c(rep(1,N_1),rep(2,N_2)),
           score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),rnorm(n=N_2,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("grdist_case2.pdf")  


# Unequal sd
sd_1 <- 5
sd_2 <- 25

data.frame(group = c(rep(1,N_1),rep(2,N_2)),
           score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),rnorm(n=N_2,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("grdist_case3.pdf")


# Unequal N
sd_1 <- 15
sd_2 <- 15
N_1 <- 5
N_2 <- 40

data.frame(group = c(rep(1,N_1),rep(2,N_2)),
           score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),rnorm(n=N_2,mean=mean_2,sd=sd_2))) %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
  ggsave("grdist_case4.pdf")
  
  
# Simulate sampling dist of independent means under ideal conditions
####################################################################
  
N_1 <- 2000
N_2 <- 2000
mean_1 <- 60
mean_2 <- 30
sd_1 <- 5
sd_2 <- 5
nsamples <- 10000
samsize <- 50
  
pop <- data.frame(group = c(rep(1,N_1),rep(2,N_2)),
           score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),
                     rnorm(n=N_2,mean=mean_2,sd=sd_2)))  

pop %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
  ggsave("meandiff_sim_pop.pdf")
  
pop %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("meandiff_sim_pop_wide.pdf",
       width=10,
       height=5,
       units = "cm")  
  
# Draw random samples
diffs <- sapply(seq(1,nsamples,1),
                function(x){
                  sample <- dplyr::sample_n(pop,
                                            size = samsize,
                                            replace = F)
                  smean1 <- mean(sample$score[which(sample$group==1)])
                  smean2 <- mean(sample$score[which(sample$group==2)])
                  return(smean1-smean2)
                })

data.frame(diffs = diffs,
                    snum = seq(1,length(diffs),1)) %>% 
  ggplot(aes(x=diffs)) +
    geom_histogram(color="black", bins = 200, alpha = .7) +
    geom_vline(xintercept = 30, size = 1, linetype = "solid",
               color = "Orange") +
    scale_x_continuous(limits = c(0,50),
                       breaks = seq(0,50,10)) +
    labs(x = "Measured difference between group means",
         y = "Number of samples",
         title = paste0(nsamples," random samples; ",samsize," observations per sample.")) +
    theme_bw() +
    theme(axis.text = element_text(size=12))
  ggsave("meandiff_sim.pdf")
    
# Again under H_0 
N_1 <- 2000
N_2 <- 2000
mean_1 <- 50
mean_2 <- 50
sd_1 <- 5
sd_2 <- 5
nsamples <- 10000
samsize <- 50

pop <- data.frame(group = c(rep(1,N_1),rep(2,N_2)),
                  score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),
                            rnorm(n=N_2,mean=mean_2,sd=sd_2)))  

pop %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("meandiff_sim_pop_h0.pdf")

# Draw random samples
diffs <- sapply(seq(1,nsamples,1),
                function(x){
                  sample <- dplyr::sample_n(pop,
                                            size = samsize,
                                            replace = F)
                  smean1 <- mean(sample$score[which(sample$group==1)])
                  smean2 <- mean(sample$score[which(sample$group==2)])
                  return(smean1-smean2)
                })

data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(color="black", bins = 200, alpha = .7) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  labs(x = "Measured difference between group means",
       y = "Number of samples",
       title = paste0(nsamples," random samples; ",samsize," observations per sample.")) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
ggsave("meandiff_sim_h0.pdf")

# w/o title
data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(color="black", bins = 200, alpha = .7) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  labs(x = "Measured difference between group means",
       y = "Number of samples") +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  ggsave("meandiff_sim_h0_notitle.pdf")

data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(color="black", bins = 200, alpha = .7) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  labs(x = "Measured difference between group means",
       y = "Number of samples") +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  ggsave("meandiff_sim_h0_wide.pdf",
         width=10,
         height=5,
         units = "cm")

  
# quantiles indicated
data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  mutate(within = ifelse(diffs<=mean(diffs)-1.96*sd(diffs) | diffs>=mean(diffs)+1.96*sd(diffs),
                         "no","yes")) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(aes(fill=within), bins = 200) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  scale_x_continuous(limits = c(-7.5,7.5),
                     breaks = seq(-7.5,7.5,2.5)) +
  scale_fill_manual(values = c("red","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  labs(x = "Measured difference between group means",
       y = "Number of samples") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom")  
  ggsave("meandiff_quantiles.pdf")
  
  

# Measured, H_0 confirmed
data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  mutate(within = ifelse(diffs<=mean(diffs)-1.96*sd(diffs) | diffs>=mean(diffs)+1.96*sd(diffs),
                         "no","yes")) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(aes(fill=within),color="black", bins = 200) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  geom_vline(xintercept = 1.5,size = 1.5, linetype = "dashed",
             color = "#00AB08") +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  scale_fill_manual(values = c("#FFAE42","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  labs(x = "Measured difference between group means",
       y = "Number of samples",
       title = "Green dashed line: Measured difference = 1.5") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom")
  ggsave("meandiff_h0_confirm.pdf")
  
  
# Measured, H_0 rejected
data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  mutate(within = ifelse(diffs<=mean(diffs)-1.96*sd(diffs) | diffs>=mean(diffs)+1.96*sd(diffs),
                         "no","yes")) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(aes(fill=within),color="black", bins = 200) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  geom_vline(xintercept = 10,size = 1.5, linetype = "dashed",
             color = "#00AB08") +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  scale_fill_manual(values = c("#FFAE42","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  labs(x = "Measured difference between group means",
       y = "Number of samples",
       title = "Green dashed line: Measured difference = 10") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = "bottom")
ggsave("meandiff_h0_reject.pdf")

data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  mutate(within = ifelse(diffs<=mean(diffs)-1.96*sd(diffs) | diffs>=mean(diffs)+1.96*sd(diffs),
                         "no","yes")) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(aes(fill=within),color="black", bins = 200) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  geom_vline(xintercept = 10,size = 1.5, linetype = "dashed",
             color = "#00AB08") +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  scale_fill_manual(values = c("#FFAE42","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  labs(x = "Measured difference between group means",
       y = "Number of samples") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.title = element_blank(),
        legend.position = "none")
ggsave("meandiff_H0_rejected_wide.pdf",
       width = 10,
       height = 5,
       units = "cm")


# Large variance, smaller difference
####################################
N_1 <- 2000
N_2 <- 2000
mean_1 <- 45
mean_2 <- 30
sd_1 <- 40
sd_2 <- 40
nsamples <- 10000
samsize <- 50

pop <- data.frame(group = c(rep(1,N_1),rep(2,N_2)),
                  score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),
                            rnorm(n=N_2,mean=mean_2,sd=sd_2)))  


pop %>% 
  ggplot(aes(x=group,y=score,fill=factor(group))) +
  geom_point(shape=21,size=6, alpha = .5) +
  geom_segment(aes(x=.85,xend=1.15, y=mean_1,yend=mean_1)) +
  geom_segment(aes(x=1.85,xend=2.15, y=mean_2,yend=mean_2)) +
  scale_fill_manual(values = c("#ffae42","#007ba7")) +
  scale_y_continuous(limits = c(0,100),
                     breaks = seq(0,100,10)) +
  scale_x_continuous(limits = c(.5,2.5),
                     breaks = c(1,2)) +
  labs(x = "Group", y = "Score",
       title = paste0("Group 1: Mean = ",mean_1,"; St.Dev. = ",sd_1,"; N = ",N_1,
                      "\nGroup 2: Mean = ",mean_2,"; St.Dev. = ",sd_2,"; N = ",N_2)) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "none")
ggsave("meandiff_sim_pop_largevar.pdf")

# Draw random samples - for H_0! 
N_1 <- 2000
N_2 <- 2000
mean_1 <- 50
mean_2 <- 50
sd_1 <- 30
sd_2 <- 30
nsamples <- 10000
samsize <- 50

pop <- data.frame(group = c(rep(1,N_1),rep(2,N_2)),
                  score = c(rnorm(n=N_1,mean=mean_1,sd=sd_1),
                            rnorm(n=N_2,mean=mean_2,sd=sd_2)))

diffs <- sapply(seq(1,nsamples,1),
                function(x){
                  sample <- dplyr::sample_n(pop,
                                            size = samsize,
                                            replace = F)
                  smean1 <- mean(sample$score[which(sample$group==1)])
                  smean2 <- mean(sample$score[which(sample$group==2)])
                  return(smean1-smean2)
                })

data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(color="black", bins = 200, alpha = .7) +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  geom_vline(xintercept = 15, size = 1, linetype = "dashed",
             color = "#00AB08") +
  scale_x_continuous(limits = c(-30,30),
                     breaks = seq(-30,30,10)) +
  labs(x = "Measured difference between group means",
       y = "Number of samples",
       title = paste0(nsamples," random samples; ",samsize," observations per sample.")) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
ggsave("meandiff_sim_largevar.pdf")


# With quantile value
data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  ggplot(aes(x=diffs)) +
  geom_histogram(color="black", bins = 200, fill = "black") +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  geom_vline(xintercept = 15, size = 1.5, linetype = "dashed",
             color = "#00AB08") +
  annotate("segment", x = 0, xend = 15, y = 20, yend = 20,size=1.5,
           arrow = arrow(ends='both'), color = "#00AB08") + 
  annotate("text",x=15/2,y=30,color="#00AB08",size=10,
           label = paste0(round(15/(sd(diffs)),digits = 2))) +
  scale_x_continuous(limits = c(-30,30),
                     breaks = seq(-30,30,10)) +
  labs(x = "Measured difference between group means",
       y = "Number of samples",
       title = paste0("Our result is Z = ",round(15/(sd(diffs)),digits = 2)," standard deviations from the center.")) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  ggsave("meandiff_sim_largevar_quant1.pdf")

# with 95% shaded
data.frame(diffs = diffs,
           snum = seq(1,length(diffs),1)) %>% 
  mutate(within = ifelse(diffs<=mean(diffs)-1.96*sd(diffs) | diffs>=mean(diffs)+1.96*sd(diffs),
                         "no","yes")) %>% 
  ggplot(aes(x=diffs,fill=within)) +
  geom_histogram(bins = 200, color = "black") +
  geom_vline(xintercept = 0, size = 1, linetype = "solid",
             color = "Orange") +
  geom_vline(xintercept = 15, size = 1.5, linetype = "dashed",
             color = "#00AB08") +
  scale_fill_manual(values = c("#FFAE42","black"),
                    labels = c("Outer 5%","Inner 95%")) +
  annotate("segment", x = 0, xend = 15, y = 22, yend = 22,size=1.5,
           arrow = arrow(ends='both'), color = "#00AB08") + 
  annotate("text",x=15/2,y=30,color="#00AB08",size=9,
           label = paste0(round(15/(sd(diffs)),digits = 2))) +
  annotate("segment", x = 0, xend = mean(diffs)+1.96*sd(diffs), y = 2, yend = 2,size=1.5,
           arrow = arrow(ends='both'), color = "#FFAE42") + 
  annotate("text",x=15/2,y=10,color="#FFAE42",size=9,
           label = "1.96") +
  scale_x_continuous(limits = c(-30,30),
                     breaks = seq(-30,30,10)) +
  labs(x = "Measured difference between group means",
       y = "Number of samples",
       title = paste0("Our result is ",round(15/(sd(diffs)),digits = 2)," standard deviations from the center.")) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "bottom", legend.title = element_blank())
  ggsave("meandiff_sim_largevar_quant2.pdf")

  
# Calculate p-value: 
round(2*(pnorm(-abs(1.76))),
      digits = 3)  
  

# Different hypotheses for t-test
#################################

# Two-sided
ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
            xlim = c(-5, qt(.025, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "grey30", 
            xlim = c(qt(.025, df=3), qt(.975, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
            xlim = c(qt(.975, df=3), 5)) +
  geom_vline(color = "#7570b3", size = 1.5,
             aes(xintercept = 6.5, linetype = "Scenario 1")) +
  geom_vline(color = "#7570b3", size = 1.5, 
             aes(xintercept = -6.5, linetype = "Scenario 2")) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = qt(.975, df=3), color = "#d95f02", linetype = "dashed",size=1.5) +
  geom_vline(xintercept = qt(.025, df=3), color = "#d95f02", linetype = "dashed",size=1.5) +
  scale_y_continuous(limits = c(0,.4)) +
  scale_x_continuous(limits = c(-7.5,7.5)) +
  scale_linetype_manual(guide = guide_legend(reverse = T),
                        values = c("dotted","solid"),
                        labels = c("Scenario 2","Scenario 1")) +
  labs(y = "Density", x = "",
       title = paste0("Critical values for 95% = [",round(qt(.025, df=3),digits = 3),"; ",round(qt(.975, df=3),digits = 3),"]")) +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "bottom",
        legend.title = element_blank())
  ggsave("ttest_twosided.pdf")

# Larger
ggplot(NULL, aes(c(-10,10))) + 
  # geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
  #           xlim = c(-5, qt(.025, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "grey30", 
            xlim = c(-5, qt(.95, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
            xlim = c(qt(.95, df=3), 5)) +
  geom_vline(xintercept = 6.5, color = "#7570b3", size = 1.5) +
  geom_vline(xintercept = qt(.95, df=3), color = "#d95f02", linetype = "dashed",size=1.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(0,.4)) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,2)) +
  labs(y = "Density", x = "",
       title = paste0("Critical value for 95% = ",round(qt(.95, df=3),digits = 3)),
       caption = "Purple solid line: Possible 'true' difference") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "bottom",
        legend.title = element_blank())
ggsave("ttest_larger.pdf")

# Smaller
ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02",
            xlim = c(-5, qt(.05, df=3))) +
  geom_area(stat = "function", fun = dt, args = list(df=3), fill = "grey30", 
            xlim = c(qt(.05, df=3),5)) +
  # geom_area(stat = "function", fun = dt, args = list(df=3), fill = "#d95f02", 
  #           xlim = c(qt(.95, df=3), 5)) +
  geom_vline(xintercept = -6.5, color = "#7570b3", size = 1.5) +
  geom_vline(xintercept = qt(.05, df=3), color = "#d95f02", linetype = "dashed",size=1.5) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(limits = c(0,.4)) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,2)) +
  labs(y = "Density", x = "",
       title = paste0("Critical value for 95% = ",round(qt(.05, df=3),digits = 3)),
       caption = "Purple solid line: Possible 'true' difference") +
  theme_bw() +
  theme(axis.text = element_text(size=12),
        legend.position = "bottom",
        legend.title = element_blank())
ggsave("ttest_smaller.pdf")
