
# Graphs & data: Exercises Week 7 , t-test & correlation
########################################################

# Carlo Knotz

library(tidyverse)
library(essurvey)
library(MASS)
library(xtable)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# t-test
set.seed(17)

# Gender wage gap
femwage <- rnorm(512, mean = 43400, sd = 1199)
malwage <- rnorm(489, mean = 46150, sd = 1253)

gendwag <- data.frame(wage = c(rnorm(512, mean = 43400, sd = 1199),
                               rnorm(489, mean = 46150, sd = 1253)),
                      gender = c(rep("female",512),rep("male",489)))

# Solution
t.test(femwage,malwage,
       var.equal = T)

t.test(wage~gender,var.equal = T, gendwag)


ggplot(NULL, aes(c(-5,5))) +
  geom_line(stat = "function", fun = dt,args = list(df=999)) +
  scale_x_continuous(limits = c(-5,5),
                     breaks = seq(-5,5,1)) +
  labs(y = "Density", x = "t") +
  theme_bw()
  ggsave("ex_tdist_week7.pdf",
         width=15,
         height = 10,
         units = "cm")

# critical t-value, graphically
df <- 999
alpha <- .99

ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "grey30",
            xlim = c(-7.5, qt(alpha, df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3", 
            xlim = c(qt(alpha, df=df),7.5), color = "black") +
  geom_vline(xintercept = qt(alpha, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,1)) +
  labs(x = "", y = "Density",
       title = paste0("One-sided ('larger than') at ",alpha,
                      " significance level & df = ",df,": ",round(qt(alpha,df=df), digits = 4))) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  ggsave("gendergap_tcrit.pdf",
         width = 20,
         height = 15,
         units = "cm")
  

# p-value by hand:
2*pt(35.77, df = 999, lower.tail = F)
  

# Two-sided
ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(-7.5, qt((1-alpha)/2, df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "grey30", 
            xlim = c(qt((1-alpha)/2, df=df),qt(1-(1-alpha)/2,df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(qt(1-(1-alpha)/2, df=df),7.5), color = "black") +
  geom_vline(xintercept = qt(1-(1-alpha)/2, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = qt((1-alpha)/2, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,1)) +
  labs(x = "", y = "Density",
       title = paste0("Two-sided at ",alpha," significance level & df = ",df,": [",
                      round(qt((1-alpha)/2,df=df), digits = 3),"; ",round(qt(1-(1-alpha)/2,df=df), digits = 3),"]")) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  ggsave("gendergap_tcrit_twotail.pdf",
         width = 20,
         height = 15,
         units = "cm")
  




# correlation coefficient


cordat <- data.frame(inc = c(45079,44416,49120,47869,46075,46108,47730,45047,45084,44294),
                     red = c(7,8,8,7,7,9,7,6,9,7))

xtable(cordat, digits = 0)


# Solution
cordat %>% 
  ggplot(aes(x=inc,y=red)) +
    geom_point() +
    geom_smooth(method = "lm", se=F)

cov(cordat$inc,cordat$red, method = "pearson")

cor.test(formula = ~ inc + red, data = cordat)

# Step-by-step solution
cordat %>% 
  mutate(meaninc = mean(inc),
         meanred = mean(red),
         diffinc = inc - meaninc,
         diffred = red - meanred,
         covs = diffinc*diffred) -> cordat

# Table output 
cordat %>% 
  xtable(digits = c(0,0,0,2,2,2,2,2))

# Sum of covarance scores
cordat %>% 
  summarize(scovs = sum(covs))

# Variable variances
cordat %>% 
  mutate(sqdiffinc = diffinc^2,
         sqdiffred = diffred^2) %>% 
  summarize(sum(sqdiffred),
            sum(sqdiffinc))

# Test & result, graphically
alpha <- 0.95
df <- 8

ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(-7.5, qt((1-alpha)/2, df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "grey30", 
            xlim = c(qt((1-alpha)/2, df=df),qt(1-(1-alpha)/2,df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(qt(1-(1-alpha)/2, df=df),7.5), color = "black") +
  geom_vline(xintercept = qt(1-(1-alpha)/2, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = qt((1-alpha)/2, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = 0.091, linetype = "dashed", size = 1) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,1)) +
  labs(x = "", y = "Density",
       title = paste0("Two-sided at ",alpha," significance level & df = ",df,": [",
                      round(qt((1-alpha)/2,df=df), digits = 3),"; ",round(qt(1-(1-alpha)/2,df=df), digits = 3),"]")) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  ggsave("ex_cor_ttest.pdf",
         width=15,
         height = 10,
         units = "cm")
  
  
  
# Ad-hoc visualization
######################

# One-sided, larger
df <- 452
alpha <- .9
stat <- -1.64
  
ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "grey30",
            xlim = c(-7.5, qt(alpha, df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3", 
            xlim = c(qt(alpha, df=df),7.5), color = "black") +
  geom_vline(xintercept = qt(alpha, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = stat, color = "red", linetype = "dashed",
             size=1.5) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,1)) +
  labs(x = "", y = "Density",
       title = paste0("One-sided ('larger than') at ",alpha,
                      " significance level & df = ",df,": ",round(qt(alpha,df=df), digits = 4))) +
  theme_bw() +
  theme(axis.text = element_text(size=12))  
  
    
# One-sided, smaller
df <- 1415
alpha <- .95
stat <- -1.74

ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(-7.5, qt((1-alpha), df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "grey30", 
            xlim = c(qt((1-alpha), df=df),10), color = "black") +
  geom_vline(xintercept = qt(1-alpha, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = stat, color = "red", linetype = "dashed",
             size=1.5) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,1)) +
  labs(x = "", y = "Density",
       title = paste0("One-sided ('smaller than') at ",alpha,
                      " significance level & df = ",df,": ",round(qt(alpha,df=df), digits = 4))) +
  theme_bw() +
  theme(axis.text = element_text(size=12))  


# Two-sided
df <- 436
alpha <- .95
stat <- -3.62
  
ggplot(NULL, aes(c(-10,10))) + 
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(-7.5, qt((1-alpha)/2, df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "grey30", 
            xlim = c(qt((1-alpha)/2, df=df),qt(1-(1-alpha)/2,df=df)), color = "black") +
  geom_area(stat = "function", fun = dt, args = list(df=df), fill = "#d3d3d3",
            xlim = c(qt(1-(1-alpha)/2, df=df),7.5), color = "black") +
  geom_vline(xintercept = qt(1-(1-alpha)/2, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = qt((1-alpha)/2, df=df), color = "#d3d3d3", linetype = "dashed",
             size=1.5) +
  geom_vline(xintercept = stat, color = "red", linetype = "dashed",
             size=1.5) +
  scale_x_continuous(limits = c(-8,8),
                     breaks = seq(-8,8,1)) +
  labs(x = "", y = "Density",
       title = paste0("Two-sided at ",alpha," significance level & df = ",df,": [",
                      round(qt((1-alpha)/2,df=df), digits = 3),"; ",round(qt(1-(1-alpha)/2,df=df), digits = 3),"]")) +
  theme_bw() +
  theme(axis.text = element_text(size=12))
  
  
  
  
  
  

