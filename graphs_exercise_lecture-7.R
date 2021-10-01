
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
malwage <- rnorm(498, mean = 46150, sd = 1253)

# Solution
t.test(femwage,malwage,
       var.equal = T)


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

# correlation coefficient


cordat <- data.frame(inc = round(rnorm(n=10, mean = 47329, sd = 2376), digits = 0),
                     red = round(rnorm(n=10, mean = 7.4, sd = 2.1), digits = 0))

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


cordat %>% 
  xtable(digits = c(0,0,0,2,2,2,2,2))

cordat %>% 
  mutate(sqdiffinc = diffinc^2,
         sqdiffred = diffred^2) %>% 
  summarize(sum(sqdiffred),
            sum(sqdiffinc))


ggplot(NULL, aes(c(-5,5))) +
  geom_line(stat = "function", fun = dt,args = list(df=8)) +
  geom_vline(xintercept = 2.306, linetype = "dashed", size = 1) +
  geom_vline(xintercept = -2.306, linetype = "dashed", size = 1) +
  geom_vline(xintercept = 0.091, linetype = "solid", size = 1) +
  scale_x_continuous(limits = c(-5,5),
                     breaks = seq(-5,5,1)) +
  labs(y = "Density", x = "t") +
  theme_bw()
  ggsave("ex_cor_ttest.pdf",
         width=15,
         height = 10,
         units = "cm")

