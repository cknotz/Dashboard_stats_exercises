
# Graphs & tables, Exercise Week 6: Tabular Analysis
####################################################

library(tidyverse)
library(xtable)
library(essurvey)
library(labelled)
library(viridis)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

essurvey::set_email("carlo.knotz@gmail.com")

ess <- import_rounds(rounds = 2) %>% 
  recode_missings() %>% 
  filter(cntry %in% c("NO")) %>% 
  mutate(money = ifelse(mnyacth>2,"Agree","Do not agree")) #

ess %>% 
  mutate(edu = recode(ess$edlvno, 
                      `0`=1L,`1`=1L,`2`=1L,
                      `3`=2L,`4`=2L,`5`=2L,
                      `6`=3L,`7`=3L,`8`=3L,
                      .keep_value_labels = FALSE)) -> ess

val_labels(ess$edu) <- c("Lower"=1,"Middle"=2,"Higher"=3)
val_labels(ess$edu)
unique(ess$edu)


# Exercise 1:
#############
freq <- table(ess$money,ess$edu)
freq <- addmargins(freq)

freq <- xtable(freq,
               caption = "Frequencies: Level of education \\& willingness to behave dishonestly",
               align = c("l","c","c","c","c"),
               digits = 0)

align(freq) <- xalign(freq)
digits(freq) <- xdigits(freq)
display(freq) <- xdisplay(freq)

print(freq,file="ess_freq.tex",table.placement = "h",booktabs = T,
      caption.placement="bottom")

freq

# Percentages
freqs <- as.matrix(table(ess$money,ess$edu))
freqs <- cbind(freqs,rowSums(freqs))

colnames(freqs) <- c("Lower","Middle","Higher","Sum")

prop <- prop.table(freqs,2)

prop <- 100*prop.table(freqs,2) # Column percentages
prop <- addmargins(prop, 1)

prop <- xtable(prop,
               caption = "Percentages: Vaccine receipt and COVID-19 incidences",
               align = c("l","c","c","c"),
               digits = 1)

align(prop) <- xalign(prop)
display(prop) <- xdisplay(prop)


# Chi-squared distributions
df <- 1
ggplot(NULL,aes(c(0,12))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,1)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
  ggsave("ex_chi_1.pdf")
  
df <- 2
ggplot(NULL,aes(c(0,15))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  scale_x_continuous(limits = c(0,15),
                     breaks = seq(0,15,1)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
ggsave("ex_chi_2.pdf")


df <- 5
ggplot(NULL,aes(c(0,25))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  #scale_y_continuous(limits = c(0,.75)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
ggsave("ex_chi_5.pdf")

df <- 10
ggplot(NULL,aes(c(0,35))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  #scale_y_continuous(limits = c(0,.75)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
ggsave("ex_chi_10.pdf")

df <- 42
ggplot(NULL,aes(c(0,80))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  #scale_x_continuous(limits = c(0,.75)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
ggsave("ex_chi_42.pdf")


# Chi-squared test
res <- chisq.test(as.matrix(table(ess$money,ess$edu)))

res$expected


# Correct chi-sq with critical vals
df <- 2
ggplot(NULL,aes(c(0,15))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  scale_x_continuous(limits = c(0,15),
                     breaks = seq(0,15,1)) +
  scale_color_viridis(discrete = T,guide = guide_legend(reverse = T),
                      direction = -1) +
  geom_vline(aes(xintercept = 4.605, color = "0.1"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 5.991, color = "0.05"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 7.378, color = "0.025"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 9.210, color = "0.01"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 13.816, color = "0.001"),
             linetype = "dashed", size = 1.5) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df),
       color = "Level of significance") +
  theme_bw() +
  theme(legend.position = "bottom")
  ggsave("ex_week6_solution.pdf")
  
# same, but with test result
ggplot(NULL,aes(c(0,20))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  scale_x_continuous(limits = c(0,20),
                     breaks = seq(0,20,1)) +
  scale_color_viridis(discrete = T,guide = guide_legend(reverse = T),
                      direction = -1) +
  geom_vline(aes(xintercept = 4.605, color = "0.1"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 5.991, color = "0.05"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 7.378, color = "0.025"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 9.210, color = "0.01"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 13.816, color = "0.001"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 17.97), color = "#de2d26",
             linetype = "solid", size = 1.5) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df),
       color = "Level of significance",
       caption = "Solid red line indicates test value: 17.97") +
  theme_bw() +
  theme(legend.position = "bottom")
ggsave("ex_week6_solution_res.pdf")

