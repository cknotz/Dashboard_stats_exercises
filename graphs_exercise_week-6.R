
# Graphs & tables, Exercise Week 6: Tabular Analysis
####################################################

library(tidyverse)
library(xtable)
library(essurvey)
library(labelled)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

essurvey::set_email("carlo.knotz@gmail.com")

ess <- import_rounds(rounds = 2) %>% 
  recode_missings() %>% 
  filter(cntry %in% c("NO")) %>% 
  mutate(money = ifelse(mnyacth>2,"Agree","Do not agree"),
         edu = recode(ess$edlvno, 
                      `0`=1L,`1`=1L,`2`=1L,
                      `3`=2L,`4`=2L,`5`=2L,
                      `6`=3L,`7`=3L,`8`=3L,
                      .keep_value_labels = FALSE))

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
  #scale_y_continuous(limits = c(0,.75)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
  ggsave("ex_chi_1.pdf")
  
df <- 2
ggplot(NULL,aes(c(0,12))) +
  geom_line(stat = "function", fun = dchisq,args = list(df=df)) +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,1)) +
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
  scale_x_continuous(limits = c(0,.75)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(title = paste0("Degrees of freedom = ",df)) +
  theme_bw()
ggsave("ex_chi_42.pdf")


# Chi-squared test
chisq.test(as.matrix(table(ess$money,ess$edu)))



