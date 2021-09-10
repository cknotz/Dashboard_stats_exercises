
# Graphs & Tables, Kvantitativ forskningsmetode
###############################################

# Lecture 6: Tabular analysis & chi-squared test

library(tidyverse)
library(xtable)
library(viridis)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# Replication of Logunov et al., Sputnik V trial (Lancet 2021)
##############################################################
sputnik <- matrix(c(4840,62,14948,16), ncol = 2, byrow = T)

colnames(sputnik) <- c("Did not get COVID","Got COVID")
rownames(sputnik) <- c("Did not get vaccine","Got vaccine")

# Frequency table
freq <- as.table(t(sputnik))
freq <- addmargins(freq)

freq <- xtable(freq,
               caption = "Frequencies: Vaccine receipt and COVID-19 incidences",
               align = c("l","c","c","c"),
               digits = 0)

align(freq) <- xalign(freq)
digits(freq) <- xdigits(freq)
display(freq) <- xdisplay(freq)

print(freq,file="sputnik_freq.tex",table.placement = "h",booktabs = T,
      caption.placement="bottom")

# Percentages
propsput <- t(rbind(sputnik,sputnik[1,] + sputnik[2,]))
colnames(propsput) <- c("Did not get vaccine","Got vaccine","Sum")

prop <- 100*prop.table(propsput,2) # Column percentages
prop <- addmargins(prop, 1)

prop <- xtable(prop,
               caption = "Percentages: Vaccine receipt and COVID-19 incidences",
               align = c("l","c","c","c"),
               digits = 1)

align(prop) <- xalign(prop)
display(prop) <- xdisplay(prop)

print(prop,file="sputnik_prop.tex",table.placement = "h",booktabs = T,
      caption.placement="bottom",hline.after=c(-1, 0),
      add.to.row = list(pos = list(nrow(prop)),
                        command = paste0("\\bottomrule \n \\multicolumn{4}{l}",
                                       "{\\scriptsize{Note: Table shows column percentages.}} \n")))

# Percentages, graphically - w/ null result
data.frame(group = c("Unvaccinated","Vaccinated"),
           prob = c(1.3,0.1)) %>% 
  ggplot(aes(x=group,y=prob)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(limits = c(0,1.5),
                     breaks = seq(0,1.5,.25)) +
  labs(y="Share of COVID-19 diagnoses",
       x = "Treatment group") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=14))
ggsave("sputnik_probs.pdf")


# Percentages, graphically - w/ null result
data.frame(group = c("Unvaccinated","Vaccinated"),
                      prob = c(1.3,0.1),
           null = rep(0.4,2)) %>% 
  pivot_longer(cols = c("null","prob"),
               names_to = "result",
               values_to = "vals") %>% 
  ggplot(aes(x=group,y=vals,fill=result)) +
    geom_bar(stat = "identity", position = "dodge") +
    scale_y_continuous(limits = c(0,1.5),
                       breaks = seq(0,1.5,.25)) +
    scale_fill_manual(values = c("#ff8633","#3393ff"),
                      labels = c("Expected if there was no effect","Observed")) +
  labs(x="", y = " Probability of contracting COVID-19 (%)") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  ggsave("sputnik_probs_null.pdf")

# Percentage difference
inc_untreat <- round((62/4902)*100, digits = 1) # incidence among untreated
inc_untreat

inc_treat <- round((16/14964)*100, digits = 1) # incidence among treated
inc_treat

inc_treat-inc_untreat # difference
round(inc_treat/inc_untreat, digits = 3)*100 # relative risk


# odds-ratio, using frequencies
base <- 78/19788
treat <- 16/14948 # odds in exposed group
place <- 62/4840 # odds in unexposed group

round(treat/place, digits = 3) # odds-ratio

round(1-(treat/place), digits = 3)*100 # efficacy

# odds-ration, using probabilities
base_prob <- 0.4/99.6
treat_prob <- 0.1/99.9
place_prob <- 1.3/98.7

treat_prob/place_prob
1-(treat_prob/place_prob)


# chi-square
Xsq <- chisq.test(sputnik, correct = F) 
  Xsq$observed
  Xsq$expected

# Exact p-value, computed chi-stat
pchisq(Xsq$statistic,
       df = 1,
       lower.tail = F)

# Exact p-value, chi-stat by hand
pchisq(124.266,
       df = 1,
       lower.tail = F)

# Generate chi-squared distributions
####################################
  
# df=1, shaded
data.frame(chisq = 0:7000 / 100) %>% 
  mutate(density = dchisq(x = chisq, df = 1)) %>% 
  mutate(within = ifelse(chisq>=3.841,"no","yes")) %>% 
  filter(chisq<=12) %>% 
  ggplot(aes(x=chisq,y=density)) +
  geom_area(fill="grey30") +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,2)) +
  scale_y_continuous(limits = c(0,1)) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=14))
  ggsave("chidist.pdf")

# df=1:10
data.frame(chisq = 0:7000 / 100) %>% 
  mutate(df_01 = dchisq(x = chisq, df = 1),
         df_03 = dchisq(x = chisq, df = 3),
         df_05 = dchisq(x = chisq, df = 5),
         df_10 = dchisq(x = chisq, df = 10)) %>% 
  pivot_longer(cols = starts_with("df_"),
               names_to = "dist",
               values_to = "density") %>% 
  ggplot(aes(x=chisq,y=density,color=factor(dist))) +
    geom_line(size = 1.5) +
  scale_x_continuous(limits = c(0,20)) +
  scale_y_continuous(limits = c(0,.5)) +
  scale_color_viridis(discrete = T,option = "turbo",
                      labels = c("df=1","df=3","df=5","df=10")) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=14))
  ggsave("chidist_dfs.pdf")

  
  
# df=1, with 95% quantile shaded
data.frame(chisq = 0:7000 / 100) %>% 
  mutate(density = dchisq(x = chisq, df = 1)) %>% 
  mutate(within = ifelse(chisq>=3.841,"no","yes")) %>% 
  filter(chisq<=12) %>% 
  ggplot(aes(x=chisq,y=density,fill=within)) +
    geom_area() +
    geom_vline(xintercept = 3.841, color = "#ff8633",
               linetype = "dashed", size = 1.5) +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,2)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_fill_manual(values = c("#ff8633","grey30"),
                    labels = c("Outer 5%","Inner 95%")) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        axis.text = element_text(size=14))
  ggsave("chi_crit.pdf")
  
  
# Different thresholds indicated
data.frame(chisq = 0:7000 / 100) %>% 
  mutate(density = dchisq(x = chisq, df = 1)) %>% 
  filter(chisq<=12) %>% 
  ggplot(aes(x=chisq,y=density)) +
  geom_area(fill = "grey30") +
  geom_vline(aes(xintercept = 2.706, color = "0.1"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 3.841, color = "0.05"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 5.024, color = "0.025"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 6.635, color = "0.01"),
             linetype = "dashed", size = 1.5) +
  geom_vline(aes(xintercept = 10.828, color = "0.001"),
             linetype = "dashed", size = 1.5) +
  scale_x_continuous(limits = c(0,12),
                     breaks = seq(0,12,2)) +
  scale_y_continuous(limits = c(0,1)) +
  scale_color_viridis(discrete = T,guide = guide_legend(reverse = T),
                      direction = -1) +
  ylab("Density") +
  xlab(~ paste(chi ^ 2, "-value")) +
  labs(color = "Level of significance") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.text = element_text(size=12),
        axis.text = element_text(size=14))
  ggsave("chi_diffsig.pdf")
  
pdf("chi_crit.pdf")
dist <- rchisq(10000,1)
  dens <- density(dist)
  plot(dens,xlim = c(0,12),
       main = "", xlab = expression(chi^2-value))
  value <- 3.841
  polygon(c(dens$x[dens$x >= value ], value),
          c(dens$y[dens$x >= value ], 0),
          col = "#ff8633",
          border = 1)
  polygon(c(dens$x[dens$x < value ], value),
          c(dens$y[dens$x < value ], 0),
          col = "grey30",
          border = 1)
  abline(v=value,col = "#ff8633",lty=2, lwd=3)
  dev.off()

# Larger table
##############
  
ltab <- matrix(c(587,398,116,423,756,178,265,934,512), ncol = 3, byrow = T)
  
ltab  

colnames(ltab) <- c("Y_1","Y_2","Y_3")
rownames(ltab) <- c("X_1","X_2","X_3")

# Frequency table
ltab <- as.table(ltab)

ltab <- xtable(ltab,
               caption = "A fictional 3x3 table",
               align = c("l","c","c","c"),
               digits = 0)

align(ltab) <- xalign(ltab)
digits(ltab) <- xdigits(ltab)
display(ltab) <- xdisplay(ltab)

print(ltab,file="largertab.tex",table.placement = "h",booktabs = T,
      caption.placement="bottom")
  

# Exercise solutions, as graphs
###############################

# Case 1
nrow  <-  2
ncol <- 4

df <- (nrow-1)*(ncol-1)
siglev <-  0.95
xmax <- 15

ggplot(NULL, aes(c(0,xmax))) + 
  geom_area(stat = "function", fun = dchisq, fill = "grey30", xlim = c(0, qchisq(siglev, df=df)), args = list(df=df)) +
  geom_area(stat = "function", fun = dchisq, fill = "#d95f02", xlim = c(qchisq(siglev, df=df), xmax), args = list(df=df)) +
  geom_vline(xintercept = qchisq(siglev, df=df), color = "#d95f02", linetype = "dashed", size = 1.25) +
  labs(y = "Density",
       title = paste0("Critical value = ",round(qchisq(siglev, df=df),digits = 3)," (df=",df,")")) +
  xlab(~ paste(chi ^ 2, "-value")) +
  theme_bw() +
  theme(axis.text = element_text(size=14))
  ggsave("chi_case1.pdf")

# Case 2
nrow  <-  4
ncol <- 6

df <- (nrow-1)*(ncol-1)
siglev <- 0.99
xmax <- 40

ggplot(NULL, aes(c(0,xmax))) + 
  geom_area(stat = "function", fun = dchisq, fill = "grey30", xlim = c(0, qchisq(siglev, df=df)), args = list(df=df)) +
  geom_area(stat = "function", fun = dchisq, fill = "#d95f02", xlim = c(qchisq(siglev, df=df), xmax), args = list(df=df)) +
  geom_vline(xintercept = qchisq(siglev, df=df), color = "#d95f02", linetype = "dashed", size = 1.25) +
  labs(y = "Density",
       title = paste0("Critical value = ",round(qchisq(siglev, df=df),digits = 3)," (df=",df,")")) +
  xlab(~ paste(chi ^ 2, "-value")) +
  theme_bw() +
  theme(axis.text = element_text(size=14))
ggsave("chi_case2.pdf")

# Case 3
nrow  <-  12
ncol <- 3

df <- (nrow-1)*(ncol-1)
siglev <- 0.90
xmax <- 50

ggplot(NULL, aes(c(0,xmax))) + 
  geom_area(stat = "function", fun = dchisq, fill = "grey30", xlim = c(0, qchisq(siglev, df=df)), args = list(df=df)) +
  geom_area(stat = "function", fun = dchisq, fill = "#d95f02", xlim = c(qchisq(siglev, df=df), xmax), args = list(df=df)) +
  geom_vline(xintercept = qchisq(siglev, df=df), color = "#d95f02", linetype = "dashed", size = 1.25) +
  labs(y = "Density",
       title = paste0("Critical value = ",round(qchisq(siglev, df=df),digits = 3)," (df=",df,")")) +
  xlab(~ paste(chi ^ 2, "-value")) +
  theme_bw() +
  theme(axis.text = element_text(size=14))
ggsave("chi_case3.pdf")







