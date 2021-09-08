
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

# Percentages, graphically
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
  ggsave("sputnik_probs.pdf")

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

  
  data.frame(dist = dchisq(10000,1),
             id = seq(1,10000,1))
