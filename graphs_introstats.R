
# Graphs for Intro to quant. methods course, 2021
#################################################

library(tidyverse)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# Example: News framing experiment
rally <- data.frame(rally = c(3.96,3.31),
                    speech = c(4.17,3.54),
                    treat = c("free","order"))

rally %>% 
  pivot_longer(cols = c("rally","speech"),
               names_to = "outcome",
               values_to = "value") %>% 
  filter(outcome=="rally") %>% 
  ggplot(aes(x=treat,y=value)) +
    geom_bar(stat = "identity") +
    scale_x_discrete(labels = c("Free speech frame",
                                "Public order frame")) +
  ylab("Support") +
  xlab("") +
  labs(title = "Support for allowing KKK rally") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  ggsave("rallygraph.pdf")
  
  
  
# Example: Normal distribution
set.seed(42)  
nodist <- data.frame(vals = rnorm(5000,
                                  sd = 2),
                     id = seq(1,5000,1))

nodist %>% 
  ggplot(aes(x=vals)) +
    stat_density(adjust = 2,
                 alpha = .5,
                 color = "red",
                 fill = "red") +
  theme_bw() +
  theme(axis.text = element_blank(),
        axis.title = element_blank(),
        axis.ticks = element_blank())
  ggsave("snorm.pdf")
    
    
# Visualization, confidence intervals
set.seed(17)
cidist <- data.frame(vals1 = rnorm(5000,
                                   mean = 1.96,
                                  sd = 1),
                     id = seq(1,5000,1))
  
cidist$vals2 <- cidist$vals1 - 0.6533333
cidist$vals3 <- cidist$vals2 - 0.6533333
cidist$vals4 <- cidist$vals3 - 0.6533333
cidist$vals5 <- cidist$vals4 - 0.6533333
cidist$vals6 <- cidist$vals5 - 0.6533333
cidist$vals7 <- cidist$vals6 - 0.6533333
    
    
cidist %>% 
  pivot_longer(cols = starts_with("vals"),
               values_to = "vals",
               names_to = "nums") %>% 
  ggplot(aes(x=vals,color = nums)) +
    geom_density(stat = "density",
                 adjust = 1.5) + 
    #geom_vline(xintercept = 1.96) +
    #geom_vline(xintercept = -1.96) +
    geom_vline(xintercept = 0) +
  scale_color_manual(values = c("gray","gray","gray","gray","gray","gray","gray")) +
  theme_bw()
  

# Graphs to illustrate differences in spread
############################################

# w/o SD
set.seed(42)  

for(i in seq(1,5,1)){
nodist <- data.frame(vals = rnorm(5000,
                                  sd = i),
                     id = seq(1,5000,1))

nodist %>% 
  ggplot(aes(x=vals)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(-20,20),
                     breaks = seq(-20,20,5)) +
  scale_y_continuous(limits = c(0,850),
                     breaks = seq(0,800,100)) +
  theme_bw() +
  theme(axis.title = element_blank())
ggsave(paste0("sd_",i,".pdf"))
}

# w/ sd
for(i in c(1.5,5)){
nodist <- data.frame(vals = rnorm(5000,
                                  sd = i,
                                  mean = 178),
                     id = seq(1,5000,1))

nodist %>% 
  ggplot(aes(x=vals)) +
  geom_histogram(bins = 100) +
  scale_x_continuous(limits = c(160,200),
                     breaks = seq(160,200,10)) +
  scale_y_continuous(limits = c(0,600),
                     breaks = seq(0,600,100)) +
  labs(title = paste0("Variance = ",round(var(nodist$vals), digits = 2),
                      "\n Standard deviation = ",round(sd(nodist$vals), digits = 2)),
       caption = "N = 5000",
       x = "Body height (cm)", y = "Observations") +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text = element_text(size = 14),
        axis.title = element_text(size=14))
ggsave(paste0("sd_",i,"_no.pdf"))
}

# Skewed distributions
data.frame(vals = rbeta(5000,5,2),
                   id = seq(1,5000,1)) %>% 
  ggplot(aes(x=vals)) +
    geom_histogram(colour = "black", alpha = .8) +
  labs(x = "", y = "Observations") +
  theme_bw()
  ggsave("leftskew.pdf")
  
data.frame(vals = rbeta(5000,2,5),
           id = seq(1,5000,1)) %>% 
  ggplot(aes(x=vals)) +
  geom_histogram(colour = "black", alpha = .8) +
  labs(x = "", y = "Observations") +
  theme_bw()
  ggsave("rightskew.pdf")
  
data.frame(vals = rnorm(5000),
           id = seq(1,5000,1)) %>% 
  ggplot(aes(x=vals)) +
  geom_histogram(colour = "black", alpha = .8) +
  labs(x = "", y = "Observations") +
  theme_bw()
  ggsave("symmetric.pdf")


# Temperature distribution
##########################
set.seed(17)
temp <- data.frame(temp = rnorm(1000,
                                mean = 37,
                                sd = .25))

temp %>% 
  ggplot(aes(x=temp)) +
    geom_histogram(colour = "black", alpha = .8) +
    geom_vline(xintercept = mean(temp$temp),
               linetype = "dashed", size = 1.25) +
    scale_x_continuous(limits = c(36,38),
                     breaks = seq(36,38,.5)) +
    labs(x = "Body temperature (Celsius)",
         y = "Number of observations",
         caption = paste0("The vertical dashed line indicates the average measured temperature: ",
                          round(mean(temp$temp),digits = 1),
                          "\n N = 1000")) +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size=14))
    ggsave("tempdist.pdf")
    
temp %>% 
  ggplot(aes(x=temp)) +
  geom_histogram(colour = "black", alpha = .8) +
  geom_vline(xintercept = mean(temp$temp),
             linetype = "dashed", size = 1.25) +
  geom_vline(xintercept = 36.5, color = "red") +
  geom_vline(xintercept = 37.5, color = "red") +
  scale_x_continuous(limits = c(36,38),
                     breaks = seq(36,38,.5)) +
  labs(x = "Body temperature (Celsius)",
       y = "Number of observations") +
  theme_bw() +
  theme(axis.text = element_text(size = 14),
        axis.title = element_text(size=14))
  ggsave("normalrange.pdf")



# Difference sampling vs. frequency distribution
################################################

# Frequency
pop <- 10*sample(seq(1,10,1),
              125,
              replace = T,
              prob = c(.02,.11,.17,.29,.14,.10,.09,.04,.03,0.01))
    
data <- data.frame(pop = pop,
                   idno = seq(1,length(pop),1))

data %>% 
  group_by(pop) %>% 
  summarize(n = n()) %>% 
  ggplot(aes(x=pop,y=n)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(10,100,10),
                     limits = c(5,105)) +
  geom_vline(xintercept = mean(pop), color = "#d95f02", size = 1.25) +
  labs(x = "Left-right self-placement",
       y = "Frequency") +
  theme_bw() 
  ggsave("freqdist.pdf",
         width = 10,
         height = 5,
         units = "cm")
    
# Sampling
means <- sapply(seq(1,10000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 20,
                                   replace = F)
                  return(mean(sample))
                })

sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))

sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) + #,
           #position = position_dodge(width=.1)) +
  # geom_vline(xintercept = mean(pop),
  #            color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("Sample means") +
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("sampdist.pdf",
       width = 10,
       height = 5,
       units = "cm")
  
# Without x-scale
sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) + #,
  #position = position_dodge(width=.1)) +
  # geom_vline(xintercept = mean(pop),
  #            color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("") +
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(axis.text.x = element_blank())
  ggsave("sampdist_square.pdf")
  
# With true mean
sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = mean(pop),
             color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("Sample means") +
  labs(caption = "The orange line indicates the 'true' mean.") + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("sampdist_true.pdf",
       width = 10,
       height = 5,
       units = "cm")
  
# Different sample sizes
########################
  
# Small
means <- sapply(seq(1,10000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 10,
                                   replace = F)
                  return(mean(sample))
                })

sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))
sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = mean(pop),
             color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("Sample means") +
  labs(caption = "The orange line indicates the 'true' mean.") + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("sampdist_small.pdf")

# Large
means <- sapply(seq(1,10000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 60,
                                   replace = F)
                  return(mean(sample))
                })

sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))

sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = mean(pop),
             color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("Sample means") +
  labs(caption = "The orange line indicates the 'true' mean.") + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("sampdist_large.pdf")
  
  
# With quantiles shaded
#######################
  
# New sampling
means <- sapply(seq(1,10000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 20,
                                   replace = F)
                  return(mean(sample))
                })

sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))
  
# 95%
sims_sd <- sd(sims$means)
sims_mean <- mean(sims$means)
  
sims$within <- ifelse(sims$means<=sims_mean + 1.96*sims_sd & sims$means>= sims_mean - 1.96*sims_sd,
                      "Yes","No")

sims %>%
  ggplot(mapping = aes(x=means,fill=within)) +
  geom_bar(stat = "count",
           width = 1) +
  scale_fill_manual(values = c("tomato","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  # geom_vline(xintercept = mean(pop),
  #            color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("") +
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom",
        axis.text.x = element_blank())
  ggsave("sampling_quantiles_95.pdf")

# 90%
sims_sd <- sd(sims$means)
sims_mean <- mean(sims$means)

sims$within <- ifelse(sims$means<=sims_mean + 1.645*sims_sd & sims$means>= sims_mean - 1.645*sims_sd,
                      "Yes","No")

sims %>%
  ggplot(mapping = aes(x=means,fill=within)) +
  geom_bar(stat = "count",
           width = 1) +
  scale_fill_manual(values = c("tomato","gray30"),
                    labels = c("Outer 10%","Inner 90%")) +
  # geom_vline(xintercept = mean(pop),
  #            color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("Sample means") +
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
  ggsave("sampling_quantiles_90.pdf")

# 99%
sims_sd <- sd(sims$means)
sims_mean <- mean(sims$means)

sims$within <- ifelse(sims$means<=sims_mean + 2.576*sims_sd & sims$means>= sims_mean - 2.576*sims_sd,
                      "Yes","No")

sims %>%
  ggplot(mapping = aes(x=means,fill=within)) +
  geom_bar(stat = "count",
           width = 1) +
  scale_fill_manual(values = c("tomato","gray30"),
                    labels = c("Outer 1%","Inner 99%")) +
  # geom_vline(xintercept = mean(pop),
  #            color = "#d95f02", size = 1.25) +
  ylab("Number of samples") +
  xlab("Sample means") +
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
ggsave("sampling_quantiles_99.pdf")


# Only sample mean
##################
sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",alpha=0,
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  scale_y_continuous(limits = c(0,1),
                     breaks = seq(0,1,1)) +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(title = "Our sample mean, 34.1") + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("sampmean.pdf",
         width = 10,
         height = 5,
         units = "cm")
  
# Different scenarios for true mean
###################################
  
means <- sapply(seq(1,10000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 20,
                                   replace = F)
                  return(mean(sample))
                })

# Scenario I
sims <- data.frame(means = means-7.5,
                   draws = seq(1,length(means),1))

sims_sd <- sd(sims$means)
sims_mean <- mean(sims$means)

sims$within <- ifelse(sims$means<=sims_mean + 1.96*sims_sd & sims$means>= sims_mean - 1.96*sims_sd,
                      "Yes","No")

sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  geom_vline(xintercept = mean(sims$means),
             color = "gray", size = 1.25, linetype = "dashed") +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(caption = paste0("Green solid line: Our sample mean, 34.1\nGray dashed line: Possible true mean, ",round(mean(sims$means),digits = 1))) + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("truemeanscen_1.pdf")
  
#  With quantiles
sims %>%
  ggplot(mapping = aes(x=means,fill=within)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  geom_vline(xintercept = mean(sims$means),
             color = "gray", size = 1.25, linetype = "dashed") +
  scale_fill_manual(values = c("tomato","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(caption = paste0("Green solid line: Our sample mean, 34.1\nGray dashed line: Possible true mean, ",round(mean(sims$means),digits = 1))) + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
ggsave("truemeanscen_1_quant.pdf")  
  
  
# Scenario II
sims <- data.frame(means = means-15,
                   draws = seq(1,length(means),1))

sims_sd <- sd(sims$means)
sims_mean <- mean(sims$means)

sims$within <- ifelse(sims$means<=sims_mean + 1.96*sims_sd & sims$means>= sims_mean - 1.96*sims_sd,
                      "Yes","No")

sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  geom_vline(xintercept = mean(sims$means),
             color = "gray", size = 1.25, linetype = "dashed") +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(caption = paste0("Green solid line: Our sample mean, 34.1\nGray dashed line: Possible true mean, ",round(mean(sims$means),digits = 1))) + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
  ggsave("truemeanscen_2.pdf")
  
  
#  With quantiles
sims %>%
  ggplot(mapping = aes(x=means,fill=within)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  geom_vline(xintercept = mean(sims$means),
             color = "gray", size = 1.25, linetype = "dashed") +
  scale_fill_manual(values = c("tomato","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(caption = paste0("Green solid line: Our sample mean, 34.1\nGray dashed line: Possible true mean, ",round(mean(sims$means),digits = 1))) + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
ggsave("truemeanscen_2_quant.pdf")   
  
  
# Scenario III
sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))

sims_sd <- sd(sims$means)
sims_mean <- mean(sims$means)

sims$within <- ifelse(sims$means<=sims_mean + 1.96*sims_sd & sims$means>= sims_mean - 1.96*sims_sd,
                      "Yes","No")

sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  geom_vline(xintercept = mean(sims$means),
             color = "gray", size = 1.25, linetype = "dashed") +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(caption = paste0("Green solid line: Our sample mean, 34.1\nGray dashed line: Possible true mean, ",round(mean(sims$means),digits = 1))) + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw()
ggsave("truemeanscen_3.pdf")
  
  
#  With quantiles
sims %>%
  ggplot(mapping = aes(x=means,fill=within)) +
  geom_bar(stat = "count",
           width = 1) +
  geom_vline(xintercept = 34.1,
             color = "#1b9e77", size = 1.25) +
  geom_vline(xintercept = mean(sims$means),
             color = "gray", size = 1.25, linetype = "dashed") +
  scale_fill_manual(values = c("tomato","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  ylab("Number of samples") +
  xlab("Sample mean") +
  labs(caption = paste0("Green solid line: Our sample mean, 34.1\nGray dashed line: Possible true mean, ",round(mean(sims$means),digits = 1))) + 
  scale_x_continuous(limits = c(5,105),
                     breaks = seq(10,100,10)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = "bottom")
ggsave("truemeanscen_3_quant.pdf")  

    
# Replication of Logunov et al., Sputnik V trial (Lancet 2021)
##############################################################
sputnik <- matrix(c(4840,62,14948,16), ncol = 2, byrow = T)

colnames(sputnik) <- c("Did not get COVID","Got COVID")
rownames(sputnik) <- c("Did not get vaccine","Got vaccine")

as.table(sputnik) # table

# Percentage difference
inc_untreat <- round((62/4902)*100, digits = 1) # incidence among untreated
inc_treat <- round((16/14964)*100, digits = 1) # incidence among treated
inc_treat-inc_untreat # difference
round(inc_treat/inc_untreat, digits = 3)*100 # relative risk


# odds-ratio
treat <- 16/14948 # odds in exposed group
place <- 62/4840 # odds in unexposed group

round(treat/place, digits = 3) # odds-ratio

round(1-(treat/place), digits = 3)*100 # efficacy

# chi-square
chisq.test(sputnik, correct = F) 


