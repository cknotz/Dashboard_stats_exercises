
# Graphs & other results: Exercise I for lecture 5, Sampling distributions & 
# Confidence intervals
#############################################################################


library(tidyverse)
library(essurvey)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# Empty plot: Household time
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,10)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  #geom_vline(xintercept = 120, linetype="dashed") +
  labs(x = "Results: Average time spent on household work", y = "Number of samples") +
  theme_bw()
ggsave("ex_housetime_sampling.pdf",
       width=15,
       height = 10,
       units = "cm")

# With one sample mean
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,10)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  geom_vline(xintercept = 8.5, linetype="dashed") +
  geom_vline(xintercept = 7.5, linetype="solid", color = "orange") +
  labs(x = "Results: Average time spent on household work", y = "Number of samples",
       caption = "Dashed line: Sample mean = 8.5 hours/week\nSolid orange line: True population mean = 7.5 hours/week") +
  theme_bw()
ggsave("ex_housetime_sampling_onemean.pdf",
       width=15,
       height = 10,
       units = "cm")

# With other sample mean
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,10)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  geom_vline(xintercept = 7.29, linetype="dashed") +
  labs(x = "Results: Average time spent on household work", y = "Number of samples",
       caption = "Dashed line: Sample mean = 7.29 hours/week") +
  theme_bw()
ggsave("ex_housetime_sampling_othmean.pdf",
       width=15,
       height = 10,
       units = "cm")


# ESS Data
##########

essurvey::set_email("carlo.knotz@gmail.com")

ess <- import_country(country = "Norway",
                      rounds = 5) %>% 
  recode_missings() %>% 
  select(idno,cntry,hwwkhs,gndr)


summary(ess$hwwkhs)
malemean <- mean(ess$hwwkhs[which(ess$gndr==1)], na.rm = T)
malesd <- sd(ess$hwwkhs[which(ess$gndr==1)], na.rm = T)
maleobs <- length(ess$hwwkhs[which(ess$gndr==1)])

femalemean <- mean(ess$hwwkhs[which(ess$gndr==2)], na.rm = T)

# Graph
ess %>% 
  filter(gndr==1) %>% 
  filter(hwwkhs<50) %>% 
  ggplot(aes(x=hwwkhs)) +
  geom_histogram(color="black",fill = "grey40",
                 aes(y=..density..)) +
  geom_vline(xintercept = malemean, linetype="dashed", size= 1.5) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  # annotate("segment", x = malemean-malesd,xend = malemean+malesd,
  #          y = 0.025, yend = 0.025, arrow = arrow(ends = "both"), size = 1.5) +
  labs(x = "Time spent on household work per week", y = "Density",
       caption = paste0("Dashed vertical line indicates mean: ",round(malemean,digits=1)," hours/week")) + 
  theme_bw()
ggsave("ess_menhhtime.pdf",
       width=15,
       height = 10,
       units = "cm")


# Simulating sampling distribution
##################################

# pop <- rep.int(ess$hwwkhs[which(ess$gndr==1)],
#            3292) # 3292=2650000/805 to approximate NO male population

pop <- rnorm(2650000,
             mean = 7.5,
             sd = 40)

# This does the 1000 "surveys" & calculates results
means <- sapply(seq(1,1000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 800,
                                   replace = F)
                  return(mean(sample, na.rm = T))
                })

sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))

# Zoomed in graph
sims %>% 
  ggplot(aes(x=means)) +
  geom_histogram(binwidth = .1, color = "black") +
  geom_vline(xintercept = 7.5, color = "orange") +
  # scale_x_continuous(limits = c(0,40),
  #                    breaks = seq(0,40,5)) +
  labs(x = "Time spent on household work per week", y = "Number of samples") + 
  theme_bw()
ggsave("ess_samdist_zoom.pdf",
       width=15,
       height = 10,
       units = "cm")


# Same scale as above
sims %>% 
  ggplot(aes(x=means)) +
    geom_histogram(binwidth = .5,color = "black") +
    geom_vline(xintercept = 7.5, color = "orange") +
    scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Orange vertical line indicates true population mean = 7.5 hours/week")) + 
  theme_bw()
ggsave("ess_samdist_fullscale.pdf",
       width=15,
       height = 10,
       units = "cm")

# True mean, scenario 1
sims %>% 
  ggplot(aes(x=means-0.21)) +
  geom_histogram(binwidth = .5,color = "black") +
  geom_vline(xintercept = 7.29, color = "#39ff14", linetype="dashed", size = 1.25) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Dashed green line indicates sample mean = 7.29 hours/week")) + 
  theme_bw()
ggsave("ess_samdist_scen1.pdf",
       width=15,
       height = 10,
       units = "cm")


# True mean, scenario 2
sims %>% 
  ggplot(aes(x=means-2)) +
  geom_histogram(binwidth = .5,color = "black") +
  geom_vline(xintercept = 7.29, color = "#39ff14", linetype="dashed", size = 1.25) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Dashed green line indicates sample mean = 7.29 hours/week")) + 
  theme_bw()
ggsave("ess_samdist_scen2.pdf",
       width=15,
       height = 10,
       units = "cm")

# True mean, scenario 3
sims %>% 
  ggplot(aes(x=means+2)) +
  geom_histogram(binwidth = .5,color = "black") +
  geom_vline(xintercept = 7.29, color = "#39ff14", linetype="dashed", size = 1.25) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Dashed green line indicates sample mean = 7.29 hours/week")) + 
  theme_bw()
ggsave("ess_samdist_scen3.pdf",
       width=15,
       height = 10,
       units = "cm")

# True mean, scenario 4
sims %>% 
  ggplot(aes(x=means+5)) +
  geom_histogram(binwidth = .5,color = "black") +
  geom_vline(xintercept = 7.29, color = "#39ff14", linetype="dashed", size = 1.25) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Dashed green line indicates sample mean = 7.29 hours/week")) + 
  theme_bw()
ggsave("ess_samdist_scen4.pdf",
       width=15,
       height = 10,
       units = "cm")


# With critical range
#####################

# Scenario 1

# Which range is this finding in?
pnorm(7.29, mean = 5.47, sd = 1.38) - pnorm(7.29, mean = 5.47, sd = 1.38, 
                                            lower.tail = F)
sims %>% 
  mutate(means = means-2,
    within = ifelse(means<=mean((means))+1.96*sd((means)) &
                           means>=mean((means))-1.96*sd((means)),
                         "Yes","No")) %>%
  ggplot(aes(x=means,fill=within)) +
  geom_histogram(binwidth = .5,color = "black") +
  geom_vline(xintercept = 7.29, color = "#39ff14", linetype="dashed", size = 1.25) +
  geom_vline(xintercept = mean((sims$means-2)), color = "Cornflowerblue", linetype="solid", size = 1.25) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  scale_fill_manual(values = c("#FFAE42","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Dashed green line indicates sample mean = 7.29 hours/week
                        Blue line indicates POTENTIAL population mean = ",round(mean((sims$means-2)),digits = 2)," hours/week"), #
       title = paste0("Here we are 1.31 stand. devs. from the POTENTIAL population mean")) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  ggsave("scenprobs_scen1.pdf")

# Scenario 2
sims %>% 
  mutate(means = means+5,
         within = ifelse(means<=mean((means))+1.96*sd((means)) &
                           means>=mean((means))-1.96*sd((means)),
                         "Yes","No")) %>% 
  ggplot(aes(x=means,fill=within)) +
  geom_histogram(binwidth = .5,color = "black") +
  geom_vline(xintercept = 7.29, color = "#39ff14", linetype="dashed", size = 1.25) +
  geom_vline(xintercept = mean((sims$means+5)), color = "Cornflowerblue", linetype="solid", size = 1.25) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,5)) +
  scale_fill_manual(values = c("#FFAE42","gray30"),
                    labels = c("Outer 5%","Inner 95%")) +
  labs(x = "Time spent on household work per week", y = "Number of samples",
       caption = paste0("Dashed green line indicates sample mean = 7.29 hours/week
                        Blue line indicates POTENTIAL population mean = ",round(mean((sims$means+5)),digits = 2)," hours/week"), #
       title = paste0("Here we are 3.75 stand. devs. from the POTENTIAL population mean")) + 
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
  ggsave("scenprobs_scen2.pdf")


# Other sample mean, with CI
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(0,40),
                     breaks = seq(0,40,10)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  geom_vline(xintercept = 7.29, linetype="dashed") +
  geom_errorbar(aes(y = 200,
                    xmax = 7.29+1.96*(60/sqrt(800)), xmin = 7.29-1.96*(60/sqrt(800))), color = "orange",
                    size = 1.25, width = 25) +
  labs(x = "Results: Average time spent on household work", y = "Number of samples",
       caption = "Dashed line: Sample mean = 7.29 hours/week") +
  theme_bw()
ggsave("ex_housetime_sampling_othmean_CI.pdf",
       width=15,
       height = 10,
       units = "cm")


# Final result, w/o women
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(5,15),
                     breaks = seq(5,15,2.5)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  geom_vline(xintercept = 7.29, linetype="dashed") +
  geom_vline(xintercept = 7.5, linetype="solid") +
  geom_errorbar(aes(y = 200,
                    xmax = 7.29+1.96*(7.1/sqrt(850)), xmin = 7.29-1.96*(7.1/sqrt(850))), color = "orange",
                size = 1.25, width = 25) +
  labs(x = "Results: Average time spent on household work", y = "",
       caption = "Dashed line: Sample mean = 7.29 hours/week
       Solid line: TRUE population mean = 7.5 hours/week") +
  theme_bw()
ggsave("result_task4_1.pdf",
       width=15,
       height = 10,
       units = "cm")


# Final result, w/ women
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(5,15),
                     breaks = seq(5,15,2.5)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  geom_vline(xintercept = 7.29, linetype="dashed") +
  geom_vline(xintercept = 7.5, linetype="solid") +
  geom_vline(xintercept = 13.8, linetype="solid", color = "Cornflowerblue") +
  geom_errorbar(aes(y = 200,
                    xmax = 7.29+1.96*(7.1/sqrt(850)), xmin = 7.29-1.96*(7.1/sqrt(850))), color = "orange",
                size = 1.25, width = 25) +
  labs(x = "Results: Average time spent on household work", y = "",
       caption = "Dashed line: Sample mean = 7.29 hours/week
       Solid black line: TRUE population mean = 7.5 hours/week
       Solid blue line: Mean for women = 13.8 hours/week") +
  theme_bw()
ggsave("result_task4_2.pdf",
       width=15,
       height = 10,
       units = "cm")

# 99% level of confidence
ggplot(NULL,aes(x=c(50,150),y=c(0,400))) +
  scale_x_continuous(limits = c(5,15),
                     breaks = seq(5,15,2.5)) +
  scale_y_continuous(limits = c(0,400),
                     breaks = seq(0,400,50)) +
  geom_vline(xintercept = 7.29, linetype="dashed") +
  geom_vline(xintercept = 7.5, linetype="solid") +
  geom_vline(xintercept = 13.8, linetype="solid", color = "Cornflowerblue") +
  geom_errorbar(aes(y = 200,
                    xmax = 7.29+1.96*(7.1/sqrt(850)), xmin = 7.29-1.96*(7.1/sqrt(850)), color = "orange"),
                size = 1.25, width = 25) +
  geom_errorbar(aes(y = 250,
                    xmax = 7.29+2.576*(7.1/sqrt(850)), xmin = 7.29-2.576*(7.1/sqrt(850)), color = "#39ff14"),
                size = 1.25, width = 25) +
  scale_color_manual(labels = c("99%","95%"),
                     values = c("#39ff14","orange")) +
  labs(x = "Results: Average time spent on household work", y = "",
       caption = "Dashed line: Sample mean = 7.29 hours/week
       Solid black line: TRUE population mean = 7.5 hours/week
       Solid blue line: Mean for women = 13.8 hours/week",
       title = paste0("99% CI: [",
                      round(7.29-2.576*(7.1/sqrt(850)),digits = 2),"; ",
                      round(7.29+2.576*(7.1/sqrt(850)),digits = 2),"]")) +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank())
ggsave("result_task4_3.pdf",
       width=15,
       height = 10,
       units = "cm")


# Simulation: Close to 0
########################

pop <- rchisq(n=90000,df=1)

hist(pop)
mean(pop)

# This does the 1000 "surveys" & calculates results
means <- sapply(seq(1,1000,1),
                function(x){
                  sample <- sample(pop,
                                   size = 50,
                                   replace = F)
                  return(mean(sample, na.rm = T))
                })


sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))

# Zoomed in graph
sims %>% 
  ggplot(aes(x=means)) +
  geom_histogram(color = "black") +
  geom_vline(xintercept = 0, color = "orange") +
  labs(x = "", y = "") + 
  theme_bw()

