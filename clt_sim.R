
# Simulation for Central Limit Theorem class
############################################

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Dashboard_stats_exercises")

library(tidyverse)
library(gganimate)
library(gifski)

# "True population" (from COVID survey, round 2)
pop <- readRDS("clt_popdata.RDS")

popdat <- data.frame(vacc=pop,
                  id=seq(1,length(pop)))
  rm(pop)

# "True" distribution
popdat %>% 
  count(vacc) %>% 
  mutate(perc = 100*(n/nrow(popdat))) %>% 
  ggplot(aes(x=vacc,y=perc)) +
    geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(0,10,1),
                     labels = c("Not not at all agree",
                               "1",
                               "2",
                               "3",
                               "4",
                               "5",
                               "6",
                               "7",
                               "8",
                               "9",
                               "Agree completely")) +
  scale_y_continuous(limits = c(0,25)) +
  labs(y="Percent",x=" 'If a vaccine were available, I would get vaccinated' ",
       title = "The 'true' population distribution",
       caption = paste0("Number of observations: ",format(length(popdat$vacc),big.mark=" "))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25,hjust = 1))


# "True" mean
mean(popdat$vacc)

# Draw sample
sample <- sample(popdat$vacc,
                 size = 1500,
                 replace = F)

sample <- data.frame(vacc = sample,
                     id = seq(1,length(sample),1))

# Sample mean
mean(sample$vacc)

# Sample distribution
sample %>% 
  count(vacc) %>% 
  mutate(perc = 100*(n/nrow(sample))) %>% 
  ggplot(aes(x=vacc,y=perc)) +
  geom_bar(stat = "identity") +
  scale_x_continuous(breaks = seq(0,10,1),
                     labels = c("Not not at all agree",
                                "1",
                                "2",
                                "3",
                                "4",
                                "5",
                                "6",
                                "7",
                                "8",
                                "9",
                                "Agree completely")) +
  scale_y_continuous(limits = c(0,25)) +
  labs(y="Percent",x=" 'If a vaccine were available, I would get vaccinated' ",
       title = "The random sample",
       caption = paste0("Number of observations: ",format(length(sample$vacc),big.mark=" "))) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 25,hjust = 1))

# Simulate repeat sampling
means <- sapply(seq(1,10000,1),
       function(x){
         sample <- sample(popdat$vacc,
                          size = 1500,
                          replace = F)
         return(mean(sample))
       })

sims <- data.frame(means = means,
                      draws = seq(1,length(means),1))
  rm(means)

sims %>% 
ggplot(mapping = aes(x=means)) + 
  geom_density() +
  geom_vline(xintercept = mean(popdat$vacc), color = "red") +
  geom_vline(xintercept = mean(sims$means), 
             color = "gray34") +
  ylab("Density") +
  xlab("Sample means") +
  theme_classic()


# Animation with large sample
#############################
means <- sapply(seq(1,5000,1), # draws 5000 random samples from "population"
                function(x){
                  sample <- sample(popdat$vacc,
                                   size = 1500,
                                   replace = F)
                  return(mean(sample))
                })

sims <- data.frame(means = means, # binds into data.frame
                   draws = seq(1,length(means),1))
rm(means) # removes clutter


# Animated plot
p <- ggplot(sims, aes(means)) # base for plot

for(i in seq(0,length(sims$draws),length.out=100)){ # loop for individual frames 
  p <- p + geom_dotplot(data=sims[1:i,], dotsize=0.04) +
    scale_x_continuous(limits = c(5,6.5),
                       breaks = seq(5,6.5,.25)) +
    xlab("Sample means") +
    ylab("Count") +
    theme_bw()
}

# Animation
animate(p +
          transition_layers(keep_layers = FALSE),
        renderer = gifski_renderer(loop = F),
        nframes=120, fps=5)


# not necessary; cleaning after earlier call
# sapply(paste0("gganim_plot",stringr::str_pad(seq(1,100,1),4,pad = "0"),".png"),
#        unlink) # clean clutter


