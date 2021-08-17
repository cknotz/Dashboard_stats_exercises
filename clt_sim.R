
# Simulation for Central Limit Theorem class
############################################

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Dashboard_stats_exercises")

library(tidyverse)
library(gganimate)
library(gifski)
library(shiny)

# New true population, simulated
pop <- sample(rep(round(rnorm(10,4,1.5), digits = 0),100),121)

# population mean
mean(pop)

# Simulate repeat sampling
means <- sapply(seq(1,500,1),
                function(x){
                  sample <- sample(pop,
                                   size = 100,
                                   replace = F)
                  return(mean(sample))
                })

sims <- data.frame(means = means,
                   draws = seq(1,length(means),1))
rm(means)

sims %>%
  ggplot(mapping = aes(x=means)) +
  geom_density() +
  geom_vline(xintercept = mean(pop), color = "red") +
  geom_vline(xintercept = mean(sims$means),
             color = "gray34") +
  ylab("Density") +
  xlab("Sample means") +
  theme_classic()

ui <- fluidPage(
  titlePanel(""),
  sidebarLayout(
    
  )
)


server <- function(input,output,session) {
  
  
}







# ### NOT USED ###
# 
# # Animation with large sample
# #############################
# means <- sapply(seq(1,5000,1), # draws 5000 random samples from "population"
#                 function(x){
#                   sample <- sample(pop,
#                                    size = 60,
#                                    replace = F)
#                   return(mean(sample))
#                 })
# 
# sims <- data.frame(means = means, # binds into data.frame
#                    draws = seq(1,length(means),1))
# rm(means) # removes clutter
# 
# 
# # Animated plot
# p <- ggplot(sims, aes(means)) # base for plot
# 
# for(i in seq(0,length(sims$draws),length.out=100)){ # loop for individual frames 
#   p <- p + geom_dotplot(data=sims[1:i,], dotsize=0.04) +
#     scale_x_continuous(limits = c(5,6.5),
#                        breaks = seq(5,6.5,.25)) +
#     xlab("Sample means") +
#     ylab("Count") +
#     theme_bw()
# }
# 
# # Animation
# animate(p +
#           transition_layers(keep_layers = FALSE),
#         renderer = gifski_renderer(loop = F),
#         nframes=120, fps=5)


# not necessary; cleaning after earlier call
# sapply(paste0("gganim_plot",stringr::str_pad(seq(1,100,1),4,pad = "0"),".png"),
#        unlink) # clean clutter


