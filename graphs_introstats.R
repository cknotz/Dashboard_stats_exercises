
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
  
