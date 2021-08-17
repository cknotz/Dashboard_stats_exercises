
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

