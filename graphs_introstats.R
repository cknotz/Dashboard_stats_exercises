
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

# Temperature distribution
temp <- data.frame(temp = rnorm(1000,
                                mean = 36.75,
                                sd = .25))

temp %>% 
  ggplot(aes(x=temp)) +
    geom_histogram(colour = "black", alpha = .8) +
    geom_vline(xintercept = mean(temp$temp),
               linetype = "dashed", size = 1.25) +
    labs(x = "Body temperature (Celsius)",
         y = "Number of observations",
         caption = paste0("The vertical dashed line indicates the average measured temperature: ",
                          round(mean(temp$temp),digits = 1),
                          "\n N = 1000")) +
    theme_bw() +
    theme(axis.text = element_text(size = 14),
          axis.title = element_text(size=14))
    ggsave("tempdist.pdf")







