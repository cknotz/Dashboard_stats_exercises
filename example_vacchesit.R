

library(tidyverse)
library(labelled)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics/Slides")

# Reading in data
#################
data <- haven::read_dta(file="/Users/carloknotz/IDHEAP_NCCR/Switchdrive_IDHEAP/NCCR Project/Survey_III_Healthcrisis/Data/R Analysis/covid_dashboarddashdata_w2.dta") %>% 
  select(res_id, covid_vacc)


data %>% 
  select(covid_vacc) %>% 
  group_by(covid_vacc) %>% 
  summarize(n=n()) %>% 
  mutate(perc = 100*(n / sum(n))) %>% 
  ggplot(aes(x=covid_vacc,y=perc)) +
    geom_col() +
  scale_x_continuous(breaks = seq(0,10,1)) +
  scale_y_continuous(limits = c(0,25),
                     breaks = seq(0,25,5)) +
  ylab("Percent") +
  xlab("I would get vaccinated against COVID-19") +
  labs(caption = paste0("Mean = ",round(mean(data$covid_vacc),digits = 2),
                        "\nStandard dev. = ",round(sd(data$covid_vacc),digits = 2),
                        "\nSample size = ",length(data$covid_vacc))) +
  theme_bw()
  ggsave("ch_vaccintent.pdf")


