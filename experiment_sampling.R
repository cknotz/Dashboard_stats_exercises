
# Sampling, classroom experiment
################################

library(tidyverse)

setwd("/Users/carloknotz/Documents/Work/Stavanger/Teaching/Statistics")

# reading & cleaning data
studs <- readxl::read_excel("students_ht-21.xlsx") %>% 
  filter(!is.na(role)) %>% 
  select(mail) %>% 
  mutate(idno = row_number())

# Sampling
set.seed(17)

sample <- sample(studs$mail,
                 size=80)

# Print out for copy/paste
cat(paste0(sample,
       collapse = "; "))

