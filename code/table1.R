#Load R Packages
library(tidyverse)
library(lubridate)
library(extrafont)
library(car)
library(broom)
library(ggthemes)
library(viridis)
loadfonts()

#Load in data

data <- read_csv("data/temp_resid.csv")


#Perform type 2 anova 
#the first 20 PCs capture 99.7% of the variance in shape so I will just include those 
temp_aov <-Anova(lm(.resid~pop+day+PC1+PC2+PC3+PC4+PC5+PC6+PC7+PC8+PC9+PC10+PC11+PC12+PC13+PC14+PC15+PC16+PC17+PC18+PC19+PC20+ln_area+veins_to_blade+proximal_lobing+distal_lobing, data=data), type=2)

temp_aov <- tidy(temp_aov)
temp_var <- temp_aov %>% mutate(total_sum=sum(temp_aov$sumsq)) %>% filter(term !="Residuals") %>% dplyr::select(term, total_sum,sumsq, p.value) %>% mutate(var=(sumsq/total_sum)*100) %>% mutate(phenotype="temp") %>% dplyr::select(phenotype, term, var, p.value) 

temp_var %>% filter(p.value <0.05)

