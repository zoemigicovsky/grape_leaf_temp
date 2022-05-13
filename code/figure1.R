#Load R Packages
library(tidyverse)
library(lubridate)
library(extrafont)
loadfonts()

#Load in data

data <- read_csv("data/shape_and_temp.csv")

#Set factor levels for dates so they plot in order

data <- data %>% mutate(day=as.factor(day))
data <- data %>% mutate(day=fct_relevel(day, "July 19 2018", "August 10 2018","July 24 2019","August 1 2019"))
  
pdf("figures/Figure1.pdf", family="Arial",width=8, height=5)
bg_dat <- data %>% select(time, temp)
ggplot(data, aes(x=time, y=temp)) +
  geom_point(data = bg_dat, colour = "grey",alpha=0.6, stroke=0, size=2) +
  facet_wrap(~as.factor(day))+
  geom_point(alpha=0.6, stroke=0, size=2)+
  geom_smooth(method="lm", se=F, size=1, colour="red")+
  theme_bw()+
  theme(axis.text=element_text(colour="black", size=12),
        axis.title=element_text(colour="black", face="bold", size=14),
        title=element_text(colour="black", face="bold", size=10))+
  labs(colour = "Date")
dev.off()

#Run model to get residuals for temperature

#this has to be done for each date based on the temp and the time 
temp_1 <-  data %>% filter(day=="July 19 2018")
temp_2 <-  data %>% filter(day=="August 10 2018")
temp_3 <-  data %>% filter(day=="July 24 2019")
temp_4 <-  data %>% filter(day=="August 1 2019")

#Number of measurements ranged from 416 to 442
temp_1_resid <- broom::augment(lm(temp ~ time, data = temp_1), data = temp_1)
temp_2_resid <- broom::augment(lm(temp ~ time, data = temp_2), data = temp_2)
temp_3_resid <- broom::augment(lm(temp ~ time, data = temp_3), data = temp_3)
temp_4_resid <- broom::augment(lm(temp ~ time, data = temp_4), data = temp_4)

#merge back together and save
temp_resid <- temp_1_resid %>% bind_rows(temp_2_resid,temp_3_resid,temp_4_resid)

write.table(temp_resid, "data/temp_resid.csv", sep=",", row.names = F, col.names = T)
