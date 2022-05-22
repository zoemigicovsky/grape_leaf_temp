#Load R Packages
library(tidyverse)
library(lubridate)
library(extrafont)
library(car)
library(broom)
library(ggthemes)
library(viridis)
library(rmcorr)
library(weathermetrics)
loadfonts()

#Load in data

data <- read_csv("data/temp_resid.csv")

#Use rmcorr to check correlation of ln_area and temp residuals

library(rmcorr)
rmcorr_ln_area <- rmcorr(participant=as.factor(day), measure1=.resid, measure2=ln_area, dataset=data)

rmcorr_ln_area$r # -0.1784123
rmcorr_ln_area$p # 1.516374e-12
rmcorr_ln_area$CI #-0.2262391 -0.1297273

#Set factor levels for dates so they plot in order

data <- data %>% mutate(day=as.factor(day))
data <- data %>% mutate(day=fct_relevel(day, "July 19 2018", "August 10 2018","July 24 2019","August 1 2019"))

#Colour lines by temperature on day of sampling: July 19 2018 = 27.7, August 10 2018 = 28,July 24 2019 = 34, and August 1 2019 = 26.2
data <- data %>% mutate(sampling_temp=ifelse(day=="July 19 2018",27.7, ifelse(day=="August 10 2018", 28, ifelse(day=="July 24 2019", 34,26.2)) ))

pdf("figures/Figure4.pdf", family="Arial",width=8, height=6)
bg_data <- data %>% select(ln_area, .resid)
ggplot(data, aes(x=ln_area, y=.resid, colour=.fitted)) +
  geom_point(data = bg_data, colour = "grey",alpha=0.6, stroke=0, size=2) +
  facet_wrap(~as.factor(day))+
  geom_point(alpha=0.6, stroke=0, size=2)+
  geom_smooth(method="lm", se=F, size=1.5,colour="black")+
  scale_colour_viridis(option="mako", direction=1, begin=0.2, end=0.95)+
  theme_bw()+
  theme(axis.text=element_text(colour="black", size=12),
        axis.title=element_text(colour="black", face="bold", size=14),
        title=element_text(colour="black", face="bold", size=10))+
  labs(x = "ln(area)", y="Residuals from temp ~ time")
dev.off()


