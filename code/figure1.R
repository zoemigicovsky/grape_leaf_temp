library(tidyverse)
library(ggforce)
library(ggpubr)
library(ggthemes)
#Make an eigenleaves figure for paper

#Load in data so I have list of samples etc to include
data <- read_csv("data/landmarks_adjusted_with_pcs.csv")

#Remove raw landmarks they are no longer needed
data <- data %>% select(-(x1:y21))

#Take average value per accession per year 
data <- data %>% mutate(accession_year=paste(accession, year, sep="_")) %>% dplyr::select(-accession, -year)
data_avg <- data %>% group_by(accession_year) %>% summarise(across(everything(), list(mean)))

#Plot quartiles for leaves for PC1 PC2 PC3 and PC4

#PC1
#Divide into 4 groups, since I have 776/4 = 194 per group
#define number of data frames to split into
n <- 4

#split data frame into n equal-sized data frames
data_avg_pc1 <- data_avg %>% dplyr::select(accession_year:y21_adjusted_1, PC1_1)

data_avg_pc1 <- data_avg_pc1 %>% arrange(PC1_1)
pc_1_dat <- split(data_avg_pc1, factor(sort(rank(row.names(data_avg_pc1))%%n)))

data1 <- as.data.frame(pc_1_dat[1])
#Remove X0 from column names 
colnames(data1) <-  sub("X0.", "", colnames(data1))

data2 <- as.data.frame(pc_1_dat[2])
colnames(data2) <-  sub("X1.", "", colnames(data2))

data3 <- as.data.frame(pc_1_dat[3])
colnames(data3) <-  sub("X2.", "", colnames(data3))

data4 <- as.data.frame(pc_1_dat[4])
colnames(data4) <-  sub("X3.", "", colnames(data4))

#Plot average leaf for each of those datasets 
data1_avg <- data1 %>% dplyr::select(veins_to_blade_1:PC1_1) %>% summarise(across(everything(), list(mean)))
data2_avg <- data2 %>% dplyr::select(veins_to_blade_1:PC1_1) %>% summarise(across(everything(), list(mean)))
data3_avg <- data3 %>% dplyr::select(veins_to_blade_1:PC1_1) %>% summarise(across(everything(), list(mean)))
data4_avg <- data4 %>% dplyr::select(veins_to_blade_1:PC1_1) %>% summarise(across(everything(), list(mean)))

colnames(data1_avg) <-  sub("_1", "", colnames(data1_avg))
colnames(data2_avg) <-  sub("_1", "", colnames(data2_avg))
colnames(data3_avg) <-  sub("_1", "", colnames(data3_avg))
colnames(data4_avg) <-  sub("_1", "", colnames(data4_avg))

#How to plot shape with 21 landmarks?
size <- 1
alpha <- 0.6

#Let's visualize these average leaf shapes against each other with different colors for group along PC1

data4_col <- "#440154"
data3_col <- "#31688e"
data2_col <- "#35b779"
data1_col <- "#fde725"

#Code to get 21 landmarks plotted

p1 <- ggplot(data1_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1))+ 
  geom_segment(colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha)+
  geom_segment(data=data2_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha)+geom_segment(data=data3_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha)+
  geom_segment(data=data4_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha)+
  theme_few() + coord_fixed() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank())

#PC2

#Divide into 4 groups, since I have 776/4 = 194 per group
data_avg_pc2 <- data_avg %>% dplyr::select(accession_year:y21_adjusted_1, PC2_1)
#define number of data frames to split into
n <- 4

#split data frame into n equal-sized data frames
data_avg_pc2 <- data_avg_pc2 %>% arrange(PC2_1)
pc_2_dat <- split(data_avg_pc2, factor(sort(rank(row.names(data_avg_pc2))%%n)))

data1 <- as.data.frame(pc_2_dat[1])
#Remove X0 from column names 
colnames(data1) <-  sub("X0.", "", colnames(data1))

data2 <- as.data.frame(pc_2_dat[2])
colnames(data2) <-  sub("X1.", "", colnames(data2))

data3 <- as.data.frame(pc_2_dat[3])
colnames(data3) <-  sub("X2.", "", colnames(data3))

data4 <- as.data.frame(pc_2_dat[4])
colnames(data4) <-  sub("X3.", "", colnames(data4))

#Plot average leaf for each of those datasets 
data1_avg <- data1 %>% dplyr::select(veins_to_blade_1:PC2_1) %>% summarise(across(everything(), list(mean)))
data2_avg <- data2 %>% dplyr::select(veins_to_blade_1:PC2_1) %>% summarise(across(everything(), list(mean)))
data3_avg <- data3 %>% dplyr::select(veins_to_blade_1:PC2_1) %>% summarise(across(everything(), list(mean)))
data4_avg <- data4 %>% dplyr::select(veins_to_blade_1:PC2_1) %>% summarise(across(everything(), list(mean)))

colnames(data1_avg) <-  sub("_1", "", colnames(data1_avg))
colnames(data2_avg) <-  sub("_1", "", colnames(data2_avg))
colnames(data3_avg) <-  sub("_1", "", colnames(data3_avg))
colnames(data4_avg) <-  sub("_1", "", colnames(data4_avg))

#How to plot shape with 21 landmarks?
size <- 1
alpha <- 0.6

#Let's visualize these average leaf shapes against each other with different colors for group along PC2

data4_col <- "#440154"
data3_col <- "#31688e"
data2_col <- "#35b779"
data1_col <- "#fde725"

#Code to get 21 landmarks plotted

p2 <- ggplot(data1_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1))+ 
  geom_segment(colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha)+
  geom_segment(data=data2_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha)+geom_segment(data=data3_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha)+
  geom_segment(data=data4_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha)+
  theme_few() + coord_fixed() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank())

#PC3

#Divide into 4 groups, since I have 776/4 = 194 per group
data_avg_pc3 <- data_avg %>% dplyr::select(accession_year:y21_adjusted_1, PC3_1)
#define number of data frames to split into
n <- 4

#split data frame into n equal-sized data frames
data_avg_pc3 <- data_avg_pc3 %>% arrange(PC3_1)
pc_3_dat <- split(data_avg_pc3, factor(sort(rank(row.names(data_avg_pc3))%%n)))

data1 <- as.data.frame(pc_3_dat[1])
#Remove X0 from column names 
colnames(data1) <-  sub("X0.", "", colnames(data1))

data2 <- as.data.frame(pc_3_dat[2])
colnames(data2) <-  sub("X1.", "", colnames(data2))

data3 <- as.data.frame(pc_3_dat[3])
colnames(data3) <-  sub("X2.", "", colnames(data3))

data4 <- as.data.frame(pc_3_dat[4])
colnames(data4) <-  sub("X3.", "", colnames(data4))

#Plot average leaf for each of those datasets 
data1_avg <- data1 %>% dplyr::select(veins_to_blade_1:PC3_1) %>% summarise(across(everything(), list(mean)))
data2_avg <- data2 %>% dplyr::select(veins_to_blade_1:PC3_1) %>% summarise(across(everything(), list(mean)))
data3_avg <- data3 %>% dplyr::select(veins_to_blade_1:PC3_1) %>% summarise(across(everything(), list(mean)))
data4_avg <- data4 %>% dplyr::select(veins_to_blade_1:PC3_1) %>% summarise(across(everything(), list(mean)))

colnames(data1_avg) <-  sub("_1", "", colnames(data1_avg))
colnames(data2_avg) <-  sub("_1", "", colnames(data2_avg))
colnames(data3_avg) <-  sub("_1", "", colnames(data3_avg))
colnames(data4_avg) <-  sub("_1", "", colnames(data4_avg))

#How to plot shape with 21 landmarks?
size <- 1
alpha <- 0.6

#Let's visualize these average leaf shapes against each other with different colors for group along PC2

data4_col <- "#440154"
data3_col <- "#31688e"
data2_col <- "#35b779"
data1_col <- "#fde725"

#Code to get 21 landmarks plotted

p3 <- ggplot(data1_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1))+ 
  geom_segment(colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha)+
  geom_segment(data=data2_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha)+geom_segment(data=data3_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha)+
  geom_segment(data=data4_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha)+
  theme_few() + coord_fixed() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank())

#PC4

#Divide into 4 groups, since I have 776/4 = 194 per group
data_avg_pc4 <- data_avg %>% dplyr::select(accession_year:y21_adjusted_1, PC4_1)
#define number of data frames to split into
n <- 4

#split data frame into n equal-sized data frames
data_avg_pc4 <- data_avg_pc4 %>% arrange(PC4_1)
pc_4_dat <- split(data_avg_pc4, factor(sort(rank(row.names(data_avg_pc4))%%n)))

data1 <- as.data.frame(pc_4_dat[1])
#Remove X0 from column names 
colnames(data1) <-  sub("X0.", "", colnames(data1))

data2 <- as.data.frame(pc_4_dat[2])
colnames(data2) <-  sub("X1.", "", colnames(data2))

data3 <- as.data.frame(pc_4_dat[3])
colnames(data3) <-  sub("X2.", "", colnames(data3))

data4 <- as.data.frame(pc_4_dat[4])
colnames(data4) <-  sub("X3.", "", colnames(data4))

#Plot average leaf for each of those datasets 
data1_avg <- data1 %>% dplyr::select(veins_to_blade_1:PC4_1) %>% summarise(across(everything(), list(mean)))
data2_avg <- data2 %>% dplyr::select(veins_to_blade_1:PC4_1) %>% summarise(across(everything(), list(mean)))
data3_avg <- data3 %>% dplyr::select(veins_to_blade_1:PC4_1) %>% summarise(across(everything(), list(mean)))
data4_avg <- data4 %>% dplyr::select(veins_to_blade_1:PC4_1) %>% summarise(across(everything(), list(mean)))

colnames(data1_avg) <-  sub("_1", "", colnames(data1_avg))
colnames(data2_avg) <-  sub("_1", "", colnames(data2_avg))
colnames(data3_avg) <-  sub("_1", "", colnames(data3_avg))
colnames(data4_avg) <-  sub("_1", "", colnames(data4_avg))

#How to plot shape with 21 landmarks?
size <- 1
alpha <- 0.6

#Let's visualize these average leaf shapes against each other with different colors for group along PC2

data4_col <- "#440154"
data3_col <- "#31688e"
data2_col <- "#35b779"
data1_col <- "#fde725"

#Code to get 21 landmarks plotted

p4 <- ggplot(data1_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1))+ 
  geom_segment(colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data1_col, size=size, alpha=alpha) +
  geom_segment(data=data1_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data1_col, size=size, alpha=alpha)+
  geom_segment(data=data2_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data2_col, size=size, alpha=alpha) +
  geom_segment(data=data2_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data2_col, size=size, alpha=alpha)+geom_segment(data=data3_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data3_col, size=size, alpha=alpha) +
  geom_segment(data=data3_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data3_col, size=size, alpha=alpha)+
  geom_segment(data=data4_avg, aes(x=x4_adjusted_1, y=y4_adjusted_1, xend=x13_adjusted_1, yend=y13_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x13_adjusted_1, y=y13_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x21_adjusted_1, y=y21_adjusted_1, xend=x11_adjusted_1, yend=y11_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x11_adjusted_1, y=y11_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x12_adjusted_1, yend=y12_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x12_adjusted_1, y=y12_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x10_adjusted_1, yend=y10_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x10_adjusted_1, y=y10_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x8_adjusted_1, yend=y8_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x8_adjusted_1, y=y8_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x9_adjusted_1, yend=y9_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x9_adjusted_1, y=y9_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x7_adjusted_1, yend=y7_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x7_adjusted_1, y=y7_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x5_adjusted_1, yend=y5_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x5_adjusted_1, y=y5_adjusted_1, xend=x14_adjusted_1, yend=y14_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x6_adjusted_1, yend=y6_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x6_adjusted_1, y=y6_adjusted_1, xend=x1_adjusted_1, yend=y1_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x1_adjusted_1, y=y1_adjusted_1, xend=x2_adjusted_1, yend=y2_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x2_adjusted_1, y=y2_adjusted_1, xend=x3_adjusted_1, yend=y3_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x3_adjusted_1, y=y3_adjusted_1, xend=x4_adjusted_1, yend=y4_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x14_adjusted_1, y=y14_adjusted_1, xend=x15_adjusted_1, yend=y15_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x15_adjusted_1, y=y15_adjusted_1, xend=x16_adjusted_1, yend=y16_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x16_adjusted_1, y=y16_adjusted_1, xend=x17_adjusted_1, yend=y17_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x17_adjusted_1, y=y17_adjusted_1, xend=x18_adjusted_1, yend=y18_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x18_adjusted_1, y=y18_adjusted_1, xend=x19_adjusted_1, yend=y19_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x19_adjusted_1, y=y19_adjusted_1, xend=x20_adjusted_1, yend=y20_adjusted_1), colour=data4_col, size=size, alpha=alpha) +
  geom_segment(data=data4_avg, aes(x=x20_adjusted_1, y=y20_adjusted_1, xend=x21_adjusted_1, yend=y21_adjusted_1), colour=data4_col, size=size, alpha=alpha)+
  theme_few() + coord_fixed() + theme(axis.line=element_blank(), axis.text.x=element_blank(), axis.text.y=element_blank(), axis.ticks=element_blank(), axis.title.x=element_blank(), axis.title.y=element_blank(), panel.grid.major=element_blank(), panel.grid.minor=element_blank(), panel.border=element_blank())

pdf("figures/Figure1.pdf", width=8, height=5)
ggarrange(p1,p2,p3,p4, ncol=4,labels="AUTO")
dev.off()
