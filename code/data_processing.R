#Calculate residuals for temperature model and merge data with morphometric data from previous manuscript, "Increases in vein length compensate for leaf area lost to lobing in grapevine"

#Load R Packages
library(shapes)
library(tidyverse)
library(lubridate)
library(weathermetrics)
#Load in temperature data

data <- read_csv("data/goali_leaf_temp_only_time.csv")

#Add in population column 
data <- data %>% mutate(Pop=ifelse(Seedling >=16118001 & Seedling <=16118125 , "pop1", ifelse(Seedling >=16119001 & Seedling <=16119100 , "pop2", ifelse(Seedling >=16117001 & Seedling <=16117150 , "pop3", ifelse(Seedling >=16129001 & Seedling <=16129075 , "pop4", ifelse(Seedling >=16113001 & Seedling <=16113050 , "pop5", Seedling))))))

#Remove all measurements where there's no temperature
data <- data %>% filter(!is.na(Temp))
#Now down to 1808 measurements 

#Reformat time
data <- data %>% mutate(hour=hour(Time), minute=minute(Time))
data <- data %>% mutate(day=Date)

#Look at temperature distribution
head(arrange(data, desc(Temp)))

data %>% filter(Temp > 105) %>% nrow()
#There are only 3 measurements greater than 105 
#3/1808 = 0.166% 
#Less than 0.2% of the measurements so I will set those to zero 

data <- data %>% mutate(Temp = replace(Temp, Temp>105, NA))

head(arrange(data, Temp))

#only one value less than 64 so I'm going to remove that as well
data <- data %>% mutate(Temp = replace(Temp, Temp<64, NA))%>% filter(!is.na(Temp))

#Down to 1804 values

#How many measurements per date?
table(data$day)
#August 1 2019 August 10 2018   July 19 2018   July 24 2019 
#449            454            448            453 

#Link with morpometric data and only keep data which there are samples for both
#Load in data
shape_dat <- read_csv("data/goali_all_data.csv")

#Get rid of adjusted landmarks because I will redo that with the filtered data. 
shape_dat <- shape_dat %>% select(accession:y21,all_area, veins_to_blade,distal_lobing,proximal_lobing)

#Only keep vines that overlap in both tables for all dates

#Relabel columns for temp data
data <- data %>% rename(accession=Seedling, date=Date, time=Time, temp=Temp, pop=Pop) %>% select(-Block_3_798, -Row, -Vine)

shape_dat <- shape_dat %>% mutate(accession=as.character(accession)) %>% mutate(year=as.character(year))

#Get a list of overlapping accessions 
#table(table(data$accession))
#1   2   3   4 
#10  16  30 418 

#418 accessions were measured all four times, keep those

data <-  data %>% group_by(accession) %>% filter(n()==4)

#Only keep leaves that were measured in both years
shape_dat_2018 <- shape_dat %>% filter(year==2018)
shape_dat_2019 <- shape_dat %>% filter(year==2019)

shape_dat <- shape_dat[shape_dat$accession %in% shape_dat_2018$accession,]
shape_dat <- shape_dat[shape_dat$accession %in% shape_dat_2019$accession,]
shape_dat <- shape_dat[shape_dat$accession %in% data$accession,]

data <- data[data$accession %in% shape_dat$accession,]

table(table(data$accession))
#4 
#388 

table(table(shape_dat$accession))
#4   5   6 
#4   9 375 

#Filtered down to 388 accessions that occur at ALL four time points for temperature and both years for shape 

#Convert temp from F to C

data <- data %>% mutate(temp=fahrenheit.to.celsius(temp, round = 2))

#Convert area to natural log of area 
shape_dat$ln_area <- log(shape_dat$all_area)

#Save temp data
write.table(data, "data/temp_dat.csv", sep=",", row.names = F, col.names = T)

#Save shape data
write.table(shape_dat, "data/shape_dat.csv", sep=",", row.names = F, col.names = T)

#Run landmarks through GPA to adjust and generate PCs and eigenleaves 

####Generalized Procrustes Analysis####

#Need a version of the landmark file with just the landmarks
shape_dat <- read_csv("data/shape_dat.csv")

shape_dat_info <- shape_dat %>% select(accession, year,veins_to_blade:ln_area)
shape_dat_landmarks <- shape_dat %>% select(x1:y21)

n_landmarks <- 21
n_leaves <- dim(shape_dat_landmarks)[1]

write.table(as.matrix(shape_dat_landmarks), col.names=F, row.names=F, file='data/shape_dat_landmarks.txt')

morpho_reformat_gpa <- read.in('data/shape_dat_landmarks.txt', n_landmarks, 2)

dim(morpho_reformat_gpa)

# fit the GPA
GPA <- procGPA(morpho_reformat_gpa, reflect=TRUE)

stdscores <- as.matrix(GPA$stdscores)
pca_results <- cbind(shape_dat_info, stdscores)
write.csv(pca_results, file="data/PC_scores_info.csv", quote=FALSE, row.names = F)
write.csv(as.matrix(GPA$stdscores), file="data/PC_scores.csv", quote=FALSE)
write.csv(as.matrix(GPA$percent), file="data/PC_percents.csv", quote=FALSE)
write.csv(as.matrix(GPA$rotated), file="data/GPA_rotated.csv", quote=FALSE)

#restructuring of matrix

morpho_GPA_rotated_flat <- matrix(nrow=n_leaves, ncol=(n_landmarks*2))

morpho_GPA_rotated <- as.matrix(GPA$rotated)
for(j in c(1:n_leaves)) {
  # extract all coordinates from the original table as blocks of 42 (n_landmarks*2) rows,
  # each representing the x coordinates of a leaf, one by one, as calculated from j.
  sub.data <- as.matrix(morpho_GPA_rotated[ (1+42*(j-1)):((1+42*(j-1))+41), 1])
  sub.data.x <- as.matrix(sub.data[1:n_landmarks,])
  sub.data.y <- as.matrix(sub.data[(n_landmarks+1):(n_landmarks*2),])
  
  # dissect out each x and y coordinate of the landmark data and put it into every other 
  # column of a single row (for a single leaf) in the overall table
  
  for (i in 1:n_landmarks){
    morpho_GPA_rotated_flat[j,(i*2-1)] <- sub.data.x[i, 1]
    morpho_GPA_rotated_flat[j, (i*2)] <- sub.data.y[i, 1]
  }
}

colnames(morpho_GPA_rotated_flat) <- c("x1_adjusted", "y1_adjusted", "x2_adjusted", "y2_adjusted", "x3_adjusted", "y3_adjusted", "x4_adjusted", "y4_adjusted", "x5_adjusted", "y5_adjusted", "x6_adjusted", "y6_adjusted", "x7_adjusted", "y7_adjusted", "x8_adjusted", "y8_adjusted", "x9_adjusted", "y9_adjusted", "x10_adjusted", "y10_adjusted", "x11_adjusted", "y11_adjusted", "x12_adjusted", "y12_adjusted", "x13_adjusted", "y13_adjusted", "x14_adjusted", "y14_adjusted", "x15_adjusted", "y15_adjusted", "x16_adjusted", "y16_adjusted", "x17_adjusted", "y17_adjusted","x18_adjusted", "y18_adjusted","x19_adjusted", "y19_adjusted","x20_adjusted", "y20_adjusted","x21_adjusted", "y21_adjusted")

head(morpho_GPA_rotated_flat)

shapepca(GPA, pcno=c(1:3), joinline=c(1,2,3,4,13,21,20,19,18,17,16,15,14,6,1,6,14,5,15,7,2,9,17,8,18,10,3,12,20,11,21,13,4))

shapepca(GPA, pcno=c(1:3), joinline=c(1,2,3,4,13,21,20,19,18,17,16,15,14,6,1,6,14,5,15,7,2,9,17,8,18,10,3,12,20,11,21,13,4), type="s")

shapepca(GPA,pcno=c(1), joinline=c(1,2,3,4,13,21,20,19,18,17,16,15,14,6,1,6,14,5,15,7,2,9,17,8,18,10,3,12,20,11,21,13,4))

#Add info for Procrustes adjusted landmarks and PCs

landmarks_adjusted <- cbind(shape_dat_info,shape_dat_landmarks,morpho_GPA_rotated_flat,stdscores)
write.table(landmarks_adjusted, "data/landmarks_adjusted_with_pcs.csv", sep=",", row.names = F, col.names = T)

#Merge shape and temperature data

temp_dat <- read_csv("data/temp_dat.csv")
shape_dat <- read_csv("data/landmarks_adjusted_with_pcs.csv")

#Need a year column for the temperature data
temp_dat <- temp_dat %>% mutate(year=ifelse(str_detect(date, "2018"), "2018", "2019")) 
temp_dat <- temp_dat %>% mutate(accession=as.character(accession))

shape_dat <- shape_dat %>% mutate(accession=as.character(accession)) %>% mutate(year=as.character(year))

#Remove raw landmarks and average shape per vine, per year 
shape_dat <- shape_dat %>% select(-(x1:y21))
shape_dat <- shape_dat %>% mutate(accession_year=paste(accession, year, sep="_")) %>% select(-accession, -year)
shape_dat <- shape_dat %>%
  group_by(accession_year) %>% 
  summarise(across(veins_to_blade:PC42, ~ mean(.x, na.rm = TRUE)))

temp_dat <- temp_dat %>% mutate(accession_year=paste(accession, year, sep="_")) %>% select(-accession, -year)

all_info <- temp_dat %>% inner_join(shape_dat, by=c("accession_year"))
#This removes any samples that have shape data but not temperature data, or temp but not shape. 

dim(all_info)
#[1] 1552   96

#776 measurements per year 
all_info <- all_info %>% separate(accession_year,c("accession", "year"))
all_info <- all_info %>% select(accession, year, date:day, veins_to_blade:PC42)

write.table(all_info, "data/shape_and_temp.csv", sep=",", row.names = F, col.names = T)

#Load in weather data

weather_data <- read_csv("data/weather_dat.csv")

#Convert temp from F to C

weather_data <- weather_data %>% mutate(temp=fahrenheit.to.celsius(temp_f, round = 2))

weather_data %>% group_by(date) %>% summarize(Sum = sum(rain_in, na.rm=TRUE))
#No rain 

#Mean temperature
weather_data %>% group_by(date) %>% summarize(Mean = mean(temp, na.rm=TRUE))
# 1 August 1 2019   24.6
# 2 August 10 2018  26.9
# 3 July 19 2018    27.1
# 4 July 24 2019    28.1

#Max temperature

weather_data %>% group_by(date) %>% summarize(Max = max(temp, na.rm=TRUE))
# 1 August 1 2019   34.5
# 2 August 10 2018  38.1
# 3 July 19 2018    37.3
# 4 July 24 2019    39.1

#Min temperature

weather_data %>% group_by(date) %>% summarize(Min = min(temp, na.rm=TRUE))
# 1 August 1 2019   14.1
# 2 August 10 2018  15.3
# 3 July 19 2018    16.9
# 4 July 24 2019    18  

#Check weather for windows of sampling
#July 19 2018 sampled from 08:48  to 10:48

weather_data %>% filter(date=="July 19 2018") %>% filter(time=="900"|time=="1000"|time=="1100") %>% summarize(Mean = mean(temp, na.rm=TRUE))

#27.7

#August 10 2018 sampled from 09:04 to 10:49

weather_data %>% filter(date=="August 10 2018") %>% filter(time=="900"|time=="1000"|time=="1100") %>% summarize(Mean = mean(temp, na.rm=TRUE))
#28

#July 24 2019 sampled from 11:35 to 13:40

weather_data %>% filter(date=="July 24 2019") %>% filter(time=="1100"|time=="1200"|time=="1300"|time=="1400") %>% summarize(Mean = mean(temp, na.rm=TRUE))
#34.0

#August 1 2019 sampled from 08:54 to 10:41

weather_data %>% filter(date=="August 1 2019") %>% filter(time=="900"|time=="1000"|time=="1100") %>% summarize(Mean = mean(temp, na.rm=TRUE))

#26.2

