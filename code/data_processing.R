#Calculate residuals for temperature model and merge data with morphometric data from previous manuscript, "Increases in vein length compensate for leaf area lost to lobing in grapevine"

#Load R Packages

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

#Link with morpometric data 
#Take average value per vine within a year 
#Load in data
shape_dat1 <- read_csv("data/goali_all_data.csv")
shape_dat2 <- read_csv("data/goali_pcs_with_landmarks.csv")
#Merge together so I have the PCs with the landmarks 

#Get rid of columns of data not needed for this study such as raw landmarks. From this table, keep area, vein to blade ratio, and lobing measurements 
shape_dat1 <- shape_dat1 %>% select(accession, year,all_area, veins_to_blade,distal_lobing,proximal_lobing)
#Create  grouping column for each accession for each year
shape_dat1 <- shape_dat1 %>% mutate(accession_year=paste(accession, year, sep="_")) %>% select(-accession, -year)

table(table(shape_dat1$accession_year))
#1   2   3 
#12  17 862

shape_dat1_mean <- shape_dat1 %>%
  group_by(accession_year) %>% 
  summarise(across(all_area:proximal_lobing, ~ mean(.x, na.rm = TRUE)))

#Keep PCS only from second table
shape_dat2 <- shape_dat2 %>% select(accession:PC42)
#Add grouping column
shape_dat2 <- shape_dat2 %>% mutate(accession_year=paste(accession, year, sep="_")) %>% select(-accession, -year)

#Get mean value
shape_dat2_mean <- shape_dat2 %>%
  group_by(accession_year) %>% 
  summarise(across(PC1:PC42, ~ mean(.x, na.rm = TRUE)))

#Join all shape data together

all_shape_dat <- shape_dat1_mean %>% full_join(shape_dat2_mean)

#Only keep vines for both tables that occur in both 

#First need to split accession and year again
all_shape_dat <- all_shape_dat %>% separate(accession_year,c("accession", "year"))
all_shape_dat$accession <- as.character(all_shape_dat$accession)
all_shape_dat$year <- as.character(all_shape_dat$year)

#Relabel columns for temp data
data <- data %>% rename(accession=Seedling, date=Date, time=Time, temp=Temp, pop=Pop) %>% select(-Block_3_798, -Row, -Vine)

#Need a year column for the temperature data
data <- data %>% mutate(year=ifelse(str_detect(date, "2018"), "2018", "2019")) 
data <- data %>% mutate(accession=as.character(accession))

all_info <- all_shape_dat %>% inner_join(data, by=c("accession","year"))
#This removes any samples that have shape data but not temperature data, or temp but not shape. 

dim(all_info)
#[1] 1714  55

#Convert temp from F to C

all_info <- all_info %>% mutate(temp=fahrenheit.to.celsius(temp, round = 2))

#Convert area to natural log of area 
all_info$ln_area <- log(all_info$all_area)

#Save table
write.table(all_info, "data/shape_and_temp.csv", sep=",", row.names = F, col.names = T)
