#load packages
library(readr)
library(tidyverse)
library(ggplot2)
library(scales)
library(lubridate)
library(cowplot)
library(reshape2)
library(ggcorrplot)
library(class)
library("gmodels")
library("ROSE")

#write_csv(accidents,"accidents.csv")

#set working director
setwd("~/Documents/Graduate_School/MIS_581_Capstone/Data")

#load dataset
accidents <- read_csv("US_Accidents.csv")

#explore dataset
head(accidents)

colnames(accidents)

accidents <- accidents %>%
  select(-Amenity, -Give_Way, -Railway, -Station, -Turning_Loop, -Traffic_Calming, -No_Exit, -Side, -Number, -End_Time, -Start_Lat, -Start_Lng, -End_Lat, -End_Lng, -Country, -Airport_Code,-Weather_Timestamp, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight, -Description, -Zipcode, -Timezone) #Eliminate columns that will not be used in this analysis

str(accidents) #structure of columns

#missing data analysis
colSums(is.na(accidents)) #check for missing values

 
#State analysis
table(accidents$State) #table of car accidents by state

ordered_state <- sort(table(accidents$State), decreasing=T)
ordered_state <- data.frame(ordered_state)
colnames(ordered_state) <- c("state","accidents")
colnames(ordered_state)
    
#state bar graph
lim_ordered_state <- data.frame(ordered_state[1:25,])

lim_ordered_state%>%
  ggplot(aes(x=state, y=accidents, fill = accidents)) +
  geom_bar(stat='identity') +
  labs(title = "Top 25 States With Most Car Accidents",
       x="",
       y="Number of Accidents")+
  theme(legend.position = "none")+
  scale_y_continuous(labels = comma)

#CA analysis
ca <- subset(accidents, State=="CA") #just want to look at CA 
ca_cities <- data.frame(table(ca$City)) 
colnames(ca_cities) <- c("CA_city","accidents") #data frame of accidents by CA cities
ca_cities <- ca_cities[order(-ca_cities$accidents),] #sort in descending order
ca_cities <- data.frame(ca_cities[1:25,]) #just want top 25 cities

ca_cities %>%
  ggplot(aes(x=reorder(CA_city,-accidents), y=accidents, fill=accidents))+
  geom_bar(stat="identity") +
  labs(title="Top 25 Cities in CA With the Most Accidents",
       x= "",
       y="Number of Accidents") +
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45), legend.position = "none")
  
#severity analysis
severity<- data.frame(table(accidents$State, accidents$Severity))
colnames(severity) <- c("state", "severity", "accidents")

severity <- severity[order(-severity$accidents),] #sort by accidents in descending order

severity %>%
  ggplot(aes(x=reorder(state,-accidents), y=accidents,fill=severity))+
  geom_bar(position="stack", stat="identity")+
  labs (x="",
        title = "Severity of Car Accident By State",
        y="Number of Accidents")+
  scale_y_continuous(labels=comma)


#Most severe accidents by state
state4 <- subset(severity, severity=="4") #only want states with severity 4
state4 <- data.frame(state4[1:25,]) #only want top 25 states

state4 %>%
  ggplot(aes(x=reorder(state,-accidents), y=accidents,fill=severity))+
  geom_bar(position="stack", stat="identity")+
  labs (x="",
        title = "Most Severe Car Accidents By State",
        y="Number of Accidents")+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x=element_text(angle=45))


#weather condition analysis
table(accidents$Weather_Condition)

weather <- data.frame(table(accidents$Weather_Condition, accidents$Severity))
colnames(weather) <- c("weather","severity","accidents")

weather <- weather[order(-weather$accidents),] #order by accidents in descending order

colSums(is.na(weather)) #no missing values but there is a "nan" category under weather condition we want to eliminate. 

weather <- weather[-c(14),] #drop nan rows
weather <- weather[-c(23),]

sub_weather <- subset(weather, accidents >10000) #make a subset of weather dataset with only weather conditions where accidents >10,000

sub_weather %>%
  ggplot(aes(x=reorder(weather,-accidents), y=accidents,fill=severity))+
  geom_bar(position="stack", stat="identity")+
  labs (x="",
        title = "Severity of Car Accident By Weather Condition",
        y="Number of Accidents")+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x=element_text(angle=45))


#lets look at severity of accident by level 4 severity
sev4 <- subset(weather, severity=="4") #and still in descending order
sev4 <- subset(sev4, accidents >1000) #only look at weather conditions with more than 1,000 accidents
sev4<- sev4[-c(9),] #drop nan category

sev4 %>%
  ggplot(aes(x=reorder(weather,-accidents),y=accidents, fill=severity))+
  geom_bar(stat="identity")+
  labs(title="Most Severe Car Accidents by Weather Condition",
       x="")+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x=element_text(angle=45))


#weekday analysis
as_date(accidents$Start_Time, tz = NULL)

accidents <- accidents %>%
  mutate(day = wday(Start_Time, label = TRUE)) #create column with just date of the week of accidents

weekday <- data.frame(table(accidents$day, accidents$Severity))
colnames(weekday) <-c("weekday","severity","accidents")

weekday <- weekday[order(-weekday$accidents),]

weekday %>%
  ggplot(aes(x=reorder(weekday,-accidents), y=accidents,fill=severity))+
  geom_bar(position="stack", stat="identity")+
  labs (x="",
        title = "Severity of Car Accident By Day of Week",
        y="Number of Accidents")+
  scale_y_continuous(labels=comma)+
  theme(axis.text.x=element_text(angle=45))


#Month analysis
accidents <- accidents %>%
  mutate(month = month(Start_Time,label=TRUE))

month <- data.frame(table(accidents$month, accidents$Severity))
colnames(month) <- c("month","severity","accidents")

month <- month[order(-month$accidents),]

month %>%
  ggplot(aes(x=month,y=accidents, fill=severity))+
  geom_bar(position = "stack", stat="identity")+
  labs(title = "Severity of Car Accidents by Month",
       x="",
       y="Number of Accidents")+
  scale_y_continuous(labels = comma) +
  theme(axis.text.x=element_text(angle = 45))


#Day/night analysis
daynight <- data.frame(table(accidents$Sunrise_Sunset, accidents$Severity))
colnames(daynight) <- c("daynight","severity","accidents")
daynight <- daynight[-2,]
daynight <-daynight[-4,]
daynight <- daynight[-6,]
daynight <- daynight[-8,]

daynight %>%
  ggplot(aes(x=daynight, y=accidents, fill=severity)) +
  geom_bar(stat="identity",position = "stack") +
  labs(title="Severity of Accident by Time of Day",
       x="",
       y="Number of Accidents")+
  scale_y_continuous(labels=comma) +
  theme(axis.text.x=element_text(angle=45))

#Street analysis
accidents <- accidents %>%
  mutate(road_type = case_when(str_detect(Street, "Fwy|Expy|Highway|US-|I-") ~ "Highway",
                               !str_detect(Street, "Fwy|Expy|Highway|US-|I-")~ "City"))

road_type <- data.frame(table(accidents$Severity,accidents$road_type))
colnames(road_type) <- c("severity","road_type","accidents")

road_type %>%
  ggplot(aes(x=road_type,y=accidents,fill=severity))+
  geom_bar(stat="identity", position="stack") +
  labs(title = "Severity of Car Accidents by Road Type",
       x="",
       y="Number of Accidents")+
  scale_y_continuous(labels=comma) 
  
accidents$Street <- NULL #drop original street variable


#Temperature analysis to show effect of imputing mean value for missing values
accidents %>%
  ggplot(aes(x=`Temperature(F)`))+
  geom_density()+
  scale_y_continuous(labels=comma)

summary(accidents$`Temperature(F)`) #67,224 missing values

accidents <- accidents %>%
  mutate(temp_avg = replace_na(`Temperature(F)`,60.99)) #replace missing values with mean
colSums(is.na(accidents)) #confirm no more missing values

temp_reg <- ggplot(accidents, aes(x=`Temperature(F)`))+
  geom_histogram()+
  scale_y_continuous(labels=comma)+
  labs(x="Temperature", 
       title="Histogram of Temperature",
       y="Accidents") #histogram of original temp variable

temp_mean <- ggplot(accidents, aes(x=temp_avg))+
  geom_histogram()+
  scale_y_continuous(labels=comma)+
  labs(x="Imputed Temperature", 
       title="Histogram of Temperature",
       y="Accidents") #histogram of temp with imputed values

plot_grid(temp_reg, temp_mean) #temp comparison with imputed value


#imput rest of missing values
colSums(is.na(accidents))

summary(accidents)

accidents <- accidents %>%
  mutate(`Wind_Chill(F)` = replace_na(`Wind_Chill(F)`,55)) %>%
  mutate(`Humidity(%)`= replace_na(`Humidity(%)`,65.38)) %>%
  mutate(`Pressure(in)` = replace_na(`Pressure(in)`,29.66))%>%
  mutate(`Visibility(mi)` = replace_na(`Visibility(mi)`,9.12)) %>%
  mutate(`Wind_Speed(mph)` = replace_na(`Wind_Speed(mph)`,7.82)) %>%
  mutate(`Temperature(F)` = replace_na(`Temperature(F)`,60.99)) #replace missing values with means

summary(accidents) #confirm no more missing values

accidents$`Precipitation(in)` <- NULL #drop preciptiation - too many missing values >1M


#remove outliers in distance variable
knn=accidents

boxplot(knn$`Distance(mi)`)

Q <- quantile(knn$`Distance(mi)`, probs = c(.25,.75), na.rm=FALSE)

iqr <- IQR(knn$`Distance(mi)`)
knn <- subset(knn, knn$`Distance(mi)` > (Q[1]-1.5*iqr) & knn$`Distance(mi)` < (Q[2]+1.5*iqr))

#remove outliers in Temp variable
boxplot(knn$`Temperature(F)`)

Q <- quantile(knn$`Temperature(F)`, probs = c(.25,.75), na.rm=FALSE)

iqr <- IQR(knn$`Temperature(F)`)
knn <- subset(knn, knn$`Temperature(F)` > (Q[1]-1.5*iqr) & knn$`Temperature(F)` < (Q[2]+1.5*iqr))

#remove visibility variable
knn <- knn %>%
  select(-`Visibility(mi)`)

#remove outliers in wind speed variable
boxplot(knn$`Wind_Speed(mph)`)

Q <- quantile(knn$`Wind_Speed(mph)`, probs = c(.25,.75), na.rm=FALSE)

iqr <- IQR(knn$`Wind_Speed(mph)`)
knn <- subset(knn, knn$`Wind_Speed(mph)` > (Q[1]-1.5*iqr) & knn$`Wind_Speed(mph)` < (Q[2]+1.5*iqr))

#remove outliers in wind chill variable
boxplot(knn$`Wind_Chill(F)`)

Q <- quantile(knn$`Wind_Chill(F)`, probs = c(.25,.75), na.rm=FALSE)

iqr <- IQR(knn$`Wind_Chill(F)`)
knn <- subset(knn, knn$`Wind_Chill(F)` > (Q[1]-1.5*iqr) & knn$`Wind_Chill(F)` < (Q[2]+1.5*iqr))


#correlation analysis
correlation = knn

#change character data types to factor then numeric
chr_col <- c("Severity", "ID", "City", "County","State","Wind_Direction","Sunrise_Sunset","Weather_Condition","road_type")

correlation[,chr_col] <- correlation %>%
  select("Severity", "ID", "City", "County","State","Wind_Direction","Sunrise_Sunset","Weather_Condition","road_type") %>%
  lapply(as.factor)

correlation[,chr_col] <- correlation %>%
  select("Severity", "ID", "City", "County","State","Wind_Direction","Sunrise_Sunset","Weather_Condition","road_type") %>%
  lapply(as.numeric)

#change logic to factor then numeric
log_col <- c("Bump", "Crossing", "Junction","Roundabout", "Stop","Traffic_Signal")

correlation[,log_col] <- correlation %>%
  select("Bump", "Crossing", "Junction","Roundabout", "Stop","Traffic_Signal") %>%
  lapply(as.factor)

correlation[,log_col] <- correlation %>%
  select("Bump", "Crossing", "Junction","Roundabout", "Stop","Traffic_Signal") %>%
  lapply(as.numeric)


#change date types to numeric
dates <- c("Start_Time","day","month")
correlation[,dates] <- correlation %>%
  select("Start_Time","day","month") %>%
  lapply(as.numeric)

cor <- cor(correlation, use = "complete.obs")
cor <- melt(cor)
cor <- rename(cor, c("Variable" = "Var1",
                     "Target" = "Var2",
                     "Correlation"="value"))

C <- filter(cor, Target=="Severity") #only want to look at severity correlation

C <- subset(C, abs(C$Correlation)>0.05) #only want to look at variables with greater than 5% correlation

cor.test(accidents2$Severity, accidents2$road_type)


#combine classes into 2 rather than 4 given class imbalance
knn <- knn %>%
  select(Severity,Start_Time, County,`Wind_Chill(F)`,`Wind_Speed(mph)`,Weather_Condition,Crossing,Traffic_Signal,month,road_type) #only use variables with >0.05 correlation

table(knn$Severity) #due to class imbalance going to combine class 1&2 tog and 3&4 together to fix imbalance

knn$Severity = gsub(2,1,knn$Severity) #replace 2 severity with 1
knn$Severity = gsub(3,4,knn$Severity) #replace 3 severity with 4


#convert logical variables to numeric
log_col <- c("Crossing","Traffic_Signal")

knn[,log_col] <- knn %>%
  select("Crossing", "Traffic_Signal") %>%
  lapply(as.factor)

knn[,log_col] <- knn %>%
  select("Crossing", "Traffic_Signal") %>%
  lapply(as.numeric)

#change character variable to factor then numeric
chr_col <- c("County","Weather_Condition","road_type")

knn[,chr_col] <- knn %>%
  select("County","Weather_Condition","road_type") %>%
  lapply(as.factor)

knn[,chr_col] <- knn %>%
  select("County","Weather_Condition","road_type") %>%
  lapply(as.numeric)

#change month variable to numeric
dates <- c("Start_Time","month")
knn[,dates] <- knn %>%
  select("Start_Time","month") %>%
  lapply(as.numeric)

#create function to normalize data
normalize <- function(x) {
  return ((x-min(x)) / (max(x)-min(x)))
}


#fix class imbalance and partition data
table(knn$Severity)
prop.table(table(knn$Severity)) #distribution is 74% vs 25% - not what we want!

train_knn <- knn[1:80000, ,drop=TRUE] #training dataset

train_knn <- train_knn %>%
  rename(wind_speed = `Wind_Speed(mph)`) %>%
  rename(wind_chill = `Wind_Chill(F)`) #clean up variable names for ROSE function to work

data.rose <- ROSE(Severity ~., data=train_knn,seed=101)$data
prop.table(table(data.rose$Severity)) #now distribution is 50% vs 49% - great!
train_knn <- data.rose[,2:10] #only want numeric features
train_knn_labels <- data.rose[,1] 
train_knn <- as.data.frame(lapply(train_knn,normalize)) #normalize numeric features


test_knn <- knn[80001:100000, ,drop=TRUE] #testing dataset
test_knn_labels <- test_knn[,1] 
test_knn <- test_knn[,2:10]
test_knn <- as.data.frame(lapply(test_knn,normalize))


train_knn <- as.data.frame(train_knn)
test_knn <- as.data.frame(test_knn)
train_knn_labels <- as.data.frame(train_knn_labels)
test_knn_labels<- as.data.frame(test_knn_labels)

knn_pred <-knn(train_knn,test_knn,train_knn_labels$train_knn_labels,k=282)
knn_pred <- as.data.frame(knn_pred)

CrossTable(test_knn_labels$Severity, knn_pred$knn_pred, prop.chisq=FALSE)




