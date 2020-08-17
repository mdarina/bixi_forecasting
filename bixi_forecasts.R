#Assigning the directory
setwd("~/Concordia/SASUniversityEdition/sas_code/portfolio/portfolio_sets") #Change the directory

#Install packages if you need to
install.packages("lubridate")
install.packages("rgdal")
install.packages("gridExtra")
install.packages("scales")
install.packages("ggplot2")
install.packages("forecast")
install.packages("smooth")
install.packages("zoo")


#Loading the required libraries
library(lubridate)
library(rgdal)
library(gridExtra)
library(scales)
library(ggplot2)
library(forecast)
library(smooth)
library(zoo)


#Importing the data 
bixi2014_2019 <- read.csv("date_allm_everyone.csv")
bixi2014_2019$Date <- as.Date(bixi2014_2019$Date, format="%m/%d/%Y")
bixi2014_2019$Year <- year(bixi2014_2019$Date)

bixi_year <- aggregate(x=list(Rides=bixi2014_2019$Rides),
                       by=list(Year=bixi2014_2019$Year), FUN=sum)


bixi_year$Rides <- as.character(bixi_year$Rides)
bixi_year$Rides[bixi_year$Rides=="5597845"] <- "5800000"

bixi_year$Rides <- as.numeric(bixi_year$Rides)

#Plot all rides by year
bixi_year_all <- aggregate(x=list(Rides=bixi2014_2019$Rides),
                           by=list(Year=bixi2014_2019$Year), FUN=sum)

#Plotting the rides
ggplot(bixi_year_all, aes(Year, Rides/1000, fill=as.character(Year)))+
  geom_bar(stat="identity", position="dodge")+
  geom_text(aes(label=comma(Rides)), position=position_dodge(width=0.9), vjust=-0.25)+
  ylim(0, 6000)+
  scale_fill_manual(values=c("gray74", "gray74", "gray74", "gray74", "gray74", "indianred1"))+
  ggtitle("Member rides: By year (2014-2019)")+
  ylab("Rides, '000")+
  theme(legend.position="none")


#Importing data on BIXI stations
st14 <- read.csv("Stations_2014.csv")
y2014 <- as.numeric(as.character(length(st14$code)))
st15 <- read.csv("Stations_2015.csv")
y2015 <- as.numeric(as.character(length(st15$code)))
st16 <- read.csv("Stations_2016.csv")
y2016 <- as.numeric(as.character(length(st16$code)))
st17 <- read.csv("Stations_2017.csv")
y2017 <- as.numeric(as.character(length(st17$code)))
st18 <- read.csv("Stations_2018.csv")
y2018 <- as.numeric(as.character(length(st18$code)))
st19 <- read.csv("Stations_2019.csv")
y2019 <- as.numeric(as.character(length(st19$Code)))
stations <- cbind.data.frame(year=c(seq(as.Date("2014/1/4"), as.Date("2019/1/4"), "years")), 
                             stations=c(y2014, y2015, y2016, y2017, y2018, y2019))

#Plotting the stations growth
ggplot(stations, aes(x=year, y=stations))+geom_line(color="cornflowerblue")+ggtitle("The number of BIXI stations: 2014-2019")+
  xlab("Year")+ylab("Number of BIXI stations")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

#Mapping stations: 2014 and 2019
mySHP <- readOGR("LIMADMIN.shp")
dfSHP <- fortify(mySHP) 

#Importing 2014 data
stations14<- read.csv("Stations_2014.csv")
#Plotting 2014 stations
s2014 <- ggplot()+geom_polygon(data=dfSHP, aes(x=long,y=lat, 
                                               group=group), color="black", fill="lightgrey")+
  geom_point(data=stations14, aes(x=longitude, y=latitude), 
             alpha=0.5, color="indianred3")+
  labs(title = "BIXI stations: 2014", 
       x="Longitude", y="Latitude")+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

#Importing 2019 data
stations19<- read.csv("Stations_2019.csv")
#Plotting 2019 stations
s2019 <- ggplot()+geom_polygon(data=dfSHP, aes(x=long,y=lat, 
                                               group=group), color="black", fill="lightgrey")+
  geom_point(data=stations19, aes(x=longitude, y=latitude), 
             alpha=0.5, color="navy")+
  labs(title = "BIXI stations: 2019", 
       x="Longitude", y="Latitude")+
  theme(axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),legend.position="none",
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

grid.arrange(s2014,s2019,ncol=2)


#Renaming the classes of the variable: 1=Member, 0=Non-member
bixi2014_2019$Member <- ifelse(bixi2014_2019$Member==1, "Member", "Occasional")


#Aggregating the data per member
bmember <- aggregate(x=list(Rides=bixi2014_2019$Rides),
                     by=list(Member=as.character(bixi2014_2019$Member)), FUN=sum)
bmember$Total <- sum(bixi2014_2019$Rides)
bmember$Percent <- percent(bmember$Rides/bmember$Total)

absolute <- ggplot(bmember, aes(Member, Rides, fill=Member))+geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("lavenderblush3","cornflowerblue"))+
  geom_text(aes(label=comma(Rides)), position=position_dodge(width=0.9), vjust=-0.25)+
  ylab(" ")+
  ggtitle("Member vs non-member rides: 2014-2019")+
  theme(legend.position="none", axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())

percent <- ggplot(bmember, aes(Member, Percent, fill=Member))+geom_bar(stat="identity", position="dodge")+
  scale_fill_manual(values=c("lavenderblush3","cornflowerblue"))+
  geom_text(aes(label=Percent), position=position_dodge(width=0.9), vjust=-0.25)+
  ylab(" ")+
  ggtitle("Percent of total rides: 2014-2019")+
  theme(legend.position="none", axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank(),
        axis.text.y=element_blank(), axis.ticks.y=element_blank())

grid.arrange(absolute, percent, ncol=2)


#Checking for patterns in the data
#Importing the data 
bixi14_19 <- read.csv("date_allm_everyone.csv")

#Creating a new date
bixi14_19$Date <- as.Date(bixi14_19$Date, format="%m/%d/%Y")
bixi14_19$Month <- make_date(year(bixi14_19$Date),month(bixi14_19$Date),1)

#Aggregating data by month
bixi_month <- aggregate(x=list(Rides=bixi14_19$Rides),
                        by=list(Month=bixi14_19$Month), FUN=sum)


#calculating the values for november 2019
nov <- 5800000-5597845
date <- make_date(year=2019L, month=11L, day=1L)

#Creatining a dataset only for november 2019
bixi_nov <- cbind.data.frame(Month=as.Date(date,  format="%m/%d/%Y"), Rides=as.integer(nov))

#Appending November to the monthly dataset
bixi_month19<- rbind.data.frame(bixi_month, bixi_nov)

#Creating a time series object
bixi.ts <- ts(bixi_month19$Rides, start=c(2014), frequency=8)


#Plotting the time series
autoplot(bixi.ts/1000, main="Bixi rides by months: 2014-2019", xlab="Year", ylab="Bixi rides, '000", color="navy")


#Plotting the data - checking for the trend
ggplot(bixi_year, aes(x=Year, y=Rides/1000))+geom_line(color="navy")+
  ggtitle("Bixi rides: 2014-2019")+ylab("Rides, '000")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())

#Creating a seasonal plot of Bixi rides
ggseasonplot(bixi.ts)+ggtitle("Bixi rides by months of the season: 2014-2019")+
  xlab("Months of the season")+ylab("Rides, '000")


#Checking for distributions of various Box-Cox transformations
r1000 <- ggplot(bixi_month19, aes((Rides/1000)))+geom_histogram(bins=9, col="black", fill="cornflowerblue",
                                                                alpha=.2)+
  ggtitle("BIXI rides no transformation: 2014-2019")+xlab("Rides, '000")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),plot.background=element_blank())  

rlog <- ggplot(bixi_month19, aes(log(Rides)))+geom_histogram(bins=9, col="black", fill="cornflowerblue",
                                                             alpha=.2)+
  ggtitle("BIXI logged rides: 2014-2019")+xlab("Log of rides")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        plot.background=element_blank()) 

rinvert <- ggplot(bixi_month19, aes(1/(Rides)))+geom_histogram(bins=9, col="black", fill="cornflowerblue",
                                                               alpha=.2)+
  ggtitle("Inverse of BIXI rides: 2014-2019")+xlab("1/Rides")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        plot.background=element_blank()) 

rsqrt <- ggplot(bixi_month19, aes(sqrt(Rides)))+geom_histogram(bins=9, col="black", fill="cornflowerblue",
                                                               alpha=.2)+
  ggtitle("Square root BIXI rides: 2014-2019")+xlab("Square root of rides")+
  theme(axis.line=element_blank(),
        panel.background=element_blank(),panel.border=element_blank(),panel.grid.major=element_blank(),
        plot.background=element_blank())

grid.arrange(r1000, rlog, rinvert, rsqrt, ncol=2, nrow=2)


#Partitioning the data
nvalid <- 16 #recording a number into a vector
ntrain <- length(bixi.ts) - nvalid #the rest for 2014-2016 years - 32 observations

train.ts <- window(bixi.ts, start=2014, end=c(2014,ntrain), freq=8)
valid.ts <- window(bixi.ts, start=c(2014, ntrain+1), freq=8)

#Simple Naive
#Training the model and forecasting
trainN <- naive(train.ts, h=nvalid, level=c(95)) #training the model
#Checking the model summary
summary(trainN)
#Checking accuracy
accuracy(trainN, valid.ts) #checking for accuracy
#Checking residuals
checkresiduals(trainN)
#Plotting the values
autoplot(trainN) + autolayer(fitted(trainN)) +
  autolayer(valid.ts, color="black") 

#Seasonal naive
trainSN <- snaive(train.ts, h=nvalid, level=c(95))
#Checking the model summary
summary(trainSN)
#Checking accuracy
accuracy(trainSN, valid.ts) #checking accuracy
#Checking residuals
checkresiduals(trainSN)
#Plotting the values
autoplot(trainSN) + autolayer(fitted(trainSN)) +
  autolayer(valid.ts, color="black")


#Simple mean
trainMF <- meanf(train.ts, h=nvalid, level=c(95))
#Getting the summary
summary(trainMF)
#Checking accuracy
accuracy(trainMF, valid.ts) 
#Checking residuals
checkresiduals(trainMF)
#Plotting the values
autoplot(trainMF) + autolayer(fitted(trainMF)) +
  autolayer(valid.ts, color="black") 


#Exponential smoothing: library(smooth)
trS <- ses(train.ts, h=nvalid, alpha=0.5, level=95)
#Getting the summary
summary(trS)
#Checking accuracy
accuracy(trS, valid.ts) #checking accuracy
#Checking residuals
checkresiduals(trS)
#Plotting the values
autoplot(trS) + autolayer(fitted(trS)) +
  autolayer(valid.ts, color="black") 


#Holt's
Htr <- holt(train.ts, h=nvalid, level=95)
#Summarizing the model
summary(Htr)
#Checking accuracy
accuracy(Htr, valid.ts)
#Checking residuals
checkresiduals(Htr)
#Plotting the values
autoplot(Htr) + autolayer(fitted(Htr)) +
  autolayer(valid.ts, color="black")


#Winter's additive
Hwtr <- hw(train.ts, seasonal="additive", 
           h=nvalid, level=95)
#Summarizing the model
summary(Hwtr)
#Checking accuracy
accuracy(Hwtr, valid.ts)
#Checking residuals
checkresiduals(Hwtr)
#Plotting the values
autoplot(Hwtr) + autolayer(fitted(Hwtr)) +
  autolayer(valid.ts, color="black") 


#Winter's multiplicative
Hwtr1 <- hw(train.ts, seasonal="multiplicative", 
            h=nvalid, level=95)
#Summarizing the model
summary(Hwtr1)
#Checking accuracy
accuracy(Hwtr1, valid.ts)
#Checking residuals
checkresiduals(Hwtr1)
#Plotting the values
autoplot(Hwtr1) + autolayer(fitted(Hwtr1)) +
  autolayer(valid.ts, color="black") 


#ARIMA models
#differencing the data
#differencing at lag 1
lag1 <- diff(bixi.ts, lag=1)
#differencing at lag 8 (seasonal)
lag8 <- diff(bixi.ts, lag=8)
#double differencing at lag 1 and lag 8
lag1_8 <- diff(diff(bixi.ts,8),1)


#plotting regular and differenced plots
par(mfrow=c(2,2))
plot(bixi.ts, col="navy", ylab="Regular", xlab="Year", main="Bixi rides: 2014-2019")
plot(lag1, col="navy", ylab="Lag of 1", xlab="Year", main="Lag(1) rides: 2014-2019")
plot(lag8, col="navy", ylab="Lag of 8", xlab="Year", main="Lag(8) rides: 2014-2019")
plot(lag1_8, col="navy", ylab="Lags of 1 and 8", xlab="Year", main="Lag(1,8) rides: 2014-2019")


#Checking for correlations
Box.test(bixi.ts, lag=8, type="Ljung-Box")

#Plotting autocorrelation plots
par(mfrow=c(2,2)) #placing 4 plots in one window
acf(bixi.ts, main="Regular Rides") #autocorrelation plot 
pacf(bixi.ts, main="Regular Rides") #partial autocorrelation plot
acf(lag1, main="Lag(1) Rides")
pacf(lag1, main="Lag(1) Rides")
par(mfrow=c(2,2)) #placing 4 plots in one window
acf(lag8, main="Lag(8) Rides")
pacf(lag8, main="Lag(8) Rides")
acf(lag1_8, main="Lag(1,8) Rides")
pacf(lag1_8, main="Lag(1,8) Rides")


#ARIMA: AR(1)
train.arima <- forecast(arima(train.ts, order=c(1,0,0)), h=nvalid, level=c(95))
#Summarizing the model
summary(train.arima)
#Checking accuracy
accuracy(train.arima, valid.ts) #checking accuracy
#Checking residuals
checkresiduals(train.arima)
#Plotting the values
autoplot(train.arima) + autolayer(fitted(train.arima)) +
  autolayer(valid.ts, color="black") 


#ARIMA: AR(1), Differencing at lag1
tar <- forecast(arima(train.ts, order=c(1,1,0)), h=nvalid, level=c(95))
#Summarizing the model
summary(tar)
#Checking accuracy
accuracy(tar, valid.ts)
#Checking residuals
checkresiduals(tar)
#Plotting the values
autoplot(tar) + autolayer(fitted(tar)) +
  autolayer(valid.ts, color="black") 


#ARIMA: AR(1), Differencing at lag1, MA(1)
tar2 <- forecast(arima(train.ts, order=c(1,1,1)), h=nvalid, level=c(95))
#Summarizing the model
summary(tar2)
#Checking accuracy
accuracy(tar2, valid.ts)
#Checking residuals
checkresiduals(tar2) 
#Plotting the values
autoplot(tar2) + autolayer(fitted(tar2)) +
  autolayer(valid.ts, color="black") 


#AutoARIMA
tr.arima <- forecast(auto.arima(train.ts), h=nvalid, level=c(95))
#Summarizing the model
summary(tr.arima)
#Checking accuracy
accuracy(tr.arima, valid.ts)
#Checking residuals
checkresiduals(tr.arima) 
#Plotting the values
autoplot(tr.arima) + autolayer(fitted(tr.arima)) +
  autolayer(valid.ts, color="black") 


#AutoARIMA: Non-seasonal
t.arima <- forecast(auto.arima(train.ts, seasonal=FALSE), h=nvalid, level=c(95))
#Summarizing the model
summary(tr.arima)
#Checking accuracy
accuracy(t.arima, valid.ts)
#Checking residuals
checkresiduals(t.arima)
#Plotting the values
autoplot(t.arima) + autolayer(fitted(t.arima)) +
  autolayer(valid.ts, color="black")


#CHECKING ACCURACY
#Seasonal Naive
accuracy(trainSN, valid.ts) 

#Winter-Holt's multiplicative
accuracy(Hwtr1, valid.ts) #checking accuracy THE BEST

#AutoARIMA
accuracy(tr.arima, valid.ts) #checking accuracy


#Checking residuals
#Holt-Winter's multiplicative
checkresiduals(Hwtr1)
#AutoARIMA
checkresiduals(tr.arima)


#Forecasting the BIXI rides in 2020 using the 2014-2019 set
bixi2020 <- hw(bixi.ts, seasonal="multiplicative", 
               h=8, level=95)

summary(bixi2020)

#Plotting the data
autoplot(bixi2020) + autolayer(fitted(bixi2020))


#Importing the data
bmember <- aggregate(x=list(Rides=bixi2014_2019$Rides),
                     by=list(Member=bixi2014_2019$Member), FUN=sum)
bmember$Member <- ifelse(bmember$Member==1, "Member", "Occasional")
bmember$Total <- sum(bmember$Rides)


#Retriving non-member rides percentage
nonm <- subset(bmember, Member=="Occasional")
nonm$Rate <- nonm$Rides/nonm$Total
nonm$Rate


#Tourism drop rate
drop <- 0.3
drop


#Calculating non-member index
Tour <- (1-drop)*nonm$Rate
Tour


#Calculating the proportion of member rides out of total for all years
#Retriving non-member rides percentage
mem <- subset(bmember, Member=="Member")
mem$Rate <- mem$Rides/mem$Total
mem$Rate


#The average employment rate in 2019
u2019 <- 62.68 #from April-July 2019
u2020 <- 55.35 #from April-July 2020
emp_change <- abs((u2020-u2019)/u2019)
emp_change


#Calculating member index
Index <- (1-emp_change)*mem$Rate
Index


#Adjusting forecasts
bixi_adj2020 <- cbind.data.frame(Forecast=as.numeric(bixi2020$mean), Mem_Ind = Index, Oc_Ind=Tour)
bixi_adj2020$Adj_FC <- bixi_adj2020$Forecast*bixi_adj2020$Mem_Ind + bixi_adj2020$Forecast*bixi_adj2020$Oc_Ind
bixi_adj2020[,c(1,4)]
#Creating a time series object
bixi_adj2020.ts <- ts(bixi_adj2020$Adj_FC, start=c(2020,1), freq=8)

#Plotting the data
autoplot(bixi2020) + autolayer(fitted(bixi2020))+autolayer(bixi_adj2020.ts)
