####################################################################################################################################
#Assignment 5
#Part1
#Author: Anthea Li
#        Tianying Zhang
#        Senhao Li
####################################################################################################################################

## station 46035 has missing data in 2012 and 2013
## Replaced the data of these two years with Station VCVA2(the nearest active station found on map)

####################################################################################################################################

library(tidyverse)
library(stringr)
library(reshape2)
library(ggplot2)

#source the tables from the website
url_1 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=46035h"
url_2 <- "http://www.ndbc.noaa.gov/view_text_file.php?filename=vcva2h"
url_3 <- ".txt.gz&dir=data/historical/stdmet/"
years <- c(1988:2017)
all_urls_1 <- str_c(url_1, years, url_3, sep = "")
all_urls_2 <- str_c(url_2, years, url_3, sep = "")
filenames <- str_c("ThirtyYears", years, sep = "")
year_length <- length(years)
####################################################################################################################################

#check the files we find that year 2012 and 2013 data is missing because the urls of 2012 and 2013 cannot be found.
#Hence skip 2012 and 2013 according to 3.3.1 Missing Values(“An introduction to data cleaning with R” by Edwin de Jonge and Mark van der Loo)

####################################################################################################################################

#create files for each year 
i <- 1
while (i <= year_length){
  #2012 and 2013 use VCVA2 data
  if(i==25|i==26){
    suppressMessages(
      assign(filenames[i], read_table(all_urls_2[i], col_names = TRUE)))
  }
  #else use station 46035
  else{
    suppressMessages(
      assign(filenames[i], read_table(all_urls_1[i], col_names = TRUE)))
  }
  cleantable <- get(filenames[i])
  
  #let 88 - 99 be 1988-1999
  colnames(cleantable)[1] <-"YYYY"
  if(i>=1 && i<=11){
    cleantable$YYYY<-paste("19", cleantable$YYYY, sep = "")
  }
  
  #Select variables we need
  
  cleantable <- subset(cleantable, select=c("YYYY", "MM","DD","hh","ATMP","WTMP"))
  #change the format of the data
  
  cleantable$ATMP<-as.numeric(cleantable$ATMP)
  cleantable$WTMP<-as.numeric(cleantable$WTMP)
  
  #delete wrong data
  
  cleantable <- subset(cleantable, ATMP!= 999.0 &ATMP!= 99.0)
  cleantable <- subset(cleantable, WTMP!= 999.0 &WTMP!= 99.0)
  
  #calculate the mean
  
  annualMean_AT <- mean(cleantable$ATMP, na.rm=TRUE)
  annualMean_WT <- mean(cleantable$WTMP, na.rm=TRUE)
  
  #the start year 1988, initialize the origin Total file
  if(i==1){
    Total <- cleantable
    Total_DailyNoon <- subset(cleantable, hh==12)
    annualMeans_AT <- annualMean_AT
    annualMeans_WT <- annualMean_WT
  }
  #other years, bind the data with the origin one.
  else{
    Total <- rbind.data.frame(Total, cleantable)
    Total_DailyNoon <- rbind.data.frame(Total_DailyNoon, subset(cleantable, hh==12))
    annualMeans_AT <- rbind.data.frame(annualMeans_AT, annualMean_AT)
    annualMeans_WT <- rbind.data.frame(annualMeans_WT, annualMean_WT)
  }
  #print(Total)
  i <- i+1
}
  
####################################################################################################################################

#Time series

####################################################################################################################################

#change data to correct format
Total$FullTime <-strptime(with(Total, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")
Total_DailyNoon$FullTime <-strptime(with(Total_DailyNoon, paste(YYYY, MM, DD, hh, sep="-")), format="%Y-%m-%d-%H")


#select daily noon data 

#visualize the total thirty years data
ggplot(Total_DailyNoon, aes(x = FullTime)) + 
  geom_line(aes(y = ATMP), colour="red", size = 0.5) + 
  geom_line(aes(y = WTMP), colour = "blue", size = 0.5) +
  ylab(label="Temperature") + 
  xlab("Time") +
  labs(title="A time series composed of 30 years of daily Air Temperature
       and Sea Temperature readings recorded at noon")

Total_1988_DailyNoon <- subset(Total_DailyNoon, YYYY==1988)[,c(2:6)]  
Total_2017_DailyNoon <- subset(Total_DailyNoon, YYYY==2017)[,c(2:6)]
colnames(Total_2017_DailyNoon) <- c("MM","DD","hh","ATMP2017","WTMP2017")
colnames(Total_1988_DailyNoon) <- c("MM","DD","hh","ATMP1998","WTMP1998")


#combine 1988 and 2017 into one table
Total_1988VS2017_DailyNoon <- as.data.frame(left_join(Total_1988_DailyNoon,Total_2017_DailyNoon))
Total_1988VS2017_DailyNoon

#change date to correct format
Total_1988VS2017_DailyNoon$FullTime <-strptime(with(Total_1988VS2017_DailyNoon, paste(MM, DD, hh, sep="-")), format="%m-%d-%H")

Total_1988VS2017_DailyNoon


#visualize the data
#by 1998 and 2017 air temp
ggplot(Total_1988VS2017_DailyNoon, aes(x = FullTime)) + 
  geom_line(aes(y = ATMP1998, colour ="1998 AirTemp")) + 
  geom_line(aes(y = ATMP2017, colour = "2017 AirTemp")) +
  scale_colour_manual("", 
                      breaks = c("1998 AirTemp", "2017 AirTemp"),
                      values = c("red", "blue"))+
  xlab("Time") +
  scale_y_continuous("Temperature", limits = c(0,10)) + 
  labs(title="A time series composed of 1998 and 2017 of daily Air Temperature recorded at noon")

#by 1998 and 2017 sea temp
ggplot(Total_1988VS2017_DailyNoon, aes(x = FullTime)) + 
  geom_line(aes(y = WTMP1998, colour ="1998 SeaTemp")) + 
  geom_line(aes(y = WTMP2017, colour = "2017 SeaTemp")) +
  scale_colour_manual("", 
                      breaks = c("1998 SeaTemp", "2017 SeaTemp"),
                      values = c("red", "blue"))+
  xlab("Time") +
  scale_y_continuous("Temperature", limits = c(0,10)) + 
  labs(title="A time series composed of 1998 and 2017 of daily Sea Temperature readings recorded at noon")

#annual temp
annualTemprature <- data.frame(c(1988:2017), annualMeans_AT, annualMeans_WT)
colnames(annualTemprature) <- c("Year", "Air Temperature", "Water Temperature")


ggplot(annualTemprature, aes(x = Year)) + 
  geom_line(aes(y = annualMeans_AT), colour="blue", size = 0.5) + 
  geom_line(aes(y = annualMeans_WT), colour = "red", size = 0.5) +
  ylab(label="Temperature") + 
  xlab("Time")
######################################################################################################################

#statistics

########################################################################################################################

##1. Test the difference based on time of the day

#Demonstration:
Total_Daily00 <- subset(Total, hh=="00")[,-c(4,7)]
colnames(Total_Daily00) <- c("YYYY","MM","DD","ATMP00","WTMP00")
Total_Daily02 <- subset(Total, hh=="02")[,-c(4,7)]
colnames(Total_Daily02) <- c("YYYY","MM","DD","ATMP02","WTMP02")
Total_Daily04 <- subset(Total, hh=="04")[,-c(4,7)]
colnames(Total_Daily04) <- c("YYYY","MM","DD","ATMP04","WTMP04")
Total_Daily06 <- subset(Total, hh=="06")[,-c(4,7)]
colnames(Total_Daily06) <- c("YYYY","MM","DD","ATMP06","WTMP06")
Total_Daily08 <- subset(Total, hh=="08")[,-c(4,7)]
colnames(Total_Daily08) <- c("YYYY","MM","DD","ATMP08","WTMP08")
Total_Daily10 <- subset(Total, hh==10)[,-c(4,7)]
colnames(Total_Daily10) <- c("YYYY","MM","DD","ATMP10","WTMP10")
Total_Daily12 <- subset(Total, hh==12)[,-c(4,7)]
colnames(Total_Daily12) <- c("YYYY","MM","DD","ATMP10","WTMP10")
Total_Daily14 <- subset(Total, hh==14)[,-c(4,7)]
colnames(Total_Daily14) <- c("YYYY","MM","DD","ATMP14","WTMP14")
Total_Daily16 <- subset(Total, hh==16)[,-c(4,7)]
colnames(Total_Daily16) <- c("YYYY","MM","DD","ATMP16","WTMP16")
Total_Daily18 <- subset(Total, hh==18)[,-c(4,7)]
colnames(Total_Daily18) <- c("YYYY","MM","DD","ATMP18","WTMP18")
Total_Daily20 <- subset(Total, hh==20)[,-c(4,7)]
##obtaining same format data for daily tmp at 20:00
colnames(Total_Daily20) <- c("YYYY","MM","DD","ATMP20","WTMP20")
Total_Daily22 <- subset(Total, hh==22)[,-c(4,7)]
colnames(Total_Daily22) <- c("YYYY","MM","DD","ATMP22","WTMP22")
t.test(Total_Daily00$ATMP00, Total_Daily02$ATMP02)
t.test(Total_Daily04$WTMP04, Total_Daily06$WTMP06)
##every pair of the ATMP and WTMP have p-value > 0.05
##Choice of the hour during a day does not make a difference



##2. Test the difference from 1988 to 2017

Total_1988 <- subset(Total_Daily12, YYYY==1988)[,c(2:5)]  
Total_2017 <- subset(Total_Daily12, YYYY==2017)[,c(2:5)]
colnames(Total_2017) <- c("MM","DD","ATMP2017","WTMP2017")
colnames(Total_1988) <- c("MM","DD","ATMP1988","WTMP1988")
Total_1988VS2017 <- as.data.frame(left_join(Total_1988,Total_2017))
t.test(Total_1988VS2017["ATMP1988"],Total_1988VS2017["ATMP2017"])    ##test on air tmp
##p-value = 2.162e-13 < 0.05, we can reject the null hypothesis that in 1988 and 2017 air tmp have the same mean
##significant
t.test(Total_1988VS2017["WTMP1988"],Total_1988VS2017["WTMP2017"])    ##test on water tmp
##p-value = 9.78e-13 > 0.05, we can reject the null hypothesis that in 1988 and 2017 water tmp have the same mean
##significant

##There are significant changes in the past 30 years