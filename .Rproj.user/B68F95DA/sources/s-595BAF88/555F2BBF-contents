rm(list = ls())
getwd()
setwd("C:/Users/Alka/Documents/R/datasciencecoursera/specdata")

pollutantmean <- function(directory, pollutant, id = 1:332) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'pollutant' is a character vector of length 1 indicating
  ## the name of  the pollutant for which we will calcultate the
  ## mean; either "sulfate" or "nitrate"
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return the mean of the pollutant across all monitors list
  ## in the 'id' vector (ignoring NA values)
  ## NOTE: Do not round the result
means<-c()
 
  for(monitor in id){
    path <- paste(getwd(), "/", sprintf("%03d", monitor), ".csv", sep = "")
    monitor_data <- read.csv(path)
    interested_data <- monitor_data[pollutant]
    means <- c(means, interested_data[!is.na(interested_data)])
  }
  
  mean(means)
}
pollutantmean("specdata", "sulfate", 1:10)



#######Another way

pollutantmean <- function(directory, pollutant, id = 1:332) {
  my_list<-list.files(path=directory,pattern =".csv" )
  x<-numeric()
  for(i in id){
   my_data<-read.csv(my_list[i])
    x<-c(x,my_data[[pollutant]])}
  mean(x,na.rm  =TRUE)
}
