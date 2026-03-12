# Project: Air Pollution Data Analysis
# Author: [Ahmed Bestawy]
# Data Source: ["https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"]
# Description: This script contains functions to calculate pollutant means,
# count complete cases, and find correlations between sulfate and nitrate.

# Downloading Data ----
dataset_url <- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
if (!file.exists("specdata")) {
    download.file(dataset_url, "data.zip")
    unzip("data.zip") # Unzips directly to 'specdata' folder
}


#part 1 Pollutant mean ----

pollutantmean <- function(dirictory, pollutant, id = 1:332){
    fulldata <- data.frame()
    fullfiles <- list.files(dirictory ,full.names = TRUE)
    for(i in id){
        fulldata <- rbind(fulldata, read.csv(fullfiles[i]))
    }      
    mean(fulldata[[pollutant]], na.rm = TRUE)
}

#part 2 Complete cases ----

complete <- function(dirictory, id){
    fullfiles <- list.files(dirictory ,full.names = TRUE)
    nobs <- numeric()
    for (i in id) {
        monitor <- read.csv(fullfiles[i])
        clean <- sum(complete.cases(monitor))
        nobs <- c(as.numeric(nobs), as.numeric(clean))
    }
    data.frame(id = id, nobs = nobs )
}

##part 3 Correlation function ----

corr <- function(dirictory, threshold = 0, id = 1:332) {
 c <- complete(dirictory, 1:332)
 M <- which(c$nobs > threshold)
 fullfiles <- list.files(dirictory ,full.names = TRUE)
 fulldata <- data.frame()
 for(i in id){
     fulldata <- rbind(fulldata, read.csv(fullfiles[i]))
 } 
    results <- numeric()
 for(i in M) { D <- fulldata[fulldata$ID == i,]
    CD <- complete.cases(D)
    cleanest <- D[CD, ]
    current_cor <- cor(cleanest$sulfate , cleanest$nitrate)
   results <- c(results, current_cor)
 }
   results
 }
