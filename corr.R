

library(tidyverse)
library(readxl)
library(scales)

path <- "D:/Coursera_Project/datasciencecoursera/specdata"
setwd(path)
data.1 <- read.csv("001.csv")

##################################################################
### Part 1
pollutantmean <- function(path, pollutant, id = 1:332) {
  files <- list.files(
    path = path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  data <- do.call(rbind, lapply(files, read.csv))
  
  pollutant_sym <- sym(pollutant)
  
  mean.data <- data %>% 
    filter(
      ID %in% id
    ) %>% 
    summarise(
      mean = mean(!!pollutant_sym, na.rm = TRUE),
      .groups = "drop"
    ) %>% 
    pull(mean)
}

mean.data <- pollutantmean(path, "nitrate")
mean.data
##################################################################
### Part 2

# function 1
complete <- function(path, id = 1:332){
  files <- list.files(
    path = path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  data <- do.call(rbind, lapply(files, read.csv))
  
  data <- data %>% 
    filter(
      ID %in% id
    ) %>% 
    group_by(
      ID
    ) %>% 
    summarise(nobs = sum(complete.cases(sulfate, nitrate)))
}

# function 2
complete2 <- function(path, id = 1:332){
  files <- list.files(
    path = path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  data <- do.call(rbind, lapply(files, read.csv))
  
  data <- data %>% 
    filter(
      ID %in% id
    ) %>% 
    filter(
      !(is.na(sulfate) | is.na(nitrate))
    ) %>% 
    group_by(
      ID
    ) %>% 
    summarise(nobs = n())
}

count.data <- complete2(path, 3)
count.data

cc <- complete(path, 54)
print(cc$nobs)

#######
RNGversion("3.5.1")  
set.seed(42)
cc <- complete(path, 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])

#######
cr <- corr(path)                
cr <- sort(cr)   
RNGversion("3.5.1")
set.seed(868)                
out <- round(cr[sample(length(cr), 5)], 4)
print(out)


#######
cr <- corr(path, 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)
#######
cr <- corr(path, 2000)                
n <- length(cr)                
cr <- corr(path, 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

##################################################################
### Part 3

corr <- function(path, threshold = 0) {
  files <- list.files(
    path = path,
    pattern = "\\.csv$",
    full.names = TRUE
  )
  
  data <- do.call(rbind, lapply(files, read.csv))
  
  data %>% 
    group_by(ID) %>% 
    filter(sum(complete.cases(sulfate, nitrate)) > threshold) %>% 
    summarise(
      corr = cor(sulfate, nitrate, use = "complete.obs"),
      .groups = "drop"
    ) %>% 
    pull(corr)
}

cr <- corr(path)
cr
summary(cr)
length(cr)
