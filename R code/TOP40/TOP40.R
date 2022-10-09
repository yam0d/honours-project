# Reading .RData for for the TOP40 in and producing results
# Check file configuration in "RData" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(devtools)
library(silvermantest)
library(forecast)

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

name = "TOP40"      

# load in .RData
load(paste0("RData/",name,".RData"))

time = system.time({
  
  
  
  
  
  
  })

# check time elpapsed
time

# update loaded "TOP40.RData" file 
save.image(paste0("RData/",name,".RData"))
