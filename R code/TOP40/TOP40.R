# Reading .RData for for the TOP40 in and producing results
# Check file configuration in "RData" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(forecast)

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

name = "TOP40"      

# load in .RData
load(paste0("RData/",name,".RData"))

system.time({a = rnorm(100000000,0,1)})
