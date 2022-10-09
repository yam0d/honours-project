# Cleaning and processing file for each markets
# Check file configuration in "Excel" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(readxl)

# set working directory to "honour-project" folder
setwd("~/GitHub/rcode/honours-project")

# Name of index
# If you want to read in the Robert Shiller datasets you must take note of the
# name convention.
# e.g. name = "RS SP500 2013"

# cheat sheet of names:
# TOP40, SP500, NIKKEI, NIFTY50, HANG SENG, FTSE100, DAX, BOVEPSA, ASE
# RS SP500 2013, RS SP500 2015, RS SP500 2021
name = "RS SP500 2021"      # enter different name for different index

# Read in data from "Excel" folder
# Making appropriate variables numeric
# Clean data by removing missing data (N/A)
Data = read_excel(paste0("Excel/",name,"/",name,".xlsx"))
Data = Data[dim(Data)[1]:1,] # reverse order for time series
Data$Dividend = as.numeric(Data$Dividend) # numeric variables
Data$Price = as.numeric(Data$Price) # numeric variables
# Dont need to change to date as we will change the price and dividend
# variables in time series objects in the other scripts
Data$Date = as.Date(Data$Date)
Data = na.omit(Data)     # remove missing data
attach(Data)

# Calculating the real price and real dividend
real_price = Price * (CPI[dim(Data)[1]]/CPI) 
# Annualized dividends divided by 12
real_dividend = Dividend * (CPI[dim(Data)[1]]/CPI)/12 

# save each market in "RData" folder to be called later
save.image(paste0("RData/",name,".RData"))

