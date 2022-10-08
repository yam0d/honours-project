# Cleaning and processing file for all markets
# Add Index folder and name after Excel/
# e.g. reading in Bloomberg S&P 500 data is "Excel/SP500/SP500"
# Check file configuration in "Excel" folder for naming convention

# Clear environment 
rm(list = ls())

# Load packages 
library(readxl)

# Read in data from "Excel" folder
# Clean data by removing missing data (N/A)
Data = read_excel("Excel/SP500/SP500.xlsx")
Data = Data[dim(Data)[1]:1,] # reverse order for time series
Data$Dividend = as.numeric(Data$Dividend) # numeric variables
Data$Price = as.numeric(Data$Price) # numeric variables
Data$Date = as.Date(Data$Date)
Data = na.omit(Data)     # remove missing data
attach(Data)

# Calculating the real price and real dividend
real_price = Price * (CPI[dim(Data)[1]]/CPI) 
# Annualised dividends divided by 12
real_dividend = Dividend * (CPI[dim(Data)[1]]/CPI)/12 

# save each market in "RData" folder to be called later
save.image("RData/SP500.RData")
