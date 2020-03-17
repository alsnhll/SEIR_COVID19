## ---------------------------
##
## Script name: getPopData
##
## Purpose of script: extract data from world development indicators and munges into a useful format
##
## Author: Ben Phillips
##
## Date Created: 2020-03-16
##
## Email: phillipsb@unimelb.edu.au
##
## ---------------------------
##
## Notes:
##   
##
## --------------------------
## load up the packages we will need 
library(tidyverse)
## ---------------------------

## load up our functions into memory

## ---------------------------
## Data sourced from https://databank.worldbank.org/source/population-estimates-and-projections
  # Population estimates and projections
  # All countries
  # Age classes in 5yr increments
  # yr = 2018 (trying not to use an extrapolated number)

popDat <- read.csv(file = "COVID19seir/dataPipe/Data_Extract_From_Population_estimates_and_projections/208e48bd-9077-4bca-8aae-ca660a12c171_Data.csv", 
                   stringsAsFactors = FALSE,
                   na.strings = "..")

popDat <- popDat %>% filter(grepl(pattern = "\\d", x = popDat$Series.Code)) %>% 
    select(Country.Name, Series.Code, X2018..YR2018.) %>% 
    group_by(Country.Name) %>% 
    spread(key = Series.Code, value = X2018..YR2018., ) 
#colnames(popDat)<-gsub(pattern = "SP.POP.", replacement = "", x = colnames(popDat))
fMat<-popDat[,grepl(pattern = "FE", x = colnames(popDat))]
mMat<-popDat[,grepl(pattern = "MA", x = colnames(popDat))]
cMat<-fMat+mMat
colnames(cMat)<-gsub(pattern = ".FE", replacement = "", x = colnames(cMat))
popDat<-data.frame(country = popDat$Country.Name, cMat, stringsAsFactors = FALSE)
save(popDat, file = "COVID19seir/dataPipe/popData.RData")
