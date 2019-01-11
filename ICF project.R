## ICF PROJECT
install.packages("maps")
install.packages("ggthemes")
install.packages("albersusa")
install_github("hrbrmstr/albersusa")
install.packages("ggmap")
install.packages("mapdata")
install.packages("gdata")
devtools::install_github("dkahle/ggmap")
devtools::install_github("hrbrmstr/albersusa")
library(readxl)
library(tidyverse)
library(dplyr)
library(lubridate)
library(scales)
library(gridExtra)
library(zipcode)
library(maps)
library(viridis)
library(ggthemes)
library(albersusa)
library(ggmap)

rawdata<- read.csv("Downloads/Data_Feed.csv")
data(zipcode)
cbp1 <- rawdata %>%
      select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
                           descriptionofcontractrequirement, signeddate, effectivedate,
                           ultimatecompletiondate,vendorname, city, state, placeofperformancezipcode,
                           productorservicecode, numberofoffersreceived, numberofemployees)

cbp$zipcode <- substr(cbp$zipcode, 0, 5)




#import HomelandSecurity_CustomsAndBorderProtection excel file
cbp<-HomelandSecurity_CustomsAndBorderProtection


#How to extract first 5 numbers from zipcode 
realZip <- substr(forMap$place, 0, 5)

forMap$place <- substr(forMap$place, 0, 5)

#data to use for map
forMap<-cbp %>% select(dollars=dollarsobligated, 
                       place= placeofperformancezipcode, parentcompany= mod_parent)

#align forMap zipcodes with correct logitude and latitide numbers
#left_join

#vector to be matched %in% vector to be matched against
matching <- forMap$place %in% zipcode$zip

# use baseandexercisedoptionsvalue and baseandalloptionsvalue and dollarsobligated as a proxy 














