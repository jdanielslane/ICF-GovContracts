install.packages("maps")
install.packages("ggthemes")
install.packages("albersusa")
install_github("hrbrmstr/albersusa")
install.packages("ggmap")
install.packages("mapdata")
devtools::install_github("dkahle/ggmap")
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

rawdata <- read.csv("Downloads/Data_Feed.csv")
data(zipcode)

cbp1 <- rawdata %>%
  select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
         descriptionofcontractrequirement, signeddate, effectivedate,
         ultimatecompletiondate,vendorname, city, state, placeofperformancezipcode,
         productorservicecode, numberofoffersreceived, numberofemployees)

cbp1$placeofperformancezipcode <- substr(cbp1$placeofperformancezipcode, 0, 5)

cbp1$ultimatecompletiondate <- mdy(cbp1$ultimatecompletiondate)
cbp1$signeddate <- mdy(cbp1$signeddate)
cbp1$effectivedate <- mdy(cbp1$effectivedate)

# filter contracts with signeddate or effectivedate greater than or equal to ymd 2016-10-01

cbp1 <- cbp1 %>%
  select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
         descriptionofcontractrequirement, signeddate, effectivedate,
         ultimatecompletiondate,vendorname, city, state, placeofperformancezipcode,
         productorservicecode, numberofoffersreceived, numberofemployees) %>%
  filter(signeddate >= as.Date("2016-10-01")) 

cbp1 <- cbp1 %>%
  select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
         descriptionofcontractrequirement, signeddate, effectivedate,
         ultimatecompletiondate,vendorname, city, state, placeofperformancezipcode,
         productorservicecode, numberofoffersreceived, numberofemployees) %>%
  filter(effectivedate >= as.Date("2016-10-01"))

#filter contracts expiring greater or equal to ymd, 2018-04-01

cbp1 <- cbp1 %>%
   select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
                        descriptionofcontractrequirement, signeddate, effectivedate,
                          ultimatecompletiondate,vendorname, city, state, placeofperformancezipcode,
                        productorservicecode, numberofoffersreceived, numberofemployees) %>%
 filter(ultimatecompletiondate >= as.Date("2018-01-01"))

#filter dollarsobligated larger than 100,000

cbp1 <- cbp1 %>%
  select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
         descriptionofcontractrequirement, signeddate, effectivedate,
         ultimatecompletiondate,vendorname, city, state, placeofperformancezipcode,
         productorservicecode, numberofoffersreceived, numberofemployees) %>%
filter (dollarsobligated >= "100000")

# add lat and long 
cbp1 <- cbp1 %>% mutate(placeofperformancezipcode = substr(cbp1$placeofperformancezipcode, start = 1, stop = 5)) %>%
  left_join(zipcode[, c(1,4,5)], by = c("placeofperformancezipcode" = "zip"))

#sample map in leaflet() on expiring ultimate expiration date

sample.map <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng= cbp1$longitude, lat= cbp1$latitude, popup="Expiring Soon")
# http://localhost:16302/session/viewhtml4f9c45730d6e/index.html



