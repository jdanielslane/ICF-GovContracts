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
rawdata <- read_csv("D:/Survive in Pitt/4-2018spring/PIA2096Capstone/ICF/Data_Feed.csv")
data(zipcode)
cbp1 <- rawdata %>%
  select(dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue,
         descriptionofcontractrequirement, signeddate, effectivedate,
         ultimatecompletiondate,vendorname, city, state, zipcode,
         productorservicecode, numberofoffersreceived, numberofemployees)
cbp1 <- cbp1 %>% mutate(zipcode = substr(cbp1$zipcode, start = 1, stop = 5)) %>%
  left_join(zipcode[, c(1,4,5)], by = c("zipcode" = "zip"))
# extract the largest value among three variables
maxdollar <- rep(NA, nrow(cbp1))
for(i in 1:nrow(cbp1)){
  maxdollar[i] <- max(cbp1[i, c(1:3)])
}
cbp1 <- cbp1 %>% mutate(maxdollar = maxdollar) %>%
  select(starts_with("maxdollar"), everything())

any(is.na(cbp1$zipcode))
any(is.na(cbp1$state))
any(is.na(cbp1$city))

world<-map_data("world")
# map for dollarsobligated (all contracts that dollar > 0) by color degree
cbp1 %>% filter(dollarsobligated > 0) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray", fill = NA, alpha = 0.35) +
  geom_point(aes(color = dollarsobligated), size = 0.15, alpha = 0.25) +
  xlim(-170,-50) + ylim(10,75)
### alternative by point size
cbp1 %>% filter(dollarsobligated > 0) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray", fill = NA, alpha = 0.35) +
  geom_point(aes(size = dollarsobligated), color = "coral1") +
  scale_size(name = "Dollars\nObligated") + xlim(-170,-50) + ylim(10,70)

# map for max of 3 var (all contracts that dollar > 0) by color degree
cbp1 %>% filter(dollarsobligated > 0) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray", fill = NA, alpha = 0.35) +
  geom_point(aes(color = maxdollar), size = 0.15, alpha = 0.25) +
  xlim(-170,-20) + ylim(0,75)
### alternative by point size
cbp1 %>% filter(dollarsobligated > 0) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray", fill = NA, alpha = 0.35) +
  geom_point(aes(size = maxdollar), color = "red") +
  scale_size(name = "Max\nDollar") + xlim(-170,-50) + ylim(10,70)

# map on baseandalloptionsvalue (all contracts that dollar > 0) by color degree
cbp1 %>% filter(dollarsobligated > 0) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray", fill = NA, alpha = 0.35) +
  geom_point(aes(color = baseandalloptionsvalue), size = 0.15, alpha = 0.25) +
  xlim(-170,-20) + ylim(0,75)
### alternative by point size
cbp1 %>% filter(dollarsobligated > 0) %>%
  ggplot(aes(longitude, latitude)) +
  geom_polygon(data = world, aes(x = long, y = lat, group = group),
               color = "gray", fill = NA, alpha = 0.35) +
  geom_point(aes(size = baseandalloptionsvalue), color = "orange") +
  scale_size(name = "Base & Dollar\nOption Value") + xlim(-170,-50) + ylim(10,70)

#https://stackoverflow.com/questions/30787877/making-a-zip-code-choropleth-in-r-using-ggplot2-and-ggmap
#https://austinwehrwein.com/digital-humanities/creating-a-density-map-in-r-with-zipcodes/
