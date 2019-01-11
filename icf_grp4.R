#ICF GROUP 4
# VISUALIZATION & GROUPING 

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

#select variables needed
rawdata <- read.csv("Downloads/Data_Feed.csv")
data(zipcode)
icf <- rawdata %>%
  select(id = unique_transaction_id, dollarsobligated, baseandexercisedoptionsvalue, 
         baseandalloptionsvalue, signedDate= signeddate, complettionDate = ultimatecompletiondate, 
        zipPlacePerformance = placeofperformancezipcode, numbBids = numberofoffersreceived, 
        vendorName = vendorname, vendorNumEmployee = numberofemployees, 
        vendorRevenue = annualrevenue, zipVendor = zipcode, 
        contractRequirement = descriptionofcontractrequirement, 
        productCode = productorservicecode)

# change zipVendor and zipPlacePerformance

icf$zipPlacePerformance <- substr(icf$zipPlacePerformance, 0, 5)
icf$zipVendor <- substr(icf$zipVendor, 0, 5)

changeZip <- function (x) {
  substr(x, 0, 5)
}

#filter for dollarsobligated, baseandexercisedoptionsvalue, and baseandalloptionsvalue > 0 

icf.prac <- icf.prac %>% 
  select( id, dollarsobligated, baseandexercisedoptionsvalue, baseandalloptionsvalue, signedDate,
          complettionDate, zipPlacePerformance, numbBids, vendorName, vendorNumEmployee, 
          vendorRevenue, zipVendor, contractRequirement, productCode) %>% 
  filter( dollarsobligated > 0 | baseandexercisedoptionsvalue > 0 | baseandalloptionsvalue > 0)

greaterthan0 <- function (w, x, y, z){
    w %>% 
    filter( x > 0 | y > 0 | z > 0 )
}

# greatest value of all three dollar amounts 
max(dat, na.rm=TRUE)
colMax <- function(data) sapply(data, max, na.rm = TRUE)

        #your data.frame contains NA. 
        #Function to calculate the max of each rows 
        maxFun<-function(x){max(na.omit(x))} 

        #Apply the function to each row of the data.frame 
        MAX<-apply(data, 1, maxFun)# data being your data.frame 

        #Add MAX column to your data.frame 
        data=cbind(data, MAX)
        
        apply(YDF, 2, max, na.rm=FALSE) 

moneyteam <- icf %>% 
    select (id, dollarsobligated ,baseandalloptionsvalue, baseandexercisedoptionsvalue)
largest <- tapply (moneyteam, 2, max)

dollarAmount <- max (icf.prac$dollarsobligated[1:6525], icf.prac$baseandexercisedoptionsvalue [1:6525], 
                     icf.prac$baseandalloptionsvalue [1:6525])

# change all NA values to 0 
x[is.na(x)] <- 0
icf.prac [is.na(icf.prac)] <- 0

noZero <- function (x) {
  x [is.na(x)] <- 0 
}

# making a scatterplot for competitiveness
    
library(ggplot2)
library(gtable)
library(grid)

p1 <- ggplot(icf, aes(vendorNumEmployee, numbBids)) + 
  geom_point() + 
  expand_limits(y = c(min(icf$numbBids) - 0.1 * diff(range(icf$numbBids)), 
                      max(icf$numbBids) + 0.1 * diff(range(icf$numbBids)))) + 
  expand_limits(x = c(min(icf$vendorNumEmployee) - 0.1 * diff(range(icf$vendorNumEmployee)), 
                      max(icf$vendorNumEmployee) + 0.1 * diff(range(icf$vendorNumEmployee)))) + 
  theme(plot.margin = unit(c(0.2, 0.2, 0.5, 0.5), "lines"))


        #Horizontal marginal boxplot - to appear at the top of the chart
p2 <- ggplot(icf, aes(x = factor(1), y = vendorNumEmployee)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitter(width = 0.05)) + 
  expand_limits(y = c(min(icf$vendorNumEmployee) - 0.1 * diff(range(icf$vendorNumEmployee)), 
                      max(icf$vendorNumEmployee) + 0.1 * diff(range(icf$vendorNumEmployee)))) + 
  coord_flip() + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        plot.margin = unit(c(1, 0.2, -0.5, 0.5), "lines"))

        #Vertical marginal boxplot - to appear at the right of the chart
p3 <- ggplot(icf, aes(x = factor(1), y = numbBids)) + 
  geom_boxplot(outlier.colour = NA) + 
  geom_jitter(position = position_jitter(width = 0.05)) + 
  expand_limits(y = c(min(icf$numbBids) - 0.1 * diff(range(icf$numbBids)), 
                      max(icf$numbBids) + 0.1 * diff(range(icf$numbBids)))) + 
  theme(axis.text = element_blank(), 
        axis.title = element_blank(), 
        axis.ticks = element_blank(), 
        plot.margin = unit(c(0.2, 1, 0.5, -0.5), "lines"))

    gt1 <- ggplot_gtable(ggplot_build(p1))
    gt2 <- ggplot_gtable(ggplot_build(p2))
    gt3 <- ggplot_gtable(ggplot_build(p3))

    # Get maximum widths and heights
    maxWidth <- unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
    maxHeight <- unit.pmax(gt1$heights[4:5], gt3$heights[4:5])
    
    # Set the maximums in the gtables for gt1, gt2 and gt3
    gt1$widths[2:3] <- as.list(maxWidth)
    gt2$widths[2:3] <- as.list(maxWidth)
    
    gt1$heights[4:5] <- as.list(maxHeight)
    gt3$heights[4:5] <- as.list(maxHeight)

    # Create a new gtable
    gt <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))
    
    # Instert gt1, gt2 and gt3 into the new gtable
    gt <- gtable_add_grob(gt, gt1, 2, 1)
    gt <- gtable_add_grob(gt, gt2, 1, 1)
    gt <- gtable_add_grob(gt, gt3, 2, 2)
    
    
    # And render the plot
    grid.newpage()
    grid.draw(gt)
    
    grid.rect(x = 0.5, y = 0.5, height = 0.995, width = 0.995, default.units = "npc", 
              gp = gpar(col = "black", fill = NA, lwd = 1))




    
    p1 <- ggplot(mtcars, aes(mpg, hp)) + 
      geom_point() + 
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      expand_limits(y = c(min(mtcars$hp) - 0.1 * diff(range(mtcars$hp)), 
                          max(mtcars$hp) + 0.1 * diff(range(mtcars$hp)))) + 
      expand_limits(x = c(min(mtcars$mpg) - 0.1 * diff(range(mtcars$mpg)), 
                          max(mtcars$mpg) + 0.1 * diff(range(mtcars$mpg)))) + 
      theme(plot.margin = unit(c(0.2, 0.2, 0.5, 0.5), "lines"))
    # Horizontal marginal boxplot - to appear at the top of the chart
    p2 <- ggplot(mtcars, aes(x = factor(1), y = mpg)) + 
      geom_boxplot(outlier.colour = NA) + 
      geom_jitter(position = position_jitter(width = 0.05)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      expand_limits(y = c(min(mtcars$mpg) - 0.1 * diff(range(mtcars$mpg)), 
                          max(mtcars$mpg) + 0.1 * diff(range(mtcars$mpg)))) + 
      coord_flip() + 
      theme(axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            plot.margin = unit(c(1, 0.2, -0.5, 0.5), "lines"))
    
    # Vertical marginal boxplot - to appear at the right of the chart
    p3 <- ggplot(mtcars, aes(x = factor(1), y = hp)) + 
      geom_boxplot(outlier.colour = NA) + 
      geom_jitter(position = position_jitter(width = 0.05)) + 
      scale_y_continuous(expand = c(0, 0)) + 
      expand_limits(y = c(min(mtcars$hp) - 0.1 * diff(range(mtcars$hp)), 
                          max(mtcars$hp) + 0.1 * diff(range(mtcars$hp)))) + 
      theme(axis.text = element_blank(), 
            axis.title = element_blank(), 
            axis.ticks = element_blank(), 
            plot.margin = unit(c(0.2, 1, 0.5, -0.5), "lines"))
    gt1 <- ggplot_gtable(ggplot_build(p1))
    gt2 <- ggplot_gtable(ggplot_build(p2))
    gt3 <- ggplot_gtable(ggplot_build(p3))
    # Get maximum widths and heights
    maxWidth <- unit.pmax(gt1$widths[2:3], gt2$widths[2:3])
    maxHeight <- unit.pmax(gt1$heights[4:5], gt3$heights[4:5])
    
    # Set the maximums in the gtables for gt1, gt2 and gt3
    gt1$widths[2:3] <- as.list(maxWidth)
    gt2$widths[2:3] <- as.list(maxWidth)
    
    gt1$heights[4:5] <- as.list(maxHeight)
    gt3$heights[4:5] <- as.list(maxHeight)
    
    
    # Create a new gtable
    gt <- gtable(widths = unit(c(7, 1), "null"), height = unit(c(1, 7), "null"))
    
    # Instert gt1, gt2 and gt3 into the new gtable
    gt <- gtable_add_grob(gt, gt1, 2, 1)
    gt <- gtable_add_grob(gt, gt2, 1, 1)
    gt <- gtable_add_grob(gt, gt3, 2, 2)
    
    
    # And render the plot
    grid.newpage()
    grid.draw(gt)
    
    grid.rect(x = 0.5, y = 0.5, height = 0.995, width = 0.995, default.units = "npc", 
              gp = gpar(col = "black", fill = NA, lwd = 1))









