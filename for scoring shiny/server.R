
library(shiny)
library(dplyr)
library(stringr)
library(ggplot2)

names(cbp[24])="catscore"

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    if(input$select==1){
      cbp$competitvenessscore=0.5*cbp$bidscore+0.3*cbp$employeerevenuescore+0.2*cbp$companysizescore
    }else if(input$select==2){
      cbp$competitvenessscore=0.5*cbp$bidscore+0.2*cbp$employeerevenuescore+0.3*cbp$companysizescore
    }else if(input$select==3){
      cbp$competitvenessscore=0.3*cbp$bidscore+0.5*cbp$employeerevenuescore+0.2*cbp$companysizescore
    }else if(input$select==4){
      cbp$competitvenessscore=0.2*cbp$bidscore+0.5*cbp$employeerevenuescore+0.3*cbp$companysizescore
    }else if(input$select==5){
      cbp$competitvenessscore=0.3*cbp$bidscore+0.2*cbp$employeerevenuescore+0.5*cbp$companysizescore
    }else{
      cbp$competitvenessscore=0.2*cbp$bidscore+0.3*cbp$employeerevenuescore+0.5*cbp$companysizescore
    }
    if(input$select2==1){
      cbp$totalscore=0.5*cbp$competitvenessscore+0.3*cbp$contractsize+0.2*cbp$catscore
    }else if(input$select2==2){
      cbp$totalscore=0.5*cbp$competitvenessscore+0.2*cbp$contractsize+0.3*cbp$catscore
    }else if(input$select2==3){
      cbp$totalscore=0.3*cbp$competitvenessscore+0.5*cbp$contractsize+0.2*cbp$catscore
    }else if(input$select2==4){
      cbp$totalscore=0.2*cbp$competitvenessscore+0.5*cbp$contractsize+0.3*cbp$catscore
    }else if(input$select2==5){
      cbp$totalscore=0.3*cbp$competitvenessscore+0.2*cbp$contractsize+0.5*cbp$catscore
    }else{
      cbp$totalscore=0.2*cbp$competitvenessscore+0.3*cbp$contractsize+0.5*cbp$catscore
    }
    
    dumy1=ifelse(cbp$category.maintenance,"MAINTENANCE",FALSE)
    dumy2=ifelse(cbp$category.IT,"IT",FALSE)
    dumy3=ifelse(cbp$category.office,"OFFICE",FALSE)
    dumy4=ifelse(cbp$category.telecommunications,"TELECOM",FALSE)
    dumy5=ifelse(cbp$category.housekeeping,"HOUSE",FALSE)
    dumy6=ifelse(cbp$category.transportation,"TRANSPORT",FALSE)
    dumy7=ifelse(cbp$category.education,"EDU",FALSE)
    dumy8=ifelse(cbp$category.security,"SECURITY",FALSE)
    dumy9=ifelse(cbp$category.management,"MANAGEMENT",FALSE)
    dumy10=ifelse(dumy1=="FALSE" &
                    dumy2=="FALSE" &
                    dumy4=="FALSE" &
                    dumy5=="FALSE" &
                    dumy6=="FALSE" &
                    dumy7=="FALSE" &
                    dumy8=="FALSE" &
                    dumy9=="FALSE","OTHER",FALSE)
    
    pattern=paste(strsplit(input$text, " ")[[1]], collapse = "|")
    
    if(input$select3==1){
      dumy11=str_detect(cbp$productorservicecode, pattern)
    }else if(input$select3==2){
      dumy11=str_detect(cbp$descriptionofcontractrequirement, pattern)
    }else if(input$select3==3){
      a=str_detect(cbp$productorservicecode, pattern)
      b=str_detect(cbp$descriptionofcontractrequirement, pattern)
      dumy11=a|b 
    }else if(input$select3==4){
      a=str_detect(cbp$productorservicecode, pattern)
      b=str_detect(cbp$descriptionofcontractrequirement, pattern)
      dumy11=a&b 
    }
    
    
    if (input$text=="e.g. WEB"){
      dumy11=!dumy11
    }else{
      dumy11=dumy11
    }
    
    idx = ( (dumy1 %in% input$checkGroup |
               dumy2 %in% input$checkGroup |
               dumy3 %in% input$checkGroup |
               dumy4 %in% input$checkGroup |
               dumy5 %in% input$checkGroup |
               dumy6 %in% input$checkGroup |
               dumy7 %in% input$checkGroup |
               dumy8 %in% input$checkGroup |
               dumy9 %in% input$checkGroup |
               dumy9 %in% input$checkGroup |
               dumy10 %in% input$checkGroup) & 
              dumy11 &  
              cbp$totalscore >= input$range[1] &
              cbp$totalscore <= input$range[2])
    
    
    ggplot( cbp [idx,],aes (x=totalscore)) + geom_density()
  })
})


