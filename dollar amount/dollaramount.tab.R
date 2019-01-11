library (shiny)
library (ggplot2)
library (ggthemes)
theme_set(theme_classic())



######## ui.R #############

shinyUI(fluidPage(
  titlePanel("Dollar Amount"),
   sidebarLayout(  
     sidebarPanel(
       selectInput ("dollars", "Dollar Amount:", 
            choices= list("dollaramount", "dollarsobligated", "baseandexerciseoptionsvalue", 
            "baseandalloptionsvalue", "all"), selected = "all"),
       
       selectInput ("zero", "Inlcude or Exclude Zero:",
                   choices = "include zero", "exclude zero")),
     
       sliderInput("contracts", "Contract Numbers", 1, 53000, 20000),
     
     mainPanel(
       plotOutput("dollarsPlot")  
     )
))
)



########## server.R ###########


shinyServer(function(input, output) {

  output$dollarsPlot <- renderPlot({
    chartData <- switch(input$dollars, 
                  "dollaramount" = list(cbp$dollaramount),
                  "dollarsobligated" = list(cbp$dollarsobligated), 
                  "baseandexerciseoptionsvalue" = list(cbp$baseandexercisedoptionsvalue),
                  "baseandalloptionsvalue" = list (cbp$baseandalloptionsvalue),
        "all" = list (cbp$dollaramount, cbp$dollarsobligated, cbp$baseandexercisedoptionsvalue, 
                    cbp$baseandalloptionsvalue))
    
    chartTitle <- switch(input$dollars,
                    "dollaramount" = "Max contract dollar amount",
                    "dollarsobligated" = "Obligated dollars",
                    "baseandexerciseoptionsvalue" = "Base exercised dollars",
                    "baseandalloptionsvalue" = "Base Option dollars",   
                    "all" = "All dollars")
    

    xrange <- cbp$X
  
    ggplot(cbp, aes (xrange)) +       
    geom_line(aes(y = dollaramount), colour="red") + 
    geom_line(aes(y = dollarsobligated), colour = "black") +
    geom_line(aes(y = baseandexercisedoptionsvalue), colour = "orange") +
    geom_line(aes(y = baseandalloptionsvalue), colour = "blue") +
      ggtitle("Federal Contracts by", chartTitle) +
      labs(y="Dollars", x = "Contract ID Number")
        
   
  })

})

















#################### PRACTICE YEAH ###########


ggplot(cbp, aes(x = X)) + 
  geom_line(aes(y = dollaramount), colour="red") + 
  geom_line(aes(y = dollarsobligated), colour = "black") +
  geom_line(aes(y = baseandexercisedoptionsvalue), colour = "orange") +
  geom_line(aes(y = baseandalloptionsvalue), colour = "blue") 

ggplot(cbp, aes(x = NewIDNum)) +
  geom_area(aes(y = dollarsobligated), colour="red") +
  geom_area(aes(y = NewMax), colour = "orange")  

library(ggridges)

g <- ggplot(cbp$NewIDNum, aes(cbp$dollarsobligated))
g + geom_density(aes(fill=factor(cbp$dollarsobligated)), alpha=0.8)


idx = which(df$mag >= input$magnitude[1] & df$mag <= input$magnitude[2] & df$depth 
            >= input$depth[1] & df$depth <= input$depth[2])


ggplot(cbp , aes(x=dollarsobligated)) + geom_density()
+ scale_x_continuous(label = dollar)


ggplot(dollaramount, aes (x)) +       
  geom_line(aes(y = NewMax), colour="red") + 
  geom_line(aes(y = dollarsobligated), colour = "black") +
  geom_line(aes(y = baseandexercisedoptionsvalue), colour = "orange") +
  geom_line(aes(y = baseandalloptionsvalue), colour = "blue") +
  ggtitle("Federal Contracts by") +
  labs(y="Dollars", x = "Contract ID Number")


dollaramount %>%
  filter(idx, idx2) %>%
  ggplot(aes (x=NewIDNum)) +       
  geom_line(aes(y = NewMax), colour = "purple") +
  ggtitle("Federal Contracts") +
  labs(y="Dollars", x = "Contract ID Number") + 
  scale_y_continuous(labels = dollar)


## Kayi's try ##
idx=(dollaramount$NewIDNum >= 40000 & dollaramount$NewIDNum <= 50000)

dollaramount %>%
  filter(idx) %>%
  ggplot(aes (x=NewIDNum)) +       
  geom_line(aes(y = NewMax), colour="red") + 
  geom_line(aes(y = dollarsobligated), colour = "black") +
  geom_line(aes(y = baseandexercisedoptionsvalue), colour = "orange") +
  geom_line(aes(y = baseandalloptionsvalue), colour = "blue") +
  ggtitle("Federal Contracts by")  




