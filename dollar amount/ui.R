library (shiny)
library (ggplot2)
library (ggthemes)
# theme_set(theme_classic())

shinyUI (fluidPage(
  titlePanel("Dollar Amount"),
  sidebarLayout(  
    sidebarPanel(
      checkboxInput ("zero", "Exlcude contracts with 0 dollar amount", TRUE),
    
    sliderInput("contracts", "Contract Numbers", 1, 53000, c (1, 53000), step = 1000), 
    sliderInput("amount", "Dollar Amount", -13451443, 551201941, c (-13451443, 551201941), 
         step = 10000) 
  ),
    
mainPanel( 
  plotOutput("distPlot")  
    )
  )
))

