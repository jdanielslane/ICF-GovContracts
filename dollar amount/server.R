library (shiny)




shinyServer(function(input, output) {
  
  output$distPlot <- renderPlot({
    
    idx=(dollaramount$NewIDNum >= input$contracts[1] & dollaramount$NewIDNum <= input$contracts[2])
   idx2 = (dollaramount$NewMax >= input$amount[1] & dollaramount$NewMax <= input$amount[2])
    
    dollaramount %>%
      filter(idx, idx2) %>%
    ggplot(aes (x=NewMax)) +       
      geom_density() +
      ggtitle("Federal Contracts") +
      labs(y="Number of Contracts", x = "Dollars") 
    
    
      
  })
  
})





