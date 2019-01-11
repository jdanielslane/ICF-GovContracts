
library(shiny)
shinyUI(fluidPage(
  
  titlePanel("Scoring"),
  
  sidebarLayout(
    sidebarPanel(
      h2("scoring"),
      h3("what you mainly focus"),
      selectInput("select", "Select focus",
                  choices = list("Competitiveness-contractsize-category"=1, "contractsize-Competitiveness-category" = 2,"contractsize-category-Competitiveness" = 3,"category-contractsize-Competitiveness" = 4,"category-Competitiveness-contractsize" = 5,"Competitiveness-category-contractsize" = 6), selected = 1),
      h3("what you mainly focus on competitiveness"),
      selectInput("select2", "Select focus",
                  choices = list("bidscore-employeerevenuescore-companysizescore"= 1, "bidscore-companysizescore-employeerevenuescore" = 2,"employeerevenuescore-bidscore-companysizescore" = 3,"employeerevenuescore-companysizescore-bidscore" = 4,"companysizescore-bidscore-employeerevenuescore" = 5,"companysizescore-employeerevenuescore-bidscore" = 6), selected = 1),
      
      checkboxGroupInput("checkGroup", "Category Checkbox",
                         choices = list("IT" = "IT",
                                        "MAINTENANCE" = "MAINTENANCE",
                                        "OFFICE" = "OFFICE",
                                        "TELECOM"="TELECOM",
                                        "EDU"="EDU",
                                        "HOUSE"="HOUSE",
                                        "TRANSPORT"="TRANSPORT",
                                        "MANAGEMENT"="MANAGEMENT",
                                        "SECURITY"="SECURITY",
                                        "OTHER"="OTHER"),
                         selected =c("IT","MAINTENANCE",
                                     "OFFICE", "TELECOM",
                                     "TRANSPORT","EDU","HOUSE",
                                     "MANAGEMENT","SECURITY")),
      selectInput("select3", "I look for keywords in",
                  choices = list("1. service code"= 1,
                                 "2. contract requirement" = 2,
                                 "either 1 or 2" = 3,
                                 "both 1 and 2" = 4), selected = 1),
      textInput("text", "type keywords", value = "e.g.WEB"),
      sliderInput("range", "Range of final scores", 0, 100, c(0, 100), step = 1)
    ),
    mainPanel(
      plotOutput("distPlot"),
      h3(textOutput("Glossary"),
         h4("Contract Size: the max amount of dollars obligated to each contract."),
         
         h4("Categories: the 9 most repeated words in the dataset, used to describe a contract. 
            Each category contains a few keywords."), 
         
         h4("Keywords: keywords are words found in descriptive column of the dataset and fall under specific categories."),
         
         h4("Competitiveness: is determined by a vendor's number of employees, the number of bids per contract, and the vendor's annual revenue. 
            The competitiveness score can be changed depending on these 3 factors."),
         
         h4 ("Contract Score/Final Score: is determined by contract size, number of categories, and competitiveness. 
             These three variables are combined to create an overall score for each individual contract."))
         ))
  ))
  
  
  

