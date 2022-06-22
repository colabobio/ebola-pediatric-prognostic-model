library(shiny)

ui <- fillPage(
  titlePanel('Using machine learning to predict survival in children with Ebola Virus Disease'),
  padding = 15,
  numericInput("PatientAge", "Patient age:", min = 0, max = 18, value = 10),
  numericInput("CT", "CT value:", min = 0, max = 100, value = 25),
  selectInput("AnyBleeding", "Bleeding", c("No" = 'n', 'Yes' = 'y', selected = NULL)),
  selectInput("Diarrhoea", "Diarrhea", c("No" = 'n', 'Yes' = 'y', selected = NULL)),
  selectInput("Breathlessness", "Breathlessness", c("No" = 'n', 'Yes' = 'y', selected = NULL)),
  selectInput("SwallowingProblems", "Swallowing Problems", c("No" = 'n', 'Yes' = 'y', selected = NULL)),
  "Probability of death is", textOutput("prob", inline = TRUE),"%"
)

server <- function(input, output, session) {
  output$prob <- renderText({ 
    LP <- (6.5163882 -
             0.3806482*input$PatientAge +
             0.3033613*(0.005102041*max(input$PatientAge - 2.0, 0)^3 +
                          -0.01190476*max(input$PatientAge - 10.0, 0)^3 +
                          0.006802721*max(input$PatientAge - 16.0, 0)^3) -
             0.2139306*input$CT +
             0.1545426*(0.00395554*max(input$CT - 18.6, 0)^3 +
                          -0.006762697*max(input$CT - 25.2, 0)^3 +
                          0.002807157*max(input$CT - 34.5, 0)^3) +
             0.3245020*ifelse(input$AnyBleeding == 'y', 1, 0) +
             0.2671774*ifelse(input$Diarrhoea == 'y', 1, 0) +
             0.3624056*ifelse(input$Breathlessness == 'y', 1, 0) +
             0.4269878*ifelse(input$SwallowingProblems == 'y', 1, 0))
    prob <- round(1/(1+exp(-LP))*100, 2)
  })
}

shinyApp(ui, server)