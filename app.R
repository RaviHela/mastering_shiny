
#calls Library
library(shiny)

#define user interface
ui <- fluidPage(
  selectInput("dataset", label = "Dataset", choices = ls("package:datasets")),
  verbatimTextOutput("summary"),
  tableOutput("table")
)

#specify app behavior
server <- function(input, output, session) {
  
  # Create a reactive expression
  dataset <- reactive({
    get(input$dataset, "package:datasets")
  })
  
  #output functions 1
  output$summary <- renderPrint({
    summary(dataset())
  })
  
  #output functions 2
  output$table <- renderTable({
    dataset()
  })
}
#executes app
shinyApp(ui, server)