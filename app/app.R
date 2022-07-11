library(shiny)
library(tidyverse)
library(tidytext)

source("\\Users\ravihela\Documents\mastering_shiny\wrd_freq_df.R")
source("/Users/ravihela/Documents/mastering_shiny/bigrm_df.R")

ui <- fluidPage(
  fileInput("upload", NULL, accept = c(".csv", ".tsv")),
  actionButton(inputId = "submit", label = "RUN", class = "btn-primary"),
  numericInput("n", "Rows", value = 5, min = 1, step = 1),
  tableOutput("head")
)

options(shiny.maxRequestSize = 60*1024^2)
server <- function(input, output, session) {
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  rv <- reactiveValues()
  
  observeEvent(input$submit, {
  rv$data_head <- data() %>% 
    mutate(ID = as.character(round(ID))) %>%
    mutate(group = if_else(rating >= 4, "High", if_else(rating > 2, "Avg", "Low"))) %>%
    select(ID, group,  reviewText)
  
  rv$word_freq <- wrd_freq_df(rv$data_head)
  rv$bigrm_freq <- bigrm_freq_df(rv$data_head)
  rv$bigrm_tf_idf <- bigrm_freq_tf_idf(rv$bigrm_freq)
  
  })
    
  
  output$head <- renderTable({
    # head(data(), input$n)
    rv$bigrm_tf_idf %>% head()
    # rv$data_head
  })
}

#executes app
shinyApp(ui, server)
