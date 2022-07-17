#Load libraries ----
library(shiny)
library(tidyverse)
library(tidytext)
library(ggraph)

# Source functions ----
source("//Users/ravihela/Documents/mastering_shiny/wrd_freq_df.R")
source("//Users/ravihela/Documents/mastering_shiny/bigrm_df.R")

# UI design ----
ui <- fluidPage(fluidRow(
  column(
    4,
    fileInput("upload", NULL, accept = c(".csv", ".tsv")),
    actionButton(
      inputId = "submit",
      label = "RUN",
      class = "btn-primary"
    ),fluidRow(
    column(4, numericInput(
      "n",
      "Top n words",
      value = 5,
      min = 1,
      step = 1
    )),
    column(4, selectizeInput(
      "myselect",
       label = "Choose Words"
      , choices = NULL
      , multiple = TRUE
      # , options = list(create = TRUE)
      # , selected = NULL
        ))),
    plotOutput("wrd_frq_plot"),
    plotOutput("bigrm_frq_plot")
    
    # tableOutput("head")
  ),
  column(8,
         sliderInput("ntDensity", label = "Choose density", min = 10, max = 1000,value = 20),
         plotOutput("bigrm_nt"))
))

# SERVER ----
options(shiny.maxRequestSize = 60 * 1024 ^ 2)
server <- function(input, output, session) {
  
  #upload csv data
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    switch(
      ext,
      csv = vroom::vroom(input$upload$datapath, delim = ","),
      tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
      validate("Invalid file; Please upload a .csv or .tsv file")
    )
  })
  
  #create different data subset to be used for plotting word_freq and bigrams
  rv <- reactiveValues()
  
  #run analysis on pressing run button
  observeEvent(input$submit, {
    rv$data_head <- data() %>%
      mutate(ID = as.character(round(ID))) %>%
      mutate(group = if_else(rating >= 4, "High", if_else(rating > 2, "Avg", "Low"))) %>%
      select(ID, group,  reviewText)
    
    #get base analysis dataset
    rv$word_freq <- wrd_freq_df(rv$data_head)
    rv$bigrm_freq <- bigrm_freq_df(rv$data_head)
    rv$bigrm_tf_idf <- bigrm_freq_tf_idf(rv$bigrm_freq)
    
    print(rv$bigrm_freq)
    View(rv$bigrm_tf_idf)
    
    #get all unique words from the document
    rv$choices <- rv$word_freq %>%
      select(word) %>% 
      unique() %>% unlist() %>% str_remove_all("^word")
    
    #print(rv$choices)
    #ensure that if select input has no value then use all words are  analysis
    if (is.null(input$myselect)) {
      rv$options = rv$choices
      # print(rv$options)
    }
    else{
      rv$options = input$myselect
      # print(rv$options)
    }
    updateSelectizeInput(session, "myselect", choices = rv$options)
    
    
    output$wrd_frq_plot <- renderPlot({
      
      #get relevant document which contains chosen word for analysis
      word <- rv$options
      # print(paste0("testin", word))
      word_df_chose <- data.frame(word)
      # print(word_df_chose)
      
      #get IDs with relevant word
      rv$relevant_ID <- word_df_chose %>%
        inner_join(rv$word_freq) %>%
        select(ID) %>%
        unique() %>% unlist()
      
      # print(rv$relevant_ID)

      
      rv$word_freq %>%
        filter(ID %in% rv$relevant_ID) %>%
        left_join(rv$data_head %>%
                    select(ID, group) %>%
                    unique()) %>%
        
        #remove low tf_IDF words
        filter(tf_idf > quantile(tf_idf, 0.25)) %>%
        group_by(group, word) %>%
        summarise(n_tot = sum(n)) %>% ungroup() %>%
        group_by(group) %>%
        slice_max(n_tot, n = input$n) %>%
        ungroup() %>%
        mutate(group = as.factor(group),
               word = reorder_within(word, n_tot, group)) %>%
        ggplot(aes(x = word, y = n_tot, fill = group)) + geom_col() + facet_wrap(group ~
                                                                                   ., scales = "free") +
        coord_flip() + scale_x_reordered()
      
    })
    
    output$bigrm_frq_plot <- renderPlot({

      #get relevant document which contains chosen word for analysis

      rv$bigrm_tf_idf %>%
        filter(ID %in% rv$relevant_ID) %>%
        # left_join(rv$data_head %>%
        #             select(ID, group) %>%
        #             unique()) %>%
        
        #remove low tf_IDF words
        filter(tf_idf > quantile(tf_idf, 0.25, na.rm = T)) %>%

        group_by(group, bigram) %>%
        summarise(n_tot = sum(n)) %>% ungroup() %>%
        group_by(group) %>%
        slice_max(n_tot, n = input$n) %>%
        ungroup() %>%
        mutate(group = as.factor(group),
               bigram = reorder_within(bigram, n_tot, group)) %>%
        ggplot(aes(x = bigram, y = n_tot, fill = group)) + geom_col() + facet_wrap(group ~
                                                                                   ., scales = "free") +
        coord_flip() + scale_x_reordered()

    })
    
    output$bigrm_nt <- renderPlot({
      
      #get relevant document which contains chosen word for analysis
      
      bigram_graph <- rv$bigrm_tf_idf %>%
        filter(ID %in% rv$relevant_ID) %>%
        
        #remove low tf_IDF words
        filter(tf_idf > quantile(tf_idf, 0.25, na.rm = T)) %>%
        
        group_by(bigram) %>%
        summarise(n = sum(n)) %>% ungroup() %>%
        ungroup() %>%
        filter(n > input$ntDensity) %>%
        ungroup() %>%
        
        separate(bigram,
                 c("word1", "word2"),
                 sep = " ",
                 remove = FALSE) %>%
        select(-bigram) %>%
        graph_from_data_frame()
      
      
      # set.seed(2017)
      ggraph(bigram_graph, layout = "fr") +
        geom_edge_link() +
        geom_node_point() +
        geom_node_text(aes(label = name), vjust = 1, hjust = 1)
        
    })
    
  })
}

#executes app
shinyApp(ui, server)
