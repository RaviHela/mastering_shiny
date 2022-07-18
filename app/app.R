#Load libraries ----
library(shiny)
library(tidyverse)
library(tidytext)
library(ggraph)
library(igraph)
library(DT)

# Source functions ----
source("//Users/ravihela/Documents/mastering_shiny/wrd_freq_df.R")
source("//Users/ravihela/Documents/mastering_shiny/bigrm_df.R")

# UI design ----
ui <- fluidPage(column(3,
  fileInput("upload", NULL, accept = c(".csv", ".tsv")),

       selectInput(
        "grpSelect",
        "Select Group",
        multiple = TRUE,
        selected = c("High", "Low", "Avg"),
        choices = c("High", "Low", "Avg")
      ),
    numericInput(
        "n",
        "Top_n words",
        value = 10,
        min = 1,
        step = 1,
        width = "100px"
      ),
    
      selectizeInput(
        "myselect",
        label = "Choose Words"
        ,
        choices = NULL
        ,
        multiple = TRUE
      ),
  sliderInput(
    "ntDensity",
    label = "Choose density",
    min = 5,
    max = 1000,
    value = 20
  ),
  actionButton(
    inputId = "submit",
    label = "RUN",
    class = "btn-primary"
  ),
  textOutput("cellText")),
  
  column(4,
  plotOutput("wrd_frq_plot"),
  plotOutput("bigrm_frq_plot"))
  ,
  column(5,
    plotOutput("bigrm_nt"),
    dataTableOutput("dataTbl"))
  
)

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
    rv$data_head <- data() %>% sample_n(1000) %>%
      mutate(ID = as.character(round(ID))) %>%
      mutate(group = if_else(rating >= 4, "High", if_else(rating > 2, "Avg", "Low"))) %>%
      filter(group %in% input$grpSelect) %>%
      select(ID, group,  reviewText)
    
    #get base analysis dataset
    rv$word_freq <- wrd_freq_df(rv$data_head)
    rv$bigrm_freq <- bigrm_freq_df(rv$data_head)
    rv$bigrm_tf_idf <- bigrm_freq_tf_idf(rv$bigrm_freq)
    
    #get all unique words from the document ----
    rv$choices <- rv$word_freq %>%
      select(word) %>%
      unique() %>% unlist() %>% str_remove_all("^word")
    
    #ensure that if select input has no value then use all words for analysis
    if (is.null(input$myselect)) {
      rv$options = rv$choices
    }
    else{
      rv$options = input$myselect
    }
    updateSelectizeInput(session, "myselect", choices = rv$options)
    
    # get IDs with relevant word ----
    word <- rv$options
    word_df_chose <- data.frame(word)
    rv$relevant_ID <- word_df_chose %>%
      inner_join(rv$word_freq) %>%
      select(ID) %>%
      unique() %>% unlist()
    
    
    output$wrd_frq_plot <- renderPlot({
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
        coord_flip() + scale_x_reordered() +  guides(fill = "none") +  theme_light()
      
    })
    output$bigrm_frq_plot <- renderPlot({
      rv$bigrm_tf_idf %>%
        filter(ID %in% rv$relevant_ID) %>%
        
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
        coord_flip() + scale_x_reordered() + guides(fill = "none") +  theme_light()
      
    })
    output$bigrm_nt <- renderPlot({
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
        geom_node_text(aes(label = name), vjust = 1, hjust = 1) + theme_light()
      
    })
    output$dataTbl <- DT::renderDataTable({
      df <- rv$data_head %>%
        filter(ID %in% rv$relevant_ID)
      datatable(
        df,
        rownames = FALSE,
        class = "nowrap display",
        selection = list(mode = 'single', target = "cell"),
        filter = "bottom",
        extensions = "Buttons",
        options = list(
          paging = TRUE,
          pageLength = 10,
          scrollX = TRUE,
          scrollY = TRUE,
          server = FALSE,
          dom = 'Bfrtip',
          buttons = c('csv', 'excel')
        )
      )
      
    })
    
    output$cellText <- renderPrint({
      dtTbl <- rv$data_head %>%
        filter(ID %in% rv$relevant_ID)
      
      cell <- input$dataTbl_cells_selected
      if (!is.null(cell) && ncol(cell) != 0) {
        dtTbl[cell[1, 1], cell[1, 2] + 1] %>% unlist()
      } else{
        NULL
      }
    })
    
    
    
  })
}


#executes app
shinyApp(ui, server)
