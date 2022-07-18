#Load libraries ----
library(shiny)
library(tidyverse)
library(tidytext)
library(ggraph)
library(igraph)

# Source functions ----
source("//Users/ravihela/Documents/mastering_shiny/wrd_freq_df.R")
source("//Users/ravihela/Documents/mastering_shiny/bigrm_df.R")

# UI design ----
ui <- fluidPage(fluidRow(column(2, fileInput())),
                column(2, actionButton())),
fluidRow(column(2, numericInput()),
         column(
           2,
           selectizeInput(),
           plotOutput("wrd_frq_plot"),
           plotOutput("bigrm_frq_plot")
         )),
column(
  8,
  sliderInput(),
  plotOutput("bigrm_nt"),
  column(6, dataTableOutput("dataTbl"))
  
  
  #executes app
  shinyApp(ui, server)
  