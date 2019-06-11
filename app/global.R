library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(DT)








# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}


