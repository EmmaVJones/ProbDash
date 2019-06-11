# Run in R 3.5.2
source('global.R')

shinyUI(fluidPage(theme="yeti.css",
                  shinyjs::useShinyjs(),
                  div(
                    id = "loading_page",
                    h1("Loading...")
                  ),
                  hidden(
                    div(
                      id = "main_content",
                      # suppress error messages as data loads, hacky
                      tags$style(type="text/css",
                                 ".shiny-output-error { visibility: hidden; }",
                                 ".shiny-output-error:before { visibility: hidden; }"
                      ),
                      navbarPage("VDEQ Probabilistic Monitoring EDA Tool",
                                 tabPanel('Dashboard',
                                          sidebarPanel(
                                            br(),br(),
                                            selectInput('parameterChoice','Choose a parameter to analyze',
                                                        choices = unique(probCDF$Indicator)),
                                            radioButtons('radio','Choose a XXX', choices = c('Status','Trend','Percentile'))
                                          ),
                                          mainPanel(
                                            uiOutput('sliderUI'), br(),hr(), br(),
                                            verbatimTextOutput('verbatim')
                                          )),
                                 tabPanel('Raw Data'),
                                 tabPanel('About')
                      )))))