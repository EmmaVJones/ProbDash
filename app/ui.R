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
                                          sidebarPanel(width = 3,
                                            uiOutput('parameterChoiceUI'),
                                            radioButtons('radio','Analyze by...', choices = c('Status','Trend')),
                                            uiOutput('sliderUI')
                                          ),
                                          mainPanel(
                                            conditionalPanel(condition = "input.radio == 'Status'",
                                                             tabsetPanel(
                                                               tabPanel('Superbasin', statusSuperbasinUI('super')),
                                                               tabPanel('Subbasin', statusSubbasinUI('sub')),
                                                               tabPanel('Ecoregion', statusEcoregionUI('eco')),
                                                               tabPanel('Bioregion', statusBioregionUI('bio')),
                                                               tabPanel('Stream Order', statusStreamOrderUI('streamOrder')),
                                                               tabPanel('Watershed Size', statusWatershedSizeUI('watershedSize')),
                                                               tabPanel('Stream Size', statusStreamSizeUI('streamSize')))  ),
                                            conditionalPanel(condition = "input.radio == 'Trend'",
                                                             tabsetPanel(
                                                               tabPanel('IR Window'),# statusSuperbasinUI('super')),
                                                               tabPanel('Year'),
                                                               tabPanel('Bay / NonBay'),
                                                               tabPanel('VSCI by Year'),
                                                               tabPanel('Biophase'),
                                                               tabPanel('Biophase x Stream Size')))
                                                               
                                            #verbatimTextOutput('verbatim'),
                                            
                                          )),
                                 tabPanel('Raw Data'),
                                 tabPanel('About')
                      )))))