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
                                 tabPanel('About',
                                          h5("This tool is meant to expedite the data analysis process for each Integrated Reporting cycle.
                                             This tool accepts data output directly from the 'HowToWrite2018IRProbMonChapter.Rmd' script, the 
                                             same script that feeds into the automated reporting script, to streamline the analysis process prior
                                             to autogenerating a final report."),
                                          h5("It is advised that users work through the parameters of interest using the tabs on the status and 
                                             trend subpopulation classes."),
                                          h3(strong('Pro Tip: The threshold value only controls barplots.')),
                                          h3(strong('Pro Tip: You can turn layers on and off in the interactive plotly plots by clicking on them in the legend.')),
                                          br(), hr(), br(),
                                          h4('Please contact Emma Jones (emma.jones@deq.virginia.gov) with any questions or concerns about the app.') ),
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
                                                               tabPanel('IR Window', trendIRWindowUI('IRWindow')),
                                                               tabPanel('Year', trendYearUI('year')),
                                                               tabPanel('Year Groups', trendVSCIyearUI('VSCIyear')),
                                                               tabPanel('Biophase', trendBiophaseUI('biophase')),
                                                               tabPanel('Bay / NonBay', trendBayNonBayUI('bayNonBay')),
                                                               tabPanel('Biophase x Stream Size', trendBiophaseXStreamSizeUI('biophaseX'))))
                                                               
                                            #verbatimTextOutput('verbatim'),
                                            
                                          )),
                                 tabPanel('Raw Data',
                                          DT::dataTableOutput('rawData'))
                                 
                      )))))