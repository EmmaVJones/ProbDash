# Run in R 3.6.1 (for micromap)
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
                                             This tool accepts data output directly from the 'HowToWrite2022IRProbMonChapter.Rmd' script, the 
                                             same script that feeds into the automated reporting script, to streamline the analysis process prior
                                             to autogenerating a final report."),
                                          h5("It is advised that users work through the parameters of interest using the tabs on the status and 
                                             trend subpopulation classes."),
                                          h3(strong('Pro Tip: The threshold value only controls barplots.')),
                                          h3(strong('Pro Tip: You can turn layers on and off in the interactive plotly plots by clicking on them in the legend.')),
                                          br(), hr(), br(),
                                          h4('Please contact Emma Jones (emma.jones@deq.virginia.gov) with any questions or concerns about the app.') ),
                                 tabPanel('Status and Trend Plots',
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
                                                               tabPanel('VAHUSB', statusVAHUSBUI('vahusb')),
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
                                 tabPanel('Micromap Plots',
                                          sidebarPanel(width = 3,
                                                       uiOutput('micromapParameterChoiceUI'),
                                                       radioButtons('micromapRadio','Plot by...', choices = c('Interquartile Range',
                                                                                                              'Percent Suboptimal',
                                                                                                              'Percent Optimal')),
                                                       radioButtons('micromapDropVA', 'Drop Statewide estimate from Panel?',
                                                                    choices = c(TRUE, FALSE))),
                                          mainPanel(
                                            tabsetPanel(
                                              tabPanel('Subbasin Micromap', 
                                                       helpText('Vertical dashed line represents the Virginia median estimate. For assistance reading
                                                                a micromap, see the `Micromap Help Page` tab.'),
                                                       plotOutput("micromapIRbasin", width = "75%", height = "700px") ),
                                              tabPanel('VAHUSB Micromap',
                                                       h5(strong("Note: not all VAHUSB's are represented due to a lack of data in missing VAHUSB to draw
                                                                 meaningful CDF estimates. These basins will be included in future analyses as data density
                                                                 increases in these VAHUSB's.")),
                                                       helpText('Vertical dashed line represents the Virginia median estimate. For assistance reading
                                                                a micromap, see the `Micromap Help Page` tab.'),
                                                       plotOutput("micromapIRvahusb", width = "100%", height = "700px")),
                                              tabPanel('Ecoregion Micromap', 
                                                       helpText('Vertical dashed line represents the Virginia median estimate. For assistance reading
                                                                a micromap, see the `Micromap Help Page` tab.'),
                                                       plotOutput("micromapIReco", width = "75%", height = "700px")),
                                              tabPanel('Micromap Help Page', 
                                                       p('To understand a micromap, begin at the top panel where the top three subpopulations are highlighted. 
                                                       Moving from left to right, the n column specifies how many samples were collected in each subpopulation. 
                                                       The dotplot identifies the median indicator measurements in each subpopulation with a solitary dot and the 
                                                       interquartile range, a measure of variability in the dataset between the 25th and 75th percentile, 
                                                       illustrated by a horizontal line spanning these percentiles. The dashed vertical line highlights the
                                                       Virginia median estimate for the given indicator. On the far right, a map illustrates which subpopulations 
                                                       are described statistically in that panel with colors corresponding to the dot left of the subpopulation
                                                       name. Subpopulations are grayed out in lower maps to indicate they are highlighted in a map above. 
                                                       Because the subpopulations are ordered by median results, subpopulations at the top of the micromap 
                                                       have the highest median for a given indicator and subpopluations at the bottom have the lowest medians.'),
                                                       p('Interquartile range micromaps indicate the central tendencies of the data presented with smaller interquartile
                                                         ranges (shorter horizontal bars) indicating less variability of that indicator across the given subpopulation.'),
                                                       p('Percent Suboptimal and Percent Optimal micromaps allow for broad generalizations to be drawn across indicators
                                                         and subpopulations, but do not indicate any causal relationships.') )
                                              ))),
                                                               
                                          
                                 tabPanel('Raw Data',
                                          DT::dataTableOutput('rawData'))
                                 
                      )))))