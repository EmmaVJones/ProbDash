# Run in R 3.6.1 (for micromaps)
source('global.R')


# Read in Data
probCDF <- read_csv('data/IR2022/allCDF.csv') %>%
  #read_csv('data/IR2020/allCDF.csv') %>%
  filter(!is.na(Indicator) | !is.na(Subpopulation)) %>% 
  mutate(MoE = StdError.P * 1.96)
# read in geospatial data in server to avoid bringing it in twice
basinssmooth_sp <- readOGR('data/GIS','VAbasins_smoothNoChesPeeDee')# basin shapefile for micromaps (sp)
ecoregions_sp <- readOGR('data/GIS','ecoregions_Micromap')# ecoregion shapefile for micromaps (sp)
vahusb_sp <- readOGR('data/GIS','vahusb_Micromap')# vahusb shapefile for micromaps (sp)

# need this for micromap
map.tableBasin <- create_map_table(basinssmooth_sp,'BASIN')
map.tableEcoregion <- create_map_table(ecoregions_sp,'US_L3NAME')
map.tableVAHUSB <- create_map_table(vahusb_sp,'VAHUSB_')



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ############### Status Tab Panel
  
  #Parameter Choice on UI
  output$parameterChoiceUI <- renderUI({
    selectInput('parameterChoice','Choose a parameter to analyze',
                choices = sort(unique(probCDF$Indicator)))  })
  
  # Select Parameter
  parameter <- reactive({
    req(input$parameterChoice)
    filter(probCDF, Indicator == input$parameterChoice)  })
  
  output$sliderUI <- renderUI({
    req(parameter())
    sliderInput('slider','Choose threshold value (for barplot only)', min = min(parameter()$Value), max = max(parameter()$Value),
                value = median(filter(parameter(), Subpopulation == 'Virginia')$Value))})

  superBasinBox <- reactive({
    req(input$slider, parameter())
    percentileSubpopN(filter(parameter(), Subpopulation %in% superBasinSubpopulations),
                           superBasinSubpopulations, input$slider)  })
  subBasinBox <- reactive({
    req(input$slider, parameter())
    percentileSubpopN(filter(parameter(), Subpopulation %in% subBasinSubpopulations),
                           subBasinSubpopulations, input$slider)  })
  VAHUSBBasinBox <- reactive({
    req(input$slider, parameter())
    percentileSubpopN(filter(parameter(), Subpopulation %in% VAHUSBSubpopulations),
                      VAHUSBSubpopulations, input$slider)  })
  ecoregionBox  <- reactive({
    req(input$slider, parameter())
    percentileSubpopN(filter(parameter(), Subpopulation %in% ecoregionSubpopulations),
                           ecoregionSubpopulations, input$slider)  })
  bioregionBox  <- reactive({
    req(input$slider, parameter())
    percentileSubpopN(filter(parameter(), Subpopulation %in% bioregionSubpopulations),
                           bioregionSubpopulations, input$slider)  })
  streamOrderBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% streamOrderSubpopulations),
                     streamOrderSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order" ), ordered = T)
    return(z)  })
  watershedSizeBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% watershedSizeSubpopulations),
                          watershedSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("<1 square mile", "1 to 10 square mile", "10 to 50 square mile", ">50 square mile"), ordered = T)
    return(z)  })
  streamSizeBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% streamSizeSubpopulations),
                          streamSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("Small", "Medium", "Large"), ordered = T)
    return(z)  })
  IRWindowBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% IRWindowSubpopulations),
                          IRWindowSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = IRwindows, ordered = T)
                                #c("IR2008", "IR2010", "IR2012", "IR2014", "IR2016", "IR2018"  , "IR2020", "IR2022" ), ordered = T)
    return(z)  })
  yearBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% yearSubpopulations),
                          yearSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = years, ordered = T)
    # c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
    #   "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
    #   "Year 2013", "Year 2014", "Year 2015", "Year 2016", "Year 2017", "Year 2018",
    #   "Year 2019", "Year 2020"), ordered = T)
    return(z)  })
  bayNonBayBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% bayNonBaySubpopulations),
                          bayNonBaySubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  c(paste0('Bay Watersheds ',panelWindow[1]), paste0('Non-Bay Watersheds ',panelWindow[1]),
                                                           paste0('Bay Watersheds ',panelWindow[2]), paste0('Non-Bay Watersheds ',panelWindow[2]),
                                                           paste0('Bay Watersheds ',panelWindow[3]), paste0('Non-Bay Watersheds ',panelWindow[3])), ordered = T)
                                # c("Bay Watersheds 2001-2018", "Non-Bay Watersheds 2001-2018", 
                                #   "Bay Watersheds 2001-2008", "Non-Bay Watersheds 2001-2008",
                                #   "Bay Watersheds 2009-2018", "Non-Bay Watersheds 2009-2018"), ordered = T)
    return(z)  })
  VSCIyearBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% VSCIyearSubpopulations),
                          VSCIyearSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  paste0('VSCI Scores ',bioPanelWindow), ordered = T)
                                # c("VSCI Scores 2001-2004", "VSCI Scores 2005-2008",
                                #   "VSCI Scores 2009-2013", "VSCI Scores 2014-2018"), ordered = T)
    return(z)  })
  biophaseBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% biophaseSubpopulations),
                          biophaseSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =   c(paste0("Phase One ", panelWindow[2]), paste0("Phase Two ", panelWindow[3])), ordered = T)
                                # c("Phase One 2001-2008", "Phase Two 2009-2018"), ordered = T)
    return(z)  })
  biophaseXBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% biophaseXStreamSizeSubpopulations),
                          biophaseXStreamSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  c("Phase1Small", "Phase2Small", "Phase1Medium",
                                                           "Phase2Medium", "Phase1Large", "Phase2Large"), ordered = T)
    return(z)  })
  
 
  
  # Status Tabs
  callModule(statusSuperbasin,'super', parameter, superBasinBox, reactive(input$slider))
  callModule(statusSubbasin,'sub', parameter, subBasinBox, reactive(input$slider))
  callModule(statusVAHUSB,'vahusb', parameter, VAHUSBBasinBox, reactive(input$slider))
  callModule(statusEcoregion,'eco', parameter, ecoregionBox, reactive(input$slider))
  callModule(statusBioregion,'bio', parameter, bioregionBox, reactive(input$slider))
  callModule(statusStreamOrder,'streamOrder', parameter, streamOrderBox, reactive(input$slider))
  callModule(statusWatershedSize,'watershedSize', parameter, watershedSizeBox, reactive(input$slider))
  callModule(statusStreamSize,'streamSize', parameter, streamSizeBox, reactive(input$slider))

  # Trend Tabs
  callModule(trendIRWindow,'IRWindow', parameter, IRWindowBox, reactive(input$slider), IRWindowSubpopulations) ## Still need to manually update MoE
  callModule(trendYear,'year', parameter, yearBox, yearSubpopulations)## Still need to manually update MoE
  callModule(trendBayNonBay,'bayNonBay', parameter, bayNonBayBox, reactive(input$slider), bayNonBaySubpopulations)
  callModule(trendVSCIyear,'VSCIyear', parameter, VSCIyearBox, reactive(input$slider), VSCIyearSubpopulations)
  callModule(trendBiophase,'biophase', parameter, biophaseBox, reactive(input$slider), biophaseSubpopulations)
  callModule(trendBiophaseXStreamSize,'biophaseX', parameter, biophaseXBox, reactive(input$slider))
  
  
  
  
  ############### Micromap Tab Panel
  
  #Parameter Choice on UI
  output$micromapParameterChoiceUI <- renderUI({
    selectInput('micromapParameterChoice','Choose a parameter to analyze',
                choices = sort(unique(probCDF$Indicator)))  })
  
  output$micromapIRbasin <- renderPlot({req(input$micromapRadio, input$micromapParameterChoice)
    if(input$micromapRadio == 'Interquartile Range'){
      micromapInterquartileRange(dataOrgInterquartileRange(probCDF,
                                                           subBasinSubpopulations,
                                                           input$micromapParameterChoice) %>%
                                   mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ "Roanoke",
                                                                    Subpopulation == 'James Basin' ~ "James",
                                                                    TRUE ~ as.character(Subpopulation)),
                                          Subpopulation = as.factor(Subpopulation),
                                          Indicator = as.factor(Indicator)),
                                 map.tableBasin,
                                 indicatorRanges = indicatorRanges,
                                 dropVA = input$micromapDropVA)    }    })
  
  output$micromapIReco <- renderPlot({req(input$micromapRadio, input$micromapParameterChoice)
    if(input$micromapRadio == 'Interquartile Range'){
      micromapInterquartileRange(dataOrgInterquartileRange(probCDF,
                                                           c('Virginia', ecoregionSubpopulations),
                                                           input$micromapParameterChoice) %>%
                                   mutate(Subpopulation = case_when(Subpopulation == "Central Appalachian Ridges and Valleys" ~ "Ridge and Valley",
                                                                    Subpopulation == "Blue Ridge Mountains" ~ "Blue Ridge",
                                                                    TRUE ~ as.character(Subpopulation)),
                                          Subpopulation = as.factor(Subpopulation),
                                          Indicator = as.factor(Indicator)),
                                 map.tableEcoregion,
                                 indicatorRanges = indicatorRanges,
                                 dropVA = input$micromapDropVA) } })
    
  
  output$micromapIRvahusb <- renderPlot({req(input$micromapRadio, input$micromapParameterChoice)
    if(input$micromapRadio == 'Interquartile Range'){
      micromapInterquartileRange(dataOrgInterquartileRange(probCDF,
                                                           VAHUSBSubpopulations,
                                                           input$micromapParameterChoice) %>%
                                   mutate(Subpopulation = as.factor(Subpopulation),
                                          Indicator = as.factor(Indicator)),
                                 map.tableVAHUSB,
                                 indicatorRanges = indicatorRanges,
                                 dropVA = input$micromapDropVA) } })
      
  
  
  
  ############### Raw Data Tab
  output$rawData <- DT::renderDataTable({
    DT::datatable(probCDF)
  })
})