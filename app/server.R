# Run in R 3.6.0
source('global.R')


# Read in Data
probCDF <- read_csv('data/allCDF.csv') %>%
  filter(!is.na(Indicator) | !is.na(Subpopulation)) %>% 
  mutate(MoE = StdError.P * 1.96)



shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
  ############### Status Tab Panel
  
  #Parameter Choice on UI
  output$parameterChoiceUI <- renderUI({
    selectInput('parameterChoice','Choose a parameter to analyze',
                choices = unique(probCDF$Indicator))
  })
  
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
    percentileSubpop(filter(parameter(), Subpopulation %in% superBasinSubpopulations),
                           superBasinSubpopulations, input$slider)  })
  subBasinBox <- reactive({
    req(input$slider, parameter())
    percentileSubpop(filter(parameter(), Subpopulation %in% subBasinSubpopulations),
                           subBasinSubpopulations, input$slider)  })
  ecoregionBox  <- reactive({
    req(input$slider, parameter())
    percentileSubpop(filter(parameter(), Subpopulation %in% ecoregionSubpopulations),
                           ecoregionSubpopulations, input$slider)  })
  bioregionBox  <- reactive({
    req(input$slider, parameter())
    percentileSubpop(filter(parameter(), Subpopulation %in% bioregionSubpopulations),
                           bioregionSubpopulations, input$slider)  })
  streamOrderBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% streamOrderSubpopulations),
                     streamOrderSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order" ), ordered = T)
    return(z)  })
  watershedSizeBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% watershedSizeSubpopulations),
                          watershedSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("<1 square mile", "1 to 10 square mile", "10 to 200 square mile", ">200 square mile"), ordered = T)
    return(z)  })
  streamSizeBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% streamSizeSubpopulations),
                          streamSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("Small", "Medium", "Large"), ordered = T)
    return(z)  })
  IRWindowBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% IRWindowSubpopulations),
                          IRWindowSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("IR2008", "IR2010", "IR2012", "IR2014", "IR2016", "IR2018"  ), ordered = T)
    return(z)  })
  yearBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% yearSubpopulations),
                          yearSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
                                                          "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
                                                          "Year 2013", "Year 2014", "Year 2015", "Year 2016"), ordered = T)
    return(z)  })
  bayNonBayBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% bayNonBaySubpopulations),
                          bayNonBaySubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  c("Bay Watersheds 2001-2016", "Non-Bay Watersheds 2001-2016", 
                                                           "Bay Watersheds 2001-2009", "Non-Bay Watersheds 2001-2009",
                                                           "Bay Watersheds 2009-2016", "Non-Bay Watersheds 2009-2016"), ordered = T)
    return(z)  })
  VSCIyearBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% VSCIyearSubpopulations),
                          VSCIyearSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  c("VSCI Scores 2001-2004", "VSCI Scores 2005-2008",
                                                           "VSCI Scores 2009-2012", "VSCI Scores 2013-2016"), ordered = T)
    return(z)  })
  biophaseBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% biophaseSubpopulations),
                          biophaseSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  c("Phase One 2001-2009", "Phase Two 2009-2016"), ordered = T)
    return(z)  })
  biophaseXBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% biophaseXStreamSizeSubpopulations),
                          biophaseXStreamSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels =  c("Phase1Small", "Phase2Small", "Phase1Medium",
                                                           "Phase2Medium", "Phase1Large", "Phase2Large"), ordered = T)
    return(z)  })
  
 
  
  # Status Tabs
  callModule(statusSuperbasin,'super', parameter, superBasinBox, reactive(input$slider))
  callModule(statusSubbasin,'sub', parameter, subBasinBox, reactive(input$slider))
  callModule(statusEcoregion,'eco', parameter, ecoregionBox, reactive(input$slider))
  callModule(statusBioregion,'bio', parameter, bioregionBox, reactive(input$slider))
  callModule(statusStreamOrder,'streamOrder', parameter, streamOrderBox, reactive(input$slider))
  callModule(statusWatershedSize,'watershedSize', parameter, watershedSizeBox, reactive(input$slider))
  callModule(statusStreamSize,'streamSize', parameter, streamSizeBox, reactive(input$slider))

  # Trend Tabs
  callModule(trendIRWindow,'IRWindow', parameter, IRWindowBox, reactive(input$slider))
  callModule(trendYear,'year', parameter, yearBox, reactive(input$slider))
  callModule(trendBayNonBay,'bayNonBay', parameter, bayNonBayBox, reactive(input$slider))
  callModule(trendVSCIyear,'VSCIyear', parameter, VSCIyearBox, reactive(input$slider))
  callModule(trendBiophase,'biophase', parameter, biophaseBox, reactive(input$slider))
  callModule(trendBiophaseXStreamSize,'biophaseX', parameter, biophaseXBox, reactive(input$slider))
  
  
  
  #### Raw Data Tab
  output$rawData <- DT::renderDataTable({
    DT::datatable(probCDF)
  })
})