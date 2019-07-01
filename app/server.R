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
    return(z)
  })
  watershedSizeBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% watershedSizeSubpopulations),
                          watershedSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("<1 square mile", "1 to 10 square mile", "10 to 200 square mile", ">200 square mile"), ordered = T)
    return(z)
  })
  streamSizeBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpop(filter(parameter(), Subpopulation %in% streamSizeSubpopulations),
                          streamSizeSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = c("Small", "Medium", "Large"), ordered = T)
    return(z)
  })
  
 
  
  
  callModule(statusSuperbasin,'super', parameter, superBasinBox, reactive(input$slider))
  callModule(statusSubbasin,'sub', parameter, subBasinBox, reactive(input$slider))
  callModule(statusEcoregion,'eco', parameter, ecoregionBox, reactive(input$slider))
  callModule(statusBioregion,'bio', parameter, bioregionBox, reactive(input$slider))
  callModule(statusStreamOrder,'streamOrder', parameter, streamOrderBox, reactive(input$slider))
  callModule(statusWatershedSize,'watershedSize', parameter, watershedSizeBox, reactive(input$slider))
  callModule(statusStreamSize,'streamSize', parameter, streamSizeBox, reactive(input$slider))
  
})