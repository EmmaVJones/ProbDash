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
  
  # Select Parameter
  parameter <- reactive({
    req(input$parameterChoice)
    filter(probCDF, Indicator == input$parameterChoice)  })
  
  output$sliderUI <- renderUI({
    req(parameter())
    sliderInput('slider','Choose threshold value', min = min(parameter()$Value), max = max(parameter()$Value),
                value = median(filter(parameter(), Subpopulation == 'Virginia')$Value))})
  
  output$verbatim <- renderPrint({
    req(parameter)
    head(parameter())
  })
  
  
})