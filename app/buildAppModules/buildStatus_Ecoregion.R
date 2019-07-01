
statusEcoregionUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','Boxplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))
    )
  )
}



statusEcoregion <- function(input,output,session, parameterData, barData, thresholdValue){
  ns <- session$ns
  
  cdfData <- reactive({
    req(parameterData)
    filter(parameterData(), Subpopulation %in% ecoregionSubpopulations)  %>%
      mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE) })
  
  output$plot <- renderPlotly({
    
    if(input$radio %in% 'Boxplot'){
      plot_ly(cdfData(), x = ~Subpopulation, y = ~Value,  color = ~Subpopulation, type = "box") %>%
        layout( yaxis=list(title="Value"),xaxis=list(title="Subpopulation"))
    } else if(input$radio %in% 'CDF'){
      plot_ly(cdfData()) %>%
        add_trace(data = cdfData(), x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
                  hoverinfo = 'text', text = ~paste(sep = '<br>',
                                                    paste("Subpopulation: ", Subpopulation),
                                                    paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                                    paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
        addMoE(cdfData(),'Blue Ridge Mountains') %>%
        addMoE(cdfData(),'Central Appalachian Ridges and Valleys') %>%
        addMoE(cdfData(),'Central Appalachians') %>%
        addMoE(cdfData(),'Northern Piedmont') %>%
        addMoE(cdfData(),'Piedmont') %>%
        addMoE(cdfData(),'Southeastern Plains') %>%
        layout(#showlegend=FALSE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation"))
    } else if(input$radio %in% 'Barplot'){
      plot_ly(barData(), x = ~Subpopulation, y = ~Percentile, type = 'bar', 
              width = .10,
              hoverinfo="text", text=~paste(sep="<br>",
                                            paste("Subpopulation: ", Subpopulation),
                                            #paste("Threshold Value: ",thresholdValue()),
                                            paste("Percentile: ", format(Percentile, digits = 3), 
                                                  ' +/- ', format(MoE, digits = 3)))) %>%
        layout(#showlegend=FALSE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation"))
    }
    
  })
}

ui <- fluidPage( uiOutput('parameterChoiceUI'),
                 uiOutput('sliderUI'),
                 statusEcoregionUI('eco') ,
                 dataTableOutput('table'))

server <- function(input,output,session){
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
    sliderInput('slider','Choose threshold value', min = min(parameter()$Value), max = max(parameter()$Value),
                value = median(filter(parameter(), Subpopulation == 'Virginia')$Value))})
  
  ecoregionBox  <- reactive({
    req(input$slider, parameter())
    print(percentileSubpop(filter(parameter(), Subpopulation %in% ecoregionSubpopulations),
                           ecoregionSubpopulations, input$slider)  )})
  
  
  callModule(statusEcoregion,'eco', parameter, ecoregionBox, reactive(input$slider))
  
  output$table <- renderDataTable({
    DT::datatable( parameter()) })
  
}

shinyApp(ui, server)

