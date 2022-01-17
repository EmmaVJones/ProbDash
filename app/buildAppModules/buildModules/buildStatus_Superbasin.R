source('global.R')

# Read in Data
probCDF <- read_csv('data/allCDF.csv') %>%
  filter(!is.na(Indicator) | !is.na(Subpopulation)) %>% 
  mutate(MoE = StdError.P * 1.96)


parameterData1 <- filter(probCDF, Indicator == 'pH')

parameterData2 <- filter(parameterData1, Subpopulation %in% c('Virginia',"Roanoke Basin","James Basin",
                                                             "Potomac-Shenandoah","Rappahannock-York",
                                                             "New","Chowan","Tennessee"))  %>%
  mutate(ymin = Estimate.P - (MoE)/2, ymax = Estimate.P + (MoE)/2)

plot_ly(parameterData2) %>%
  add_trace(data = parameterData2, x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
            hoverinfo = 'text', text = ~paste(sep = '<br>',
                                             paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                             paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) )))


parameterData3 <- percentileSubpop(parameterData2, c('Virginia',"Roanoke Basin","James Basin",
                             "Potomac-Shenandoah","Rappahannock-York",
                             "New","Chowan","Tennessee"), 9.6)
plot_ly(parameterData3, x = ~Subpopulation, y = ~Percentile, type = 'bar', 
        width = .10,
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("Subpopulation: ", Subpopulation),
                                      #paste("Threshold Value: ",thresholdValue()),
                                      paste("Percentile: ", Percentile, ' +/- ', MoE))) %>%
  layout(#showlegend=FALSE,
    yaxis=list(title="Percentile"),
    xaxis=list(title="Subpopulation"))

# boxplot
plot_ly(parameterData2, x = ~Subpopulation, y = ~Value, color = ~Subpopulation, type = "box") %>%
  layout(showlegend=TRUE, yaxis=list(title="Value"),
         xaxis=list(title="Subpopulation"))



superBasinSubpopulations <- c('Virginia',"Roanoke Basin","James Basin",
                              "Potomac-Shenandoah","Rappahannock-York",
                              "New","Chowan","Tennessee")
addMoE <- function(p, dataset, subpopulation){
  add_ribbons(p, data = filter(dataset, Subpopulation== subpopulation),
              x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(7, 164, 181, 0.2)', name = paste(subpopulation," Margin of Error",sep=""), visible = 'legendonly')
  
}


statusSuperbasinUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','Boxplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))
    )
  )
}
    
      

statusSuperbasin <- function(input,output,session, parameterData, barData, thresholdValue){
  ns <- session$ns
  
  cdfData <- reactive({
    req(parameterData)
    filter(parameterData(), Subpopulation %in% superBasinSubpopulations)  %>%
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
        addMoE(cdfData(),'Virginia') %>%
        addMoE(cdfData(),'Chowan') %>%
        addMoE(cdfData(),'James Basin') %>%
        addMoE(cdfData(),'New') %>%
        addMoE(cdfData(),'Potomac-Shenandoah') %>%
        addMoE(cdfData(),'Rappahannock-York') %>%
        addMoE(cdfData(),'Roanoke Basin') %>%
        addMoE(cdfData(),'Tennessee') %>%
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
                 statusSuperbasinUI('super') ,
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
    #req(parameter())
    sliderInput('slider','Choose threshold value', min = min(parameter()$Value), max = max(parameter()$Value),
                value = median(filter(parameter(), Subpopulation == 'Virginia')$Value))})
 
  bar <- reactive({
    req(input$slider, parameter())
    print(percentileSubpop(filter(parameter(), Subpopulation %in% superBasinSubpopulations),
                           superBasinSubpopulations, input$slider)  )})
  
  
  callModule(statusSuperbasin,'super', parameter, bar, reactive(input$slider))

  output$table <- renderDataTable({
    DT::datatable( parameter()) })

}

shinyApp(ui, server)

