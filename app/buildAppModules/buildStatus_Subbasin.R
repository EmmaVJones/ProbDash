source('global.R')

# Read in Data
probCDF <- read_csv('data/allCDF.csv') %>%
  filter(!is.na(Indicator) | !is.na(Subpopulation)) %>% 
  mutate(MoE = StdError.P * 1.96)


parameterData1 <- filter(probCDF, Indicator == 'DO')

parameterData2 <- filter(parameterData1, Subpopulation %in% c('Virginia',"Roanoke Basin","James Basin",
                                                              "Potomac","Shenandoah","Rappahannock","York",
                                                              "New","Chowan","Holston", "Big Sandy", "Clinch-Powell")) %>%
  mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE)

plot_ly(parameterData2) %>%
  add_ribbons(data = filter(parameterData2_error, Subpopulation== 'Virginia'),
              x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Virginia Margin of Error", visible = 'legendonly') %>%
  add_ribbons(data = filter(parameterData2_error, Subpopulation== 'Roanoke Basin'),
              x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Roanoke Basin Margin of Error", visible = 'legendonly') %>%
  add_trace(data = parameterData2, x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
              #error_y = ~list(array = MoE),
            hoverinfo = 'text', text = ~paste(sep = '<br>',
                                              paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                              paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) 


parameterData3 <- percentileSubpop(parameterData2, c('Virginia',"Roanoke Basin","James Basin",
                                                     "Potomac","Shenandoah","Rappahannock","York",
                                                     "New","Chowan","Holston", "Big Sandy", "Clinch-Powell"), 9)
plot_ly(parameterData3, x = ~Subpopulation, y = ~Percentile, type = 'bar', 
        width = .10,
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("Subpopulation: ", Subpopulation),
                                      #paste("Threshold Value: ",thresholdValue()),
                                      paste("Percentile: ", Percentile, ' +/- ', MoE))) %>%
  layout(#showlegend=FALSE,
    yaxis=list(title="Percentile"),
    xaxis=list(title="Subpopulation"))






statusSubbasinUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))
    )
  )
}



statusSubbasin <- function(input,output,session, parameterData, thresholdValue){
  ns <- session$ns
  
  subB <- reactive({
    req(parameterData)
    filter(parameterData, Subpopulation %in% c('Virginia',"Roanoke Basin","James Basin",
                                               "Potomac","Shenandoah","Rappahannock","York",
                                               "New","Chowan","Holston", "Big Sandy", "Clinch-Powell")) %>%
      mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE)  })
  
  subBox <- reactive({
    req(thresholdValue())
    percentileSubpop(subB(), c('Virginia',"Roanoke Basin","James Basin",
                                 "Potomac","Shenandoah","Rappahannock","York",
                                 "New","Chowan","Holston", "Big Sandy", "Clinch-Powell"), thresholdValue())  })
  
  
  output$plot <- renderPlotly({
    if(input$radio == 'CDF'){
      plot_ly(subB()) %>%
        add_trace(data = subB(), x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
                  hoverinfo = 'text', text = ~paste(sep = '<br>',
                                                    paste("Subpopulation: ", Subpopulation),
                                                    paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                                    paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Virginia'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Virginia Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Big Sandy'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Big Sandy Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Chowan'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Chowan Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Clinch-Powell'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Clinch-Powell Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Holston'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Holston Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'James Basin'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "James Basin Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'New Basin'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "New Basin Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Potomac'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Potomac Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Rappahannock'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Rappahannock Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Roanoke Basin'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Roanoke Basin Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'Shenandoah'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Shenandoah Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(subB(), Subpopulation== 'York'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "York Margin of Error", visible = 'legendonly') %>%
        
        layout(#showlegend=FALSE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation"))
        
    } else {
      plot_ly(subBox(), x = ~Subpopulation, y = ~Percentile, type = 'bar', 
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

ui <- fluidPage( uiOutput('sliderUI'),
                 statusSubbasinUI('sub')  )

server <- function(input,output,session){
  output$sliderUI <- renderUI({
    #req(parameter())
    sliderInput('slider','Choose threshold value', min = min(parameterData1$Value), max = max(parameterData1$Value),
                value = median(filter(parameterData1, Subpopulation == 'Virginia')$Value))})
  
  callModule(statusSubbasin,'sub', parameterData1, reactive(input$slider))
  
  
}

shinyApp(ui, server)

