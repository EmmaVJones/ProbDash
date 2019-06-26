statusSuperbasinUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))
    )
  )
}



statusSuperbasin <- function(input,output,session, parameterData, thresholdValue){
  ns <- session$ns
  
  superB <- reactive({
    req(parameterData)
    filter(parameterData, Subpopulation %in% c('Virginia',"Roanoke Basin","James Basin",
                                               "Potomac-Shenandoah","Rappahannock-York")) %>%#,"James Basin",
                                               #"Potomac-Shenandoah","Rappahannock-York",
                                               #"New","Chowan","Tennessee"))  %>%
      mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE) })
  
  superBox <- reactive({
    req(thresholdValue())
    print(percentileSubpop(superB(), c('Virginia',"Roanoke Basin","James Basin",
                                       "Potomac-Shenandoah","Rappahannock-York"), thresholdValue()) )})
  #,"Roanoke Basin","James Basin",
                                       #"Potomac-Shenandoah","Rappahannock-York",
                                       #"New","Chowan","Tennessee"), thresholdValue())  )})
  
  
  output$plot <- renderPlotly({
    if(input$radio == 'CDF'){
      plot_ly(superB()) %>%
        add_trace(data = superB(), x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
                  hoverinfo = 'text', text = ~paste(sep = '<br>',
                                                    paste("Subpopulation: ", Subpopulation),
                                                    paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                                    paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
        
        add_ribbons(data = filter(superB(), Subpopulation== 'Virginia'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Virginia Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'Chowan'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Chowan Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'James Basin'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "James Basin Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'New Basin'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "New Basin Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'Potomac-Shenandoah'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Potomac-Shenandoah Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'Rappahannock-York'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Rappahannock-York Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'Roanoke Basin'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Roanoke Basin Margin of Error", visible = 'legendonly') %>%
        add_ribbons(data = filter(superB(), Subpopulation== 'Tennessee'),
                    x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
                    fillcolor = 'rgba(7, 164, 181, 0.2)', name = "Tennessee Margin of Error", visible = 'legendonly') %>%
        layout(#showlegend=FALSE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation"))
    } else {
      plot_ly(superBox(), x = ~Subpopulation, y = ~Percentile, type = 'bar', 
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