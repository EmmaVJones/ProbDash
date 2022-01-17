statusVAHUSBUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','Boxplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))#, width = "600px", height = "600px")
    )
  )
}



statusVAHUSB <- function(input,output,session, parameterData, barData, thresholdValue){
  ns <- session$ns
  
  cdfData <- reactive({
    req(parameterData)
    filter(parameterData(), Subpopulation %in% VAHUSBSubpopulations)  %>%
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
        addMoE(cdfData(),VAHUSBSubpopulations[1]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[2]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[3]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[4]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[5]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[6]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[7]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[8]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[9]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[10]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[11]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[12]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[13]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[14]) %>%
        addMoE(cdfData(),VAHUSBSubpopulations[15]) %>%
        layout(#showlegend=FALSE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation"))
    } else if(input$radio %in% 'Barplot'){
      plot_ly(barData(), x = ~Subpopulation, y = ~Percentile,color =  ~as.factor(Subpopulation),  colors = ~"#2074b4",
              type = 'bar', 
              width = .10,
              hoverinfo="text", text=~paste(sep="<br>",
                                            paste("Subpopulation: ", Subpopulation),
                                            #paste("Threshold Value: ",thresholdValue()),
                                            paste("Percentile: ", format(Percentile, digits = 3), 
                                                  ' +/- ', format(MoE, digits = 3)),
                                            paste("n: ", n))) %>%
        layout(showlegend=TRUE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation", tickangle = 45))
    }
  })
}