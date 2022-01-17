
trendBayNonBayUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','Boxplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))
    )
  )
}



trendBayNonBay <- function(input,output,session, parameterData, barData, thresholdValue, bayNonBaySubpopulations){
  ns <- session$ns
  
  cdfData <- reactive({
    req(parameterData)
    z <- filter(parameterData(), Subpopulation %in% bayNonBaySubpopulations)  %>%
      mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE) 
    z$Subpopulation <- factor(z$Subpopulation, levels =  bayNonBaySubpopulations, ordered = T)
    # c("Bay Watersheds 2001-2018", "Non-Bay Watersheds 2001-2018", 
    #                                                        "Bay Watersheds 2001-2008", "Non-Bay Watersheds 2001-2008",
    #                                                        "Bay Watersheds 2009-2018", "Non-Bay Watersheds 2009-2018"), ordered = T)
    return( z)})
  
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
        addMoE(cdfData(),bayNonBaySubpopulations[1]) %>% 
        addMoE(cdfData(),bayNonBaySubpopulations[2]) %>%
        addMoE(cdfData(),bayNonBaySubpopulations[3]) %>%
        addMoE(cdfData(),bayNonBaySubpopulations[4]) %>%
        addMoE(cdfData(),bayNonBaySubpopulations[5]) %>%
        addMoE(cdfData(),bayNonBaySubpopulations[6]) %>%
        # addMoE(cdfData(),"Bay Watersheds 2001-2018") %>%
        # addMoE(cdfData(),"Non-Bay Watersheds 2001-2018") %>%
        # addMoE(cdfData(),"Bay Watersheds 2001-2008") %>%
        # addMoE(cdfData(),"Non-Bay Watersheds 2001-2008") %>%
        # addMoE(cdfData(),"Bay Watersheds 2009-2018") %>%
        # addMoE(cdfData(),"Non-Bay Watersheds 2009-2018") %>%
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
                                                  ' +/- ', format(MoE, digits = 3)),
                                            paste("n: ", n))) %>%
        layout(#showlegend=FALSE,
          yaxis=list(title="Percentile"),
          xaxis=list(title="Subpopulation"))
    }
    
  })
}
