
trendYearUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','Boxplot','CDF')),
      br(),
      plotlyOutput(ns('plot'))
    )
  )
}



trendYear <- function(input,output,session, parameterData, barData, thresholdValue){
  ns <- session$ns
  
  cdfData <- reactive({
    req(parameterData)
    z <- filter(parameterData(), Subpopulation %in% yearSubpopulations)  %>%
      mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE) 
    z$Subpopulation <- factor(z$Subpopulation, levels = c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
                                                          "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
                                                          "Year 2013", "Year 2014", "Year 2015", "Year 2016"), ordered = T)
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
        addMoE(cdfData(),"Year 2001") %>%
        addMoE(cdfData(),"Year 2002") %>%
        addMoE(cdfData(),"Year 2003") %>%
        addMoE(cdfData(),"Year 2004") %>%
        addMoE(cdfData(),"Year 2005") %>%
        addMoE(cdfData(),"Year 2006") %>%
        addMoE(cdfData(),"Year 2007") %>%
        addMoE(cdfData(),"Year 2008") %>%
        addMoE(cdfData(),"Year 2009") %>%
        addMoE(cdfData(),"Year 2010") %>%
        addMoE(cdfData(),"Year 2011") %>%
        addMoE(cdfData(),"Year 2012") %>%
        addMoE(cdfData(),"Year 2013") %>%
        addMoE(cdfData(),"Year 2014") %>%
        addMoE(cdfData(),"Year 2015") %>%
        addMoE(cdfData(),"Year 2016") %>%
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
