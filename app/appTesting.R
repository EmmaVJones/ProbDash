# Run in R 3.6.0
source('global.R')


# Read in Data
probCDF <- read_csv('data/IR2022/allCDF.csv') %>%
  #read_csv('data/IR2020/allCDF.csv') %>%
  filter(!is.na(Indicator) | !is.na(Subpopulation)) %>% 
  mutate(MoE = StdError.P * 1.96)


parameterChoice <- unique(probCDF$Indicator)[1]


parameter <-  filter(probCDF, Indicator == parameterChoice)  

slider <- median(filter(parameter, Subpopulation == 'Virginia')$Value)


# MAKE DATA TO SEND TO MODULES

superBasinBox <- percentileSubpopN(filter(parameter, Subpopulation %in% superBasinSubpopulations),
                    superBasinSubpopulations, slider) 
subBasinBox <- percentileSubpopN(filter(parameter, Subpopulation %in% subBasinSubpopulations),
                    subBasinSubpopulations, slider) 

VAHUSBBasinBox <- percentileSubpopN(filter(parameter, Subpopulation %in% VAHUSBSubpopulations),
                                    VAHUSBSubpopulations, slider)  

ecoregionBox  <- percentileSubpopN(filter(parameter, Subpopulation %in% ecoregionSubpopulations),
                    ecoregionSubpopulations, slider) 
bioregionBox  <- percentileSubpopN(filter(parameter, Subpopulation %in% bioregionSubpopulations),
                    bioregionSubpopulations, slider) 
z <- percentileSubpopN(filter(parameter, Subpopulation %in% streamOrderSubpopulations),
                       streamOrderSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels = c("First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order" ), ordered = T)
streamOrderBox  <- z

z <- percentileSubpopN(filter(parameter, Subpopulation %in% watershedSizeSubpopulations),
                       watershedSizeSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels = c("<1 square mile", "1 to 10 square mile", "10 to 50 square mile", ">50 square mile"), ordered = T)
watershedSizeBox  <- z 

z <- percentileSubpopN(filter(parameter, Subpopulation %in% streamSizeSubpopulations),
                       streamSizeSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels = c("Small", "Medium", "Large"), ordered = T)
streamSizeBox  <- z

z <- percentileSubpopN(filter(parameter, Subpopulation %in% IRWindowSubpopulations),
                       IRWindowSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels = IRwindows, ordered = T)
#c("IR2008", "IR2010", "IR2012", "IR2014", "IR2016", "IR2018"  , "IR2020", "IR2022" ), ordered = T)
IRWindowBox  <- z

z <- percentileSubpopN(filter(parameter, Subpopulation %in% yearSubpopulations),
                       yearSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels = years, ordered = T)
# c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
#   "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
#   "Year 2013", "Year 2014", "Year 2015", "Year 2016", "Year 2017", "Year 2018",
#   "Year 2019", "Year 2020"), ordered = T)
yearBox  <- z 

z <- percentileSubpopN(filter(parameter, Subpopulation %in% bayNonBaySubpopulations),
                       bayNonBaySubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels =  c(paste0('Bay Watersheds ',panelWindow[1]), paste0('Non-Bay Watersheds ',panelWindow[1]),
                                                       paste0('Bay Watersheds ',panelWindow[2]), paste0('Non-Bay Watersheds ',panelWindow[2]),
                                                       paste0('Bay Watersheds ',panelWindow[3]), paste0('Non-Bay Watersheds ',panelWindow[3])), ordered = T)
# c("Bay Watersheds 2001-2018", "Non-Bay Watersheds 2001-2018", 
#   "Bay Watersheds 2001-2008", "Non-Bay Watersheds 2001-2008",
#   "Bay Watersheds 2009-2018", "Non-Bay Watersheds 2009-2018"), ordered = T)
bayNonBayBox  <- z
  
z <- percentileSubpopN(filter(parameter, Subpopulation %in% VSCIyearSubpopulations),
                       VSCIyearSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels =  paste0('VSCI Scores ',bioPanelWindow), ordered = T)
# c("VSCI Scores 2001-2004", "VSCI Scores 2005-2008",
#   "VSCI Scores 2009-2013", "VSCI Scores 2014-2018"), ordered = T)
VSCIyearBox  <- z 
  
z <- percentileSubpopN(filter(parameter, Subpopulation %in% biophaseSubpopulations),
                       biophaseSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels =   c(paste0("Phase One ", panelWindow[2]), paste0("Phase Two ", panelWindow[3])), ordered = T)
# c("Phase One 2001-2008", "Phase Two 2009-2018"), ordered = T)
biophaseBox  <- z 

z <- percentileSubpopN(filter(parameter, Subpopulation %in% biophaseXStreamSizeSubpopulations),
                       biophaseXStreamSizeSubpopulations, slider)  
z$Subpopulation <- factor(z$Subpopulation, levels =  c("Phase1Small", "Phase2Small", "Phase1Medium",
                                                       "Phase2Medium", "Phase1Large", "Phase2Large"), ordered = T)
biophaseXBox  <- z



# TEST INDIVIDUAL MODULES

parameterData <- parameter
barData <- VAHUSBBasinBox
thresholdValue <- slider
yearSubpopulations

#callModule(trendYear,'year', parameter, yearBox, reactive(input$slider), yearSubpopulations)## Still need to manually update MoE


  z <- filter(parameterData, Subpopulation %in% yearSubpopulations)  %>%
    mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE) 
  z$Subpopulation <- factor(z$Subpopulation, levels = yearSubpopulations, ordered = T)
  # c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
  #   "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
  #   "Year 2013", "Year 2014", "Year 2015", "Year 2016", "Year 2017", "Year 2018"), ordered = T)
  cdfData <- z
# boxplot 
plot_ly(cdfData, x = ~Subpopulation, y = ~Value,  color = ~Subpopulation, type = "box") %>%
  layout( yaxis=list(title="Value"),xaxis=list(title="Subpopulation"))

# cdf plot
plot_ly(cdfData) %>%
  add_trace(data = cdfData, x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
            hoverinfo = 'text', text = ~paste(sep = '<br>',
                                              paste("Subpopulation: ", Subpopulation),
                                              paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                              paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
  addMoE(cdfData,"Year 2001") %>%
  addMoE(cdfData,"Year 2002") %>%
  addMoE(cdfData,"Year 2003") %>%
  addMoE(cdfData,"Year 2004") %>%
  addMoE(cdfData,"Year 2005") %>%
  addMoE(cdfData,"Year 2006") %>%
  addMoE(cdfData,"Year 2007") %>%
  addMoE(cdfData,"Year 2008") %>%
  addMoE(cdfData,"Year 2009") %>%
  addMoE(cdfData,"Year 2010") %>%
  addMoE(cdfData,"Year 2011") %>%
  addMoE(cdfData,"Year 2012") %>%
  addMoE(cdfData,"Year 2013") %>%
  addMoE(cdfData,"Year 2014") %>%
  addMoE(cdfData,"Year 2015") %>%
  addMoE(cdfData,"Year 2016") %>%
  addMoE(cdfData,"Year 2017") %>%
  addMoE(cdfData,"Year 2018") %>%
  
  addMoE(cdfData,"Year 2019") %>%
  addMoE(cdfData,"Year 2020") %>%
  
  layout(#showlegend=FALSE,
    yaxis=list(title="Percentile"),
    xaxis=list(title="Subpopulation"))

plot_ly(barData, x = ~Subpopulation, y = ~Percentile, type = 'bar', 
        width = .10,
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("Subpopulation: ", Subpopulation),
                                      #paste("Threshold Value: ",thresholdValue()),
                                      paste("Percentile: ", format(Percentile, digits = 3), 
                                            ' +/- ', format(MoE, digits = 3)),
                                      paste("n: ", n))) %>%
  layout(showlegend=TRUE,
    yaxis=list(title="Percentile"),
    xaxis=list(title="Subpopulation"))

barData2 <- barData %>% 
  mutate(Subpopulation = as.factor(Subpopulation))

plot_ly(barData2,#filter(barData, Subpopulation == VAHUSBSubpopulations[3]), 
        x = ~Subpopulation, y = ~Percentile, color =  ~as.factor(Subpopulation),  colors = ~"#2074b4", type = 'bar', 
        width = .10, #name = VAHUSBSubpopulations[3],
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("Subpopulation: ", Subpopulation),
                                      #paste("Threshold Value: ",thresholdValue()),
                                      paste("Percentile: ", format(Percentile, digits = 3), 
                                            ' +/- ', format(MoE, digits = 3)),
                                      paste("n: ", n))) %>%
  #add_trace(y = ~filter(barData, Subpopulation == VAHUSBSubpopulations[4]), name = VAHUSBSubpopulations[4]) %>%
  layout(showlegend=TRUE,
         yaxis=list(title="Percentile"),
         xaxis=list(title="Subpopulation"))


barData2 <- barData %>% 
  group_by(Subpopulation) %>% 
  pivot_longer(cols = -Subpopulation, names_to = 'Stat', values_to = 'Value') %>% 
  pivot_wider(names_from = Subpopulation, values_from = Value)
  
  

plot_ly(barData2, x = ~Stat, y = ~Virginia, type = 'bar', 
        width = .10),
        hoverinfo="text", text=~paste(sep="<br>",
                                      paste("Subpopulation: ", Subpopulation),
                                      #paste("Threshold Value: ",thresholdValue()),
                                      paste("Percentile: ", format(Percentile, digits = 3),
                                            ' +/- ', format(MoE, digits = 3)),
                                      paste("n: ", n))) %>%
  add_trace(y = ~filter(barData, Subpopulation == VAHUSBSubpopulations[2]), name = VAHUSBSubpopulations[2],
            width = .10,
            hoverinfo="text", text=~paste(sep="<br>",
                                          paste("Subpopulation: ", Subpopulation),
                                          #paste("Threshold Value: ",thresholdValue()),
                                          paste("Percentile: ", format(Percentile, digits = 3),
                                                ' +/- ', format(MoE, digits = 3)),
                                          paste("n: ", n))) %>%
  add_trace(y = ~filter(barData, Subpopulation == VAHUSBSubpopulations[3]), name = VAHUSBSubpopulations[3]) %>%
  add_trace(y = ~filter(barData, Subpopulation == VAHUSBSubpopulations[4]), name = VAHUSBSubpopulations[4]) %>%
  add_trace(y = ~filter(barData, Subpopulation == VAHUSBSubpopulations[5]), name = VAHUSBSubpopulations[5]) %>%
  add_trace(y = ~filter(barData, Subpopulation == VAHUSBSubpopulations[6]), name = VAHUSBSubpopulations[6]) %>%
  
  layout(showlegend=TRUE,
         yaxis=list(title="Percentile"),
         xaxis=list(title="Subpopulation"), 
         barmode = 'group')

plot_ly(filter(barData,Subpopulation %in% yearSubpopulations[1:10]),
               x = ~Subpopulation, y = ~Percentile, type = 'bar', 
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

plot_ly(filter(barData,Subpopulation %in% yearSubpopulations[11:20]),
        x = ~Subpopulation, y = ~Percentile, type = 'bar', 
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



trendYearUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      #verbatimTextOutput(ns('test')),
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('Barplot','Boxplot','CDF')),
      br(),
      plotlyOutput(ns('plot1')),
      plotlyOutput(ns('plot2'))
    )
  )
}



trendYear <- function(input,output,session, parameterData, barData, yearSubpopulations){
  ns <- session$ns
  #output$test <- renderPrint({cdfData1()})
  
  barData1 <- reactive({req(barData())
     filter(barData(), Subpopulation %in% yearSubpopulations[1:10]) })
  
  barData2 <- reactive({req(barData())
    filter(barData(), Subpopulation %in% yearSubpopulations[11:20]) })
  
   cdfData1 <- reactive({  req(parameterData)
     z <- filter(parameterData(), Subpopulation %in% yearSubpopulations[1:10])  %>%
       mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE)
     z$Subpopulation <- factor(z$Subpopulation, levels = yearSubpopulations[1:10], ordered = T)
     return( z)})

   cdfData2 <- reactive({    req(parameterData)
     z <- filter(parameterData(), Subpopulation %in% yearSubpopulations[11:20])  %>%
       mutate(ymin = Estimate.P - MoE, ymax = Estimate.P + MoE)
     z$Subpopulation <- factor(z$Subpopulation, levels = yearSubpopulations[11:20], ordered = T)
     return( z)})
   
   
   output$plot1 <- renderPlotly({
     if(input$radio %in% 'Boxplot'){
       plot_ly(cdfData1(), x = ~Subpopulation, y = ~Value,  color = ~Subpopulation, type = "box") %>%
         layout( yaxis=list(title="Value"),xaxis=list(title="Subpopulation"))
     } else if(input$radio %in% 'CDF'){
       plot_ly(cdfData1()) %>%
         add_trace(data = cdfData1(), x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation,
                   hoverinfo = 'text', text = ~paste(sep = '<br>',
                                                     paste("Subpopulation: ", Subpopulation),
                                                     paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                                     paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
         addMoE(cdfData1(),"Year 2001") %>%
         addMoE(cdfData1(),"Year 2002") %>%
         addMoE(cdfData1(),"Year 2003") %>%
         addMoE(cdfData1(),"Year 2004") %>%
         addMoE(cdfData1(),"Year 2005") %>%
         addMoE(cdfData1(),"Year 2006") %>%
         addMoE(cdfData1(),"Year 2007") %>%
         addMoE(cdfData1(),"Year 2008") %>%
         addMoE(cdfData1(),"Year 2009") %>%
         addMoE(cdfData1(),"Year 2010") %>%
       layout(#showlegend=FALSE,
         yaxis=list(title="Percentile"),
         xaxis=list(title="Subpopulation")) 
     } else if(input$radio %in% 'Barplot'){
       plot_ly(barData1(),
               x = ~Subpopulation, y = ~Percentile, type = 'bar',
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
   
   output$plot2 <- renderPlotly({
     if(input$radio %in% 'Boxplot'){
       plot_ly(cdfData2(), x = ~Subpopulation, y = ~Value,  color = ~Subpopulation, type = "box") %>%
         layout( yaxis=list(title="Value"),xaxis=list(title="Subpopulation"))
     } else if(input$radio %in% 'CDF'){
       plot_ly(cdfData2()) %>%
         add_trace(data = cdfData2(), x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation,
                   hoverinfo = 'text', text = ~paste(sep = '<br>',
                                                     paste("Subpopulation: ", Subpopulation),
                                                     paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                                     paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
         addMoE(cdfData2(),"Year 2011") %>%
         addMoE(cdfData2(),"Year 2012") %>%
         addMoE(cdfData2(),"Year 2013") %>%
         addMoE(cdfData2(),"Year 2014") %>%
         addMoE(cdfData2(),"Year 2015") %>%
         addMoE(cdfData2(),"Year 2016") %>%
         addMoE(cdfData2(),"Year 2017") %>%
         addMoE(cdfData2(),"Year 2018") %>%
         addMoE(cdfData2(),"Year 2019") %>%
         addMoE(cdfData2(),"Year 2020") %>%
         layout(#showlegend=FALSE,
           yaxis=list(title="Percentile"),
           xaxis=list(title="Subpopulation")) 
     } else if(input$radio %in% 'Barplot'){
       plot_ly(barData2(),
               x = ~Subpopulation, y = ~Percentile, type = 'bar',
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


ui <- fluidPage( uiOutput('parameterChoiceUI'),
                 uiOutput('sliderUI'),
                 
                 #verbatimTextOutput('test'),
                 
                 trendYearUI('year') )

server <- function(input,output,session){
  #Parameter Choice on UI
  output$parameterChoiceUI <- renderUI({
    selectInput('parameterChoice','Choose a parameter to analyze', choices = unique(probCDF$Indicator))  })
  
  # Select Parameter
  parameter <- reactive({
    req(input$parameterChoice)
    filter(probCDF, Indicator == input$parameterChoice)  })
  
  
  
  output$sliderUI <- renderUI({
    #req(parameter())
    sliderInput('slider','Choose threshold value', min = min(parameter()$Value), max = max(parameter()$Value),
                value = median(filter(parameter(), Subpopulation == 'Virginia')$Value))})
  
  yearBox  <- reactive({
    req(input$slider, parameter())
    z <- percentileSubpopN(filter(parameter(), Subpopulation %in% yearSubpopulations),
                           yearSubpopulations, input$slider)  
    z$Subpopulation <- factor(z$Subpopulation, levels = years, ordered = T)
    return(z)  })  
  
  callModule(trendYear,'year', parameter, yearBox, yearSubpopulations)## Still need to manually update MoE
  
  #input,output,session, parameterData, barData, thresholdValue, yearSubpopulations
}

shinyApp(ui, server)








output$plot1 <- renderPlotly({
  print(barData1())
if(input$radio %in% 'Boxplot'){
  plot_ly(cdfData1(), x = ~Subpopulation, y = ~Value,  color = ~Subpopulation, type = "box") %>%
    layout( yaxis=list(title="Value"),xaxis=list(title="Subpopulation"))
} else if(input$radio %in% 'CDF'){
  plot_ly(cdfData1()) %>%
    add_trace(data = cdfData1(), x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation,
              hoverinfo = 'text', text = ~paste(sep = '<br>',
                                                paste("Subpopulation: ", Subpopulation),
                                                paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                                paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) ))) %>%
    addMoE(cdfData1(),"Year 2001") %>%
    addMoE(cdfData1(),"Year 2002") %>%
    addMoE(cdfData1(),"Year 2003") %>%
    addMoE(cdfData1(),"Year 2004") %>%
    addMoE(cdfData1(),"Year 2005") %>%
    addMoE(cdfData1(),"Year 2006") %>%
    addMoE(cdfData1(),"Year 2007") %>%
    addMoE(cdfData1(),"Year 2008") %>%
    addMoE(cdfData1(),"Year 2009") %>%
    addMoE(cdfData1(),"Year 2010") %>%
    # addMoE(cdfData(),"Year 2011") %>%
    # addMoE(cdfData(),"Year 2012") %>%
    # addMoE(cdfData(),"Year 2013") %>%
    # addMoE(cdfData(),"Year 2014") %>%
    # addMoE(cdfData(),"Year 2015") %>%
    # addMoE(cdfData(),"Year 2016") %>%
    # addMoE(cdfData(),"Year 2017") %>%
    # addMoE(cdfData(),"Year 2018") %>%
    #
    # addMoE(cdfData(),"Year 2019") %>%
    # addMoE(cdfData(),"Year 2020") %>%

    layout(#showlegend=FALSE,
      yaxis=list(title="Percentile"),
      xaxis=list(title="Subpopulation"))
} else if(input$radio %in% 'Barplot'){
plot_ly(barData1(),
                    x = ~Subpopulation, y = ~Percentile, type = 'bar',
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
          # }

        })
        }

