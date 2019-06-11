source('global.R')

# Read in Data
probCDF <- read_csv('data/allCDF.csv') %>%
  filter(!is.na(Indicator) | !is.na(Subpopulation))


parameterData1 <- filter(probCDF, Indicator == 'DO')

parameterData2 <- filter(parameterData1, Subpopulation %in% c('Virginia',"Roanoke Basin","James Basin",
                                                             "Potomac-Shenandoah","Rappahannock-York",
                                                             "New","Chowan","Tennessee")) 

plot_ly(parameterData2) %>%
  add_trace(data = parameterData2, x = ~Value, y = ~Estimate.P, mode = 'line', color = ~Subpopulation, 
            hoverinfo = 'text', text = ~paste(sep = '<br>',
                                             paste(unique(Indicator),":", Value), # add units!!!!!!!!!! see benthic stressor tool data manipulation??
                                             paste('Percentile:',format(Estimate.P,digits=2), '+/-', format(MoE, digits = 2) )))


statusSuperbasinUI <- function(id){
  ns <- NS(id)
  tagList(
    wellPanel(
      radioButtons(ns('radio'),'Choose a plot style:', choices = c('CDF','Barplot')),
      br(),
      plotOutput(ns('plot'))
    )
  )
}
    
      

statusSuperbasin <- function(input,output,session, parameterData){
  ns <- session$ns
  
  superB <- reactive({
    req(parameterData)
    filter(parameterData, Subpopulation %in% c('Virginia',"Roanoke Basin","James Basin",
                                               "Potomac-Shenandoah","Rappahannock-York",
                                               "New","Chowan","Tennessee"))
  })
  
  output$plot <- renderPlot({
    if(input$radio == 'CDF'){
      plot(superB()$Value, superB()$Estimate.P)
    } else {
      NULL
    }
  })
}

ui <- fluidPage(  statusSuperbasinUI('super')  )

server <- function(input,output,session){
  callModule(statusSuperbasin,'super', parameterData1)

}

shinyApp(ui, server)

