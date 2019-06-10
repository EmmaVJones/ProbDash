# Run in R 3.6.0
source('global.R')

shinyServer(function(input, output, session) {
  
  # display the loading feature until data loads into app
  load_data()
  
})