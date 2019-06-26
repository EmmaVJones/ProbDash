
library(crosstalk)
library(plotly)

dat <- data.frame(x=c('A','A','A', 'B','B','B', 'C', 'C', 'C'), 
y = c(12, 13, 18, 34, 33, 32, 55, 63, 19), 
z = c('VSCI', 'VCPMI63', 'VCPMI65','VSCI', 'VCPMI63', 'VCPMI65','VSCI', 'VCPMI63', 'VCPMI65'))

# sometimes you can cheat and use highight_key/selectize from plotly instead of just crosstalk filters
# selectize here will let you filter by row. Need to hold shift for multiple sellections. Clicking the DT # will allow muiltiple selections 

sd_highlight<- highlight_key(dat)

#_____________________
# @kent37 wrote a summary stats app widget for crosstalk.
# In this example it counts all the VSCI that are selected. I'd bet you are skilled enough to modify his 
# widget 
# so that it returns what filtered data you are after, instead of mean, count, etc. 
# code: https://github.com/kent37/summarywidget/blob/master/R/summarywidget.R
# demo: https://kent37.github.io/summarywidget/

summary=summarywidget(sd_highlight,selection=~z=="VSCI",width = 10, height = 10)

#____________________

bscols(
  list(
    filter_checkbox('x', "x", sd_highlight, ~x, inline = TRUE),
   # filter_select("z", "z", sd_highlight, ~z,allLevels = T),
    datatable(sd_highlight),
    summary,
   plot_ly(sd_highlight, x = ~z, y = ~y) %>% 
    add_markers(color="red") %>% 
    highlight("plotly_selected",selectize = T,persistent = F)
   ))

#________________________
# bscols really sucks. 
#Luckily you can still use shiny page organization for dashboards without using runtime::shiny
#e.g. fluidPage(fluidRow(column(width = 7, wellPanel(blah blah blah))))