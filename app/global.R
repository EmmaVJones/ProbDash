library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(DT)



statusModulesToReadIn <- c('Status_Superbasin','Status_Subbasin','Status_Ecoregion', 'Status_Bioregion',
                           'Status_StreamOrder','status_WatershedSize', 'status_StreamSize')
trendModulesToReadIn <- c('Trend_IRWindow','Trend_Year','Trend_BayNonBay','Trend_VSCIyear',
                          'Trend_Biophase','Trend_BiophaseXStreamSize')
for (i in 1:length(statusModulesToReadIn)){
  source(paste('appModules/',statusModulesToReadIn[i],'Module.R',sep=''))
}
for (i in 1:length(trendModulesToReadIn)){
  source(paste('appModules/',trendModulesToReadIn[i],'Module.R',sep=''))
}




# Loading screen
load_data <- function() {
  Sys.sleep(2)
  shinyjs::hide("loading_page")
  shinyjs::show("main_content")
}



percentileSubpop <- function(indicatorDataset, subpopulationDesired, indicatorLookup){
  results <- data.frame(Percentile= NA, MoE = NA)
  for(i in 1:length(subpopulationDesired)){
    rawDat <- filter(indicatorDataset, Subpopulation == subpopulationDesired[i]) %>%
      mutate(MoE.P = StdError.P * 1.96) %>%
      select(Value,Estimate.P, MoE.P) %>% as.data.frame()
    results[i,] <- data.frame(Percentile= as.numeric(vlookup(indicatorLookup,rawDat, 2, TRUE)), 
                              MoE = as.numeric(vlookup(indicatorLookup,rawDat, 3, TRUE)))
  }
  results <- mutate(results, Subpopulation = subpopulationDesired) %>% dplyr::select(Subpopulation, everything())
  return(results)
}



# VLOOKUP (Excel function hack) by Julin Maloof
vlookup <- function(ref, #the value or values that you want to look for
                    table, #the table where you want to look for it; will look in first column
                    column, #the column that you want the return data to come from,
                    range=FALSE, #if there is not an exact match, return the closest?
                    larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
{
  if(!is.numeric(column) & !column %in% colnames(table)) {
    stop(paste("can't find column",column,"in table"))
  }
  if(range) {
    if(!is.numeric(table[,1])) {
      stop(paste("The first column of table must be numeric when using range lookup"))
    }
    table <- table[order(table[,1]),] 
    index <- findInterval(ref,table[,1])
    if(larger) {
      index <- ifelse(ref %in% table[,1],index,index+1)
    }
    output <- table[index,column]
    output[!index <= dim(table)[1]] <- NA
    
  } else {
    output <- table[match(ref,table[,1]),column]
    output[!ref %in% table[,1]] <- NA #not needed?
  }
  dim(output) <- dim(ref)
  output
}





# add margin of error to plotly plots for each subpopulation efficiently
addMoE <- function(p, dataset, subpopulation){
  add_ribbons(p, data = filter(dataset, Subpopulation== subpopulation),
              x = ~Value, ymin = ~ymin, ymax = ~ymax, line = list(color = 'rgba(7, 164, 181, 0.05)'),
              fillcolor = 'rgba(7, 164, 181, 0.2)', name = paste(subpopulation," Margin of Error",sep=""), visible = 'legendonly')
}

# Define each subpopulation category for easy module building

superBasinSubpopulations <- c('Virginia',"Roanoke Basin","James Basin",
                              "Potomac-Shenandoah","Rappahannock-York",
                              "New","Chowan","Tennessee")
subBasinSubpopulations <- c('Virginia',"Roanoke Basin","James Basin",
                            "Potomac","Shenandoah","Rappahannock","York",
                            "New","Chowan","Holston", "Big Sandy", "Clinch-Powell")
ecoregionSubpopulations <- c("Piedmont", "Northern Piedmont", "Central Appalachian Ridges and Valleys",
                             "Southeastern Plains", "Blue Ridge Mountains", "Central Appalachians")
bioregionSubpopulations <- c("Mountain Bioregion", "Piedmont Bioregion", "Coast Bioregion"  )
streamOrderSubpopulations <- c("First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order" )
watershedSizeSubpopulations <- c("<1 square mile", "1 to 10 square mile", "10 to 200 square mile", ">200 square mile")
streamSizeSubpopulations <- c("Small", "Medium", "Large")
IRWindowSubpopulations <- c("IR2008", "IR2010", "IR2012", "IR2014", "IR2016", "IR2018"  )
yearSubpopulations <- c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
                        "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
                        "Year 2013", "Year 2014", "Year 2015", "Year 2016")
bayNonBaySubpopulations <- c("Bay Watersheds 2001-2016", "Non-Bay Watersheds 2001-2016", "Bay Watersheds 2001-2009", 
                             "Bay Watersheds 2009-2016", "Non-Bay Watersheds 2001-2009", "Non-Bay Watersheds 2009-2016")
VSCIyearSubpopulations <- c("VSCI Scores 2001-2004", "VSCI Scores 2005-2008", "VSCI Scores 2009-2012", "VSCI Scores 2013-2016")
biophaseSubpopulations <- c("Phase One 2001-2009","Phase Two 2009-2016")
biophaseXStreamSizeSubpopulations<- c("Phase1Small", "Phase2Small", "Phase1Medium", "Phase2Medium", "Phase1Large", "Phase2Large")
