library(shiny)
library(shinyjs)
library(tidyverse)
library(plotly)
library(DT)
library(sp) #1.3-2
library(rgdal) #1.4-8
library(micromap)

#### FIX THESE ################### EACH IR CYCLE!!!! #######################################################################

## need to manually update MoE %>% in trend IR, Year modules!! 

newIR <- 2022
newIRwindowEnd <- 2020
panelWindow <- c("2001-2020", '2001-2010', '2011-2020')
bioPanelWindow <- c('2001-2005', '2006-2010', '2011-2015', '2016-2020')

IRwindows <- paste0('IR', seq(2008, newIR, 2))
years <- paste0("Year ", 2001:newIRwindowEnd)  


# Define each subpopulation category for easy module building, rebuild years each IR update

superBasinSubpopulations <- c('Virginia',"Roanoke Basin","James Basin",
                              "Potomac-Shenandoah","Rappahannock-York",
                              "New","Chowan","Tennessee")
subBasinSubpopulations <- c('Virginia',"Roanoke Basin","James Basin",
                            "Potomac","Shenandoah","Rappahannock","York",
                            "New","Chowan","Holston", "Big Sandy", "Clinch-Powell")
VAHUSBSubpopulations <- c('Virginia',"Roanoke River, Upper", "James River, Middle (Piedmont)", "New River", 
                          "James River, Upper (Mountain)", "Roanoke River- Dan River", "York River", "Potomac River, Lower", 
                          "Chowan River, Upper", "Rappahannock River", "Big Sandy River",
                          "Tennessee-Holston River", "Potomac River-Shenandoah River", "James River- Appomattox River",
                          "Chowan River-Meherrin River", "Tennessee-Clinch River")
ecoregionSubpopulations <- c("Piedmont", "Northern Piedmont", "Central Appalachian Ridges and Valleys",
                             "Southeastern Plains", "Blue Ridge Mountains", "Central Appalachians")
bioregionSubpopulations <- c("Mountain Bioregion", "Piedmont Bioregion", "Coast Bioregion"  )
streamOrderSubpopulations <- c("First Order", "Second Order", "Third Order", "Fourth Order", "Fifth Order" )
watershedSizeSubpopulations <- c("<1 square mile", "1 to 10 square mile", "10 to 50 square mile", ">50 square mile")
streamSizeSubpopulations <- c("Small", "Medium", "Large")
biophaseXStreamSizeSubpopulations<- c("Phase1Small", "Phase2Small", "Phase1Medium", "Phase2Medium", "Phase1Large", "Phase2Large")



IRWindowSubpopulations <- paste0('IR', seq(2008, newIR, 2))
  #c("IR2008", "IR2010", "IR2012", "IR2014", "IR2016", "IR2018", "IR2020"  )
yearSubpopulations <- paste0("Year ", 2001:newIRwindowEnd) 
  # c("Year 2001", "Year 2002", "Year 2003", "Year 2004", "Year 2005", "Year 2006", 
  #   "Year 2007", "Year 2008", "Year 2009", "Year 2010", "Year 2011", "Year 2012", 
  #   "Year 2013", "Year 2014", "Year 2015", "Year 2016", "Year 2017", "Year 2018")
bayNonBaySubpopulations <- c(paste0('Bay Watersheds ',panelWindow[1]), paste0('Non-Bay Watersheds ',panelWindow[1]),
                             paste0('Bay Watersheds ',panelWindow[2]), paste0('Non-Bay Watersheds ',panelWindow[2]),
                             paste0('Bay Watersheds ',panelWindow[3]), paste0('Non-Bay Watersheds ',panelWindow[3]))
  # c("Bay Watersheds 2001-2018", "Non-Bay Watersheds 2001-2018", "Bay Watersheds 2001-2008", 
  #    "Bay Watersheds 2009-2018", "Non-Bay Watersheds 2001-2008", "Non-Bay Watersheds 2009-2018")
VSCIyearSubpopulations <-  paste0('VSCI Scores ',bioPanelWindow)
  #c("VSCI Scores 2001-2004", "VSCI Scores 2005-2008", "VSCI Scores 2009-2013", "VSCI Scores 2014-2018")
biophaseSubpopulations <- c(paste0("Phase One ", panelWindow[2]), paste0("Phase Two ", panelWindow[3]))
  #c("Phase One 2001-2008","Phase Two 2009-2018")
######################################################################################################################


source('micromapFunction.R')


statusModulesToReadIn <- c('Status_Superbasin','Status_Subbasin','Status_VAHUSB','Status_Ecoregion',
                           'Status_Bioregion', 'Status_StreamOrder','Status_WatershedSize', 'Status_StreamSize')
trendModulesToReadIn <- c('Trend_IRWindow','Trend_Year','Trend_BayNonBay','Trend_VSCIyear',
                          'Trend_Biophase','Trend_BiophaseXStreamSize')
for (i in 1:length(statusModulesToReadIn)){
  source(paste('module_',statusModulesToReadIn[i],'Module.R',sep=''))
}
for (i in 1:length(trendModulesToReadIn)){
  source(paste('module_',trendModulesToReadIn[i],'Module.R',sep=''))
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

percentileSubpopN <- function(indicatorDataset, subpopulationDesired, indicatorLookup){
  results <- data.frame(Percentile= NA, MoE = NA, n = NA)
  for(i in 1:length(subpopulationDesired)){
    rawDat <- filter(indicatorDataset, Subpopulation == subpopulationDesired[i]) %>%
      mutate(MoE.P = StdError.P * 1.96) %>%
      select(Value,Estimate.P, MoE.P, NResp) %>% as.data.frame()
    results[i,] <- data.frame(Percentile= as.numeric(vlookup(indicatorLookup,rawDat, 2, TRUE)), 
                              MoE = as.numeric(vlookup(indicatorLookup,rawDat, 3, TRUE)),
                              n = as.numeric(vlookup(indicatorLookup,rawDat, 4, TRUE)))
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
  # 2020 addition, make tibbles dataframes
  table <- as.data.frame(table)
  
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

