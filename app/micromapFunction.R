# script to functionalize micromap development
# R 3.6.1

#suppressPackageStartupMessages(library(tidyverse))#1.3.0
# suppressPackageStartupMessages(library(sp)) #1.3-2
# suppressPackageStartupMessages(library(rgdal)) #1.4-8
# suppressPackageStartupMessages(library(micromap))#1.9.3

# basinssmooth_sp <- readOGR('data/GIS','VAbasins_smoothNoChesPeeDee')# basin shapefile for micromaps (sp)
# # data management for ecoregions to work
# # had to drop coastal plain from ecoregion to get micromap to work since there is not enough data for estimates yet
# # ecoregions <- st_read('data/GIS/VA_level3ecoregion.shp') %>%
# #   filter(US_L3NAME != 'Middle Atlantic Coastal Plain')
# # st_write(ecoregions, 'data/GIS/ecoregions_Micromap.shp')
# ecoregions_sp <- readOGR('data/GIS','ecoregions_Micromap')# ecoregion shapefile for micromaps (sp)
# # data management for subbasins to work
# # vahusb <- st_read('C:/HardDriveBackup/GIS/Assessment/VAHUSB.shp') %>% 
# #   st_transform(st_crs(st_read('data/GIS/ecoregions_Micromap.shp'))) %>% 
# #   filter(VAHUSB_ %in% VAHUSBSubpopulations)
# # st_write(vahusb, 'data/GIS/vahusb_Micromap.shp')
# vahusb_sp <- readOGR('data/GIS','vahusb_Micromap')# vahusb shapefile for micromaps (sp)
# 
# 
# # need this for micromap
# map.tableBasin <- create_map_table(basinssmooth_sp,'BASIN')
# map.tableEcoregion <- create_map_table(ecoregions_sp,'US_L3NAME')
# map.tableVAHUSB <- create_map_table(vahusb_sp,'VAHUSB_')




# VLOOKUP (Excel function hack) by Julin Maloof
# vlookup <- function(ref, #the value or values that you want to look for
#                     table, #the table where you want to look for it; will look in first column
#                     column, #the column that you want the return data to come from,
#                     range=FALSE, #if there is not an exact match, return the closest?
#                     larger=FALSE) #if doing a range lookup, should the smaller or larger key be used?)
# {
#   # 2020 addition, make tibbles dataframes
#   table <- as.data.frame(table)
#   
#   if(!is.numeric(column) & !column %in% colnames(table)) {
#     stop(paste("can't find column",column,"in table"))
#   }
#   if(range) {
#     if(!is.numeric(table[,1])) {
#       stop(paste("The first column of table must be numeric when using range lookup"))
#     }
#     table <- table[order(table[,1]),] 
#     index <- findInterval(ref,table[,1])
#     if(larger) {
#       index <- ifelse(ref %in% table[,1],index,index+1)
#     }
#     output <- table[index,column]
#     output[!index <= dim(table)[1]] <- NA
#     
#   } else {
#     output <- table[match(ref,table[,1]),column]
#     output[!ref %in% table[,1]] <- NA #not needed?
#   }
#   dim(output) <- dim(ref)
#   output
# }
# 
# # CDF data
# dat <- read.csv('processedData/allCDF.csv')# CDF results


# indicator information
indicatorRanges <- list(DO = list(breaks = list(7,8,9,10,11),
                                  units = 'mg/L',
                                  prettyName = 'Dissolved Oxygen'),
                        pH = list(breaks = list(6, 7, 8, 9), 
                                  units = 'unitless',
                                  prettyName = 'pH'),
                        SpCond = list(breaks = list(0, 200, 400, 600, 800),
                                      units = 'uS/cm',
                                      prettyName = 'Specific Conductivity'),
                        TN = list(breaks = list(0, 0.5, 1.0, 1.5),
                                  units = 'mg/L',
                                  prettyName = 'Total Nitrogen'),
                        TP = list(breaks = list(0, 0.02, 0.04,0.06, 0.08),
                                  units = 'mg/L',
                                  prettyName = 'Total Phosphorus'),
                        TDS = list(breaks = list(0, 50, 100, 150, 200, 250),
                                   units = 'mg/L',
                                   prettyName = 'Total Dissolved Solids'),
                        NH4 = list(breaks = list(0, 0.01, 0.02, 0.03, 0.04, 0.05), 
                                   units = 'mg/L',
                                   prettyName = 'Ammonia Nitrogen'),
                        NO3 = list(breaks = list(0, 0.25, 0.5, 0.75, 1.0),
                                   units = 'mg/L',
                                   prettyName = 'Total Nitrate Nitrogen'),                        
                        TKN = list(breaks = list(0, 0.2, 0.4, 0.6),   
                                   units = 'mg/L',
                                   prettyName = 'Total Kjendahl Nitrogen'),    
                        Ortho_P = list(breaks = list(0, 0.015, 0.03, 0.045),    
                                       units = 'mg/L',
                                       prettyName = 'Total Ortho-Phosphorus'),    
                        Turb = list(breaks = list(0, 5, 10, 15, 20),
                                    units = 'NTU',
                                    prettyName = 'Turbidity'),  
                        TSS = list(breaks = list(0, 5, 10, 15, 20,25),    
                                   units = 'mg/L',
                                   prettyName = 'Total Suspended Solids'),  
                        Na = list(breaks = list(0, 5, 10, 15, 20),     
                                  units = 'mg/L',
                                  prettyName = 'Dissolved Sodium'),  
                        K = list(breaks = list(0, 1, 2, 3, 4),   
                                 units = 'mg/L',
                                 prettyName = 'Dissolved Potassium'),               
                        Cl = list(breaks = list(0, 10, 20, 30, 40),
                                  units = 'mg/L',
                                  prettyName = 'Dissolved Chloride'), 
                        Sf = list(breaks = list(0, 25, 50, 75, 100, 125,150),  
                                  units = 'mg/L',
                                  prettyName = 'Dissolved Sulfate'), 
                        X70331VFine = list(breaks = list(0, 25, 50, 75, 100),
                                           units = '%',
                                           prettyName = '<62 um particle size'),
                        SSCCOARSE = list(breaks = list(0, 1, 2, 3, 4),    
                                         units = 'mg/L',
                                         prettyName = 'SSC >62 um'),
                        SSCFINE = list(breaks = list(0, 2, 4, 6, 8, 10),  
                                       units = 'mg/L',
                                       prettyName = 'SSC <62 um'),
                        SSCTOTAL = list(breaks = list(0, 5, 10, 15, 20),  
                                        units = 'mg/L',
                                        prettyName = 'SSC Course & SSC Fine'),
                        LRBS = list(breaks = list(-2.5, -1.5, -0.5, 0, 0.5),     
                                    units = 'unitless',
                                    prettyName = 'Logged Relative Bed Stability'),
                        Slope = list(breaks = list(0, 1, 2, 3),         
                                     units = '%',
                                     prettyName = 'Slope'),
                        FN_PCT = list(breaks = list(0, 10, 20, 30),
                                      units = '%',
                                      prettyName = '% Fines'),
                        SA_PCT = list(breaks = list(0, 10, 20, 30, 40, 50),
                                      units = '%',
                                      prettyName = '% Sands'),
                        SA_FN_PCT  = list(breaks = list(0, 25, 50, 75, 100),
                                          units = '%',
                                          prettyName = '% Sands & Fines'),
                        LSUB_DMM = list(breaks = list(-1.0, -0.5, 0, 0.5, 1.0, 1.5, 2.0),      
                                        units = 'unitless',
                                        prettyName = 'Logged Mean Particle Size'),
                        BL_CB_GR_Embed_PCT = list(breaks = list(0, 25, 50, 75, 100),
                                                  units = '%',
                                                  prettyName = 'Embeddedness of Bolders,Cobbles, and Gravels'),
                        Embed_PCT = list(breaks = list(0, 25, 50, 75, 100),   
                                         units = '%',
                                         prettyName = 'Embeddedness'),
                        TotHab = list(breaks = list(100, 125, 150, 175, 200),
                                      units = 'unitless',
                                      prettyName = 'Total Habitat Score'),
                        TotTaxa = list(breaks = list(0, 5, 10, 15, 20),   
                                       units = 'unitless',
                                       prettyName = 'Total Taxa'),
                        EPTTax = list(breaks = list(0, 3, 6, 9, 12), 
                                      units = 'unitless',
                                      prettyName = 'Total EPT'),
                        VEphem = list(breaks = list(0, 10, 20, 30, 40),   
                                      units = '%',
                                      prettyName = 'Ephemeratera'),
                        VPTHydropsychidae  = list(breaks = list(0, 10, 20, 30, 40),
                                                  units = '%',
                                                  prettyName = 'Plecoptera and Tricoptera - Hydropsychidae'),
                        VScrap = list(breaks = list(0, 10, 20, 30, 40),           
                                      units = '%',
                                      prettyName = 'Scrapers'),
                        VChiro  = list(breaks = list(0, 10, 20, 30, 40), 
                                       units = '%',
                                       prettyName = 'Chironomidae'),
                        V2Dom  = list(breaks = list(20, 40, 60, 80),  
                                      units = '%',
                                      prettyName = 'Two Most Dominate Families'),
                        HBI  = list(breaks = list(2, 3, 4, 5, 6),    
                                    units = 'unitless',
                                    prettyName = 'Hilsonhoff Biotic Index'),
                        EPTInd   = list(breaks = list(0, 20, 40, 60, 80),   
                                        units = 'unitless',
                                        prettyName = 'EPT Individuals'),
                        VSCIVCPMI  = list(breaks = list(0, 25, 50, 75, 100),
                                          units = 'unitless',
                                          prettyName = 'VSCI/VCPMI'),
                        MetalCCU = list(breaks = list(0, 0.5, 1.0, 1.5),      
                                        units = 'unitless',
                                        prettyName = 'Metal CCU'),
                        CALCIUM = list(breaks = list(0, 10, 20, 30 ,40),         
                                       units = 'mg/L',
                                       prettyName = 'Dissolved Calcium'),
                        MAGNESIUM= list(breaks = list(0, 5, 10, 15, 20),  
                                        units = 'mg/L',
                                        prettyName = 'Dissolved Magnesium'),
                        ARSENIC = list(breaks = list(0, 0.2, 0.4, 0.6),
                                       units = 'ug/L',
                                       prettyName = 'Dissolved Arsenic'),
                        BARIUM  = list(breaks = list(0, 15, 30, 45),
                                       units = 'ug/L',
                                       prettyName = 'Dissolved Barium'),
                        BERYLLIUM = list(breaks = list(0, 0.02, 0.04,0.06, 0.08, 0.1),
                                         units = 'ug/L',
                                         prettyName = 'Dissolved Beryllium'),
                        CADMIUM  = list(breaks = list(0, 0.02, 0.04,0.06, 0.08, 0.1),  
                                        units = 'ug/L',
                                        prettyName = 'Dissolved Cadmium'),
                        CHROMIUM = list(breaks = list(0, .10, .20, .30, .40, .5, .6),
                                        units = 'ug/L',
                                        prettyName = 'Dissolved Chromium'),
                        COPPER = list(breaks = list(0, 0.25, 0.5, 0.75, 1.0),   #list(0, 0.02, 0.04,0.06, 0.08, 0.1),   
                                      units = 'ug/L',
                                      prettyName = 'Dissolved Copper'),
                        IRON = list(breaks = list(0, 200, 400,600, 800),  
                                    units = 'ug/L',
                                    prettyName = 'Dissolved Iron'),
                        LEAD = list(breaks = list(0, 0.5, 0.10, 0.15, 0.20),     
                                    units = 'ug/L',
                                    prettyName = 'Dissolved Lead'),
                        MANGANESE = list(breaks = list(0, 20, 40, 60, 80),
                                         units = 'ug/L',
                                         prettyName = 'Dissolved Manganese'),
                        THALLIUM = list(breaks = list(0, 0.02, 0.04,0.06, 0.08, 0.1), 
                                        units = 'ug/L',
                                        prettyName = 'Dissolved Thallium'),
                        NICKEL = list(breaks = list(0, 0.02, 0.04,0.06, 0.08, 0.1), 
                                      units = 'ug/L',
                                      prettyName = 'Dissolved Nickel'),
                        SILVER = list(breaks = list(0, 0.02, 0.04,0.06),
                                      units = 'ug/L',
                                      prettyName = 'Dissolved Silver'),
                        ZINC = list(breaks = list(0, 0.10, 0.20, 0.3),
                                    units = 'ug/L',
                                    prettyName = 'Dissolved Zinc'),
                        ANTIMONY  = list(breaks = list(0, 0.10, 0.20, 0.30),  
                                         units = 'ug/L',
                                         prettyName = 'Dissolved Antimony'),
                        ALUMINUM   = list(breaks = list(0, 10, 20, 30, 40, 50, 60),
                                          units = 'ug/L',
                                          prettyName = 'Dissolved Aluminum'),
                        SELENIUM = list(breaks = list(0, 0.10, 0.20, 0.30, 0.40, 0.5, 0.60),  
                                        units = 'ug/L',
                                        prettyName = 'Dissolved Selenium'),
                        HARDNESS  = list(breaks = list(0, 50, 100, 150, 200),
                                         units = 'mg/L',
                                         prettyName = 'Hardness'),
                        MERCURY  = list(breaks = list(0, 0.5, 1.00, 1.50, 2.00, 2.5) , 
                                        units = 'ng/L',
                                        prettyName = 'Mercury'),
                        wshdImpPCT  = list(breaks = list(0, 0.5, 1.00, 1.50, 2.00, 2.5, 3.0),
                                           units = 'mg/L',
                                           prettyName = 'Dissolved Potassium')) 

# function to build dataset for interquartile range micromap
dataOrgInterquartileRange <- function(dat, # CDF data with all indicators and subpopulations
                                      subpopulations, # vector of subpopulations to pull 
                                      indicator # indicator to pull
){
  statsbasin <- tibble(Subpopulation = as.character(NA),
                       Indicator = as.character(NA),
                           x25= as.numeric(NA), x50= as.numeric(NA),
                           x75= as.numeric(NA), n= as.numeric(NA))
  for(i in 1:length(subpopulations)){
    subpopData <- filter(dat, Subpopulation == !! subpopulations[i] & Indicator == !! indicator)%>%
      select(Estimate.P,Value, StdError.P, NResp,everything())
    
    statsbasin[i,] <- tibble(Subpopulation = subpopulations[i],
                             Indicator = indicator, 
                             x25=vlookup(25,subpopData,2,TRUE),
                             x50=vlookup(50,subpopData,2,TRUE),
                             x75=vlookup(75,subpopData,2,TRUE),
                             n=max(subpopData$NResp))
    }
  return(statsbasin)
}
# organizedStats <- dataOrgInterquartileRange(dat, 
#                                     c('Chowan', 'Rappahannock', 'York', 'Potomac',
#                                            'Shenandoah', 'Roanoke Basin', 'James Basin',
#                                            'New', 'Big Sandy', 'Clinch-Powell', 'Holston', 
#                                       "Virginia"),
#                                     'DO'#'VSCIVCPMI'
#                                     ) %>% 
#   mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ "Roanoke",
#                                    Subpopulation == 'James Basin' ~ "James",
#                                    TRUE ~ as.character(Subpopulation)),
#     Subpopulation = as.factor(Subpopulation),
#          Indicator = as.factor(Indicator))


# function to plot interquartile range by subpopulation and indicator
micromapInterquartileRange <- function(organizedStats, map.table,  indicatorRanges, dropVA){
  # maxX <- round(max(organizedStats$x75)*1.2, digits = 2)
  # if(unique(organizedStats$Indicator) != 'LRBS'){minX <- 0
  # }else{minX <- round(min(organizedStats$x25)-(min(organizedStats$x25)*1.2), digits = 2)}
  # 
  # Xscale <- round(seq(from = minX, to = maxX, length.out = 5), digits = 2)
  
  plotRanges <- as.list(indicatorRanges[[as.character(unique(organizedStats$Indicator))]]$breaks)
  
  statewideMedian <- filter(organizedStats, Subpopulation == 'Virginia')$x50
  
  # drop VA from micromap plot if desired
  if(dropVA == T){ organizedStats <- filter(organizedStats, Subpopulation != 'Virginia')} 
  
  suppressWarnings(
    mmplot(stat.data=as.data.frame(organizedStats),
           map.data=map.table,
           map.link=c("Subpopulation", "ID"),
           panel.types=c('dot_legend', 'labels','labels', 'dot_cl', 'map'),
           panel.data=list(NA,'Subpopulation','n',list('x50', 'x25', 'x75'),NA),
           ord.by='x50',
           grouping=3,
           median.row=F,
           plot.height=7,
           plot.width=8,
           colors=brewer.pal(3, "Spectral"),
           rev.ord=T,
           panel.att=list(list(1, point.type=20, point.border=TRUE, point.size=2),
                          list(2, header='Subpopulation', panel.width=.5, 
                               align='left', text.size=.9),
                          list(3,header='n',panel.width=.2,align='left',text.size=.9),
                          list(4, header=paste0('Estimated Median ',
                                                indicatorRanges[[as.character(unique(organizedStats$Indicator))]]$prettyName,
                                                #unique(organizedStats$Indicator),
                                                ' and \nAssociated Interquartile Range'),
                               graph.bgcolor='lightgray', point.size=1.5,
                               xaxis.ticks=plotRanges, xaxis.labels=plotRanges,
                               #xaxis.ticks=list(Xscale), xaxis.labels=list(Xscale)
                               add.line=statewideMedian,
                               add.line.col='black',add.line.typ='dashed',
                               xaxis.title=indicatorRanges[[as.character(unique(organizedStats$Indicator))]]$units),
                                 #as.character(unique(organizedStats$Indicator))),
                          list(5, header='Light Gray Means\nPreviously Displayed',
                               map.all=TRUE, fill.regions='aggregate',
                               active.border.color='black', active.border.size=1.0,
                               inactive.border.color=gray(.7), inactive.border.size=1, 
                               panel.width=1.0))) )
}

# micromapInterquartileRange(dataOrgInterquartileRange(probCDF,
#                                                      c('Chowan', 'Rappahannock', 'York', 'Potomac',
#                                                        'Shenandoah', 'Roanoke Basin', 'James Basin',
#                                                        'New', 'Big Sandy', 'Clinch-Powell', 'Holston',
#                                                        "Virginia"),
#                                                      'DO'#'VSCIVCPMI'#'TotHab'#'TP'#'VSCIVCPMI'
#                                                      ) %>%
#                              mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ "Roanoke",
#                                                               Subpopulation == 'James Basin' ~ "James",
#                                                               TRUE ~ as.character(Subpopulation)),
#                                     Subpopulation = as.factor(Subpopulation),
#                                     Indicator = as.factor(Indicator)),
#                            map.tableBasin,
#                            indicatorRanges = indicatorRanges,
#                            dropVA = F)


# micromapInterquartileRange(dataOrgInterquartileRange(probCDF,
#                                                      c('Virginia', ecoregionSubpopulations),
#                                                      'DO'#'VSCIVCPMI'#'TotHab'#'TP'#'VSCIVCPMI'
# ) %>%
#   mutate(Subpopulation = case_when(Subpopulation == "Central Appalachian Ridges and Valleys" ~ "Ridge and Valley",
#                                    Subpopulation == "Blue Ridge Mountains" ~ "Blue Ridge",
#                                    TRUE ~ as.character(Subpopulation)),
#          Subpopulation = as.factor(Subpopulation),
#          Indicator = as.factor(Indicator)),
# map.tableEcoregion,
# indicatorRanges = indicatorRanges,
# dropVA = F)


# micromapInterquartileRange(dataOrgInterquartileRange(probCDF,
#                                                      VAHUSBSubpopulations,
#                                                      'DO'#'VSCIVCPMI'#'TotHab'#'TP'#'VSCIVCPMI'
# ) %>%
#   mutate(Subpopulation = as.factor(Subpopulation),
#          Indicator = as.factor(Indicator)),
# map.tableVAHUSB,
# indicatorRanges = indicatorRanges,
# dropVA = F)
