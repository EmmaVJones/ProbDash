dat <- probCDF
subpopulations = c('Chowan', 'Rappahannock', 'York', 'Potomac',
                   'Shenandoah', 'Roanoke Basin', 'James Basin',
                   'New', 'Big Sandy', 'Clinch-Powell', 'Holston',
                   "Virginia")
indicator <- 'TotHab'
threshold <- 120
revOrder <- TRUE
                                                                                 

# function to build dataset for optimal/suboptimal micromap
dataOrgThreshold <- function(dat, # CDF data with all indicators and subpopulations
                             subpopulations, # vector of subpopulations to pull 
                             indicator, # indicator to pull
                             threshold, # numeric break point to pull indicator information
                             revOrder # TRUE if the indicator requires the scale readjusted
){
  statsSubpop <- tibble(Subpopulation = as.character(NA),
                        Indicator = as.character(NA),
                        Estimate= as.numeric(NA), 
                        UCB95= as.numeric(NA),
                        LCB95= as.numeric(NA), 
                        n = as.numeric(NA),
                        Rank = as.numeric(NA))
  for(i in 1:length(subpopulations)){
    subpopData <- filter(dat, Subpopulation == !! subpopulations[i] & Indicator == !! indicator) %>%
      select(Value, Estimate.P, StdError.P, NResp) %>%
      mutate(MoE = StdError.P * 1.96) # add margin of error
    
    statsSubpop[i,] <- tibble(Subpopulation = subpopulations[i],
                             Indicator = indicator, 
                             Estimate= vlookup(threshold,subpopData,2,TRUE), 
                             MoE = vlookup(threshold,subpopData,5,TRUE)) %>% 
      {if(revOrder == FALSE)
        mutate(., UCB95 = Estimate + MoE,
               LCB95 = Estimate - MoE,
               n = max(subpopData$NResp))
        else mutate(., Estimate = 100 - Estimate, 
                    UCB95 = Estimate + MoE,
                    LCB95 = Estimate - MoE,
                    n = max(subpopData$NResp))}  %>% 
      mutate(UCB95 = case_when(UCB95 > 100 ~ 100,
                               UCB95 < 0 | is.na(UCB95) ~ 0,
                               TRUE~ as.numeric(UCB95)),
             LCB95 = case_when(LCB95 > 100 ~ 100,
                               LCB95 < 0 | is.na(LCB95) ~ 0,
                               TRUE~ as.numeric(LCB95))) %>% 
      dplyr::select(-MoE) 
  }
  return(statsSubpop)
}


organizedOptSuboptStats <- #left_join(
  dataOrgThreshold(probCDF,
                   subpopulations = c('Chowan', 'Rappahannock', 'York', 'Potomac',
                                      'Shenandoah', 'Roanoke Basin', 'James Basin',
                                      'New', 'Big Sandy', 'Clinch-Powell', 'Holston',
                                      "Virginia"), 
                   indicator = "TN", 
                   threshold = 2, 
                   revOrder = T ) %>% 
    mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ "Roanoke",
                                     Subpopulation == 'James Basin' ~ "James",
                                     TRUE ~ as.character(Subpopulation)))#,
  # dataOrgThreshold(probCDF,
  #                  subpopulations = c('Chowan', 'Rappahannock', 'York', 'Potomac',
  #                                     'Shenandoah', 'Roanoke Basin', 'James Basin',
  #                                     'New', 'Big Sandy', 'Clinch-Powell', 'Holston',
  #                                     "Virginia"), 
  #                  indicator = "VSCIVCPMI", 
  #                  threshold = 60, 
  #                  revOrder = F ) %>% 
  #   dplyr::select(-Rank) %>% 
  #   mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ "Roanoke",
  #                                    Subpopulation == 'James Basin' ~ "James",
  #                                    TRUE ~ as.character(Subpopulation))), 
  # by = "Subpopulation")
                                   


optimalSuboptimalRanges <- list(TN = list(Optimal = tibble(threshold = 1,
                                                           direction = FALSE),
                                          Suboptimal = tibble(threshold = 2,
                                                              direction = TRUE), 
                                          units = 'mg/L',
                                          prettyName = 'Total Nitrogen'))


dat <- probCDF
subpopulations <- c('Chowan', 'Rappahannock', 'York', 'Potomac',
                     'Shenandoah', 'Roanoke Basin', 'James Basin',
                     'New', 'Big Sandy', 'Clinch-Powell', 'Holston',
                     "Virginia")
indicator = "TN"
map.table <- map.tableBasin
optimalSuboptimal <- 'Suboptimal'
micromapOptimalSuboptimal(dat = probCDF,
                          subpopulations = c("Piedmont", "Northern Piedmont", "Central Appalachian Ridges and Valleys",
                            "Southeastern Plains", "Blue Ridge Mountains", "Central Appalachians", "Virginia"),
                          indicator = 'TN', 
                          map.table = map.tableEcoregion, 
                          optimalSuboptimalRanges, 
                          optimalSuboptimal = "Optimal")
                                      

# function to plot interquartile range by subpopulation and indicator
micromapOptimalSuboptimal <- function(dat, subpopulations, indicator,  map.table, optimalSuboptimalRanges, optimalSuboptimal){

  pullThreshold <- optimalSuboptimalRanges[indicator] %>% 
    map_df(optimalSuboptimal) %>% dplyr::select(threshold) %>% pull()
  pullDirection <- optimalSuboptimalRanges[indicator] %>% 
    map_df(optimalSuboptimal) %>% dplyr::select(direction) %>% pull()
  
  organizedOptSuboptStats <- dataOrgThreshold(dat,
                                              subpopulations = subpopulations, 
                                              indicator = indicator, 
                                              threshold = pullThreshold, 
                                              revOrder =  pullDirection ) %>% 
    mutate(Subpopulation = case_when(Subpopulation == 'Roanoke Basin' ~ "Roanoke",
                                     Subpopulation == 'James Basin' ~ "James",
                                     Subpopulation == "Central Appalachian Ridges and Valleys" ~ "Ridge and Valley",
                                     Subpopulation == "Blue Ridge Mountains" ~ "Blue Ridge",
                                     TRUE ~ as.character(Subpopulation)))

  
  statewide <- filter(organizedOptSuboptStats, Subpopulation == 'Virginia')$Estimate
  
  plotData <-  filter(organizedOptSuboptStats, Subpopulation != 'Virginia') %>% 
    arrange(desc(Estimate)) %>%
    mutate(Rank=1:nrow(.))
  
  
  suppressWarnings(
    mmplot(stat.data=as.data.frame(plotData),
           map.data=map.table,
           map.link=c("Subpopulation", "ID"),
           panel.types=c('map','labels', 'labels','bar_cl'),
           panel.data=list(NA,'Rank','Subpopulation',
                           list( "Estimate","LCB95","UCB95")),
           ord.by='Estimate',
           grouping=3,
           median.row=F,
           plot.height=7,
           plot.width=8,
           colors=brewer.pal(3, "Spectral"),
           rev.ord=T,
           panel.att=list(list(1,header='Light rmarkGray Means\nPreviously Displayed',
                               map.all=TRUE, fill.regions='aggregate',
                               active.border.color='black', active.border.size=1.0,
                               inactive.border.color=gray(.7), inactive.border.size=1, 
                               panel.width=1.0),
                          list(2, header='Rank', panel.width=.15, 
                               align='left', text.size=.9),
                          list(3, header='Subpopulation', panel.width=.55, 
                               align='left', text.size=.9),
                          list(4,header=paste0('Percent of Stream Miles with \n',optimalSuboptimal, ' ',
                                               optimalSuboptimalRanges[[as.character(unique(organizedOptSuboptStats$Indicator))]]$prettyName),
                               graph.bgcolor='lightgray',
                               graph.bar.size = .4,
                               xaxis.ticks=list(0,20,40,60,80,100),
                               xaxis.labels=list(0,20,40,60,80,100),
                               add.line=statewide ,add.line.col='black',add.line.typ='dashed',
                               xaxis.title='Percent of Stream Miles'))))
}
                         
  













stressparams <- data.frame(stress=c('Total Nitrogen (mg/L)','Total Phosphorus (mg/L)',
                                    'Habitat Degradation (unitless)',
                                    'Streambed Sedimentation (unitless)',
                                    'Ionic Strength (TDS mg/L)',
                                    'Cumulative Dissolved Metals (unitless)'),
                           Optimal=c('< 1','< 0.02','> 150','-0.5 to 0.5','< 100','< 1'),
                           Suboptimal=c('> 2','> 0.05','< 120','< -1.0','> 350','> 2'),
                           classref=c('(DEQ 2006a)','(DEQ 2006a)','(USEPA 1999)',
                                      '(Kaufmann 1999)','(DEQ 2006b)','(Clements 2000)'))
names(stressparams) <- c('Stressor Parameters','Optimal','Suboptimal','Classification Reference')





map.table <- create_map_table(basinssmooth_sp,'BASIN')
suppressWarnings(
  mmplot(stat.data=TotHabsummary,
         map.data=map.table,
         map.link=c("Subpopulation", "ID"),
         panel.types=c('map','labels','labels', 'bar_cl', 'bar_cl'),
         panel.data=list(NA,'Rank','Subpopulation',
                         list( "TotHabEstimate.P","TotHabLCB95Pct.P","TotHabUCB95Pct.P"),
                         list("VSCIVCPMIEstimate.P","VSCIVCPMILCB95Pct.P","VSCIVCPMIUCB95Pct.P")),
         ord.by='TotHabEstimate.P',
         grouping=3,
         median.row=F,
         plot.height=7,
         plot.width=8,
         colors=brewer.pal(3, "Spectral"),
         rev.ord=T,
         panel.att=list(list(1,header='Light Gray Means\nPreviously Displayed',
                             map.all=TRUE, fill.regions='aggregate',
                             active.border.color='black', active.border.size=1.0,
                             inactive.border.color=gray(.7), inactive.border.size=1, 
                             panel.width=1.0),
                        list(2, header='Rank', panel.width=0.15,
                             align='left', text.size=.9),
                        list(3, header='Basin', panel.width=.55, 
                             align='left', text.size=.9),
                        list(4,header='Percent of Stream Miles with \nSuboptimal Habitat Disturbance',
                             graph.bgcolor='lightgray',
                             graph.bar.size = .4,
                             xaxis.ticks=list(0,20,40,60,80,100), xaxis.labels=list(0,20,40,60,80,100)
                             ,add.line=16.7393531 ,add.line.col='black',add.line.typ='dashed',
                             xaxis.title='Percent of Stream Miles'),
                        list(5,header='Percent of Stream Miles below \n VSCI/VCPMI Assessment Threshold',
                             graph.bgcolor='lightgray',
                             graph.bar.size = .4,
                             add.line=45.41627,add.line.col='black',add.line.typ='dashed',
                             xaxis.title='Percent of Stream Miles',
                             xaxis.ticks = c(0,20,40,60,80,100),
                             xaxis.labels = c(0,20,40,60,80,100)))))
