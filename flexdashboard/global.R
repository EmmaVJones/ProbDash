# CDF plot function
cdfplotFunction <- function(data,parameter,indicator,confIntervals){
  n <- max(data$NResp)
  
  if(confIntervals==FALSE){
    ggplot(data, aes(x=Value,y=Estimate.P)) + geom_point() + 
      labs(x=indicator,y="Percentile") +
      ggtitle(paste(parameter,"Percentile Graph ( n =",n,")",sep=" ")) +
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15),
            axis.title = element_text(face='bold',size=12))
  }else{
    ggplot(data, aes(x=Value,y=Estimate.P)) + geom_point() + 
      labs(x=indicator,y="Percentile") +
      ggtitle(paste(parameter,"Percentile Graph ( n =",n,")",sep=" ")) +
      theme(plot.title = element_text(hjust=0.5,face='bold',size=15),
            axis.title = element_text(face='bold',size=12))+
      geom_line(aes(x=Value,y=LCB95Pct.P),color='orange')+
      geom_line(aes(x=Value,y=UCB95Pct.P),color='orange')
  }
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
