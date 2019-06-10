---
  title: "WIP Data Dashboard"
output: 
  flexdashboard::flex_dashboard:
  orientation: rows
social: menu
logo: rb.png
source_code: embed
vertical_layout: fill

---
  
  .emphasized {font-size: 1.5em;
    text-align:center;
    color: green;
    
  }
.notimportant {
  text-align:center;
  color: #263168;
    
}

```{r setup, include=FALSE}
suppressMessages(library(flexdashboard,quietly=TRUE))
suppressMessages(library(crosstalk,quietly=TRUE))
suppressMessages(library(tidyverse,quietly=TRUE))
suppressMessages(library(data.table,quietly=TRUE))
suppressMessages(library(DT,quietly=TRUE))
library(wesanderson)
library(ggrepel)
library(ggpubr)
library(ggiraph)
library(gsubfn)
library(ggalt)
library(hrbrthemes)
library(plotly)
library(htmlwidgets)
library(cowplot)
library(tidyr)

options(scipen=10000)

loads36=read.csv("loads_36.csv",h=T)


long_loads227 <- loads36 %>% gather(Title,Value ,VA.Draft.WIP.3v7_Amount:X1985.Progress_SLoadEOT) %>% 
  separate(Title, c("Year", "Progress","Type"), sep = "\\.{1}")  %>% mutate(Year=gsub('X','',Year))%>%
  unite_( "Type", c("Progress","Type"))%>%
  filter(Value > 0 & str_detect(Type, 'PLoadEOT') | str_detect(Type, 'NLoadEOT')) %>% 
  mutate(Type2=ifelse(str_detect(Type, 'NLoadEOT'),"Nitrogen","Phosphorus")) %>%
  select(everything(),-contains("Amount"))%>%
  select(everything(),-contains("SLoad"))%>%
  
  
  mutate(Geography=case_when(
    str_detect(Geography,"James") ~ "James",
    str_detect(Geography,"Eastern")~ "Eastern Shore",
    str_detect(Geography,"York") ~"York",
    str_detect(Geography,"Rappahannock") ~"Rappahannock",
    str_detect(Geography,"Potomac") ~"Potomac",
    TRUE  ~ as.character(Geography)
  )) %>% 
  mutate(Wip_Target=as.numeric(case_when(
    Type2=="Phosphorus" & Geography=="Eastern Shore" ~ "0.164",
    Type2=="Phosphorus" & Geography=="James" ~ "2.758",                        
    Type2=="Phosphorus" & Geography=="Potomac" ~ "1.867",                             
    Type2=="Phosphorus" & Geography=="Rappahannock" ~ "0.84",                              
    Type2=="Phosphorus" & Geography=="York" ~ "0.557",
    Type2=="Nitrogen" & Geography=="Eastern Shore" ~ "1.42",
    Type2=="Nitrogen" & Geography=="James" ~ "26.01",                        
    Type2=="Nitrogen" & Geography=="Potomac" ~ "15.98",                             
    Type2=="Nitrogen" & Geography=="Rappahannock" ~ "6.86",                              
    Type2=="Nitrogen" & Geography=="York" ~ "5.54",
    TRUE                      ~ "other" 
  )
  ),Climate=as.numeric(case_when(
    Type2=="Phosphorus" & Geography=="Eastern Shore" ~ "5000",
    Type2=="Phosphorus" & Geography=="James" ~ "59000",                        
    Type2=="Phosphorus" & Geography=="Potomac" ~ "82000",                             
    Type2=="Phosphorus" & Geography=="Rappahannock" ~ "27000",                              
    Type2=="Phosphorus" & Geography=="York" ~ "14000",
    Type2=="Nitrogen" & Geography=="Eastern Shore" ~ "110000",
    Type2=="Nitrogen" & Geography=="James" ~ "480000",                        
    Type2=="Nitrogen" & Geography=="Potomac" ~ "620000",                             
    Type2=="Nitrogen" & Geography=="Rappahannock" ~ "310000",                              
    Type2=="Nitrogen" & Geography=="York" ~ "200000",
    TRUE                      ~ "other" 
  ) ),Wip_Target=Wip_Target *1000000,Climate2= Wip_Target - Climate)


long_loads227_gapclose=long_loads227 %>% 
  mutate(Value2=ifelse(Year==2019 & Value > 0,case_when(
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="Eastern Shore" ~ Value-3000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="York" ~ Value-46000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="Rappahannock" ~ Value-39000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="James" ~  Value -3520000,
    Type2=="Nitrogen" & Sector=="Wastewater" & Geography=="Potomac" ~ Value-105000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="Eastern Shore" ~ Value - 0,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="York" ~ Value-20000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="Rappahannock" ~ Value-20000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="James" ~ Value -60000,
    Type2=="Nitrogen" & Sector=="Natural" & Geography=="Potomac" ~ Value-100000,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="Eastern Shore" ~ Value-400,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="York" ~ Value-5300,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="Rappahannock" ~ Value-2200,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="James" ~ Value -236500,
    Type2=="Phosphorus" & Sector=="Wastewater" & Geography=="Potomac" ~ Value-8200,
    TRUE                      ~ Value-0 
  ),Value),Diff=Value-Value2,Value2=ifelse(Value2 > 0,Value2,0))


##

long_loads227_gapclose=long_loads227_gapclose %>% 
  add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2019,Value2=300000.01,Type2="Phosphorus",Modified.Sector="Wastewater") %>%
  add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2018,Value2=300000.02,Type2="Phosphorus",Modified.Sector="Wastewater") %>%
  add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2019,Value2=3000000.03,Type2="Nitrogen",Modified.Sector="Wastewater")%>%
  add_row(Geography="James",Climate2=0,Climate=0,Wip_Target=0,Year=2018,Value2=3000000.04,Type2="Nitrogen",Modified.Sector="Wastewater") 

###

extrabar=long_loads227_gapclose %>%
  filter(Year==2019 | Year==2018) %>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  group_by(Type2,Year) %>%
  summarise(Climate3=sum(Value2)+sum(unique(Climate)))

extrabar2=long_loads227_gapclose %>%
  filter(Year==2019 | Year==2018) %>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  group_by(Type2,Year,Geography) %>%
  summarise(Climate3=sum(Value2)+sum(unique(Climate)))

##NITRO PLOT
##Phos PLot
tool_nitro=long_loads227_gapclose %>%
  filter(Type2=="Nitrogen") %>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  group_by(Modified.Sector,Year) %>%
  summarise(Val=sum(Value2))
#distinct(Modified.Sector,Year,Val,.keep_all = TRUE)


tool_nitro$tooltip= c(paste0("Sector: ", tool_nitro$Modified.Sector, "\n Year: ", tool_nitro$Year, "\n Pounds: ", scales::comma(tool_nitro$Val)))



tool_nitro_Plot = na.omit(tool_nitro) %>%
  ggplot()+
  annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
  ### didn't need ##geom_text(alpha=0.5,hjust =0.35,vjust =1.5,colour="grey75",size=8,aes(x = "2018"),y=Inf, label = "WIP III")+
  #geom_bar(fill="grey45",width=0.7,data=extrabar %>% filter(Type2=="Nitrogen"),stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
  geom_bar_interactive(fill="grey45",width=0.7,data=extrabar %>% filter(Type2=="Nitrogen"),stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
  geom_bar_interactive(aes(tooltip=tooltip,x=Year,y=Val,fill=Modified.Sector),stat="identity",width=0.7)+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  #geom_hline(data=.%>% filter(Type2=="Nitrogen"),aes(yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
  geom_hline_interactive(data=long_loads227_gapclose %>% filter(Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",scales::comma(sum(unique(Wip_Target)))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
  scale_color_manual(values=c("Climate Change"="grey65"))+
  theme_minimal()+
  #theme_ipsum_rc(grid="Y")+
  theme(legend.title=element_blank())+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  theme(legend.key.size =  unit(0.25, "in"))+
  #facet_wrap(~Type2,scales = "free",nrow=2)+
  theme(legend.text=element_text(size=12),legend.position="right")+
  scale_fill_manual(na.translate = F,name = 'WIP III Target', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
  scale_x_discrete(na.translate = F,limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","WIP.Initial","WIP.Draft"),
                   labels=c("1985","-","2009","2010","2011","2012","2013","2014","2015","2016","2017","-","WIP3.Initial","WIP3.Draft"))+
  labs(x=NULL, y=NULL, title="Nitrogen")+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=12,face="bold"))+
  theme(axis.line = element_line())+
  scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))+
  #theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(size=15, colour="white"))



##Phos PLot

tool_phos=long_loads227_gapclose %>%
  filter(!Type2=="Nitrogen") %>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  group_by(Modified.Sector,Year) %>%
  summarise(Val=sum(Value2))
#distinct(Modified.Sector,Year,Val,.keep_all = TRUE)


tool_phos$tooltip= c(paste0("Sector: ", tool_phos$Modified.Sector, "\n Year: ", tool_phos$Year, "\n Pounds: ", scales::comma(tool_phos$Val)))



tool_phos_Plot = na.omit(tool_phos) %>%
  ggplot()+
  annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
  ### didn't need ##geom_text(alpha=0.5,hjust =0.35,vjust =1.5,colour="grey75",size=8,aes(x = "2018"),y=Inf, label = "WIP III")+
  #geom_bar(fill="grey45",width=0.7,data=extrabar %>% filter(Type2=="phosgen"),stat="identity",aes(colour="Climate Change",x=Year,y=Climate3))+
  geom_bar(fill="grey45",width=0.7,data=extrabar %>% filter(!Type2=="Nitrogen"),stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
  geom_bar_interactive(aes(tooltip=tooltip,x=Year,y=Val,fill=Modified.Sector),stat="identity",width=0.7)+
  scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
  #geom_hline(data=.%>% filter(Type2=="phosgen"),aes(yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
  geom_hline_interactive(data=long_loads227_gapclose %>% filter(!Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",scales::comma(sum(unique(Wip_Target)))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
  scale_color_manual(values=c("Climate Change"="grey65"))+
  theme_minimal()+
  #theme_ipsum_rc(grid="Y")+
  theme(legend.title=element_blank())+
  theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
  theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
  theme(legend.key.size =  unit(0.25, "in"))+
  #facet_wrap(~Type2,scales = "free",nrow=2)+
  theme(legend.text=element_text(size=12),legend.position="right")+
  scale_fill_manual(na.translate = F,name = 'WIP III Target', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
  scale_x_discrete(na.translate = F,limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","WIP.Initial","WIP.Draft"),
                   labels=c("1985","-","2009","2010","2011","2012","2013","2014","2015","2016","2017","-","WIP3.Initial","WIP3.Draft"))+
  labs(x=NULL, y=NULL, title="Phosphorus")+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=12,face="bold"))+
  theme(axis.line = element_line())+
  scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))+
  #theme(panel.border = element_rect(colour = "black", fill=NA, size=1))+
  theme(strip.background = element_rect(fill="black"),
        strip.text = element_text(size=15, colour="white"))

####################### Nutrients Each BAsin

extrabar2=long_loads227_gapclose %>%
  filter(Year==2019 | Year==2018) %>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  group_by(Type2,Year,Geography) %>%
  summarise(Climate3=sum(Value2)+sum(unique(Climate)))


long_loads227_gapclose2=long_loads227_gapclose %>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  group_by(Type2,Geography,Modified.Sector,Year) %>%
  summarise(Val=sum(Value2),Wip_Target=max(Wip_Target))

long_loads227_gapclose2$tooltip3= c(paste0("Sector: ", long_loads227_gapclose2$Modified.Sector, "\n Year: ", long_loads227_gapclose2$Year, "\n Pounds: ", scales::comma(long_loads227_gapclose2$Val)))




h3=long_loads227_gapclose2 %>% 
  rowid_to_column(var = "rownum") %>% 
  #  gather(contains("."), key = "Measurement", value = "cm") %>% 
  group_by(Geography) %>% 
  nest %>% 
  mutate(plot = map2(Geography, data,function (.x,.y){
    ggplot(data = .y, aes(x=Year,y=Val,fill=Modified.Sector)) +
      annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
      scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% .x & Type2=="Nitrogen" & Year=="WIP.Draft") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x & !Type2=="Nitrogen" & Year=="WIP.Draft") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x & Type2=="Nitrogen" & Year=="WIP.Initial") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x &!Type2=="Nitrogen" & Year=="WIP.Initial") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(stat="identity",width=0.7,aes(tooltip=.y[["tooltip3"]]))+
      geom_hline_interactive(data=.%>% filter(Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",sum(unique(Wip_Target))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
      geom_hline_interactive(data=.%>% filter(!Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",sum(unique(Wip_Target))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
      facet_wrap(~Type2, scales = "free",ncol=1)+
      scale_color_manual(values=c("Climate Change"="grey65"))+
      theme_minimal()+
      theme_ipsum_rc(grid="Y")+
      theme(legend.title=element_blank())+
      theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
      theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
      theme(legend.key.size =  unit(0.25, "in"))+
      theme(legend.text=element_text(size=12),legend.position="right")+
      scale_fill_manual(name = 'WIP 3 Progress', 
                        values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                                  "Wastewater"="#d73027","Septic"="#d8b365"))+
      scale_x_discrete(limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","WIP.Initial","WIP.Draft"),
                       labels=c("1985","","2009","2010","2011","2012","2013","2014","2015","2016","2017","","WIP III Initial","WIP III Draft"))+
      labs(x=NULL, y=NULL, title="")+
      theme(strip.background =element_rect(fill="black"))+
      theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
      theme(axis.line = element_line())+
      scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))+
      ggtitle(label = .x)  
  }))


h_nitro=long_loads227_gapclose2 %>% 
  rowid_to_column(var = "rownum") %>%
  filter(Type2=="Nitrogen")%>%
  #  gather(contains("."), key = "Measurement", value = "cm") %>% 
  group_by(Geography) %>% 
  nest %>% 
  mutate(plot = map2(Geography, data,function (.x,.y){
    ggplot(data = .y, aes(x=Year,y=Val,fill=Modified.Sector)) +
      annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
      scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% .x & Type2=="Nitrogen" & Year=="WIP.Draft") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      # geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x & !Type2=="Nitrogen" & Year=="WIP.Draft") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x & Type2=="Nitrogen" & Year=="WIP.Initial") ,stat="identity",aes(tooltip=paste0(scales::comma(Climate3),"Climate"),colour="Climate Change",x=Year,y=Climate3))+
      #geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x &!Type2=="Nitrogen" & Year=="WIP.Initial") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(stat="identity",width=0.7,aes(tooltip=.y[["tooltip3"]]))+
      geom_hline_interactive(data=.%>% filter(Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",scales::comma(sum(unique(Wip_Target)))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
      #geom_hline_interactive(data=.%>% filter(!Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",sum(unique(Wip_Target))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
      #facet_wrap(~Type2, scales = "free",ncol=1)+
      scale_color_manual(values=c("Climate Change"="grey65"))+
      theme_minimal()+
      theme_ipsum_rc(grid="Y")+
      theme(legend.title=element_blank())+
      theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
      theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
      theme(legend.key.size =  unit(0.25, "in"))+
      theme(legend.text=element_text(size=12),legend.position="right")+
      scale_fill_manual(name = 'WIP 3 Progress', 
                        values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                                  "Wastewater"="#d73027","Septic"="#d8b365"))+
      scale_x_discrete(limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","WIP.Initial","WIP.Draft"),
                       labels=c("1985","","2009","2010","2011","2012","2013","2014","2015","2016","2017","","WIP III Initial","WIP III Draft"))+
      labs(x=NULL, y=NULL, title="")+
      theme(strip.background =element_rect(fill="black"))+
      theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
      theme(axis.line = element_line())+
      scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))+
      ggtitle(label = "Nitrogen") 
  }))


h_phos=long_loads227_gapclose2 %>% 
  rowid_to_column(var = "rownum") %>%
  filter(!Type2=="Nitrogen")%>%
  #  gather(contains("."), key = "Measurement", value = "cm") %>% 
  group_by(Geography) %>% 
  nest %>% 
  mutate(plot = map2(Geography, data,function (.x,.y){
    ggplot(data = .y, aes(x=Year,y=Val,fill=Modified.Sector)) +
      annotate("rect", xmin="14", xmax=Inf, ymin=0, ymax=Inf,alpha=0.1, fill="black",hjust = 0.2)+
      scale_y_continuous(expand =expand_scale(mult = c(0, .1)),labels = scales::comma)+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2 %>% filter(Geography %in% .x & !Type2=="Nitrogen" & Year=="WIP.Draft") ,stat="identity",aes(tooltip=paste0(scales::comma(Climate3),"Climate"),colour="Climate Change",x=Year,y=Climate3))+
      # geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x & !Type2=="Nitrogen" & Year=="WIP.Draft") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x & !Type2=="Nitrogen" & Year=="WIP.Initial") ,stat="identity",aes(tooltip=scales::comma(Climate3),colour="Climate Change",x=Year,y=Climate3))+
      #geom_bar_interactive(fill="grey45",width=0.7,data=extrabar2  %>% filter(Geography %in% .x &!Type2=="Nitrogen" & Year=="WIP.Initial") ,stat="identity",aes(tooltip=Climate3,colour="Climate Change",x=Year,y=Climate3))+
      geom_bar_interactive(stat="identity",width=0.7,aes(tooltip=.y[["tooltip3"]]))+
      geom_hline_interactive(data=.%>% filter(!Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",scales::comma(sum(unique(Wip_Target)))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
      #geom_hline_interactive(data=.%>% filter(!Type2=="Nitrogen"),aes(tooltip=paste0("WIP3:",sum(unique(Wip_Target))),yintercept=sum(unique(Wip_Target)),linetype = "WIP III Target"),color="black", size=1.5)+
      #facet_wrap(~Type2, scales = "free",ncol=1)+
      scale_color_manual(values=c("Climate Change"="grey65"))+
      theme_minimal()+
      theme_ipsum_rc(grid="Y")+
      theme(legend.title=element_blank())+
      theme(axis.text = element_text(colour = "black", size = rel(1.1)))+
      theme(axis.text.x=element_text(angle=55, hjust=1,size = 12))+
      theme(legend.key.size =  unit(0.25, "in"))+
      theme(legend.text=element_text(size=12),legend.position="right")+
      scale_fill_manual(name = 'WIP 3 Progress', 
                        values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                                  "Wastewater"="#d73027","Septic"="#d8b365"))+
      scale_x_discrete(limits=c("1985","1111","2009","2010","2011","2012","2013","2014","2015","2016","2017","14","WIP.Initial","WIP.Draft"),
                       labels=c("1985","","2009","2010","2011","2012","2013","2014","2015","2016","2017","","WIP III Initial","WIP III Draft"))+
      labs(x=NULL, y=NULL, title="")+
      theme(strip.background =element_rect(fill="black"))+
      theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
      theme(axis.line = element_line())+
      scale_linetype_manual(values=c("WIP III Target"="solid", "Climate Change"="dotted"))+
      ggtitle(label = "Phosphorus") 
  }))




#______________________BMPs_____________________________________________________________________________________________________________



#   Initial=April

#   Draft=May



#~~~~~~~~~~~~~~~~~~Animal~~~~~~~~~~~~~~~~~~~~~~~~~
W2017BMPDetail_Animal <- read.csv("2017BMPDetail_Animal.csv",h=T)%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)


Animal_Initial <- read.csv("animal_Initial322.csv",h=T)
Animal_Initial=Animal_Initial %>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)

#Animal_April <- read.csv("Animal_April.csv",h=T)
#Animal_April=Animal_April %>% 
#select(Geography,BMP,Unit,Sector,AmountCredited)

Animal_May <- read.csv("animal_Draft.csv",h=T)
Animal_May=Animal_May %>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)





#~~~~~~~~~~~~~~~~~~~~Land~~~~~~~~~~~~~~~~~~~~~~~~
W2017BMPDetail_Land <- read.csv("2017BMPDetail_Land.csv",h=T)%>%
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)

Land_Initial <- read.csv("land_Initial322.csv",h=T)
Land_Initial=Land_Initial %>%
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)

#Land_April <- read.csv("Land_April.csv",h=T)
#Land_April=Land_April %>% 
#filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
#select(Geography,BMP,Unit,Sector,AmountCredited)

Land_May <- read.csv("land_Draft.csv",h=T)
Land_May=Land_May %>% 
  filter(str_detect(Agency,"Non-Federal") & !str_detect(FromLoadSource, paste(c("Ms4","CSS"),collapse="|")))%>% 
  select(Geography,BMP,Unit,Sector,AmountCredited)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WIP_2017=rbind(W2017BMPDetail_Animal,W2017BMPDetail_Land)
WIP_2017=WIP_2017 %>% mutate(Progress="V2017")

Initial=rbind(Land_Initial,Animal_Initial)
Initial=Initial %>% mutate(Progress="Initial")

#April=rbind(Land_April,Animal_April)
#April=April %>% mutate(Progress="April")

May=rbind(Land_May,Animal_May)
May=May %>% mutate(Progress="May")



WIP_BMPs=rbind(May,Initial,WIP_2017)
### Outliers



####-----------------------------------------------------------------------------
#> nrow(WIP_BMPs)
#[1] 8971

WIP_no_covercrop=WIP_BMPs %>%
  filter(!str_detect(BMP,"Cover Crop"))
# nrow(WIP_no_covercrop) 8197


TRY=WIP_BMPs %>%filter(str_detect(BMP, "Cover Crop"))
#nrow=774 

wip_covercrop= TRY %>%  mutate(BMP = case_when(
  str_detect(BMP, "Late") ~ "Cover Crop (Late)",
  str_detect(BMP, "Early") ~ "Cover Crop (Early)",
  str_detect(BMP, "Commodity") ~ "Cover Crop (Commodity)",
  TRUE                   ~ "Cover Crop Traditional"
)) 

WIP_MOd=rbind(wip_covercrop,WIP_no_covercrop)
#nrow 8971

#For pasture Fence, lets combine the rotational grazing, alternate water and pasture fence into one practice, call it 
#"Livestock Stream Exclusion and Pasture Management".  That should bump it up the list
#
#

WIP_MOd_sum2=WIP_MOd %>%
  filter(!str_detect(BMP, paste(c("Upland Acres","Nutrient Management Core P","Nutrient Management P Rate","Nutrient Management P Timing",
                                  "Nutrient Management N Rate","Nutrient Management N Timing","Nutrient Management N Placement"),collapse="|")))%>%
  
  mutate(BMP=ifelse(Sector=="Agriculture" & str_detect(BMP,"Nutrient Management Core N"),"Agriculture Nutrient Management Core", BMP))%>%
  mutate(BMP=ifelse(Sector=="Agriculture" & str_detect(BMP,"Nutrient Management P Placement"),"Agriculture Nutrient Management Enhanced", BMP))%>%
  mutate(BMP=ifelse(!str_detect(BMP,"Upland Acres") & str_detect(BMP,paste(c("Exclusion Fencing","Off Stream Watering","Rotational/Perscribed Grazing"),
                                                                           collapse="|")),"Livestock Stream Exclusion and Pasture Management",BMP))%>%
  mutate(BMP=ifelse(Sector=="Agriculture" & str_detect(BMP,"Tillage Management"),"Tillage Management", BMP))%>%
  mutate(BMP=ifelse(str_detect(BMP,"Stream Restoration"),"Stream Restoration", BMP))%>%
  mutate(BMP=ifelse(str_detect(BMP,"Shoreline Management"),"Shoreline Management", BMP))%>%
  mutate(BMP=ifelse(str_detect(BMP,"Shoreline Erosion Control"),"Shoreline Management", BMP))%>%
  mutate(BMP=ifelse(str_detect(BMP,"Nutrient Management Plan"),"Urban Nutrient Management Plan", BMP))%>%
  mutate(Sector,Sector=plyr::revalue(Sector,c("Developed" ="Developed(Non-MS4)")))%>%
  
  #mutate(Sector=ifelse(Sector=="Developed" & str_detect(Sector,"Developed"),"Developed(Non-MS4)", Sector))%>%
  group_by(BMP,Geography,Progress)%>%
  mutate(AmountCredited=sum(AmountCredited))%>%
  distinct()
#distinct(AmountCredited, .keep_all = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~QA~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
WIP_BMPs_Raw=WIP_BMPs%>%
  group_by(BMP,Geography,Progress)%>%
  mutate(AmountCredited=sum(AmountCredited))%>%
  distinct() %>%
  spread(Progress,AmountCredited,fill=0)%>%
  mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
  mutate(Flag=ifelse(V2017 > May | V2017 > Initial,"Flag",""))%>%
  mutate(Flag=ifelse(Initial > May,"Flag 2",Flag))%>%  
  distinct(V2017,Initial,May,.keep_all = TRUE)

WIP_BMP_Modified=WIP_MOd_sum2 %>%
  #filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  group_by(BMP) %>%spread(Progress,AmountCredited,fill=0)%>%
  mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
  mutate(Flag=ifelse(V2017 > May | V2017 > Initial,"Flag",""))%>%
  mutate(Flag=ifelse(Initial > May,"Flag 2",Flag))%>%  
  distinct(V2017,Initial,May,.keep_all = TRUE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#  #         #             #         #              #                  #                           #

#

#       #         #     BMP          #                #                       #                           #

#



###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Bay wide~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


WIP_BMP_baywide=WIP_MOd_sum2 %>%
  #filter(!Progress=="April") %>%
  filter(str_detect(Unit, paste(c("acres","feet"),collapse="|")) & !str_detect(BMP, 'Growth Management'))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  group_by(BMP) %>%
  mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
  distinct(May,V2017,Initial,.keep_all = TRUE)

#labelbay=WIP_BMP_baywide %>% filter(BMP=="Agriculture Nutrient Management Core")
labelbay=WIP_BMP_baywide %>% filter(BMP=="Stream Restoration")

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$




All_BMP36_after_Shore= WIP_BMP_baywide %>%
  filter(str_detect(BMP,"Shoreline Management"))%>%
  filter(May > 85000) %>%
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  #geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  #geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  ###geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  geom_text(colour="black",show.legend = FALSE,size=5,aes(label=Unit,x=max(May)+378000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult =c(0.02,0.11)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.95)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.60, 0.42))+
  theme(legend.key.size = unit(1, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(colour="black",size=rel(2.1)),
        axis.text.y = element_text(colour="black",size=rel(2.1)))+
  theme(legend.position = "none")+
  theme(plot.margin = unit(c(0.25, 0.25, 0, 1), "cm"))



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!most BMPS !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
library(lemon)
library(cowplot)

All_BMP36_after= WIP_BMP_baywide %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(May > 85000) %>%
  ggplot()+ 
  geom_segment(linetype="dotted",aes(y=reorder(BMP,May,sum), yend=reorder(BMP,May,sum),xend=Initial),
               x=0,size=1,color='grey20',alpha=0.9)+
  #geom_segment(show.legend = FALSE,alpha=0.4,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= Initial,xend =May ))+
  geom_segment(show.legend = FALSE,size=5,aes(colour=Sector,y=reorder(BMP,May,sum),yend=reorder(BMP,May,sum),x= V2017,xend = Initial))+
  #geom_point(pch=124,show.legend = FALSE,size=9,aes(color=Sector,x=May,y=reorder(BMP,May,sum)))+
  geom_point(size=8,aes(color=Sector,x=Initial,y=reorder(BMP,May,sum)))+
  geom_text(colour="black",show.legend = FALSE,size=5,aes(label=Unit,x=max(May)+78000,y=reorder(BMP,May,sum)))+
  #geom_text(colour="grey37",show.legend = FALSE,alpha=0.7,size=4,aes(label=Unit,x=max(May)+100000,y=reorder(BMP,May,sum)))+
  labs(x=NULL, y=NULL, title="") +
  theme_ipsum_rc(grid="X") +
  scale_x_continuous(expand =expand_scale(mult = c(0.02,0.11)),labels = scales::comma)+
  theme(panel.grid.major.x=element_line(size=0.7))+
  theme(strip.text= element_text(colour = "grey20", face = "bold",size = rel(0.99)))+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  theme(legend.position = c(0.8, 0.2))+
  theme(legend.key.size = unit(1.5, "cm"))+
  theme(legend.text = element_text(colour="black", size=12))+
  theme(legend.background = element_rect(fill=NULL,size=0.5, linetype="solid", colour ="black"))+
  theme(axis.text.x = element_text(colour="black",size=rel(2.1)),
        axis.text.y = element_text(colour="black",size=rel(2.1)))+
  theme(legend.title=element_blank(),
        legend.margin=margin(c(2,6,2,6)))+
  #theme(legend.position="top",
  #       legend.justification="left")+
  # theme(legend.position = "none")+
  theme(plot.margin = unit(c(0, 0.25, 1, 0.25), "cm"))+
  geom_label_repel(show.legend = FALSE,color="black",data=labelbay,size=6,aes(fill=NULL,x=V2017, y=BMP,label="2017"),
                   nudge_y=0.9,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  geom_label_repel(show.legend = FALSE,color="black",data=labelbay,size=6,aes(fill=NULL,x=Initial, y=BMP,label="WIP III Initial"),
                   nudge_y=0.9,nudge_x=0,direction = "both",angle= 0,segment.size = 0.8)+
  #geom_label_repel(show.legend = FALSE,color="black",data=labelbay,size=6,aes(fill=NULL,x=May, y=BMP,label="WIP III Draft"),
  #nudge_y=1,nudge_x=30000,angle= 0,segment.size = 0.8)+
  #scale_y_discrete(position="top",expand =expand_scale(mult=(c(0.05,0.18))))
  scale_y_discrete(expand =expand_scale(add=c(0.5,1.1)))       


```

Sidebar {.sidebar data-width=250}
=======================================================================
  
  Virginia's
Phase III Watershed Implementation Plan
for the Chesapeake Bay


Links to additional information:
[DEQ WIP Webpage](https://www.deq.virginia.gov/Programs/Water/ChesapeakeBay/ChesapeakeBayTMDL/PhaseIIIWatershedImplementationPlanning.aspx)
[Public Comment form](https://www.vawipcomments.com)
[Download Document](https://www.deq.virginia.gov/Portals/0/DEQ/Water/ChesapeakeBay/Draft%20WIP%20III/Virginia%20Draft%20Chesapeake%20Bay%20TMDL%20Phase%20III%20WIP.docx)

Looking for help with the data dahsboard?
send email


Eastern Shore
=======================================================================

#Eastern Shore {.tabset .tabset-fade .tabset-pills}
-------------------------------------
   
### Nutrients Table

```{r}
 
ES=long_loads227_gapclose %>% 
filter(Geography=="Eastern Shore") %>%
#rename(Climate2= "Climate.Change",Type2="Nutrient",Value2="Amount") %>%
select(Modified.Sector,Nutrient=Type2,LoadSource,Amount=Value2,Unit,Year)%>%
mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
arrange(desc(Amount))

DT::datatable(ES,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```   

### Nutrients Plots
 

tab {
    display: inline-block; 
    margin-left: 10px; 
}
h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
 Eastern Shore 
```{r fig.width=10, fig.height=8}

 x1 <- girafe(ggobj = h_nitro[[1,3]], width = 1,width_svg = 8)
          
          x1=girafe_options(x1,
                            opts_tooltip(use_fill = TRUE,opacity = .9),
                            opts_zoom(min = .5, max = 4),
                            sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                            opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 
          
          y1 <- girafe(ggobj = h_phos[[1,3]], width = 1,width_svg = 8)
          
          y1=girafe_options(y1,
                            opts_tooltip(use_fill = TRUE,opacity = .9),
                            opts_zoom(min = .5, max = 4),
                            sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                            opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 
          
          
          
          
          
          x1
          
          y1

```

### Land Use

```{r fig.width=10, fig.height=8,fig.align="center"}

pie1= read.csv("2017_LU.csv",h=T)
pie2= read.csv("2025_LU.csv",h=T)

pies= full_join(pie1, pie2)

pies4=pies %>%
mutate(Modified3=ifelse(LoadSource=='Regulated Construction',"Developed(Non MS4)", as.character(Modified))

)

pies5=pies %>% group_by(LoadSource) %>% 
  fill(Modified) %>%
  ungroup()

p7=pies5 %>% 
  filter(str_detect(StateBasin,"Eastern Shore")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))%>%
  
  ggplot(aes(x="", y=P, fill=Modified)) + 
 geom_bar_interactive(aes(tooltip = Percent),stat="identity", width=4,size = 1.5, color = "white")+
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_classic() + 
  facet_wrap(~Year) +
  scale_fill_manual(name = 'Sector', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
"Wastewater"="#d73027","Septic"="#d8b365"))+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = 16))+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))+
  theme(legend.text=element_text(size=12),legend.position="right")+
  ggtitle("Landuse")+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))

pp7=ggiraph(code=print(p7),width = 1,width_svg = 8)
v7=girafe_options(pp7, opts_tooltip(use_fill = TRUE), sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"))

bscols(widths =c(NA,1),v7,"")
```

### BMP Table
```{r}
EastDT=WIP_MOd_sum2 %>%
  #filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography,"Eastern")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  select(-Progress,-AmountCredited,-Geography)

DT::datatable(EastDT,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))
```

### BMP Plot
```{r}

h7=WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(!str_detect(BMP,"Oyster"))%>%
  filter(str_detect(Geography,"Eastern")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  filter(Number > 100) 

h7$tooltip= c(paste0("Sector: ", h7$Sector,"\nBMP: " ,h7$Unit,"\nUnit:",h7$BMP, "\n Progress: ", h7$Year, "\n Number:", scales::comma(h7$Number)))

shared_h7 <- SharedData$new(h7)

j7=shared_h7 %>%
  ggplot()+
  geom_point(aes(text1=Number,text2=BMP,text4=Unit,text3=Year,x=Number,y=reorder(BMP,Number,mean),colour=Sector,shape=Year),size=3)+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  labs(x=NULL,y=NULL,title="")+
  scale_y_discrete(expand=expand_scale(0.05))+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())#+
#theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))


bscols(widths = c(3,NA),
       list(
         filter_checkbox("year", "Year", shared_h7, ~Year, inline = TRUE),
         filter_select("bmp", "BMP", shared_h7, ~BMP),
         "","","","","Note: for visualization purposes, shoreline management and oyster aquaculture have been omitted from this plot. Refer to the BMP table tab for those values"
       ),
       list(filter_slider("number", "Number", shared_h7, ~Number, width = "99%"),
            ggplotly(j7,tooltip=c("text1","text2","text4","text3"),dynamicTicks = "TRUE",height=500)%>%
              highlight(color = "red",on="plotly_selected",off="plotly_deselect",persistent = getOption("persistent",TRUE)
              ))
       #DT::datatable(shared_h)
       #d3scatter(shared_h, ~Number,~BMP, ~factor(Sector))
       
)
```

Potomac
=======================================================================
  
  #Potomac {.tabset .tabset-fade .tabset-pills}
  -------------------------------------
  
  ### Nutrients Table
  
  ```{r}

poto=long_loads227_gapclose %>% 
  filter(Geography=="Potomac") %>%
  #rename(Climate2= "Climate.Change",Type2="Nutrient",Value2="Amount") %>%
  select(Modified.Sector,Nutrient=Type2,LoadSource,Amount=Value2,Unit,Year)%>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft"))%>%
  arrange(desc(Amount))

DT::datatable(poto,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```   

### Nutrients Plots



tab {
  display: inline-block; 
  margin-left: 10px; 
}
h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
Potomac 
```{r fig.width=10, fig.height=8}

x3 <- girafe(ggobj = h_nitro[[3,3]], width = 1,width_svg = 8)

x3=girafe_options(x3,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 

y3 <- girafe(ggobj = h_phos[[3,3]], width = 1,width_svg = 8)

y3=girafe_options(y3,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 





x3

y3

```

### Land Use

```{r fig.width=10, fig.height=8,fig.align="center"}

p6=pies5 %>% 
  filter(str_detect(StateBasin,"Potomac")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))%>%
  
  ggplot(aes(x="", y=P, fill=Modified)) + 
  geom_bar_interactive(aes(tooltip = Percent),stat="identity", width=4,size = 1.5, color = "white")+
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_classic() + 
  facet_wrap(~Year) +
  scale_fill_manual(name = 'Sector', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = 16))+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))+
  theme(legend.text=element_text(size=12),legend.position="right")+
  ggtitle("Landuse")+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))

pp6=ggiraph(code=print(p6),width = 1,width_svg = 8)
v6=girafe_options(pp6, opts_tooltip(use_fill = TRUE), sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"))

bscols(widths =c(NA,1),v6,"")

```

### BMP Table
```{r}
PotomacDT=WIP_MOd_sum2 %>%
  #filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography,"Potomac")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  select(-Progress,-AmountCredited,-Geography)

DT::datatable(PotomacDT,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))
```

### BMP Plot
```{r}

h6=WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(!str_detect(BMP,"Oyster"))%>%
  filter(str_detect(Geography,"Potomac")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  filter(Number > 100) 

h6$tooltip= c(paste0("Sector: ", h6$Sector,"\nBMP: " ,h6$Unit,"\nUnit:",h6$BMP, "\n Progress: ", h6$Year, "\n Number:", scales::comma(h6$Number)))

shared_h6 <- SharedData$new(h6)

j6=shared_h6 %>%
  ggplot()+
  geom_point(aes(text1=Number,text2=BMP,text4=Unit,text3=Year,x=Number,y=reorder(BMP,Number,mean),colour=Sector,shape=Year),size=3)+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  labs(x=NULL,y=NULL,title="")+
  scale_y_discrete(expand=expand_scale(0.05))+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())#+
#theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))


bscols(widths = c(3,NA),
       list(
         filter_checkbox("year", "Year", shared_h6, ~Year, inline = TRUE),
         filter_select("bmp", "BMP", shared_h6, ~BMP),
         "","","","","Note: for visualization purposes, shoreline management and oyster aquaculture have been omitted from this plot. Refer to the BMP table tab for those values"
       ),
       list(filter_slider("number", "Number", shared_h6, ~Number, width = "99%"),
            ggplotly(j6,tooltip=c("text1","text2","text4","text3"),dynamicTicks = "TRUE",height=500)%>%
              highlight(color = "red",on="plotly_selected",off="plotly_deselect",persistent = getOption("persistent",TRUE)))
       #DT::datatable(shared_h)
       #d3scatter(shared_h, ~Number,~BMP, ~factor(Sector))
       
)
```

Rappahannock
=======================================================================
  
  #Rappahannock {.tabset .tabset-fade .tabset-pills}
  -------------------------------------
  
  ### Nutrients Table
  
  ```{r}

rapp=long_loads227_gapclose %>% 
  filter(Geography=="Rappahannock") %>%
  #rename(Climate2= "Climate.Change",Type2="Nutrient",Value2="Amount") %>%
  select(Modified.Sector,Nutrient=Type2,LoadSource,Amount=Value2,Unit,Year)%>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft")) %>%
  arrange(desc(Amount))

DT::datatable(rapp,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```   

### Nutrients Plots



tab {
  display: inline-block; 
  margin-left: 10px; 
}
h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
Rappahannock 
```{r fig.width=10, fig.height=8}

x4 <- girafe(ggobj = h_nitro[[4,3]], width = 1,width_svg = 8)

x4=girafe_options(x4,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 

y4 <- girafe(ggobj = h_phos[[4,3]], width = 1,width_svg = 8)

y4=girafe_options(y4,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 





x4

y4

```

### Land Use

```{r fig.width=10, fig.height=8, fig.align="center"}

p3=pies5 %>% 
  filter(str_detect(StateBasin,"Rappahannock")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))%>%
  
  ggplot(aes(x="", y=P, fill=Modified)) + 
  geom_bar_interactive(aes(tooltip = Percent),stat="identity", width=4,size = 1.5, color = "white")+
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_classic() + 
  facet_wrap(~Year) +
  scale_fill_manual(name = 'Sector', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = 16))+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))+
  theme(legend.text=element_text(size=12),legend.position="right")+
  ggtitle("Landuse")+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))

pp3=ggiraph(code=print(p3),width = 1,width_svg = 8)
v3=girafe_options(pp3, opts_tooltip(use_fill = TRUE), sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"))

bscols(widths =c(NA,1),v3,"")

```

### BMP Table
```{r}
RappDT=WIP_MOd_sum2 %>%
  #filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography,"Rappahannock")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  select(-Progress,-AmountCredited,-Geography)

DT::datatable(RappDT,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))
```

### BMP Plot
```{r}

h5=WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(!str_detect(BMP,"Oyster")) %>%
  filter(str_detect(Geography,"Rappahannock")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  filter(Number > 100) 

h5$tooltip= c(paste0("Sector: ", h5$Sector,"\nBMP: " ,h5$Unit,"\nUnit:",h5$BMP, "\n Progress: ", h5$Year, "\n Number:", scales::comma(h5$Number)))

shared_h5 <- SharedData$new(h5)

j5=shared_h5 %>%
  ggplot()+
  geom_point(aes(text1=Number,text2=BMP,text4=Unit,text3=Year,x=Number,y=reorder(BMP,Number,mean),colour=Sector,shape=Year),size=3)+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  labs(x=NULL,y=NULL,title="")+
  scale_y_discrete(expand=expand_scale(0.05))+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())


bscols(widths = c(3,NA),
       list(
         filter_checkbox("year", "Year", shared_h5, ~Year, inline = TRUE),
         filter_select("bmp", "BMP", shared_h5, ~BMP),
         "","","","","Note: for visualization purposes, shoreline management and oyster aquaculture have been omitted from this plot. Refer to the BMP table tab for those values"
       ),
       list(filter_slider("number", "Number", shared_h5, ~Number, width = "90%"),
            ggplotly(j5,tooltip=c("text1","text2","text4","text3"),dynamicTicks = "TRUE",height=500)%>%
              highlight(color = "red",on="plotly_selected",off="plotly_deselect",persistent = getOption("persistent",TRUE)))
       #DT::datatable(shared_h)
       #d3scatter(shared_h, ~Number,~BMP, ~factor(Sector))
       
)
```

York
=======================================================================
  
  #York {.tabset .tabset-fade .tabset-pills}
  -------------------------------------
  
  ### Nutrients
  
  ```{r}

york=long_loads227_gapclose %>% 
  filter(Geography=="York") %>%
  #rename(Climate2= "Climate.Change",Type2="Nutrient",Value2="Amount") %>%
  select(Modified.Sector,Nutrient=Type2,LoadSource,Amount=Value2,Unit,Year)%>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft")) %>%
  arrange(desc(Amount))

DT::datatable(york,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```   

### Nutrient Plots



tab {
  display: inline-block; 
  margin-left: 10px; 
}
h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
York 
```{r fig.width=10, fig.height=8}

x5 <- girafe(ggobj = h_nitro[[5,3]], width = 1,width_svg = 8)

x5=girafe_options(x5,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 

y5 <- girafe(ggobj = h_phos[[4,3]], width = 1,width_svg = 8)

y5=girafe_options(y5,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 





x5

y5


```

### Land Use

```{r fig.width=8, fig.height=6,fig.align="center"}
p1=pies5 %>% 
  filter(str_detect(StateBasin,"York")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))%>%
  
  ggplot(aes(x="", y=P, fill=Modified)) + 
  geom_bar_interactive(aes(tooltip = Percent),stat="identity", width=4,size = 1.5, color = "white")+
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_classic() + 
  facet_wrap(~Year) +
  scale_fill_manual(name = 'Sector', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = 16))+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))+
  theme(legend.text=element_text(size=12),legend.position="right")+
  ggtitle("Landuse")+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))

pp1=ggiraph(code=print(p1),width = 1,width_svg = 8)
v1=girafe_options(pp1, opts_tooltip(use_fill = TRUE), sizingPolicy(defaultWidth = "95%", defaultHeight = "150px"))

bscols(widths =c(NA,1),v1,"")

```

### BMP Table
```{r}
YorkDT=WIP_MOd_sum2 %>%
  #filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography,"York")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  select(-Progress,-AmountCredited,-Geography)

DT::datatable(YorkDT,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))
```

### BMP Plot
```{r}

h3=WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(!str_detect(BMP,"Oyster"))%>%
  filter(str_detect(Geography,"York")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  filter(Number > 100) 

h3$tooltip= c(paste0("Sector: ", h3$Sector,"\nBMP: " ,h3$Unit,"\nUnit:",h3$BMP, "\n Progress: ", h3$Year, "\n Number:", scales::comma(h3$Number)))

shared_h3 <- SharedData$new(h3)

j3=shared_h3 %>%
  ggplot()+
  geom_point(aes(text1=Number,text2=BMP,text4=Unit,text3=Year,x=Number,y=reorder(BMP,Number,mean),colour=Sector,shape=Year),size=3)+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  labs(x=NULL,y=NULL,title="York River BMPs")+
  scale_y_discrete(expand=expand_scale(0.05))+
  theme_bw()+
  theme(legend.title=element_blank())+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())#+
#theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))


bscols(widths = c(3,NA),
       list(
         filter_checkbox("year", "Year", shared_h3, ~Year, inline = TRUE),
         filter_select("bmp", "BMP", shared_h3, ~BMP),
         "","","","","Note: for visualization purposes, shoreline management and oyster aquaculture have been omitted from this plot. Refer to the BMP table tab for those values"
       ),
       list(filter_slider("number", "Number", shared_h3, ~Number, width = "90%"),
            ggplotly(j3,tooltip=c("text1","text2","text4","text3"),dynamicTicks = "TRUE",height=500)%>%
              highlight(color = "red",on="plotly_selected",off="plotly_deselect",persistent = getOption("persistent",TRUE)))
       #DT::datatable(shared_h)
       #d3scatter(shared_h, ~Number,~BMP, ~factor(Sector))
       
)
```

James
=======================================================================
  
  #James {.tabset .tabset-fade .tabset-pills}
  -------------------------------------
  
  ### Nutrients Table
  
  ```{r}

James=long_loads227_gapclose %>% 
  filter(Geography=="James") %>%
  #rename(Climate2= "Climate.Change",Type2="Nutrient",Value2="Amount") %>%
  select(Modified.Sector,Nutrient=Type2,LoadSource,Amount=Value2,Unit,Year)%>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft")) %>%
  arrange(desc(Amount))

DT::datatable(James,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```   

### Nutrients Plots


tab {
  display: inline-block; 
  margin-left: 10px; 
}
h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
James 
```{r fig.width=10, fig.height=8}

x2 <- girafe(ggobj = h_nitro[[2,3]], width = 1,width_svg = 8)

x2=girafe_options(x2,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 

y2 <- girafe(ggobj = h_phos[[2,3]], width = 1,width_svg = 8)

y2=girafe_options(y2,
                  opts_tooltip(use_fill = TRUE,opacity = .9),
                  opts_zoom(min = .5, max = 4),
                  sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                  opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 





x2

y2

```

### Land Use

```{r fig.align="top"}
p=pies5 %>% 
  filter(str_detect(StateBasin,"James")) %>% 
  group_by(Modified,Year)%>%
  summarise(Total=sum(PreBMPAcres))%>%
  ungroup() %>%
  group_by(Year)%>%
  mutate(P=round(100 * Total/sum(Total), 1),Percent=paste0(round(100 * Total/sum(Total), 1), "%"))%>%
  
  ggplot(aes(x="", y=P, fill=Modified)) + 
  geom_bar_interactive(aes(tooltip = Percent),stat="identity", width=4,size = 1.5, color = "white")+
  coord_polar("y", start=0) + 
  labs(x = NULL, y = NULL, fill = NULL, title = NULL)+
  theme_classic() + 
  facet_wrap(~Year) +
  scale_fill_manual(name = 'Sector', 
                    values =c('Agriculture'='#4575b4','Natural'='#7fbf7b',"Developed (MS4)"="#fee090","Developed (Non-MS4)"="#fc8d59",
                              "Wastewater"="#d73027","Septic"="#d8b365"))+
  theme(strip.text.x = element_text(colour = "white", face = "bold",size = 16))+
  theme(strip.background =element_rect(fill="black"))+
  theme(strip.text.x = element_text(colour = 'white',size=15,face="bold"))+
  theme(axis.line = element_blank(),
        axis.text.x=element_text(colour='black'),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5, color = "#666666"))+
  theme(legend.text=element_text(size=12),legend.position="right")+
  ggtitle("Landuse")


pp=ggiraph(code=print(p),width = 1,width_svg = 8)
v=girafe_options(pp, opts_tooltip(use_fill = TRUE), sizingPolicy(defaultWidth = "95%", defaultHeight = "150px"))



bscols(widths =c(NA,1),v,"")

```

### BMP Plot
```{r}

h2=WIP_MOd_sum2 %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(!str_detect(BMP,"Oysters"))%>%
  filter(str_detect(Geography,"James")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  filter(Number > 100) 

h2$tooltip= c(paste0("Sector: ", h2$Sector,"\nBMP: " ,h2$Unit,"\nUnit:",h2$BMP, "\n Progress: ", h2$Year, "\n Number:", scales::comma(h2$Number)))

shared_h2 <- SharedData$new(h2)

j2=shared_h2 %>%
  ggplot()+
  geom_point(aes(text1=Number,text2=BMP,text4=Unit,text3=Year,x=Number,y=reorder(BMP,Number,mean),colour=Sector,shape=Year),size=3)+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  labs(x=NULL,y=NULL,title="")+
  scale_y_discrete(expand=expand_scale(0.5))+
  theme_bw()+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())+
  theme(plot.margin=unit(c(.1,.1,.1,.1),"cm"))


bscols(widths = c(3,NA),
       list(
         filter_checkbox("year", "Year", shared_h2, ~Year, inline = TRUE),
         filter_select("bmp", "BMP", shared_h2, ~BMP),
         "","","","","Note: for visualization purposes, shoreline management and oyster aquaculture have been omitted from this plot. Refer to the BMP table tab for those values"
       ),
       list(filter_slider("number", "Number", shared_h2, ~Number, width = "90%"),
            ggplotly(j2,tooltip=c("text1","text2","text4","text3"),dynamicTicks = "TRUE",height=500)%>%
              highlight(color = "red",on="plotly_selected",off="plotly_deselect",persistent = getOption("persistent",TRUE)))
       
)
```

### BMP Table
```{r}
JamesDT=WIP_MOd_sum2 %>%
  #filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(str_detect(Geography,"James")) %>%
  #gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Progress,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  mutate(Number=sum(AmountCredited)) %>%
  select(-Progress,-AmountCredited,-Geography)

DT::datatable(JamesDT,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))
```


All Basins
=======================================================================
  
  #All {.tabset .tabset-fade .tabset-pills}
  -------------------------------------
  
  ### Nutrients Table
  All Basins 

h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
```{r}

all=long_loads227_gapclose %>% 
  #rename(Climate2= "Climate.Change",Type2="Nutrient",Value2="Amount") %>%
  select(Modified.Sector,Nutrient=Type2,LoadSource,Amount=Value2,Unit,Year)%>%
  mutate(Year=recode_factor(Year,`2018` ="WIP.Initial",`2019` ="WIP.Draft")) %>%
  arrange(desc(Amount))

DT::datatable(all,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```   

### Nutrients Plots


tab {
  display: inline-block; 
  margin-left: 10px; 
}
h1 {
  color: black;
  font-family: verdana;
  font-size: 55%;
} 
All Basins 
```{r fig.width=10, fig.height=8}

# 



x <- girafe(ggobj = tool_nitro_Plot, width = 1, width_svg = 8)

x <- girafe_options(x,
                    opts_tooltip(use_fill = TRUE,opacity = .9),
                    opts_zoom(min = .5, max = 4),
                    sizingPolicy(defaultWidth = "100%", defaultHeight = "200px"),
                    opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 

y <- girafe(ggobj = tool_phos_Plot, width = 1,width_svg = 8)

y=girafe_options(y,
                 opts_tooltip(use_fill = TRUE,opacity = .9),
                 opts_zoom(min = .5, max = 4),
                 sizingPolicy(defaultWidth = "100%", defaultHeight = "200px")) #,
#opts_hover(css ="background-color:red;border: 1px solid black;color:white;font-style:normal;padding:10px;border-radius:10px 10px 10px 10px;")) 

x

y

```

### BMP Table

```{r}

WIP_BMP_baywide10=WIP_MOd_sum2 %>%
  #filter(!Progress=="April") %>%
  filter(!str_detect(BMP, 'Growth Management'))%>%
  spread(Progress,AmountCredited,fill=0)%>%
  group_by(BMP) %>%
  mutate(V2017=sum(V2017),May=sum(May),Initial=sum(Initial))%>%
  distinct(May,V2017,Initial,.keep_all = TRUE)



h100=WIP_BMP_baywide10 %>%
  #filter(!str_detect(BMP,"Shoreline Management"))%>%
  gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Year,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  summarise(Sector=unique(Sector),Number=sum(Number),Unit=unique(Unit)) 

DT::datatable(h100,rownames = FALSE, filter="top", options = list(pageLength = 10, scrollX=T))

```

### BMPs Plot

```{r }

library(crosstalk)


h=WIP_BMP_baywide %>%
  filter(!str_detect(BMP,"Shoreline Management"))%>%
  filter(!str_detect(BMP,"Oyster"))%>%
  gather(Initial:V2017,value=Number,key=Year,-Geography)%>%
  mutate(Year=recode_factor(Year,`Initial` ="WIP.Initial",
                            `May`="WIP.Draft",`V2017`="2017"))%>%
  group_by(BMP, Year) %>%
  summarise(Sector=unique(Sector),Number=sum(Number),Unit=unique(Unit)) %>%
  filter(Number > 100) 

h$tooltip= c(paste0("Sector: ", h$Sector,"\nBMP: " ,h$BMP, "\n Progress: ", h$Year, "\n Number:", scales::comma(h$Number)))

shared_h <- SharedData$new(h)

j=shared_h %>%
  ggplot()+
  geom_point(aes(text1=Number,text2=BMP,text4=Unit,text3=Year,x=Number,y=reorder(BMP,Number,mean),colour=Sector,shape=Year),size=3)+
  scale_colour_manual(name = 'Sector', values =c('Agriculture'='#3288BD','Natural'="#7fbf7b",
                                                 "Developed(Non-MS4)"="#fc8d59","Septic"="#FDAE61","Progress"="grey90"))+
  labs(x=NULL,y=NULL)+
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank())




bscols(widths = c(3,NA),
       list(
         filter_checkbox("year", "Year", shared_h, ~Year, inline = TRUE),
         filter_select("bmp", "BMP", shared_h, ~BMP),
         "","","","","Note: for visualization purposes, shoreline management and oyster aquaculture have been omitted from this plot. Refer to the BMP table tab for those values"
       ),
       list(filter_slider("number", "Number", shared_h, ~Number, width = "90%"),
            ggplotly(j,tooltip=c("text1","text2","text4","text3"),dynamicTicks = "TRUE",height=500)%>%
              highlight(color = "red",on="plotly_selected",off="plotly_deselect",persistent = getOption("persistent",TRUE)))
       
)
```