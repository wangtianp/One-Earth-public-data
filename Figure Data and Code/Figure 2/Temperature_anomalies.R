library(openxlsx)
library(moments)
setwd("/Users/tianpeng/Desktop/One-Earth-public-data-master/Figure Data and Code/Figure 2")
IAMs_temperature<-read.xlsx("Temperature_anomalies.xlsx",sheet="ECS")

IAMs_temperature_ECS1.5=IAMs_temperature[IAMs_temperature$ECS == "ECS=1.5", ]
IAMs_temperature_ECS3=IAMs_temperature[IAMs_temperature$ECS == "ECS=3", ]
IAMs_temperature_ECS4.5=IAMs_temperature[IAMs_temperature$ECS == "ECS=4.5", ]
IAMs_temperature_ECS6=IAMs_temperature[IAMs_temperature$ECS == "ECS=6", ]
IAMs_temperature_ECS9=IAMs_temperature[IAMs_temperature$ECS == "ECS=9", ]

IAMs_temperature_ECS1.5 = IAMs_temperature_ECS1.5[IAMs_temperature_ECS3$Models!="MAGICC",];
IAMs_temperature_ECS3 = IAMs_temperature_ECS3[IAMs_temperature_ECS3$Models!="MAGICC",];
IAMs_temperature_ECS4.5 = IAMs_temperature_ECS4.5[IAMs_temperature_ECS4.5$Models!="MAGICC",];
IAMs_temperature_ECS6 = IAMs_temperature_ECS6[IAMs_temperature_ECS6$Models!="MAGICC",];
IAMs_temperature_ECS9 = IAMs_temperature_ECS9[IAMs_temperature_ECS9$Models!="MAGICC",];

IAMs_temperature_ECS = rbind(IAMs_temperature_ECS1.5,IAMs_temperature_ECS3,IAMs_temperature_ECS6)
IAMs_temperature_ECS_PAGE = IAMs_temperature_ECS[IAMs_temperature_ECS$Models=="PAGE",]
IAMs_temperature_ECS_FUND = IAMs_temperature_ECS[IAMs_temperature_ECS$Models=="FUND",]
IAMs_temperature_ECS_DICE = IAMs_temperature_ECS[IAMs_temperature_ECS$Models=="DICE",]
IAMs_temperature_ECS_Hector = IAMs_temperature_ECS[IAMs_temperature_ECS$Models=="Hector",]

IAMs_temperature_ECS = rbind(IAMs_temperature_ECS_PAGE,IAMs_temperature_ECS_FUND,IAMs_temperature_ECS_DICE,IAMs_temperature_ECS_Hector)

library(ggplot2)

p_ECS_SSP2<-ggplot(data=IAMs_temperature_ECS[IAMs_temperature_ECS$SSP=="SSP2",],aes(x=Year,y=Temp,group=interaction(Models, ECS),color=Models))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  geom_line(aes(linetype=factor(ECS)),size=0.5,alpha=1)+
  scale_linetype_manual(values = c(2,1,3),
                        labels=c(expression(paste("ECS=1.5 ", degree, "C")), 
                                 expression(paste("ECS=3 ", degree, "C   ")),
                                 expression(paste("ECS=6 ", degree, "C   "))))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  xlab("Year")+
  ylab(expression(paste("GMST anomalies (", degree, "C)")))+
  theme(plot.margin = unit(c(0.4,0.4,0,0.4), "cm"))+
  theme(panel.spacing = unit(1, "lines"))+
  # theme(text = element_text(family='Arial'))+
  theme(legend.title = element_blank())+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.title.x = element_text(size = 7.5, color = "black"))+
  theme(axis.title.y = element_text(size = 7.5,margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(legend.key.size = grid::unit(1.25, "lines"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.spacing.y = unit(0.1, 'cm'))


IAMs_temp_ssps_quantile<-read.xlsx("Temperature_anomalies.xlsx",sheet="Quantiles_anomaly")

p_anomalies_IAMs_SSP2<-ggplot(IAMs_temp_ssps_quantile[IAMs_temp_ssps_quantile$SSPs=="SSP2",],aes(Year,average,group=Models,fill=Models,colour=Models)) +
  # p_anomalies_IAMs<-ggplot(IAMs_temp_ssps_quantile[IAMs_temp_ssps_quantile$Models!="Hector",],aes(Year,average,group=Models,fill=Models,colour=Models)) +
  # 带状图函数：ymin设置下界，ymax设置上界；
  geom_ribbon(aes(ymin = quan5, ymax= quan95,fill = Models), alpha = 0.5,colour = NA) +
  geom_line(alpha = 1,size=0.5,aes(colour=Models))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  # facet_grid(. ~ SSPs)+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1, "lines"))+
  xlab("Year")+
  ylab(expression(paste("GMST anomalies (", degree, "C)")))+
  theme(plot.margin = unit(c(0.4,0.25,0,0.4), "cm"))+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.title.x = element_text(size = 7.5, color = "black"))+
  theme(axis.title.y = element_text(size = 7.5,margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 7.5, color = "black"))+     ##设置标签文字
  theme(axis.ticks.length = unit(0.1,'cm'))+
  scale_fill_manual(values=c("#A6F5A2", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10))+
  theme(text = element_text(family='Arial'))+
  # guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  geom_line()


library(data.table)
IAMs_temp_base_target<-read.xlsx("Temperature_anomalies.xlsx",sheet="Distribution")
IAMs_temp_base_target[IAMs_temp_base_target$Year == 2200,]$Year = "GMST anomalies by 2200"
IAMs_temp_base_target[IAMs_temp_base_target$Year == 2100,]$Year = "GMST anomalies by 2100"
IAMs_temp_base_target[IAMs_temp_base_target$Year == 2050,]$Year = "GMST anomalies by 2050"

IAMs_temp_base_target = as.data.table(IAMs_temp_base_target)

p_GMST_anomalies_SSP2_2100 = ggplot(IAMs_temp_base_target[Scenarios=="SSP2" & Year %in% c("GMST anomalies by 2100"),], aes(x=Temperature,group=Models)) +  
  geom_density(aes( fill=Models), alpha=0.6,colour=NA) + 
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  # facet_grid(Year~.,scales = "free")+
  facet_wrap(~ Year, scales = "free", ncol = 1) + 
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1, "lines"))+
  xlab(expression(paste("GMST anomalies (", degree, "C)")))+
  ylab('Density')+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.title.x = element_text(size = 7.5, color = "black",margin = margin(t = 3, r = 0, b = 3, l = 0)))+
  theme(axis.title.y = element_text(size = 7.5, color = "black",margin = margin(t = 0, r = 3, b = 0, l = 0)))+
  theme(plot.margin = unit(c(0.4,0.4,0,0.4), "cm"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+   
  theme(legend.title  = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  scale_fill_manual(values=c("#A6F5A2", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_x_continuous(limits=c(0,30), breaks=seq(0,25,5))+
  theme(legend.title = element_blank())+
  theme(text = element_text(family='Arial'))+
  theme(legend.key.size = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10),)


p_GMST_anomalies_SSP2_2200 = ggplot(IAMs_temp_base_target[Scenarios=="SSP2" & Year %in% c("GMST anomalies by 2200"),], aes(x=Temperature,group=Models)) +  
  geom_density(aes( fill=Models), alpha=0.6,colour=NA) + 
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  # facet_grid(Year~.,scales = "free")+
  facet_wrap(~ Year, scales = "free", ncol = 1) + 
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1, "lines"))+
  xlab(expression(paste("GMST anomalies (", degree, "C)")))+
  ylab('Density')+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.title.x = element_text(size = 7.5, color = "black",margin = margin(t = 3, r = 0, b = 3, l = 0)))+
  theme(axis.title.y = element_text(size = 7.5, color = "black",margin = margin(t = 0, r = 3, b = 0, l = 0)))+
  theme(plot.margin = unit(c(0.4,0.4,0,0.4), "cm"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+   
  theme(legend.title  = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  scale_fill_manual(values=c("#A6F5A2", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_x_continuous(limits=c(0,30), breaks=seq(0,25,5))+
  theme(legend.title = element_blank())+
  theme(text = element_text(family='Arial'))+
  theme(legend.key.size = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10))

library(gridExtra)
library(plyr)
library(ggpubr)
library(cowplot)

###getlend

p_anomalies_ESC_IAMs_SSP2 = ggarrange(p_anomalies_IAMs_SSP2,p_ECS_SSP2,nrow = 2,labels=c("A","B"),font.label = list(size = 10, color = "black", family = NULL),hjust = -1,vjust = 2)
p_GMST_anomalies_SSP2 = ggarrange(p_GMST_anomalies_SSP2_2100,p_GMST_anomalies_SSP2_2200,nrow = 2,labels=c("C","D"),font.label = list(size = 10, color = "black", family = NULL),hjust = -1,vjust = 2)
p_anomalies_ESC_density_IAMs_SSP2 = plot_grid(p_anomalies_ESC_IAMs_SSP2,p_GMST_anomalies_SSP2,ncol = 2, rel_widths = c(.55,0.45));

ggsave("./Figure 2.tiff", p_anomalies_ESC_density_IAMs_SSP2,width = 174,height=110, dpi=900, units = "mm", compression = "lzw",bg = "white")
