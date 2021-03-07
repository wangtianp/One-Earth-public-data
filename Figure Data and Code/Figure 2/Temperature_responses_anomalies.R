library(openxlsx)
library(moments)
setwd("/Users/tianpeng/Desktop/NCC_data_public/Figure Data and Code/Figure 2")
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

p_ECS<-ggplot(data=IAMs_temperature_ECS,aes(x=Year,y=Temp,group=interaction(Models, ECS),color=Models))+
  theme_bw()+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(. ~ SSP)+
  theme(strip.text = element_text(colour = 'black',  size = rel(3)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  geom_line(aes(linetype=factor(ECS)),size=2,alpha=0.9)+
  scale_linetype_manual(values = c( 2,1,3))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  xlab(" ")+
  ylab("GMST anomalies (degC)")+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(panel.spacing = unit(1.6, "lines"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = grid::unit(4.5, "lines"))+
  # theme(text = element_text(family='Microsoft YaHei',size = 12))+
  theme(axis.text.x = element_text(size = 25, color = "black"))+
  theme(axis.text.y = element_text(size = 25, color = "black"))+
  theme(axis.title.x = element_text(size = 25, color = "black"))+
  theme(axis.title.y = element_text(size = 25,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 25, color = "black"))     ##设置标签文字

IAMs_temp_ssps_quantile<-read.xlsx("Temperature_anomalies.xlsx",sheet="Quantiles_anomaly")

library("ggplot2")
p_anomalies_IAMs<-ggplot(IAMs_temp_ssps_quantile,aes(Year,average,group=Models,fill=Models,colour=Models)) +
  # p_anomalies_IAMs<-ggplot(IAMs_temp_ssps_quantile[IAMs_temp_ssps_quantile$Models!="Hector",],aes(Year,average,group=Models,fill=Models,colour=Models)) +
  # 带状图函数：ymin设置下界，ymax设置上界；
  geom_ribbon(aes(ymin = quan5, ymax= quan95,fill = Models), alpha = 0.5,colour = NA) +
  geom_line(alpha = 1,size=1.5,aes(colour=Models))+
  theme_bw()+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(. ~ SSPs)+
  theme(strip.text = element_text(colour = 'black',  size = rel(3)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1.6, "lines"))+
  xlab(" ")+
  ylab("GMST anomalies (degC)")+
  theme(legend.key.size = grid::unit(6, "lines"))+
  theme(plot.margin = unit(c(1.2,1,0,1), "cm"))+
  theme(axis.text.x = element_text(size = 25, color = "black"))+
  theme(axis.text.y = element_text(size = 25, color = "black"))+
  theme(axis.title.x = element_text(size = 25, color = "black"))+
  theme(axis.title.y = element_text(size = 25,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 25, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 25, color = "black"))+     ##设置标签文字
  scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.key.size = unit(1.5,"cm"),
        legend.key.width = unit(2.2,"cm"))+
  geom_line()


library(gridExtra)
library(plyr)
library(ggpubr)
library(extrafont)
extrafont::loadfonts();
options(bitmapType="cairo")
p_final = ggarrange(p_anomalies_IAMs,p_ECS,nrow = 2,labels=c("a","b"),font.label = list(size = 35, color = "black", family = NULL),hjust = 0)
ggsave("Temperature_responses_anomalies.pdf",p_final,width = 25,height = 16)
