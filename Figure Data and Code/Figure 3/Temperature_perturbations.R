library(openxlsx)
setwd("/Users/tianpeng/Desktop/NCC_data_public/Figure Data and Code/Figure 3")
Temperature_IAMs<-read.xlsx("Temperature_perturbations.xlsx",sheet="ECS")

Temperature_IAMs_ECS1.5=Temperature_IAMs[Temperature_IAMs$ECS == "ECS=1.5", ]
Temperature_IAMs_ECS3=Temperature_IAMs[Temperature_IAMs$ECS == "ECS=3", ]
Temperature_IAMs_ECS4.5=Temperature_IAMs[Temperature_IAMs$ECS == "ECS=4.5", ]
Temperature_IAMs_ECS6=Temperature_IAMs[Temperature_IAMs$ECS == "ECS=6", ]
Temperature_IAMs_ECS9=Temperature_IAMs[Temperature_IAMs$ECS == "ECS=9", ]

Temperature_IAMs_ECS1.5 = Temperature_IAMs_ECS1.5[Temperature_IAMs_ECS3$Models!="MAGICC",];
Temperature_IAMs_ECS3 = Temperature_IAMs_ECS3[Temperature_IAMs_ECS3$Models!="MAGICC",];
Temperature_IAMs_ECS4.5 = Temperature_IAMs_ECS4.5[Temperature_IAMs_ECS4.5$Models!="MAGICC",];
Temperature_IAMs_ECS6 = Temperature_IAMs_ECS6[Temperature_IAMs_ECS6$Models!="MAGICC",];
Temperature_IAMs_ECS9 = Temperature_IAMs_ECS9[Temperature_IAMs_ECS9$Models!="MAGICC",];

Temperature_IAMs_ECS = rbind(Temperature_IAMs_ECS1.5,Temperature_IAMs_ECS3,Temperature_IAMs_ECS6)
Temperature_IAMs_ECS_PAGE = Temperature_IAMs_ECS[Temperature_IAMs_ECS$Models=="PAGE",]
Temperature_IAMs_ECS_FUND = Temperature_IAMs_ECS[Temperature_IAMs_ECS$Models=="FUND",]
Temperature_IAMs_ECS_DICE = Temperature_IAMs_ECS[Temperature_IAMs_ECS$Models=="DICE",]
Temperature_IAMs_ECS_Hector = Temperature_IAMs_ECS[Temperature_IAMs_ECS$Models=="Hector",]

Temperature_IAMs_ECS = rbind(Temperature_IAMs_ECS_PAGE,Temperature_IAMs_ECS_FUND,Temperature_IAMs_ECS_DICE,Temperature_IAMs_ECS_Hector)


library(ggplot2)

p_ECS_incremental<-ggplot(data=Temperature_IAMs_ECS,aes(x=Year,y=Temp_anom,group=interaction(Models, ECS),color=Models))+
  theme_bw()+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(. ~ SSP)+
  theme(strip.text = element_text(colour = 'black',  size = rel(3)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  geom_line(aes(linetype=factor(ECS)),size=2)+
  scale_linetype_manual(values = c( 2,1,3))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  xlab(" ")+
  ylab("GMST perturbations (degC)")+
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


IAMs_temp_ssps_quantile_incremental<-read.xlsx("Temperature_perturbations.xlsx",sheet="Quantiles_perturbation")

# p_incremental_IAMs<-ggplot(IAMs_temp_ssps_quantile_incremental[IAMs_temp_ssps_quantile_incremental$Model !="Hector",],aes(Year,average,group=Model,fill=Model,colour=Model)) +
p_incremental_IAMs<-ggplot(IAMs_temp_ssps_quantile_incremental,aes(Year,average,group=Model,fill=Model,colour=Model)) +
  
  # 带状图函数：ymin设置下界，ymax设置上界；
  geom_ribbon(aes(ymin = quan5, ymax= quan95), alpha = 0.5,colour=NA) +
  geom_line(alpha = 1,size=1.5,aes(colour=Model))+
  theme_bw()+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(. ~ SSPs)+
  theme(strip.text = element_text(colour = 'black',  size = rel(3)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1.6, "lines"))+
  xlab(" ")+
  ylab("GMST perturbations (degC)")+
  theme(legend.key.size = grid::unit(6, "lines"))+
  theme(plot.margin = unit(c(1.2,1,0,1), "cm"))+
  theme(axis.text.x = element_text(size = 25, color = "black"))+
  theme(axis.text.y = element_text(size = 25, color = "black"))+
  theme(axis.title.x = element_text(size = 25, color = "black"))+
  theme(axis.title.y = element_text(size = 25,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 25, color = "black"))+     ##设置标签文字
  theme(legend.title = element_text(size = 25, color = "black"))+     ##设置标签文字
  scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.key.size = unit(1.5,"cm"),
        legend.key.width = unit(2.2,"cm"))+
  # scale_y_continuous(limits=c(-1e3, 12e5))+
  # scale_x_continuous(breaks=seq(2000, 2200, 50))+
  geom_line()


library(gridExtra)
library(plyr)
library(ggpubr)
library(extrafont)
extrafont::loadfonts();
options(bitmapType="cairo")
p_final = ggarrange(p_incremental_IAMs,p_ECS_incremental,nrow = 2,labels=c("a","b"),font.label = list(size = 35, color = "black", family = NULL),hjust = 0)
ggsave("Temperature_incremental.pdf",p_final,width = 25,height = 16)
