library(openxlsx)
setwd("/Users/tianpeng/Desktop/nonCO2-cost/SCC_decompose/Figure Data and Code/Figure 3")
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

p_ECS<-ggplot(data=Temperature_IAMs_ECS,aes(x=Year,y=Temp_anom,group=interaction(Models, ECS),color=Models))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(. ~ SSP)+
  theme(strip.text = element_text(colour = 'black',  size = rel(4)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  geom_line(aes(linetype=factor(ECS)),size=2,alpha=0.9)+
  scale_linetype_manual(values = c( 2,1,3))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  # scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "", labels = c("PAGE", "DICE", "Hector","FUND"))+
  xlab(" ")+
  ylab(expression(paste("GMST perturbations (", degree, "C)")))+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(panel.spacing = unit(1.5, "lines"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = grid::unit(4.5, "lines"))+
  # theme(text = element_text(family='Microsoft YaHei',size = 12))+
  theme(axis.text.x = element_text(size = 32, color = "black"))+
  theme(axis.text.y = element_text(size = 32, color = "black"))+
  theme(axis.ticks.length = unit(0.2,'cm'))+
  theme(axis.title.x = element_text(size = 32, color = "black"))+
  theme(axis.title.y = element_text(size = 32,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 32, color = "black"))     ##设置标签文字


p_ECS_SSP2<-ggplot(data=Temperature_IAMs_ECS[Temperature_IAMs_ECS$SSP=="SSP2",],aes(x=Year,y=Temp_anom,group=interaction(Models, ECS),color=Models))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  # facet_grid(. ~ SSP)+
  theme(strip.text = element_text(colour = 'black',  size = rel(4)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  geom_line(aes(linetype=factor(ECS)),size=2,alpha=0.9)+
  scale_linetype_manual(values = c( 2,1,3))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  # scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "", labels = c("PAGE", "DICE", "Hector","FUND"))+
  xlab(" ")+
  ylab(expression(paste("GMST perturbations (", degree, "C)")))+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(panel.spacing = unit(1.5, "lines"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = grid::unit(4.5, "lines"))+
  # theme(text = element_text(family='Microsoft YaHei',size = 12))+
  theme(axis.text.x = element_text(size = 32, color = "black"))+
  theme(axis.text.y = element_text(size = 32, color = "black"))+
  theme(axis.ticks.length = unit(0.2,'cm'))+
  theme(axis.title.x = element_text(size = 32, color = "black"))+
  theme(axis.title.y = element_text(size = 32,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 32, color = "black"))
##设置标签文字


IAMs_temp_ssps_quantile_incremental<-read.xlsx("Temperature_perturbations.xlsx",sheet="Quantiles_perturbation")

library("ggplot2")
p_incremental_IAMs<-ggplot(IAMs_temp_ssps_quantile_incremental,aes(Year,average,group=Models,fill=Models,colour=Models)) +
  # 带状图函数：ymin设置下界，ymax设置上界；
  geom_ribbon(aes(ymin = quan5, ymax= quan95,fill = Models), alpha = 0.5,colour = NA) +
  geom_line(alpha = 1,size=1.5,aes(colour=Models))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(. ~ SSPs)+
  theme(strip.text = element_text(colour = 'black',  size = rel(4)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1.5, "lines"))+
  xlab(" ")+
  ylab(expression(paste("GMST perturbations (", degree, "C)")))+
  theme(legend.key.size = grid::unit(6, "lines"))+
  theme(plot.margin = unit(c(1.2,1,0,1), "cm"))+
  theme(axis.text.x = element_text(size = 32, color = "black"))+
  theme(axis.text.y = element_text(size = 32, color = "black"))+
  theme(axis.ticks.length = unit(0.2,'cm'))+
  theme(axis.title.x = element_text(size = 32, color = "black"))+
  theme(axis.title.y = element_text(size = 32,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 32, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 32, color = "black"))+     ##设置标签文字
  # scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  # scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.key.size = unit(1.5,"cm"),
        legend.key.width = unit(2.0,"cm"))+
  theme(legend.title = element_blank())+
  geom_line()

p_incremental_IAMs_SSP2<-ggplot(IAMs_temp_ssps_quantile_incremental[IAMs_temp_ssps_quantile_incremental$SSPs=="SSP2",],aes(Year,average,group=Models,fill=Models,colour=Models)) +
  # 带状图函数：ymin设置下界，ymax设置上界；
  geom_ribbon(aes(ymin = quan5, ymax= quan95,fill = Models), alpha = 0.5,colour = NA) +
  geom_line(alpha = 1,size=1.5,aes(colour=Models))+
  theme_bw()+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  # facet_grid(. ~ SSPs)+
  theme(strip.text = element_text(colour = 'black',  size = rel(4)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1.5, "lines"))+
  xlab(" ")+
  ylab(expression(paste("GMST perturbations (", degree, "C)")))+
  theme(legend.key.size = grid::unit(6, "lines"))+
  theme(plot.margin = unit(c(1.2,1,0,1), "cm"))+
  theme(axis.text.x = element_text(size = 32, color = "black"))+
  theme(axis.text.y = element_text(size = 32, color = "black"))+
  theme(axis.ticks.length = unit(0.2,'cm'))+
  theme(axis.title.x = element_text(size = 32, color = "black"))+
  theme(axis.title.y = element_text(size = 32,margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 32, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 32, color = "black"))+     ##设置标签文字
  # scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  # scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = "Simulated Percentiles:\nExpected values (lines), \n5-95th percentiles (areas)", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.key.size = unit(1.5,"cm"),
        legend.key.width = unit(2.0,"cm"))+
  # guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  geom_line()


IAMs_temp_perturb_target<-read.xlsx("Temperature_perturbations.xlsx",sheet="Distribution")
library(data.table)

IAMs_temp_perturb_target = as.data.table(IAMs_temp_perturb_target)

p_GMST_incremental = ggplot(IAMs_temp_perturb_target[Scenarios=="SSP2" & Year %in% c(2100,2200),], aes(x=Temperature,group=Models)) +  
  geom_density(aes( fill=Models), alpha=0.6,colour=NA) + 
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid( Year~ Scenarios, scales = "free_y") + 
  theme(strip.text = element_text(colour = 'black',  size = rel(4)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(2, "lines"))+
  xlab(expression(paste("GMST perturbations (", degree, "C)")))+
  ylab('Density')+
  theme(axis.text.x = element_text(size = 32, color = "black"))+
  theme(axis.text.y = element_text(size = 32, color = "black"))+
  theme(axis.ticks.length = unit(0.2,'cm'))+
  theme(axis.title.x = element_text(size = 32, color = "black",margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  theme(axis.title.y = element_text(size = 32, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(plot.margin = unit(c(2,1,0,1), "cm"))+
  theme(legend.text = element_text(size = 32, color = "black"))+   
  theme(legend.title  = element_text(size = 32, color = "black"))+
  scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"), labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"), labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(1.5,"cm"),
        legend.key.width = unit(1.5,"cm"))#+
#scale_x_continuous(limits=c(0,25), breaks=seq(0,20,5))


p_GMST_incremental_SSP2 = ggplot(IAMs_temp_perturb_target[Scenarios=="SSP2" & Year %in% c(2100,2200),], aes(x=Temperature,group=Models)) +  
  geom_density(aes( fill=Models), alpha=0.6,colour=NA) + 
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_wrap(~Year,scales = "free", ncol = 1)+
  theme(strip.text = element_text(colour = 'black',  size = rel(4)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(2, "lines"))+
  xlab(expression(paste("GMST perturbations (", degree, "C)")))+
  ylab('Density')+
  theme(axis.text.x = element_text(size = 32, color = "black"))+
  theme(axis.text.y = element_text(size = 32, color = "black"))+
  theme(axis.ticks.length = unit(0.2,'cm'))+
  theme(axis.title.x = element_text(size = 32, color = "black",margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  theme(axis.title.y = element_text(size = 32, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(plot.margin = unit(c(2,1,0,1), "cm"))+
  theme(legend.text = element_text(size = 32, color = "black"))+   
  theme(legend.title  = element_text(size = 32, color = "black"))+
  scale_fill_manual(values=c("lightgreen", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"), labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("seagreen3", "#619CFF","#F8766D","darkorchid2"),breaks = c("PAGE", "DICE", "Hector","FUND"), labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(1.5,"cm"),
        legend.key.width = unit(1.5,"cm"))+
  scale_x_continuous(limits=c(0,0.15), breaks=seq(0,0.15,0.03))


library(gridExtra)
library(plyr)
library(ggpubr)
library(cowplot)

p_incremental_ESC_IAMs_SSP2 = ggarrange(p_incremental_IAMs_SSP2,p_ECS_SSP2,nrow = 2,labels=c("A","B"),font.label = list(size = 40, color = "black", family = NULL),hjust = -1,vjust = 1.5)
p_GMST_incremental_SSP2 = ggarrange(p_GMST_incremental_SSP2,nrow = 1,labels=c("C"),font.label = list(size = 40, color = "black", family = NULL),hjust = -1,vjust = 1.5)
p_incremental_ESC_density_IAMs_SSP2 = plot_grid(p_incremental_ESC_IAMs_SSP2,p_GMST_incremental_SSP2,ncol = 2, rel_widths = c(.55,0.45));

# p_perturbations_ESC_density_IAMs_SSP2 = ggarrange(p_perturbations_IAMs,p_ECS,p_GMST_perturbations,nrow = 3,labels=c("a","b","c."),font.label = list(size = 35, color = "black", family = NULL),hjust = 0);

ggsave("Temperature_incremental.png",p_incremental_ESC_density_IAMs_SSP2,width = 25,height = 16)
ggsave("Temperature_incremental.pdf",p_incremental_ESC_density_IAMs_SSP2,width = 25,height = 16)
ggsave("Temperature_incremental.eps",p_incremental_ESC_density_IAMs_SSP2,width = 25,height = 16,device=cairo_ps)

