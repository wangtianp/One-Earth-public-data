library(openxlsx)
setwd("/Users/tianpeng/Desktop/One-Earth-public-data-master/Figure Data and Code/Figure 3")
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

p_ECS_SSP2<-ggplot(data=Temperature_IAMs_ECS[Temperature_IAMs_ECS$SSP=="SSP2",],aes(x=Year,y=Temp_anom,group=interaction(Models, ECS),color=Models))+
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
  ylab(expression(paste("GMST perturbations (", degree, "C)")))+
  theme(plot.margin = unit(c(0.4,0.25,0,0.4), "cm"))+
  theme(panel.spacing = unit(1, "lines"))+
  theme(legend.title = element_blank())+
  # theme(text = element_text(family='Arial'))+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 7.5, color = "black"))+
  theme(axis.title.y = element_text(size = 7.5,margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+
  theme(legend.key.size = grid::unit(1.25, "lines"),
        legend.box.margin=margin(-10,-10,-10,-10),
        legend.spacing.y = unit(0.1, 'cm'))
##设置标签文字

IAMs_temp_ssps_quantile_incremental<-read.xlsx("Temperature_perturbations.xlsx",sheet="Quantiles_perturbation")

library("ggplot2")

p_incremental_IAMs_SSP2<-ggplot(IAMs_temp_ssps_quantile_incremental[IAMs_temp_ssps_quantile_incremental$SSPs=="SSP2",],aes(Year,average,group=Models,fill=Models,colour=Models)) +
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
  ylab(expression(paste("GMST perturbations (", degree, "C)")))+
  theme(plot.margin = unit(c(0.4,0.75,0,0.4), "cm"))+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 7.5, color = "black"))+
  theme(axis.title.y = element_text(size = 7.5,margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 7.5, color = "black"))+     ##设置标签文字
  scale_fill_manual(values=c("#A6F5A2", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.key.size = unit(0.5,"cm"),
        legend.key.width = unit(1,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10))+
  theme(text = element_text(family='Arial'))+
  # guides(fill=guide_legend(nrow=2,byrow=TRUE))+
  geom_line()



IAMs_temp_perturb_target<-read.xlsx("Temperature_perturbations.xlsx",sheet="Distribution")
IAMs_temp_perturb_target[IAMs_temp_perturb_target$Year == 2200,]$Year = "GMST perturbations by 2200"
IAMs_temp_perturb_target[IAMs_temp_perturb_target$Year == 2100,]$Year = "GMST perturbations by 2100"
IAMs_temp_perturb_target[IAMs_temp_perturb_target$Year == 2050,]$Year = "GMST perturbations by 2050"

library(data.table)

IAMs_temp_perturb_target = as.data.table(IAMs_temp_perturb_target)

p_GMST_incremental_SSP2_2100 = ggplot(IAMs_temp_perturb_target[Scenarios=="SSP2" & Year %in% c("GMST perturbations by 2100"),], aes(x=Temperature,group=Models)) +  
  geom_density(aes( fill=Models), alpha=0.6,colour=NA) + 
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  facet_wrap(~Year,scales = "free", ncol = 1)+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1, "lines"))+
  xlab(expression(paste("GMST perturbations (", degree, "C)")))+
  ylab('Density')+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 7.5, color = "black",margin = margin(t = 3, r = 0, b = 3, l = 0)))+
  theme(axis.title.y = element_text(size = 7.5, color = "black",margin = margin(t = 0, r = 3, b = 0, l = 0)))+
  theme(plot.margin = unit(c(0.4,0.4,0,0.4), "cm"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+   
  theme(legend.title  = element_text(size = 7.5, color = "black"))+
  scale_fill_manual(values=c("#A6F5A2", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10))+
  theme(text = element_text(family='Arial'))+
  scale_x_continuous(limits=c(0,0.15), breaks=seq(0,0.15,0.03))


p_GMST_incremental_SSP2_2200 = ggplot(IAMs_temp_perturb_target[Scenarios=="SSP2" & Year %in% c("GMST perturbations by 2200"),], aes(x=Temperature,group=Models)) +  
  geom_density(aes( fill=Models), alpha=0.6,colour=NA) + 
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  facet_wrap(~Year,scales = "free", ncol = 1)+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(1, "lines"))+
  xlab(expression(paste("GMST perturbations (", degree, "C)")))+
  ylab('Density')+
  theme(axis.text.x = element_text(size = 7.5, color = "black"))+
  theme(axis.text.y = element_text(size = 7.5, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 7.5, color = "black",margin = margin(t = 3, r = 0, b = 3, l = 0)))+
  theme(axis.title.y = element_text(size = 7.5, color = "black",margin = margin(t = 0, r = 3, b = 0, l = 0)))+
  theme(plot.margin = unit(c(0.4,0.4,0,0.4), "cm"))+
  theme(legend.text = element_text(size = 7.5, color = "black"))+   
  theme(legend.title  = element_text(size = 7.5, color = "black"))+
  scale_fill_manual(values=c("#A6F5A2", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  scale_color_manual(values=c("#7CAE00", "#00B4F0","#F7903D","#9900E6"),breaks = c("PAGE", "DICE", "Hector","FUND"),name = " ", labels = c("PAGE", "DICE", "Hector","FUND"))+
  theme(legend.title = element_blank())+
  theme(legend.key.size = unit(0.4,"cm"),
        legend.key.width = unit(0.4,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10))+
  theme(text = element_text(family='Arial'))+
  scale_x_continuous(limits=c(0,0.15), breaks=seq(0,0.15,0.03))

library(gridExtra)
library(plyr)
library(ggpubr)
library(cowplot)

p_incremental_ESC_IAMs_SSP2 = ggarrange(p_incremental_IAMs_SSP2,p_ECS_SSP2,nrow = 2,labels=c("A","B"),font.label = list(size = 10, color = "black", family = NULL),hjust = -1,vjust = 2)
p_GMST_incremental_SSP2 = ggarrange(p_GMST_incremental_SSP2_2100,p_GMST_incremental_SSP2_2200,nrow = 2,labels=c("C","D"),font.label = list(size = 10, color = "black", family = NULL),hjust = -1,vjust = 2)
p_incremental_ESC_density_IAMs_SSP2 = plot_grid(p_incremental_ESC_IAMs_SSP2,p_GMST_incremental_SSP2,ncol = 2, rel_widths = c(.55,0.45));

# ggsave("Temperature_incremental.png",p_incremental_ESC_density_IAMs_SSP2,width = 15,height = 10)
ggsave("./Figure 3.tiff", p_incremental_ESC_density_IAMs_SSP2,width = 174,height=110, dpi=900, units = "mm", compression = "lzw",bg = "white")

