library(ggplot2);
library(scales);
library("openxlsx")
setwd("/Users/tianpeng/Desktop/NCC_data_public/Figure Data and Code/Figure 1")
## set the discount rate
discount_rate_disp = "3%";
discount_rate = "ds_0.03";
uncertainty_data_disp = read.xlsx("Uncertainty analysis.xlsx",sheet = discount_rate_disp);
uncertainty_data = read.xlsx("Uncertainty analysis.xlsx",sheet = "uncertainty_standard");

uncertainty_data_disp$Type = factor(uncertainty_data_disp$Type, levels=c('Original','Unified climate modules','Unified damage modules'))

p_uncertainty_display = ggplot(uncertainty_data_disp, aes(x=Axis, y=Value)) + 
  geom_point(aes(col=factor(Inconsistent.module)),alpha = 1,size=6) +
  geom_line(alpha = 0.4,size=1.2)+
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="right")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(SSPs~Type, scales = "free", space = "free_x")+
  # scale_y_continuous(expand = c(.5,0))+
  theme(strip.placement = "outside")+
  labs(shape = " ", colour = " ")+
  theme(strip.text = element_text(colour = 'black',  size = rel(1.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(2, "lines"))+
  xlab(" ")+
  ylab(expression(paste('SCC in 2020 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  scale_color_manual(values=c("#619CFF", "darkorchid2","seagreen3"),breaks = c("PAGE", "DICE","FUND"), labels = c("PAGE", "DICE","FUND"))+
  theme(axis.text.x = element_text(size = 21, color = "black"))+
  theme(axis.text.y = element_text(size = 21, color = "black"))+
  theme(axis.title.x = element_text(size = 21, color = "black",margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  theme(axis.title.y = element_text(size = 21, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  # theme(text = element_text(family='Times New Roman',size = 21))+
  theme(legend.text = element_text(size = 21, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 21, color = "black"))     ##设置标签文字


my_breaks <- function(x) c(0, (((max(x) / 2) %/% 10) + 1) * 5, (((max(x) / 2) %/% 10) + 1) * 10, (((max(x) / 2) %/% 10) + 1) * 15,(((max(x) / 2) %/% 10) + 1) * 20)

p_uncertainty_display_original = ggplot(uncertainty_data_disp[uncertainty_data_disp$Type=="Original",], aes(x=Axis, y=Value)) + 
  geom_point(aes(col=factor(Inconsistent.module)),alpha = 1,size=6) +
  geom_line(alpha = 0.4,size=1.2)+
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(SSPs~Type, scales = "free", space = "free_x",switch = "x")+
  # scale_y_continuous(expand=c(0.6,0.6),breaks = my_breaks)+
  # scale_y_continuous(expand=c(0.8,0.8),breaks = my_breaks)+
  scale_y_continuous(expand=c(0.3,0.3),breaks = my_breaks)+
  theme(strip.placement = "outside")+
  labs(shape = " ", colour = "Original IAMs")+
  theme(strip.text = element_text(colour = 'black',  size = rel(1.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values=c("#619CFF", "darkorchid2","seagreen3"),breaks = c("PAGE", "DICE","FUND"), labels = c("PAGE", "DICE","FUND"))+
  xlab(" ")+
  ylab(expression(paste('SCC in 2020 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  theme(axis.text.x = element_text(size = 21, color = "black"))+
  theme(axis.text.y = element_text(size = 21, color = "black"))+
  # theme(axis.title.x = element_text(size = 21, color = "black",margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  # theme(axis.title.y = element_text(size = 21, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  # theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(axis.title.x = element_text(size = 21, color = "black"))+
  theme(axis.title.y = element_text(size = 21, color = "black"))+
  # theme(text = element_text(family='Times New Roman',size = 21))+
  theme(legend.text = element_text(size = 21, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 21, color = "black"))+     ##设置标签文字
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5))


p_uncertainty_display_climate = ggplot(uncertainty_data_disp[uncertainty_data_disp$Type=="Unified climate modules",], aes(x=Axis, y=Value)) + 
  geom_point(aes(col=factor(Inconsistent.module)),alpha = 1,size=6) +
  geom_line(alpha = 0.4,size=1.2)+
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(SSPs~Type, scales = "free", space = "free_x",switch = "x")+
  # scale_y_continuous(expand=c(0.6,0.6),breaks = my_breaks)+
  # scale_y_continuous(expand=c(0.8,0.8),breaks = my_breaks)+
  scale_y_continuous(expand=c(0.3,0.3),breaks = my_breaks)+
  
  theme(strip.placement = "outside")+
  labs(shape = " ", colour = "Varying damage modules")+
  theme(strip.text = element_text(colour = 'black',  size = rel(1.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values=c("#619CFF", "darkorchid2","seagreen3"),breaks = c("PAGE", "DICE","FUND"), labels = c("PAGE", "DICE","FUND"))+
  xlab(" ")+
  # ylab(expression(paste('SCC in 2020 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  ylab('')+
  theme(axis.text.x = element_text(size = 21, color = "black"))+
  theme(axis.text.y = element_text(size = 21, color = "black"))+
  # theme(axis.title.x = element_text(size = 21, color = "black",margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  # theme(axis.title.y = element_text(size = 21, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  # theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(axis.title.x = element_text(size = 21, color = "black"))+
  theme(axis.title.y = element_text(size = 21, color = "black"))+
  # theme(text = element_text(family='Times New Roman',size = 21))+
  theme(legend.text = element_text(size = 21, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 21, color = "black"))+     ##设置标签文字
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5))


p_uncertainty_display_damage = ggplot(uncertainty_data_disp[uncertainty_data_disp$Type=="Unified damage modules",], aes(x=Axis, y=Value)) + 
  geom_point(aes(col=factor(Inconsistent.module)),alpha = 1,size=6) +
  geom_line(alpha = 0.4,size=1.2)+
  theme_bw()+
  # theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(SSPs~Type, scales = "free",switch = "x")+
  # scale_y_continuous(expand=c(0.6,0.6),breaks = my_breaks)+
  # scale_y_continuous(expand=c(0.8,0.8),breaks = my_breaks)+
  scale_y_continuous(expand=c(0.3,0.3),breaks = my_breaks)+
  
  theme(strip.placement = "outside")+
  labs(shape = " ", colour = "Varying climate modules")+
  theme(strip.text = element_text(colour = 'black',  size = rel(1.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(panel.spacing = unit(2, "lines"))+
  scale_color_manual(values=c("#619CFF", "darkorchid2","seagreen3"),breaks = c("PAGE", "DICE","FUND"), labels = c("PAGE", "DICE","FUND"))+
  xlab(" ")+
  # ylab(expression(paste('SCC in 2020 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  ylab('')+
  theme(axis.text.x = element_text(size = 21, color = "black"))+
  theme(axis.text.y = element_text(size = 21, color = "black"))+
  # theme(axis.title.x = element_text(size = 21, color = "black",margin = margin(t = 20, r = 0, b = 20, l = 0)))+
  # theme(axis.title.y = element_text(size = 21, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  # theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(axis.title.x = element_text(size = 21, color = "black"))+
  theme(axis.title.y = element_text(size = 21, color = "black"))+
  # theme(text = element_text(family='Times New Roman',size = 21))+
  theme(legend.text = element_text(size = 21, color = "black"))+     ##设置标签文字
  theme(legend.title  = element_text(size = 21, color = "black"))+     ##设置标签文字
  guides(color = guide_legend(title.position = "top", 
                              title.hjust = 0.5))

###########plot the uncertainty results based on variance#################

uncertainty_source_plot = matrix(nrow = 5*3*2,ncol = 4);
uncertainty_source_plot[,1] = rep(c(rep("SSP1",2),rep("SSP2",2),rep("SSP3",2),rep("SSP4",2),rep("SSP5",2)),3);
uncertainty_source_plot[,2] = c(rep("ds_0.03",10),rep("ds_0.025",10),rep("ds_0.05",10));
uncertainty_source_plot[,3] = rep(c("Climate modules","Damage modules"),15);

uncertainty_source_plot[1:10,4] = c(as.numeric(uncertainty_data[9,10:11]),as.numeric(uncertainty_data[10,10:11]),as.numeric(uncertainty_data[11,10:11]),as.numeric(uncertainty_data[12,10:11]),as.numeric(uncertainty_data[13,10:11]));
uncertainty_source_plot[11:20,4] = c(as.numeric(uncertainty_data[9,21:22]),as.numeric(uncertainty_data[10,21:22]),as.numeric(uncertainty_data[11,21:22]),as.numeric(uncertainty_data[12,21:22]),as.numeric(uncertainty_data[13,21:22]));
uncertainty_source_plot[21:30,4] = c(as.numeric(uncertainty_data[9,32:33]),as.numeric(uncertainty_data[10,32:33]),as.numeric(uncertainty_data[11,32:33]),as.numeric(uncertainty_data[12,32:33]),as.numeric(uncertainty_data[13,32:33]));

colnames(uncertainty_source_plot) = c("SSPs","DS","Source","Value");
uncertainty_source_plot=as.data.frame(uncertainty_source_plot);
uncertainty_source_plot$Value = as.numeric(as.character(uncertainty_source_plot$Value));
uncertainty_source_plot_3 = uncertainty_source_plot[uncertainty_source_plot$DS==discount_rate,]

p_uncertainty_quan = ggplot(uncertainty_source_plot_3,aes(x=SSPs,y=Value,fill=factor(Source,levels=c("Damage modules", "Climate modules"))))+
  geom_bar(stat="identity")+
  theme_bw()+
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  theme(strip.text = element_text(colour = 'black',  size = rel(1.8)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  xlab(" ")+
  guides(fill = guide_legend(reverse=TRUE))+
  scale_y_continuous(name = 'Contributions to SCC variations', labels = percent ) +
  theme(plot.margin = unit(c(1,1,0,1), "cm"))+
  theme(axis.text.x = element_text(size = 21, color = "black"))+
  theme(axis.text.y = element_text(size = 21, color = "black"))+
  theme(axis.title.x = element_text(size = 21, color = "black"))+
  theme(axis.title.y = element_text(size = 21, color = "black",margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  theme(legend.title=element_blank())+
  theme(legend.position="top")+
  theme(legend.text = element_text(size = 21, color = "black"))     ##设置标签文字

library(gridExtra)
library(plyr)
library(ggpubr)

p_uncertainty_display_original = ggarrange(p_uncertainty_display_original,ncol = 1,labels=c("a"),font.label = list(size = 30, color = "black", family = NULL),hjust = 0);
p_uncertainty_display_climate = ggarrange(p_uncertainty_display_climate,ncol = 1,labels=c(""),font.label = list(size = 30, color = "black", family = NULL),hjust = 0);
p_uncertainty_display_damage = ggarrange(p_uncertainty_display_damage,ncol = 1,labels=c(""),font.label = list(size = 30, color = "black", family = NULL),hjust = 0);
p_uncertainty_quan= ggarrange(p_uncertainty_quan,ncol = 1,labels=c("b"),font.label = list(size = 30, color = "black", family = NULL),hjust = 0)

p_final = grid.arrange(p_uncertainty_display_original,p_uncertainty_display_climate,p_uncertainty_display_damage,p_uncertainty_quan,nrow=1,ncol=4,widths=c(1.35,2,2,2.2), heights=c(1));
ggsave("Steamflow projection_0.03.pdf",p_final,width = 24,height = 12)

