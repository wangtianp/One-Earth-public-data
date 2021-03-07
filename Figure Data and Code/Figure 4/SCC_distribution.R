library("data.table")
library("ggplot2")
library("magrittr")
library("reshape2")
library("ggthemes")

setwd("/Users/tianpeng/Desktop/NCC_data_public/Figure Data and Code/Figure 4")
SCC_distributions = fread("SCC distribution.csv",header = T)

#3% discount rate
ds="0.03";
my_breaks <- function(x) c(0, (((max(x) / 2) %/% 10) + 1) * 0.015, (((max(x) / 2) %/% 10) + 1) * 0.03, (((max(x) / 2) %/% 10) + 1) * 0.045,(((max(x) / 2) %/% 10) + 1) * 0.06)
scale_no=1;

b2=ggplot(SCC_distributions,aes(x=SCC,stat(density),group=Models,colour=Models,fill=Models)) + 
  geom_histogram(binwidth = 7,alpha=.6,position="identity",size=0.001) +
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(colour = "black"))+
  facet_grid(SSPs~category, scales = "free")+
  geom_segment(aes(x = origin,y=yposition,xend = end, yend = yposition,colour=Models ) , size = 0.8) +
  geom_pointrange(aes(x=average,y=yposition,ymin=yposition,ymax=yposition,colour=Models), size = 0.6)+
  theme(panel.spacing = unit(2, "lines"))+
  xlab(expression(paste('SCC in 2020 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  ylab("Density")+
  theme(legend.position="top")+
  theme(strip.text = element_text(colour = 'black',  size = rel(1.2)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(axis.text.x = element_text(size = 12, color = "black"))+
  theme(axis.text.y = element_text(size = 12, color = "black"))+
  theme(axis.title.x = element_text(size = 12, color = "black"))+
  theme(axis.title.y = element_text(size = 12, margin = margin(t = 0, r = 10, b = 0, l = 0), color = "black"))+
  theme(legend.title = element_text( size=12, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 12, color = "black"))+     ##设置标签文字
  scale_colour_manual(values=c("#619CFF","darkorchid2","seagreen3"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_fill_manual(values=c("#619CFF","darkorchid2","seagreen3" ),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_x_continuous(limits=c(-50, 400), breaks=seq(-50,400,50))+
  scale_y_continuous(expand = expand_scale(mult = 0.06), 
                     breaks = my_breaks)


library(gridExtra)
library(plyr)
library(ggpubr)
ggsave("SCC_distribution_DS0.03.pdf",b2,width = 10,height = 10)
