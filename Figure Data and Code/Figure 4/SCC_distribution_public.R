library(data.table)
library(openxlsx)

setwd("/Users/tianpeng/Desktop/One-Earth-public-data-master/Figure Data and Code/Figure 4")
SCC_IAMs = fread("./SCC_IAMs.csv",header = T)
IAMs_SC_CO2_seg_point = read.xlsx("./IAMs_SC_CO2_seg_point.xlsx",startRow = 1)
IAMs_SC_CO2_seg_point = as.data.table(IAMs_SC_CO2_seg_point)
SCC_regional_IAMs = fread("./SCC_regional_IAMs.csv",header = T)
ECS_distribution = read.csv("./ECS_distribution.csv")

SCC_IAMs_target = SCC_IAMs[SSPs=="SSP2"&Discount_rates=="ds_3"&category %in% c("Original IAMs","IAMs-Hector"),]
IAMs_SC_CO2_seg_point_target = IAMs_SC_CO2_seg_point[SSPs=="SSP2"&Discount_rates=="ds_3"&category %in% c("Original IAMs","IAMs-Hector"),]
SCC_IAMs_target=transform(SCC_IAMs_target,category=factor(category,levels = c("Original IAMs","IAMs-Hector","IAMs-Hector (Empirically Constrained ECS)")))
IAMs_SC_CO2_seg_point_target=transform(IAMs_SC_CO2_seg_point_target,category=factor(category,levels = c("Original IAMs","IAMs-Hector","IAMs-Hector (Empirically Constrained ECS)")))
SCC_regional_IAMs=transform(SCC_regional_IAMs,Regions=factor(Regions,levels = c("USA","Europe*","China*","Other regions")))
SCC_regional_IAMs=transform(SCC_regional_IAMs,category=factor(category,levels = c("Original IAMs","IAMs-Hector")))

discount_rate = c("ds_2.5","ds_3","ds_5")
models = c("FUND_origin","DICE_origin","PAGE_origin","FUND_Hector","DICE_Hector","PAGE_Hector","FUND_Hector_constrainecs","DICE_Hector_constrainecs","PAGE_Hector_constrainecs")
ssps = c("SSP1","SSP2","SSP3","SSP4","SSP5")

IAMs_neg_SSPs_category  = matrix(nrow = length(discount_rate)*2*length(ssps), ncol = 4)
IAMs_neg_SSPs_category[,1] = rep(c("Original IAMs","IAMs-Hector"),each = length(discount_rate)*length(ssps));
IAMs_neg_SSPs_category[,2] = rep(rep(ssps,each = length(discount_rate)),2)
IAMs_neg_SSPs_category[,3] = rep(rep(discount_rate,each = 1),2*length(ssps))

for (i in 1:nrow(IAMs_neg_SSPs_category)) {
  IAMs_neg_SSPs_category[i,4] = "Negative \n SCC"
}

IAMs_neg_SSPs_category = as.data.frame(IAMs_neg_SSPs_category)
colnames(IAMs_neg_SSPs_category) = c("category","SSPs","Ds","Label")

IAMs_neg_SSPs_category_target = IAMs_neg_SSPs_category[IAMs_neg_SSPs_category$SSPs=="SSP2" & IAMs_neg_SSPs_category$Ds=="ds_3",]
IAMs_neg_SSPs_category_target$Models = "FUND"
IAMs_neg_SSPs_category_target=transform(IAMs_neg_SSPs_category_target,category=factor(category,levels = c("Original IAMs","IAMs-Hector")))

p_SCC_density_IAMs_original=ggplot(SCC_IAMs_target[category=="Original IAMs",],aes(x=SCC,stat(density),group=Models,colour=Models,fill=Models)) + 
  geom_histogram(binwidth = 7.5,alpha=.6,position="identity",size=0.001) +
  geom_segment(aes(x = 0 , y = 0, xend = 0, yend = Inf),linetype="dashed",color="black",size=0.75) +
  geom_text(data=IAMs_neg_SSPs_category_target[IAMs_neg_SSPs_category_target$category=="Original IAMs",],aes(x=-100,y=0.028,label=Label, hjust = 0),color="black",size=3)+
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  facet_grid(.~category, scales = "free")+
  geom_segment(data=IAMs_SC_CO2_seg_point_target[IAMs_SC_CO2_seg_point_target$category=="Original IAMs",],aes(x=xorigin,y=yposition,xend=xend,yend=yposition,color=Models),size=1)+
  geom_point(data=IAMs_SC_CO2_seg_point_target[IAMs_SC_CO2_seg_point_target$category=="Original IAMs",],aes(x=xmean,y=yposition,color=Models),size=3)+
  geom_text(data=IAMs_SC_CO2_seg_point_target[IAMs_SC_CO2_seg_point_target$category=="Original IAMs",],aes(x=label_x_position,y=yposition,label=Label, hjust = 0),color="black",size=3)+
  theme(panel.spacing = unit(1, "lines"))+
  xlab(expression(paste('SCC in 2030 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  ylab("Density")+
  theme(legend.position="top")+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(axis.text.x = element_text(size = 8, color = "black"))+
  theme(axis.text.y = element_text(size = 8, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 8, color = "black"))+
  theme(axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.title = element_text( size = 8, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 8, color = "black"))+     ##设置标签文字
  # theme(text = element_text(family='Microsoft YaHei',size = 20, color = "black"))+
  scale_colour_manual(values=c("#74C25E", "#00B4F0","#C915F5"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_fill_manual(values=c("#74C25E", "#00B4F0","#C915F5"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_x_continuous(limits=c(-100, 400), breaks=seq(-50,400,50))+
  scale_y_continuous(limits=c(-0.01, 0.03), breaks=seq(0,0.03,0.01))


p_SCC_density_IAMs_Hector=ggplot(SCC_IAMs_target[category=="IAMs-Hector",],aes(x=SCC,stat(density),group=Models,colour=Models,fill=Models)) + 
  geom_histogram(binwidth = 7.5,alpha=.6,position="identity",size=0.001) +
  geom_segment(aes(x = 0 , y = 0, xend = 0, yend = Inf),linetype="dashed",color="black",size=0.75) +
  geom_text(data=IAMs_neg_SSPs_category_target[IAMs_neg_SSPs_category_target$category=="IAMs-Hector",],aes(x=-100,y=0.028,label=Label, hjust = 0),color="black",size=3)+
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  facet_grid(.~category, scales = "free")+
  geom_segment(data=IAMs_SC_CO2_seg_point_target[IAMs_SC_CO2_seg_point_target$category=="IAMs-Hector",],aes(x=xorigin,y=yposition,xend=xend,yend=yposition,color=Models),size=1)+
  geom_point(data=IAMs_SC_CO2_seg_point_target[IAMs_SC_CO2_seg_point_target$category=="IAMs-Hector",],aes(x=xmean,y=yposition,color=Models),size=3)+
  geom_text(data=IAMs_SC_CO2_seg_point_target[IAMs_SC_CO2_seg_point_target$category=="IAMs-Hector",],aes(x=label_x_position,y=yposition,label=Label, hjust = 0),color="black",size=3)+
  theme(panel.spacing = unit(1, "lines"))+
  xlab(expression(paste('SCC in 2030 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  ylab("Density")+
  theme(legend.position="top")+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(axis.text.x = element_text(size = 8, color = "black"))+
  theme(axis.text.y = element_text(size = 8, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 8, color = "black"))+
  theme(axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.title = element_text( size = 8, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 8, color = "black"))+     ##设置标签文字
  # theme(text = element_text(family='Microsoft YaHei',size = 20, color = "black"))+
  scale_colour_manual(values=c("#74C25E", "#00B4F0","#C915F5"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_fill_manual(values=c("#74C25E", "#00B4F0","#C915F5"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_x_continuous(limits=c(-100, 400), breaks=seq(-50,400,50))+
  scale_y_continuous(limits=c(-0.01, 0.03), breaks=seq(0,0.03,0.01))


SCC_regional_IAMs_target = SCC_regional_IAMs[ds=="ds_3" & SSPs=="SSP2",]

p_SCC_regional_IAMs_original=ggplot(SCC_regional_IAMs_target[SCC_regional_IAMs_target$category=="Original IAMs",],aes(x=Models,y=SCC,fill=Regions)) + 
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  facet_grid(.~category, scales = "free")+
  theme(panel.spacing = unit(1, "lines"))+
  geom_text(aes(label = round(SCC,0)), size = 3, position = position_stack(vjust = 0.5))+
  ylab(expression(paste('SCC in 2030 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  xlab(" ")+
  theme(legend.position="top")+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(axis.text.x = element_text(size = 8, color = "black"))+
  theme(axis.text.y = element_text(size = 8, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 8, color = "black"))+
  theme(axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.title = element_text( size = 8, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 8, color = "black"))+     ##设置标签文字
  scale_fill_manual(values=c("#E3E758", "#FFC89F","#F6916B","#B95D3B"),breaks = c("USA", "Europe*", "China*","Other regions"),name = " ", labels = c("USA", "Europe*", "China*","Other regions"))#+
# theme(text = element_text(family='Microsoft YaHei',size = 20, color = "black"))+
# theme(legend.title=element_blank())


p_SCC_regional_IAMs_Hector=ggplot(SCC_regional_IAMs_target[SCC_regional_IAMs_target$category=="IAMs-Hector",],aes(x=Models,y=SCC,fill=Regions)) + 
  geom_bar(stat="identity") +
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  facet_grid(.~category, scales = "free")+
  theme(panel.spacing = unit(1, "lines"))+
  geom_text(aes(label = round(SCC,0)), size = 3, position = position_stack(vjust = 0.5))+
  ylab(expression(paste('SCC in 2030 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  xlab(" ")+
  theme(legend.position="top")+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(axis.text.x = element_text(size = 8, color = "black"))+
  theme(axis.text.y = element_text(size = 8, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 8, color = "black"))+
  theme(axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.title = element_text( size = 8, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 8, color = "black"))+     ##设置标签文字
  scale_fill_manual(values=c("#E3E758", "#FFC89F","#F6916B","#B95D3B"),breaks = c("USA", "Europe*", "China*","Other regions"),name = " ", labels = c("USA", "Europe*", "China*","Other regions"))#+
# theme(text = element_text(family='Microsoft YaHei',size = 20, color = "black"))+
# theme(legend.title=element_blank())

ECS_distribution_target = matrix(nrow = 20000,ncol = 2)
simulation_no = 10000;
ECS_distribution_target[,1] = c(rep("IPCC AR5 consistent ECS",simulation_no),
                                rep("Constrained ECS",simulation_no))
ECS_distribution_target[,2] = c(ECS_distribution$IPCC.AR5.consistent.ECS,
                                ECS_distribution$Constrained.ECS.to.RCMIP2)
ECS_distribution_target=as.data.frame(ECS_distribution_target)
colnames(ECS_distribution_target) = c("Type","ECS");
ECS_distribution_target$ECS = as.numeric(ECS_distribution_target$ECS)
ECS_distribution_target=transform(ECS_distribution_target,Type=factor(Type,levels = c("IPCC AR5 consistent ECS","Constrained ECS")))

p_ECS_density = ggplot(ECS_distribution_target,aes(group=Type,linetype=Type)) + 
  geom_density(aes(x=ECS), colour="#9394E7",size=1,show_guide=FALSE)+
  stat_density(aes(x=ECS), colour="#9394E7",
               geom="line",position="identity", size = 1)+
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  # xlab("Equilibrium climate sensitivity (K)")+
  xlab(expression(paste("Equilibrium climate sensitivity (", degree, "C)")))+
  ylab("Density")+
  theme(legend.position="top")+
  theme(strip.text = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(axis.text.x = element_text(size = 8, color = "black"))+
  theme(axis.text.y = element_text(size = 8, color = "black"))+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.title.x = element_text(size = 8, color = "black"))+
  theme(axis.title.y = element_text(size = 8, margin = margin(t = 0, r = 3, b = 0, l = 0), color = "black"))+
  theme(legend.title = element_text( size = 8, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 8, color = "black"))+     ##设置标签文字
  theme(legend.title=element_blank())+
  theme(legend.key.size = unit(0.8,"cm"),
        legend.key.width = unit(1.0,"cm"),
        legend.box.margin=margin(-10,-10,-10,-10))

IAMs_SC_CO2_seg_point_ECS_target = IAMs_SC_CO2_seg_point[Discount_rates=="ds_3" & SSPs=="SSP2" & category!="Original IAMs",]
IAMs_SC_CO2_seg_point_ECS_target[category=="IAMs-Hector (Empirically Constrained ECS)",]$yposition = c(-0.012,-0.015,-0.018)
IAMs_SC_CO2_seg_point_ECS_target$label_x_position = 130

IAMs_SC_CO2_seg_point_ECS_target$label_text = c("FUND-Hector","DICE-Hector","PAGE-Hector",
                                                "FUND-Hector","DICE-Hector","PAGE-Hector")

p_ECS_SCC = ggplot(IAMs_SC_CO2_seg_point_ECS_target) + 
  theme_bw() +
  theme(panel.border = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),axis.line = element_line(size=0.2,colour = "black"),axis.ticks = element_line(size=0.2,colour = "black"))+
  geom_segment(aes(x=xorigin,y=yposition,xend=xend,yend=yposition,color=Models,linetype = category),size=0.75)+
  geom_point(aes(x=xmean,y=yposition,color=Models),size=2)+
  geom_text(aes(x=label_x_position,y=yposition,label=Label, hjust = 0),color="black",size=2.8)+
  geom_text(aes(x=label_x_position+90,y=yposition,label=label_text, hjust = 0),color="black",size=2.8)+
  geom_text(aes(label = "Constrained ECS"),x=-60,y=-0.0165,size=2.8)+
  geom_text(aes(label = "IPCC AR5 \n consistent ECS"),x=-60,y=-0.005,size=2.8)+
  geom_hline(yintercept=-0.0105,color="black")+
  theme(panel.spacing = unit(2, "lines"))+
  xlab(expression(paste('SCC in 2030 ($2005 / metric ton '*'CO'[2],')',sep="  ")))+
  ylab(" ")+
  theme(legend.position="none")+
  theme(strip.text.x = element_text(colour = 'black',  size = rel(0.9)), strip.background = element_rect(fill = 'gray91', colour = 'gray91', size = rel(1), linetype = 1))+
  theme(strip.text.y = element_blank(), strip.background = element_blank())+
  theme(axis.text.x = element_text(size = 8, color = "black"))+
  theme(axis.text.y = element_blank())+
  theme(axis.ticks.length = unit(0.1,'cm'))+
  theme(axis.ticks.y = element_blank())+
  theme(axis.title.x = element_text(size = 8, color = "black"))+
  theme(axis.title.y = element_blank())+
  theme(legend.title = element_text(size = 8, color = "black")) +   ##设置标题，face="bold"加粗
  theme(legend.text = element_text(size = 8, color = "black"))+     ##设置标签文字
  # theme(text = element_text(family='Microsoft YaHei',size = 8, color = "black"))+
  scale_colour_manual(values=c("#74C25E", "#00B4F0","#C915F5"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_fill_manual(values=c("#74C25E", "#00B4F0","#C915F5"),name = "SCC distribution\nHistogram: density distribution\nPoints: average SCC\nLines: 5th-95th percentiles", breaks = c("PAGE", "DICE","FUND"))+
  scale_x_continuous(limits=c(-100, 300), breaks=seq(-100,300,100))+
  scale_y_continuous(limits=c(-0.02, 0))

library(gridExtra)
library(plyr)
library(ggpubr)

p_SCC_density_IAMs = ggarrange(p_SCC_density_IAMs_original,p_SCC_density_IAMs_Hector,ncol = 2,labels=c("A","B"),font.label = list(size = 10, color = "black", family = NULL),common.legend = TRUE,hjust = -1.5,vjust = 1.5);
p_ECS_density = ggarrange(p_ECS_density,nrow = 1,labels=c("C"),font.label = list(size = 10, color = "black", family = NULL),hjust = -1.0,vjust = 3.6);
p_ECS_SCC_density = ggarrange(p_ECS_density,p_ECS_SCC,nrow = 2,labels=c(" ","D"),font.label = list(size = 10, color = "black", family = NULL),hjust = -1.0,vjust = 0);
p_SCC_regional_IAMs = ggarrange(p_SCC_regional_IAMs_original,p_SCC_regional_IAMs_Hector,ncol = 2,labels=c("E","F"),font.label = list(size = 10, color = "black", family = NULL),common.legend = TRUE,hjust = -1.5,vjust = 1.5);

p_ECS_SCC_density_regions = ggarrange(p_ECS_SCC_density,p_SCC_regional_IAMs,ncol = 2,labels=c("",""),font.label = list(size = 10, color = "black", family = NULL),hjust = -1.5,vjust = 1.5);
p_Fig4 = ggarrange(p_SCC_density_IAMs,p_ECS_SCC_density_regions,nrow = 2,labels=c("",""),font.label = list(size = 10, color = "black", family = NULL),hjust = -1.5,vjust = 1.5);

ggsave("./Figure 4.tiff", p_Fig4,width = 174,height=174, dpi=900, units = "mm", compression = "lzw",bg = "white")

