# FUND_R_version:authorized by Tianpeng Wang
TimeStep=251; #时间跨度
#判断气候损失对收入的影响是否会转入下一年
runwithoutdamage=FALSE;
#判断气候损失对人口的影响是否会转入下一年
runwithoutpopulationperturbation=FALSE;

model_choose="BestGuess";
a=list.files("FUND_Base");
datalist=lapply(a,function(name){
  read.table(paste("FUND_Base/",name,sep=""),sep=",");
});
emissionperiod<-71;
impulselength<-10;
options(digits = 15);
file_num=195;
t0=1; #起始年份，模型默认为1950年

library("triangle");

Region_No=16; #区域数量
Sector_No=15;
MarginalEmission_CO2=FALSE;
MarginalEmission_N2O=FALSE;
MarginalEmission_CH4=FALSE;
MarginalEmission_SF6=FALSE;
library(stringr);
clock_Current<-function(year_current){
  t<<-year_current; 
}
# 编写best_guess函数
Best_Guess<-function(a)
{
  if (model_choose=="MonteCarlo"){
    if(is.numeric(a))
    {
      Best_Guess=a;
    }
    else 
    {
      if(substr(a,start=1,stop=1)=="0"){
        Best_Guess=0;
      }
      #当为正态分布时的数据处理办法
      else if(substr(a,start=2,stop=2)=="N")
      {
        intermediate=str_split(gsub("[()]","",sub("~N","",a)),";");   #去除括号和～N的标志符，再去除封号，得到各个变量
        if(is.na(intermediate[[1]][3])){
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          Best_Guess=rnorm(1,mean,variance);
        }
        else if(substring(intermediate[[1]][3],1,3)=="min" && is.na(intermediate[[1]][4])){
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rnorm(1,mean,variance);
            if(t>=min) break;
          }
          Best_Guess=t;
        }
        else if(substring(intermediate[[1]][3],1,3)=="max" && is.na(intermediate[[1]][4])){
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          max=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rnorm(1,mean,variance);
            if(t<=max) break;
          }
          Best_Guess=t;
        }
        else{
          mean=as.numeric(intermediate[[1]][1]);#均值
          variance=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          max=as.numeric(substring(intermediate[[1]][4],5));#求最大值
          repeat{
            t=rnorm(1,mean,variance);
            if(t>=min && t<=max) break;
          }
          Best_Guess=t;
        }
      }
      #当为gamma分布时的数据处理方法
      else if(substr(a,start=2,stop=2)=="G")
      {
        intermediate=str_split(gsub("[()]","",sub("~Gamma","",a)),";");   #去除括号和～Gamma的标志符，再去除封号，得到各个变量
        if(is.na(intermediate[[1]][3])){
          shape=as.numeric(intermediate[[1]][1]);#均值
          scale=as.numeric(intermediate[[1]][2]);#方差
          Best_Guess=rgamma(1,shape,1/scale);
        }
        else if(substring(intermediate[[1]][3],1,3)=="min"&&is.na(intermediate[[1]][4])){
          shape=as.numeric(intermediate[[1]][1]);
          scale=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rgamma(1,shape,1/scale);
            if(t>=min) break;
          }
          Best_Guess=t;#计算方法见FUND模型
        }
        else if(substring(intermediate[[1]][3],1,3)=="max" && is.na(intermediate[[1]][4])){
          shape=as.numeric(intermediate[[1]][1]);
          scale=as.numeric(intermediate[[1]][2]);#方差
          max=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          repeat{
            t=rgamma(1,shape,1/scale);
            if(t<=max) break;
          }
          Best_Guess=t;#计算方法见FUND模型
        }
        else{
          shape=as.numeric(intermediate[[1]][1]);
          scale=as.numeric(intermediate[[1]][2]);#方差
          min=as.numeric(substring(intermediate[[1]][3],5));#求最小值
          max=as.numeric(substring(intermediate[[1]][4],5));#求最小值
          repeat{
            t=rgamma(1,shape,1/scale);
            if(t<=max && t>=min) break;
          }
          Best_Guess=t;#计算方法见FUND模型
        }
      }
      #当为三角分布时的处理方法:拒绝抽样法进行三角分布抽样
      else if(substr(a,start=2,stop=2)=="T")
      {
        intermediate=str_split(gsub("[()]","",sub("~Triangular","",a)),";");   #去除括号和～Triangular的标志符，再去除封号，得到各个变量
        min=as.numeric(intermediate[[1]][1]);
        max=as.numeric(intermediate[[1]][2])
        expect=as.numeric(intermediate[[1]][3]);#提取第三个数字变量
        Best_Guess=rtriangle(1,min,max,expect);
      } 
    }
  }
  else if(model_choose=="BestGuess"){
    if(is.numeric(a)==TRUE)
    {
      Best_Guess=a;
    }
    else 
    {
      if(substr(a,start=1,stop=1)=="0"){
        Best_Guess=0;
      }
      #当为正态分布时的数据处理办法
      else if(substr(a,start=2,stop=2)=="N")
      {
        intermediate=str_split(gsub("[()]","",sub("~N","",a)),";");   #去除括号和～N的标志符，再去除封号，得到各个变量
        Best_Guess=as.numeric(intermediate[[1]][1]);#提取第一个数字变量
      }
      #当为gamma分布时的数据处理方法
      else if(substr(a,start=2,stop=2)=="G")
      {
        intermediate=str_split(gsub("[()]","",sub("~Gamma","",a)),";");   #去除括号和～Gamma的标志符，再去除封号，得到各个变量
        Best_Guess=(as.numeric(intermediate[[1]][1])-1)*as.numeric(intermediate[[1]][2]);#计算方法见FUND模型
      }
      #当为三角分布时的处理方法
      else if(substr(a,start=2,stop=2)=="T")
      {
        intermediate=str_split(gsub("[()]","",sub("~Triangular","",a)),";");   #去除括号和～Triangular的标志符，再去除封号，得到各个变量
        Best_Guess=as.numeric(intermediate[[1]][3]);#提取第三个数字变量
      } 
    }
  }
}
#编写分布函数
#考虑不确定情景
TimeOfUncertaintyStart=2000;  #不确定年份的起始时间设置
TimeOfUncertaintyStart_value=TimeOfUncertaintyStart-1950;
ypcgrowth=matrix(0,nrow=TimeStep,ncol=Region_No);   #真实的ypc增长率
scenypcgrowth=matrix(0,nrow=TimeStep,ncol=Region_No); #情境下的ypc增长率
ecgradd=matrix(0,nrow=1,ncol=Region_No);  #真实情况下相对情景下的ypc增长情况
pgrowth=matrix(0,nrow=TimeStep,ncol=Region_No);   #真实的p增长率
scenpgrowth=matrix(0,nrow=TimeStep,ncol=Region_No); #情境下的p增长率
pgadd=matrix(0,nrow=1,ncol=Region_No);#真实情况下相对情景下的p增长情况率
aeei=matrix(0,nrow=TimeStep,ncol=Region_No);#真实的自发性能源强度增加比率
acei=matrix(0,nrow=TimeStep,ncol=Region_No);#真实的自发性碳强度增加比率
aeei_add=matrix(0,nrow=1,ncol=Region_No);#真实的Aeei增加比率
acei_add=matrix(0,nrow=1,ncol=Region_No);#真实的Acei增加比率
scen_aeei=matrix(0,nrow=TimeStep,ncol=Region_No);#情境下的自发性能源强度增加比率
scen_acei=matrix(0,nrow=TimeStep,ncol=Region_No);#情境下的自发性碳强度增加比率
scenforestemm=matrix(0,nrow=TimeStep,ncol=Region_No);
forestemm=matrix(0,nrow=TimeStep,ncol=Region_No);
foremadd=matrix(0,nrow=1,ncol=Region_No);
#为部分参数赋值
for(i in 1:TimeStep)    
{
  for(j in 1:Region_No){
    scenypcgrowth[i,j]=Best_Guess(datalist[[150]]$V3[(16*(i-1)+j)]);
    scenpgrowth[i,j]=Best_Guess(datalist[[148]]$V3[(16*(i-1)+j)]);
    scen_aeei[i,j]=Best_Guess(datalist[[146]]$V3[(16*(i-1)+j)]);
    scen_acei[i,j]=Best_Guess(datalist[[145]]$V3[(16*(i-1)+j)]);
    scenforestemm[i,j]=Best_Guess(datalist[[147]]$V3[(16*(i-1)+j)]);
  }
}
for(j in 1:Region_No){
  ecgradd[1,j]=Best_Guess(datalist[[77]]$V2[j]);
  pgadd[1,j]=Best_Guess(datalist[[134]]$V2[j]);
  aeei_add[1,j]=Best_Guess(datalist[[2]]$V2[j]);
  acei_add[1,j]=Best_Guess(datalist[[1]]$V2[j]);
  foremadd[1,j]=Best_Guess(datalist[[89]]$V2[j]);
}
for(i in 1:TimeStep)    
{
  for(j in 1:Region_No)
  {
    YearsFromUncertaintyStart=i-TimeOfUncertaintyStart_value;
    SdTimeFactor=(YearsFromUncertaintyStart/50)/(1+(YearsFromUncertaintyStart/50));
    ypcgrowth[i,j]=scenypcgrowth[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,ecgradd[1,j]*SdTimeFactor,0.0);
    pgrowth[i,j]=scenpgrowth[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,pgadd[1,j]*SdTimeFactor,0.0);
    aeei[i,j]=scen_aeei[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,aeei_add[1,j]*SdTimeFactor,0.0);
    acei[i,j]=scen_acei[i,j]+ifelse((i-1)>=TimeOfUncertaintyStart_value,acei_add[1,j]*SdTimeFactor,0.0);
  }
}


#编写地理变化情况
Area=matrix(0,nrow=TimeStep,ncol=Region_No);#各个地区的区域面积（扣除了海平面上升导致的）
for(r in 1:Region_No){
  Area[t0,r]=Best_Guess(datalist[[11]]$V2[r]);
}
GeographyComponent<-function(landloss) #参数landloss是一个矩阵,来自于函数impactsealevelrise
{
  t=clock_Current(year_current);
  # t=2;
  Area[t,] <<- Area[t-1,] - landloss[t-1,];
  #  return(Area);
}
#编写人口变化函数
globalpopulation=matrix(0,nrow=TimeStep,ncol=1);
population=matrix(0,nrow=TimeStep,ncol=Region_No);
population1=matrix(0,nrow=TimeStep,ncol=Region_No);
for(r in 1:Region_No){
  population[t0,r]=Best_Guess(datalist[[137]]$V2[r]);#计算全球1950年各个地区的人口
  population1[t0,r]=population[t0,r]*1000000;
  globalpopulation[t0,1]=globalpopulation[t0,1]+population1[t0,r];#计算全球1950年总人口
}

FromSimulationYear=1990;
FromSimulationYear_value=FromSimulationYear-1950;

PopulationComponent<-function(pgrowth,leave,enter,dead)#参数均为矩阵，来自不同的函数结果
{
  t=clock_Current(year_current);
  t_value=t-1;
  globalpopulation=0;
  if(runwithoutpopulationperturbation==FALSE){
    if(t_value<40)  {population_factor=0;}
    else {
      population_factor=(enter[t-1,]/1000000)-(leave[t-1,]/1000000)-(ifelse(dead[t-1,]>=0,dead[t-1,]/1000000,0));
    }
  }
  else {population_factor=0;}
  population[t,]<<-(1+0.01*pgrowth[t-1,])*(population[t-1,]+population_factor);
  
  population[50,] <<- as.vector(as.matrix(pop_ssp[1,-1]));
  if(t>=51)  {
    population_factor=(enter[t-1,]/1000000)-(leave[t-1,]/1000000)-(ifelse(dead[t-1,]>=0,dead[t-1,]/1000000,0));
    population[t,]<<-as.vector(as.matrix(pop_ssp[t-49,-1]/pop_ssp[t-50,-1]))*(population[t-1,] + population_factor);
  }
  
  population[t,]<<-ifelse(population[t,] > 0,population[t,],0.000001)
  population1[t, ]<<-population[t, ]*1000000;
  globalpopulation=globalpopulation+sum(population1[t,]);
  globalpopulation[t,1]<<-globalpopulation;
}
#编写社会经济函数
savingsrate=0.2;
income=matrix(0,nrow=TimeStep,ncol=Region_No);
ypc=matrix(0,nrow=TimeStep,ncol=Region_No);    #人均收入
consumption=matrix(0,nrow=TimeStep,ncol=Region_No);  #消费
ygrowth=matrix(0,nrow=TimeStep,ncol=Region_No);  #年收入增长率
plus=matrix(0,nrow=TimeStep,ncol=Region_No);  
plus90=matrix(0,nrow=1,ncol=Region_No);
globalconsumpation=matrix(0,nrow=TimeStep,ncol=1);
plusel=Best_Guess(datalist[[136]]$V1);
gdp90=matrix(0,nrow=1,ncol=Region_No);
pop90=matrix(0,nrow=1,ncol=Region_No);
ypc90=matrix(0,nrow=1,ncol=Region_No);  
popdens=matrix(0,nrow=TimeStep,ncol=Region_No);   #人口密度
urbpop=matrix(0,nrow=TimeStep,ncol=Region_No); 
urbcorr=matrix(0,nrow=1,ncol=Region_No); 
globalincome=matrix(0,nrow=TimeStep,ncol=1); 
globalypc=matrix(0,nrow=TimeStep,ncol=1); 
globalpop=matrix(0,nrow=TimeStep,ncol=1); 
for(r in 1:Region_No){
  income[t0,r]=Best_Guess(datalist[[91]]$V2[r]);
  ypc[t0,r]=income[t0,r]/population[t0,r]*1000;
  consumption[t0,r]=income[t0,r]*1000000000*(1-savingsrate);
  globalconsumpation[t0,1]=globalconsumpation[t0,1]+consumption[t0,r];
  plus90[1,r]=Best_Guess(datalist[[135]]$V2[r]);
  gdp90[1,r]=Best_Guess(datalist[[92]]$V2[r]);
  pop90[1,r]=Best_Guess(datalist[[138]]$V2[r]);
  ypc90[1,r]=gdp90[1,r]/pop90[1,r]*1000;
  urbcorr[1,r]=Best_Guess(datalist[[173]]$V2[r])
}
consleak=Best_Guess(datalist[[48]]$V1);
SocialEconomicComponemt<-function(ypcgrowth,pgrowth,eloss,sloss,mitigationcost,Area,globalpopulation,population,population1){
  t=clock_Current(year_current);
  t_value=t-1;
  
  ygrowth[t,]<<-(1+0.01*pgrowth[t-1,])*(1+0.01*ypcgrowth[t-1,])-1;
  if(t_value>=FromSimulationYear_value&&runwithoutdamage==FALSE) { oldincome = income[t-1,]-consleak*eloss[t-1,]/10} else{
    oldincome = income[t-1,] - 0;
  };
  income[t,]<<-(1+ygrowth[t,])*oldincome-mitigationcost[t-1,];
  
  income[50,] <<- as.vector(as.matrix(gdp_ssp[1,-1]));
  if(t>=51)  {
    if(t_value>=FromSimulationYear_value&&runwithoutdamage==FALSE){oldincome = income[t-1,] - consleak*eloss[t-1,]/10} else{
      oldincome = income[t-1,] - 0;
    }
    income[t,]<<-as.vector(as.matrix(gdp_ssp[t-49,-1]/gdp_ssp[t-50,-1]))*oldincome-mitigationcost[t-1,];
  }
  
  income[t,] <<- ifelse(income[t,]<0.01*population[t,],0.1*population[t,],income[t,]);
  
  ypc[t,]<<-income[t,]/population[t,]*1000;
  
  total_consumpation=0;
  consumption[t,]<<-ifelse((income[t,]*1000000000*(1-savingsrate)-ifelse(rep(runwithoutdamage,Region_No)==TRUE,0,(eloss[t-1,]+sloss[t-1,])*1000000000))>0,(income[t,]*1000000000*(1-savingsrate)-ifelse(rep(runwithoutdamage,Region_No)==TRUE,0,(eloss[t-1,]+sloss[t-1,])*1000000000)),0)
  
  total_consumpation<<-total_consumpation+sum(consumption[t,]);
  
  plus[t,]<<-plus90[1,]*((ypc[t,]/ypc90[1,])^plusel);
  plus[t,]<<-ifelse(plus[t,]>1,1,plus[t,])
  
  popdens[t,]<<-population[t,]/Area[t,]*1000000;
  
  urbpop_temp = (0.031*(ypc[t,]^0.5)-0.011*(popdens[t,]^0.5))/((1+0.031*(ypc[t,]^0.5)-0.011*(popdens[t,]^0.5)));
  urbpop[t,]<<-urbpop_temp/(1+(urbcorr[1,]/(1+0.001*((t_value-40)^2))));
  
  if(t>=56){
    urbpop[t,]<<-as.vector(as.matrix(urb_pop_ssp[t-55,-1]));
  }
  
  globalincome[t]<<-sum(income[t,]);
  globalpop[t]<<-sum(population1[t,]);
  globalypc[t]<<-globalincome[t]*1000000000/globalpop[t];
}


#编写各温室气体排放情况
#初始化排放变量和最初年的排放情况
emission=matrix(0,nrow=TimeStep,ncol=16);#co2排放
ch4=matrix(0,nrow=TimeStep,ncol=16);
ch4em=matrix(0,nrow=TimeStep,ncol=16);
n2o=matrix(0,nrow=TimeStep,ncol=16);
n2oem=matrix(0,nrow=TimeStep,ncol=16);
sf6=matrix(0,nrow=TimeStep,ncol=16);
energint=matrix(0,nrow=TimeStep,ncol=16);
energuse=matrix(0,nrow=TimeStep,ncol=16);
emissionint=matrix(0,nrow=TimeStep,ncol=16);
emission=matrix(0,nrow=TimeStep,ncol=16);
ch4cost=matrix(0,nrow=TimeStep,ncol=16);
n2ocost=matrix(0,nrow=TimeStep,ncol=16);
ryg=matrix(0,nrow=TimeStep,ncol=16);
reei=matrix(0,nrow=TimeStep,ncol=16);
rcei=matrix(0,nrow=TimeStep,ncol=16);
seei=matrix(0,nrow=TimeStep,ncol=16);
scei=matrix(0,nrow=TimeStep,ncol=16);
co2red=matrix(0,nrow=TimeStep,ncol=16);
know=matrix(0,nrow=TimeStep,ncol=16);
ch4red=matrix(0,nrow=TimeStep,ncol=16);
taxpar=matrix(0,nrow=TimeStep,ncol=16);
n2ored=matrix(0,nrow=TimeStep,ncol=16);
currtax=matrix(0,nrow=TimeStep,ncol=16);
currtaxch4=matrix(0,nrow=TimeStep,ncol=16);
currtaxn2o=matrix(0,nrow=TimeStep,ncol=16);
perm=matrix(0,nrow=TimeStep,ncol=16);
mitigationcost=matrix(0,nrow=TimeStep,ncol=16);
emissionwithforesty=matrix(0,nrow=TimeStep,ncol=16);
ch4costindollars=matrix(0,nrow=TimeStep,ncol=16);
cumaeei=matrix(0,nrow=TimeStep,ncol=16);
globknow=matrix(0,nrow=TimeStep,ncol=1);
globch4=matrix(0,nrow=TimeStep,ncol=1);
globn2o=matrix(0,nrow=TimeStep,ncol=1);
globsf6=matrix(0,nrow=TimeStep,ncol=1);
cumglobco2=matrix(0,nrow=TimeStep,ncol=1);
cumglobch4=matrix(0,nrow=TimeStep,ncol=1);
cumglobn2o=matrix(0,nrow=TimeStep,ncol=1);
cumglobsf6=matrix(0,nrow=TimeStep,ncol=1);
sminint=matrix(0,nrow=TimeStep,ncol=1);
mco2=matrix(0,nrow=TimeStep,ncol=1);
sf60=matrix(0,nrow=1,ncol=16);
ch4par1=matrix(0,nrow=1,ncol=16);
ch4par2=matrix(0,nrow=1,ncol=16);
n2opar1=matrix(0,nrow=1,ncol=16);
n2opar2=matrix(0,nrow=1,ncol=16);
taxmp=matrix(0,nrow=1,ncol=16);
sf6gdp=Best_Guess(datalist[[154]]$V1);
sf6pre=Best_Guess(datalist[[155]]$V1);
sf6ypc=Best_Guess(datalist[[156]]$V1);
TaxConstant=Best_Guess(datalist[[164]]$V1);
TaxEmInt=Best_Guess(datalist[[166]]$V1);
TaxThreshold=Best_Guess(datalist[[168]]$V1);
TaxDepreciation=Best_Guess(datalist[[168]]$V1);
knowpar=Best_Guess(datalist[[107]]$V1);
knowgpar=Best_Guess(datalist[[106]]$V1);
MaxCostFall=Best_Guess(datalist[[124]]$V1);
gwpn2o=Best_Guess(datalist[[94]]$V1);
gwpch4=Best_Guess(datalist[[93]]$V1);
ch4add=Best_Guess(datalist[[30]]$V1);
n2oadd=Best_Guess(datalist[[127]]$V1);
sf6add=Best_Guess(datalist[[153]]$V1);
for(t in 1:TimeStep)
{
  for(r in 1:Region_No){
    ch4em[t,r]=Best_Guess(datalist[[31]]$V3[r+(t-1)*Region_No]);
    n2oem[t,r]=Best_Guess(datalist[[128]]$V3[r+(t-1)*Region_No]);
    currtax[t,r]=Best_Guess(datalist[[49]]$V3[r+(t-1)*Region_No]);
    currtaxch4[t,r]=Best_Guess(datalist[[49]]$V3[r+(t-1)*Region_No]);
    currtaxn2o[t,r]=Best_Guess(datalist[[49]]$V3[r+(t-1)*Region_No]);
  }
}
for(r in 1:Region_No){
  energint[t0,r]=1;
  energuse[t0,r]=Best_Guess(datalist[[91]]$V2[r]);
  emissionint[t0,r]=Best_Guess(datalist[[79]]$V2[r]);
  emission[t0,r]=emissionint[t0,r]/energuse[t0,r];
  ch4par1[t0,r]=Best_Guess(datalist[[33]]$V2[r]);
  ch4par2[t0,r]=Best_Guess(datalist[[34]]$V2[r]);
  n2opar1[t0,r]=Best_Guess(datalist[[129]]$V2[r]);
  n2opar2[t0,r]=Best_Guess(datalist[[130]]$V2[r]);
  taxmp[t0,r]=Best_Guess(datalist[[167]]$V2[r]);
  ch4cost[t0,r]=0;
  n2ocost[t0,r]=0;
  ryg[t0,r]=0;
  reei[t0,r]=0;
  rcei[t0,r]=0;
  seei[t0,r]=0;
  scei[t0,r]=0;
  co2red[t0,r]=0;
  know[t0,r]=1;
  ch4red[t0,r]=0;
  n2ored[t0,r]=0;
  mitigationcost[t0,r]=0;
  sf60[t0,r]=Best_Guess(datalist[[152]]$V2[r]);
}
globknow[t0,1]=1;
cumglobco2[t0,1]=0;
cumglobch4[t0,1]=0;
cumglobn2o[t0,1]=0;
cumglobsf6[t0,1]=0;
minint=10^10;#minint等于正的无穷大
for(r in 1:Region_No){
  
  if(emission[t0,r]/income[t0,r]<minint )
    minint=emission[t0,r]/income[t0,r];
}
sminint[t0,1]=0;
#各温室气体排放情况
EmissionComponent<-function(forestemm,pgrowth,ypcgrowth,income,population){
  t=clock_Current(year_current);
  t_value=t-1;
  
  if(t_value>(2000-1950)){
    cumaeei[t,]<<-cumaeei[t-1,]*(1-0.01*aeei[t,]-reei[t,]+seei[t-1,]-seei[t,]);
  }
  else
  {
    cumaeei[t,]<<-1;
  }
  
  # #计算碳强度和能源强度
  energint[t,]<<-(1-0.01*aeei[t,]-reei[t-1,])*energint[t-1,];
  emissionint[t,]<<-(1-0.01*acei[t,]-rcei[t-1,])*emissionint[t-1,];
  #计算sf6排放
  #   sf6[t,]<<-(sf60[1,]+sf6gdp*(income[t,]-gdp90[1,])+sf6ypc*(income[t-1,]/population[t-1,]-gdp90[1,]/pop90[1,]))*ifelse(t_value<=60,1+(t_value-40)/40,1+(60-40)/40)*ifelse(t_value>60,0.99^(t_value-60),1);
  # for(r in 1:Region_No){
  #   if(sf6[t,r]<0)   sf6[t,r]<<-0;
  # }
  
  energuse[t,]<<-(1-seei[t-1,])*energint[t,]*income[t,];
  
  # emission[t,]<<-(1-scei[t-1,])*emissionint[t,]*energuse[t,];
  emissionwithforesty[t,]<<-emission[t,]+forestemm[t,];
  
  #计算CH4排放
  # ch4[t,]<<-ch4em[t,];
  #计算N2O排放
  # n2o[t,r]<<-n2oem[t,r];
  
  s_globco2=0;
  s_globch4=0;
  s_globn2o=0;
  s_globsf6=0;
  s_globco2=sum(emissionwithforesty[t,]);
  s_globch4=sum(ch4[t,]);
  s_globn2o=sum(n2o[t,]);
  s_globsf6=sum(sf6[t,]);
  
  mco2[t]<<-s_globco2;
  
  #使非二氧化碳气体排放失去波动性，与耦合模型保持一致
  # globch4[t,1]<<-max(0,s_globch4+ifelse(t_value>50,ch4add*(t_value-50),0));
  # globn2o[t,1]<<-max(0,s_globn2o+ifelse(t_value>50,n2oadd*(t_value-50),0));
  # globsf6[t,1]<<-max(0,s_globsf6+ifelse(t_value>50,sf6add*(t_value-50),0));
  globch4[t]<<-max(0,s_globch4);
  globn2o[t]<<-max(0,s_globn2o);
  globsf6[t]<<-max(0,s_globsf6);
  cumglobch4[t]<<-cumglobch4[t-1]+mco2[t];
  cumglobco2[t]<<-cumglobco2[t-1]+globch4[t];
  cumglobn2o[t]<<-cumglobn2o[t-1]+globn2o[t];
  cumglobsf6[t]<<-cumglobsf6[t-1]+globsf6[t];
}

#计算CO2气体循环模式
lifeco1=Best_Guess(datalist[[109]]$V1);
lifeco2=Best_Guess(datalist[[110]]$V1);
lifeco3=Best_Guess(datalist[[111]]$V1);
lifeco4=Best_Guess(datalist[[112]]$V1);
lifeco5=Best_Guess(datalist[[113]]$V1);
lifen2o=Best_Guess(datalist[[114]]$V1);
cbox10=Best_Guess(datalist[[18]]$V1);
cbox20=Best_Guess(datalist[[19]]$V1);
cbox30=Best_Guess(datalist[[20]]$V1);
cbox40=Best_Guess(datalist[[21]]$V1);
cbox50=Best_Guess(datalist[[22]]$V1);
co2fra1=Best_Guess(datalist[[41]]$V1);
co2fra2=Best_Guess(datalist[[42]]$V1);
co2fra3=Best_Guess(datalist[[43]]$V1);
co2fra4=Best_Guess(datalist[[44]]$V1);
co2fra5=Best_Guess(datalist[[45]]$V1);
TerrCO2Stock0=Best_Guess(datalist[[171]]$V1);
TerrCO2Sens=Best_Guess(datalist[[170]]$V1);
TerrCO2Stock=matrix(0,nrow=TimeStep,ncol=1);
cbox1=matrix(0,nrow=TimeStep,ncol=1);
cbox2=matrix(0,nrow=TimeStep,ncol=1);
cbox3=matrix(0,nrow=TimeStep,ncol=1);
cbox4=matrix(0,nrow=TimeStep,ncol=1);
cbox5=matrix(0,nrow=TimeStep,ncol=1);
acco2=matrix(0,nrow=TimeStep,ncol=1);
globc=matrix(NA,nrow=TimeStep,ncol=1);
TerrestrialCO2=matrix(0,nrow=TimeStep,ncol=1);
TerrCO2Stock[t0,1]=Best_Guess(datalist[[171]]$V1);
co2decay1=lifeco1;
co2decay2=exp(-1/lifeco2);
co2decay3=exp(-1/lifeco3);
co2decay4=exp(-1/lifeco4);
co2decay5=exp(-1/lifeco5);
cbox1[t0,1]=cbox10;
cbox2[t0,1]=cbox20;
cbox3[t0,1]=cbox30;
cbox4[t0,1]=cbox40;
cbox5[t0,1]=cbox50;
acco2[t0]=cbox1[t0,1]+cbox2[t0,1]+cbox3[t0,1]+cbox4[t0,1]+cbox5[t0,1];
ClimateCO2CycleComponent<-function(mco2,s_temp){
  t=clock_Current(year_current);
  t_value=t-1;
  if(t_value==(2011-1950)){
    tempIn2010<<-s_temp[(2010-1950+1)];
  }
  if(t_value>(2010-1950)){
    TerrestrialCO2[t]<<-(s_temp[t-1]-tempIn2010)*TerrCO2Sens*TerrCO2Stock[t-1]/TerrCO2Stock0;
  }
  else{
    TerrestrialCO2[t]<<-0;
  }
  TerrCO2Stock[t]<<-max(TerrCO2Stock[t-1]-TerrestrialCO2[t],0);
  globc[t]<<-mco2[t]+TerrestrialCO2[t];
  cbox1[t]<<-cbox1[t-1]*co2decay1+0.000471*co2fra1*globc[t];
  cbox2[t]<<-cbox2[t-1]*co2decay2+0.000471*co2fra2*globc[t];
  cbox3[t]<<-cbox3[t-1]*co2decay3+0.000471*co2fra3*globc[t];
  cbox4[t]<<-cbox4[t-1]*co2decay4+0.000471*co2fra4*globc[t];
  cbox5[t]<<-cbox5[t-1]*co2decay5+0.000471*co2fra5*globc[t];
  acco2[t]<<-cbox1[t]+cbox2[t]+cbox3[t]+cbox4[t]+cbox5[t];
}
#计算CH4循环模式
lifech4=Best_Guess(datalist[[108]]$V1);
ch4pre=Best_Guess(datalist[[35]]$V1);
acch4=matrix(0,nrow=TimeStep,ncol=1);
ch4decay=1/lifech4;
acch4[t0,1]=1222;
ClimateCH4CycleComponent<-function(globch4){
  t=clock_Current(year_current);
  acch4[t]<<-acch4[t-1]+0.3597*globch4[t]-ch4decay*(acch4[t-1]-ch4pre);
  if(acch4[t]<0){
    print("CH4浓度超出范围");
  }
}
#计算n2o循环模式
lifen2o=Best_Guess(datalist[[114]]$V1);
n2opre=Best_Guess(datalist[[131]]$V1);
acn2o=matrix(0,nrow=TimeStep,ncol=1);
n2odecay=1/lifen2o;
acn2o[t0,1]=296;
ClimateN2OCycleComponent<-function(globn2o){
  t=clock_Current(year_current);
  acn2o[t]<<-acn2o[t-1]+0.2079*globn2o[t]-n2odecay*(acn2o[t-1]-n2opre);
  if(acn2o[t]<0){
    print("N2O浓度超出范围");
  }
}
#计算sf6循环模式
lifesf6=Best_Guess(datalist[[116]]$V1);
sf6pre=Best_Guess(datalist[[155]]$V1);
acsf6=matrix(0,nrow=TimeStep,ncol=1);
sf6decay=1/lifesf6;
acsf6[t0,1]=sf6pre;
ClimateSF6CycleComponent<-function(globsf6){
  t=clock_Current(year_current);
  acsf6[t]<<-sf6pre+(acsf6[t-1]-sf6pre)*(1-sf6decay)+globsf6[t]/25.1;
  if(acsf6[t]<0){
    print("SF6浓度超出范围");
  }
}
#计算Climate Forcing
rfCO2=matrix(0,nrow=TimeStep,ncol=1);
rfCH4=matrix(0,nrow=TimeStep,ncol=1);
rfN2O=matrix(0,nrow=TimeStep,ncol=1);
rfSF6=matrix(0,nrow=TimeStep,ncol=1);
radforc=matrix(0,nrow=TimeStep,ncol=1);
rfEMF22=matrix(0,nrow=TimeStep,ncol=1);
ch4ind=Best_Guess(datalist[[32]]$V1);
co2pre=Best_Guess(datalist[[46]]$V1);
ch4pre=Best_Guess(datalist[[35]]$V1);
rfSO2=matrix(0,nrow=TimeStep,ncol=1);
# for(t in 1:TimeStep){
#   rfSO2[t]=Best_Guess(datalist[[141]]$V2[t]);
# }
Interact<-function(M,N){
  d=1+((M*N)^0.75)*2.01*10^(-5)+((M*N)^1.52)*M*5.31*10^(-15);
  return(0.47*log(d));
}
ClimateForcingComponent<-function(acco2,acch4,acn2o,acsf6){
  t=clock_Current(year_current);
  ch4n2o=Interact(ch4pre,n2opre);
  rfCO2[t]<<-5.35*log(acco2[t]/co2pre);
  rfCH4[t]<<-0.036*(1+ch4ind)*(acch4[t]^0.5-ch4pre^0.5)-Interact(acch4[t],n2opre)+ch4n2o;
  rfN2O[t]<<-0.12*(acn2o[t]^0.5-n2opre^0.5)-Interact(ch4pre,acn2o[t])+ch4n2o;
  rfSF6[t]<<-0.00052*(acsf6[t]-sf6pre);
  radforc[t]<<-rfCO2[t]+rfCH4[t]+rfN2O[t]+rfSF6[t]+rfSO2[t];
  rfEMF22[t]<<-rfCO2[t]+rfCH4[t]+rfN2O[t];
}

#计算气候变化导致的温升效应
s_temp=matrix(0,nrow=TimeStep,ncol=1);
LifeTempConst=Best_Guess(datalist[[117]]$V1);
LifeTempLin=Best_Guess(datalist[[118]]$V1);
LifeTempQd=Best_Guess(datalist[[119]]$V1);
ClimateSensitivity = Best_Guess(datalist[[40]]$V1);
s_temp[t0]=0.2;
ClimateDynamicComponent<-function(radforc){
  LifeTemp<<-max(LifeTempConst+LifeTempLin*ClimateSensitivity+LifeTempQd*(ClimateSensitivity^2),1);
  delaytemp<<-1/LifeTemp;
  temps<<-ClimateSensitivity/5.35/log(2);
  dtemp<<-delaytemp*temps*radforc[t]-delaytemp*s_temp[t-1];
  s_temp[t]<<-s_temp[t-1]+dtemp;
  if(s_temp[t]<=0){
    s_temp[t]<<-0.02;
  }
}
#计算气候变化的各地的温升影响
bregstemp=matrix(0,nrow=1,ncol=Region_No);
bregtemp=matrix(0,nrow=1,ncol=Region_No);
scentemp=matrix(0,nrow=TimeStep,ncol=Region_No);
for(r in 1:Region_No){
  bregstemp[1,r]=Best_Guess(datalist[[15]]$V2[r]);
  bregtemp[1,r]=Best_Guess(datalist[[16]]$V2[r]);
}

for(t in 1:TimeStep){
  for(r in 1:Region_No){
    scentemp[t,r]=Best_Guess(datalist[[149]]$V3[(t-1)*Region_No+r]);
  }
}
temp=matrix(0,nrow=TimeStep,ncol=Region_No);
for(r in 1:Region_No){
  temp[1,r]=0.2;
}
regtemp=matrix(0,nrow=TimeStep,ncol=Region_No);
regstemp=matrix(0,nrow=TimeStep,ncol=Region_No);
ClimateRegionComponent<-function(s_temp){
  t=clock_Current(year_current);
  regtemp[t,]<<-s_temp[t]*bregtemp[1,]+scentemp[t,];
  temp[t,]<<-regtemp[t,]/bregtemp[1,];
  regstemp[t,]<<-s_temp[t]*bregstemp[1,]+scentemp[t,];
}

#计算海洋
lifesea=Best_Guess(datalist[[115]]$V1);
sea=matrix(0,nrow=TimeStep,ncol=1);
seas=Best_Guess(datalist[[151]]$V1);
delaysea=1/lifesea;
sea[t0]=0;
OceanComponent<-function(s_temp){
  t=clock_Current(year_current);
  ds=delaysea*seas*s_temp[t]-delaysea*sea[t-1];
  sea[t]<<-sea[t-1]+ds;
}

#计算生物多样性
nospieces=matrix(0,nrow=TimeStep,ncol=1);
bioloss=Best_Guess(datalist[[12]]$V1);
biosens=Best_Guess(datalist[[13]]$V1);
nospacebase=Best_Guess(datalist[[132]]$V1);
dbsta=Best_Guess(datalist[[63]]$V1);
nospieces[t0,1]=nospacebase;
BioDiversityComponent<-function(s_temp){
  t=clock_Current(year_current);
  t_value=t-1;
  if(t_value>(2000-1950)){
    dt=abs(s_temp[t]-s_temp[t-1]);
    nospieces[t]<<-max(nospacebase/100,nospieces[t-1]*(1-bioloss-biosens*dt*dt/dbsta/dbsta));
  }
  else{
    nospieces[t]<<-nospacebase;
  }
}

#计算对生物多样性影响
bioshare=Best_Guess(datalist[[14]]$V1);
spbm=Best_Guess(datalist[[163]]$V1);
valbase=Best_Guess(datalist[[174]]$V1);
biodiv=matrix(0,nrow=TimeStep,ncol=1);
valinc=matrix(0,nrow=1,ncol=Region_No);
species=matrix(0,nrow=TimeStep,ncol=Region_No);
for(r in 1:Region_No){
  valinc[1,r]=Best_Guess(datalist[[175]]$V2[r]);
}
ImpactBiodiversityComponent<-function(temp,nospieces,income,population){
  t=clock_Current(year_current);
  biodiv[t]<<-nospacebase/nospieces[t];
  s_ypc=1000*income[t,]/population[t,];
  dt=abs(temp[t,]-temp[t-1,]);
  valadj=valbase/valinc[1,]/(1+valbase/valinc[1,]);
  species[t,]<<-spbm/valbase*s_ypc/valinc[1,]/(1+s_ypc/valinc[1,])/valadj*s_ypc*population[t,]/1000*dt/dbsta/(1+dt/dbsta)*(1-bioshare+bioshare*biodiv[t,1]);
}

#热影响
heating=matrix(0,nrow=TimeStep,ncol=Region_No);
hebm=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  hebm[1,r]=Best_Guess(datalist[[95]]$V2[r])
}
heel=Best_Guess(datalist[[96]]$V1);
ImpactHeatingComponent<-function(temp,population,income,cumaeei){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  heating[t,]<<-hebm[1,]*cumaeei[t,]*gdp90[1,]*atan(temp[t,])/atan(1)*((ypc/ypc90)^heel)*population[t,]/pop90[1,];
}
#冷影响
cooling=matrix(0,nrow=TimeStep,ncol=Region_No);
cebm=matrix(0,nrow=1,ncol=Region_No);
ceel=Best_Guess(datalist[[28]]$V1);
cenl=Best_Guess(datalist[[29]]$V1);
for(r in 1:Region_No){
  cebm[1,r]=Best_Guess(datalist[[27]]$V2[r])
}
ImpactCoolingComponent<-function(population,income,temp,cumaeei){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  cooling[t,]<<-cebm[1,]*cumaeei[t,]*gdp90[1,]*(temp[t,]^cenl)*((ypc/ypc90)^ceel)*population[t,]/pop90[1,];
}

#计算对农业部门影响
DBsT=0.04;
agrbm=matrix(0,nrow=1,ncol=Region_No);
agrate=matrix(0,nrow=TimeStep,ncol=Region_No);
agnl=Best_Guess(datalist[[7]]$V1);
agel=Best_Guess(datalist[[4]]$V1);
agtime=matrix(0,nrow=1,ncol=Region_No);
agrish=matrix(0,nrow=TimeStep,ncol=Region_No);
agrish0=matrix(0,nrow=1,ncol=Region_No);
aglevel=matrix(0,nrow=TimeStep,ncol=Region_No);
agco2=matrix(0,nrow=TimeStep,ncol=Region_No);
agcost=matrix(0,nrow=TimeStep,ncol=Region_No);
aglpar1=matrix(0,nrow=1,ncol=Region_No);
aglparq=matrix(0,nrow=1,ncol=Region_No);
agcbm=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  agrbm[1,r]=Best_Guess(datalist[[8]]$V2[r]);
  agtime[1,r]=Best_Guess(datalist[[10]]$V2[r]);
  agrish0[1,r]=Best_Guess(datalist[[9]]$V2[r]);
  agrate[t0,r]=agrbm[1,r]*((0.005/DBsT)^agnl)*agtime[1,r];
  agcbm[t0,r]=Best_Guess(datalist[[3]]$V2[r]);
  aglpar1[t0,r]=Best_Guess(datalist[[5]]$V2[r]);
  aglparq[t0,r]=Best_Guess(datalist[[6]]$V2[r]);
}
ImpactAgricultureComponent<-function(population,income,temp,acco2){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  agrish[t,]<<-agrish0[1,]*((ypc/ypc90)^(-agel));
  dtemp=abs(temp[t,r]-temp[t-1,r]);
  if(is.nan((dtemp/0.04)^agnl)==TRUE)  {agrate[t,]<<-0;}
  else {agrate[t,]<<-agrbm[1,]*((dtemp/0.04)^agnl)+(1-1/agtime[1,])*agrate[t-1,]};
  aglevel[t,]<<-aglpar1[1,]*temp[t,]+aglparq[1,]*(temp[t,]^2);
  agco2[t,]<<-agcbm[1,]/log(2)*log(acco2[t-1]/co2pre);
  agcost[t,]<<-ifelse(agrate[t,]+aglevel[t,]+agco2[t,]>1,1,agrate[t,]+aglevel[t,]+agco2[t,])*agrish[t,]*income[t,];
}

#对水资源的影响
watechrate=Best_Guess(datalist[[183]]$V1);
watech=matrix(0,nrow=TimeStep,ncol=1);
wrbm=matrix(0,nrow=1,ncol=Region_No);
wrpl=Best_Guess(datalist[[191]]$V1);
wrnl=Best_Guess(datalist[[190]]$V1);
wrel=Best_Guess(datalist[[189]]$V1);
water=matrix(0,nrow=TimeStep,ncol=Region_No);
for(r in 1:Region_No){
  wrbm[1,r]=Best_Guess(datalist[[188]]$V2[r]);
}
watech[t0,1]=1;
ImpactWaterResourceComponent<-function(population,income,temp){
  t=clock_Current(year_current);
  t_value=t-1;
  if(t_value>(2000-1950)){
    watech[t]<<-(1-watechrate)^(t_value-(2000-1950));
  }
  else{
    watech[t]<<-1;
  }
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  s_water=wrbm[1,]*gdp90[1,]*watech[t]*((ypc/ypc90)^wrel)*((population[t,]/pop90[1,])^wrpl)*(temp[t,]^wrnl);
  water[t,]<<- ifelse(s_water>0.1*income[t,],0.1*income[t,r],s_water);
}

#对疟疾发病率的影响
diadead=matrix(0,nrow=TimeStep,ncol=Region_No);
diasick=matrix(0,nrow=TimeStep,ncol=Region_No);
diamort=matrix(0,nrow=1,ncol=Region_No);
diamortel=Best_Guess(datalist[[68]]$V1);
diamortnl=Best_Guess(datalist[[69]]$V1);
diayld=matrix(0,nrow=1,ncol=Region_No);
diayldel=Best_Guess(datalist[[71]]$V1);
diayldnl=Best_Guess(datalist[[72]]$V1);
temp90=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  diamort[1,r]=Best_Guess(datalist[[67]]$V2[r]);
  diayld[1,r]=Best_Guess(datalist[[70]]$V2[r]);
  temp90[1,r]=Best_Guess(datalist[[169]]$V2[r]);
}
ImpactDiarrhoeaComponent<-function(population,income,regtemp){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  absoluteRegionalTemPreIndustrial=temp90-0.49*bregtemp[1,];
  diadead[t,] <<- ifelse(absoluteRegionalTemPreIndustrial>0,diamort[1,]*population[t,]*((ypc/ypc90)^diamortel)*(((absoluteRegionalTemPreIndustrial+regtemp[t,])/absoluteRegionalTemPreIndustrial)^diamortnl-1),0);
  diasick[t,] <<- ifelse(absoluteRegionalTemPreIndustrial>0,diayld[1,]*population[t,]*((ypc/ypc90)^diayldel)*(((absoluteRegionalTemPreIndustrial+regtemp[t,])/absoluteRegionalTemPreIndustrial)^diayldnl-1),0);
}

#计算普通热带风暴天气的影响
hurrdam=matrix(0,nrow=TimeStep,ncol=Region_No);
hurrdead=matrix(0,nrow=TimeStep,ncol=Region_No);
hurrbasedam=matrix(0,nrow=1,ncol=Region_No);
hurrbasedead=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  hurrbasedam[1,r]=Best_Guess(datalist[[98]]$V2[r]);
  hurrbasedead[1,r]=Best_Guess(datalist[[99]]$V2[r]);
}
hurrpar=Best_Guess(datalist[[103]]$V1);
hurrdeadel=Best_Guess(datalist[[101]]$V1);
hurrdamel=Best_Guess(datalist[[100]]$V1);
hurrnl=Best_Guess(datalist[[102]]$V1);
ImpactTropicalStromComponent<-function(population,income,acco2){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  hurrdam[t,]<<-0.001*hurrbasedam[1,]*income[t,]*((ypc/ypc90)^hurrdamel)*(((1+hurrpar*regstemp[t,])^hurrnl)-1);
  hurrdead[t,]<<-1000*hurrbasedead[1,]*population[t,]*((ypc/ypc90)^hurrdeadel)*(((1+hurrpar*regstemp[t,])^hurrnl)-1);
}

#计算极端风暴天气的影响
extratropicalstormsdam=matrix(0,nrow=TimeStep,ncol=Region_No);
extratropicalstormsdead=matrix(0,nrow=TimeStep,ncol=Region_No);
extratropicalstormsbasedam=matrix(0,nrow=1,ncol=Region_No);
extratropicalstormsbasedead=matrix(0,nrow=1,ncol=Region_No);
extratropicalstormsdamel=Best_Guess(datalist[[82]]$V1);
extratropicalstormsdeadel=Best_Guess(datalist[[83]]$V1);
extratropicalstormsnl=Best_Guess(datalist[[84]]$V1);
extratropicalstormspar=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  extratropicalstormsbasedam[1,r]=Best_Guess(datalist[[80]]$V2[r]);
  extratropicalstormsbasedead[1,r]=Best_Guess(datalist[[81]]$V2[r]);
  extratropicalstormspar[1,r]=Best_Guess(datalist[[85]]$V2[r]);
}
ImpactExtratropicalStormsComponent<-function(population,income,acco2){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  extratropicalstormsdam[t,]<<-extratropicalstormsbasedam[1,]*income[t,]*((ypc/ypc90)^extratropicalstormsdamel)*(((1+extratropicalstormspar[1,]*(acco2[t]/co2pre))^extratropicalstormsnl-1));
  extratropicalstormsdead[t,]<<-1000*extratropicalstormsbasedead[1,]*population[t,]*((ypc/ypc90)^extratropicalstormsdeadel)*(((1+extratropicalstormspar[1,]*(acco2[t]/co2pre))^extratropicalstormsnl-1));                                                                                                              
}

#计算对海平面上升的影响
migrate=matrix(0,nrow=Region_No,ncol=Region_No);
imigrate=matrix(0,nrow=Region_No,ncol=Region_No);
landloss=matrix(0,nrow=TimeStep,ncol=Region_No);
cumlandloss=matrix(0,nrow=TimeStep,ncol=Region_No);
cumwetlandloss=matrix(0,nrow=TimeStep,ncol=Region_No);
wetlandgrowth=matrix(0,nrow=TimeStep,ncol=Region_No);
wetval=matrix(0,nrow=TimeStep,ncol=Region_No);
wetlandloss=matrix(0,nrow=TimeStep,ncol=Region_No);
wetcost=matrix(0,nrow=TimeStep,ncol=Region_No);
drycost=matrix(0,nrow=TimeStep,ncol=Region_No);
npprotcost=matrix(0,nrow=TimeStep,ncol=Region_No);
npwetcost=matrix(0,nrow=TimeStep,ncol=Region_No);
npdrycost=matrix(0,nrow=TimeStep,ncol=Region_No);
protlev=matrix(0,nrow=TimeStep,ncol=Region_No);
protcost=matrix(0,nrow=TimeStep,ncol=Region_No);
enter=matrix(0,nrow=TimeStep,ncol=Region_No);
leave=matrix(0,nrow=TimeStep,ncol=Region_No);
entercost=matrix(0,nrow=TimeStep,ncol=Region_No);
leavecost=matrix(0,nrow=TimeStep,ncol=Region_No);
dryval=matrix(0,nrow=TimeStep,ncol=Region_No);
pc=matrix(0,nrow=1,ncol=Region_No);
slrprtp=matrix(0,nrow=1,ncol=Region_No);
wmbm=matrix(0,nrow=1,ncol=Region_No);
dlbm=matrix(0,nrow=1,ncol=Region_No);
drylandlossparam=matrix(0,nrow=1,ncol=Region_No);
wlbm=matrix(0,nrow=1,ncol=Region_No);
coastpd=matrix(0,nrow=1,ncol=Region_No);
wetmax=matrix(0,nrow=1,ncol=Region_No);
wetland90=matrix(0,nrow=1,ncol=Region_No);
maxlandloss=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  pc[1,r]=Best_Guess(datalist[[133]]$V2[r]);
  slrprtp[1,r]=Best_Guess(datalist[[157]]$V2[r]);
  wmbm[1,r]=Best_Guess(datalist[[187]]$V2[r]);
  dlbm[1,r]=Best_Guess(datalist[[73]]$V2[r]);
  drylandlossparam[1,r]=Best_Guess(datalist[[74]]$V2[r]);
  wlbm[1,r]=Best_Guess(datalist[[186]]$V2[r]);
  coastpd[1,r]=Best_Guess(datalist[[47]]$V2[r]);
  wetmax[1,r]=Best_Guess(datalist[[185]]$V2[r]);
  wetland90[1,r]=Best_Guess(datalist[[184]]$V2[r]);
  maxlandloss[1,r]=Best_Guess(datalist[[125]]$V2[r]);
}
dvbm=Best_Guess(datalist[[75]]$V1);
dvydl=Best_Guess(datalist[[76]]$V1);
incdens=Best_Guess(datalist[[105]]$V1);
emcst=Best_Guess(datalist[[78]]$V1);
immcst=Best_Guess(datalist[[104]]$V1);
wvel=Best_Guess(datalist[[193]]$V1);
wvbm=Best_Guess(datalist[[192]]$V1);
slrwvpopdens0=Best_Guess(datalist[[158]]$V1);
wvpdl=Best_Guess(datalist[[194]]$V1);
wvsl=Best_Guess(datalist[[195]]$V1);
slrwvypc0=Best_Guess(datalist[[159]]$V1);
for(r1 in 1:Region_No){
  for(r2 in 1:Region_No){
    migrate[r1,r2]=Best_Guess(datalist[[126]]$V3[(r1-1)*Region_No+r2]);
  }
}
for(r1 in 1:Region_No){
  for(r2 in 1:Region_No){
    immsum=0;
    for(i in 1:Region_No){
      immsum=immsum+migrate[i,r1];
    }
    imigrate[r1,r2]=migrate[r2,r1]/immsum;
  }
  landloss[t0,r1]=0;
  cumlandloss[t0,r1]=0;
  cumwetlandloss[t0,r1]=0;
  wetlandgrowth[t0,r]=0;
}
ImpactSeaLevelRiseComponent<-function(population,income,sea,Area){
  t=clock_Current(year_current);
  t_value=t-1;
  ds=sea[t]-sea[t-1];
  ypc=income[t,]/population[t,]*1000;
  ypcprev=income[t-1,]/population[t-1,]*1000;
  ypcgrowth=ypc/ypcprev-1;
  if(t_value==(1951-1950)) {ypcgrowth=0;}
  
  incomedens=income[t,]/Area[t,];
  incomedensprev=income[t-1,]/Area[t-1,];
  incomedensgrowth=incomedens/incomedensprev-1;
  
  popdens=population[t,]/Area[t,]*1000000;
  popdensprev=population[t-1,]/Area[t-1,]*1000000;
  popdensgrowth=popdens/popdensprev-1;
  
  dryval[t,]<<-dvbm*((incomedens/incdens)^dvydl);
  
  wetval[t,]<<-wvbm*((ypc/slrwvypc0)^wvel)*((popdens/slrwvpopdens0)^wvpdl)*(((wetland90[1,]-cumwetlandloss[t-1,])/wetland90[1,])^wvsl);
  potCumLandloss=ifelse(maxlandloss[1,]>(dlbm[1,]*((sea[t])^drylandlossparam[1,])),(dlbm[1,]*((sea[t])^drylandlossparam[1,])),maxlandloss[1,]);
  potLandloss=potCumLandloss-cumlandloss[t-1,];
  
  npprotcost[t,] <<- ifelse(ds<0 | (1+slrprtp[1,]+ypcgrowth)<0 |(1+dvydl*incomedensgrowth)<0 | (1/(1+slrprtp[1,]+ypcgrowth))>=1 | ((1+dvydl*incomedensgrowth)/(1+slrprtp[1,]+ypcgrowth))>=1 |((1+wvel*ypcgrowth+wvpdl*popdensgrowth+wvsl*wetlandgrowth[t-1,])/(1+slrprtp[1,]+ypcgrowth))>=1,0,pc[1,]*ds*(1+slrprtp[1,]+ypcgrowth)/(slrprtp[1,]+ypcgrowth));
  npwetcost[t,] <<- ifelse(ds<0 | (1+slrprtp[1,]+ypcgrowth)<0 |(1+dvydl*incomedensgrowth)<0 | (1/(1+slrprtp[1,]+ypcgrowth))>=1 | ((1+dvydl*incomedensgrowth)/(1+slrprtp[1,]+ypcgrowth))>=1 |((1+wvel*ypcgrowth+wvpdl*popdensgrowth+wvsl*wetlandgrowth[t-1,])/(1+slrprtp[1,]+ypcgrowth))>=1 | (1+wvel*ypcgrowth+wvpdl*popdensgrowth+wvsl*wetlandgrowth[t-1,])<0,0,wmbm[1,]*ds*wetval[t,]*(1+slrprtp[1,]+ypcgrowth)/(slrprtp[1,]+ypcgrowth-wvel*ypcgrowth-wvpdl*popdensgrowth-wvsl*wetlandgrowth[t-1,]));
  npdrycost[t,] <<- ifelse(ds<0 | (1+slrprtp[1,]+ypcgrowth)<0 |(1+dvydl*incomedensgrowth)<0 | (1/(1+slrprtp[1,]+ypcgrowth))>=1 | ((1+dvydl*incomedensgrowth)/(1+slrprtp[1,]+ypcgrowth))>=1 |((1+wvel*ypcgrowth+wvpdl*popdensgrowth+wvsl*wetlandgrowth[t-1,])/(1+slrprtp[1,]+ypcgrowth))>=1 | (1+dvydl*incomedensgrowth)<0,0,potLandloss*dryval[t,]*(1+slrprtp[1,]+ypcgrowth)/(slrprtp[1,]+ypcgrowth-dvydl*incomedensgrowth));
  protlev[t,] <<- ifelse(ds<0 | (1+slrprtp[1,]+ypcgrowth)<0 |(1+dvydl*incomedensgrowth)<0 | (1/(1+slrprtp[1,]+ypcgrowth))>=1 | ((1+dvydl*incomedensgrowth)/(1+slrprtp[1,]+ypcgrowth))>=1 |((1+wvel*ypcgrowth+wvpdl*popdensgrowth+wvsl*wetlandgrowth[t-1,])/(1+slrprtp[1,]+ypcgrowth))>=1 | (1-0.5*((npprotcost[t,]+npwetcost[t,])/npdrycost[t,]))<0,0,(1-0.5*((npprotcost[t,]+npwetcost[t,])/npdrycost[t,])));
  
  wetlandloss[t,]<<-ifelse((wlbm[1,]*ds+protlev[t,]*wmbm[1,]*ds)>(wetmax[1,]-cumwetlandloss[t-1,]),(wetmax[1,]-cumwetlandloss[t-1,]),(wlbm[1,]*ds+protlev[t,]*wmbm[1,]*ds));
  cumwetlandloss[t,]<<-cumwetlandloss[t-1,]+wetlandloss[t,];
  wetlandgrowth[t,]<<-(wetland90[1,]-cumwetlandloss[t,])/(wetland90[1,]-cumwetlandloss[t-1,])-1;
  wetcost[t,]<<-wetval[t,]*wetlandloss[t,];
  landloss[t,]<<-(1-protlev[t,])*potLandloss;
  cumlandloss[t,]<<-cumlandloss[t-1,]+landloss[t,];
  drycost[t,]<<-dryval[t,]*landloss[t,];
  protcost[t,]<<-protlev[t,]*pc[1,]*ds;
  
  leave[t,] <<- ifelse(landloss[t,]<0,0,coastpd[1,]*popdens*landloss[t,]);
  leavecost[t,]<<-emcst*ypc*leave[t,]/1000000000;
  
  for(destination in 1:Region_No){
    s_enter=0;
    for(source in 1:Region_No){
      s_enter=s_enter+leave[t,source]*imigrate[source,destination];
    }
    enter[t,destination]<<-s_enter;
  }
  
  ypc=income[t,]/population[t,]*1000;
  entercost[t,]<<-immcst*ypc*enter[t,]/1000000000;
}

#对森林的影响
forests=matrix(0,nrow=TimeStep,ncol=Region_No);
forbm=matrix(0,nrow=1,ncol=Region_No);
forel=Best_Guess(datalist[[88]]$V1);
fornl=Best_Guess(datalist[[90]]$V1);
forco2=Best_Guess(datalist[[87]]$V1);
for(r in 1:Region_No){
  forbm[1,r]=Best_Guess(datalist[[86]]$V2[r]);
}
ImpactForests<-function(population,income,temp,acco2){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  forests[t,]<<-forbm[1,]*income[t,]*((ypc/ypc90)^forel)*(0.5*(temp[t,]^fornl)+0.5*log(acco2[t-1]/co2pre)*forco2);
  forests[t,]<<-ifelse(forests[t,]>0.1*income[t,],0.1*income[t,],forests[t,]);
}

#计算对疾病的影响
dengue=matrix(0,nrow=TimeStep,ncol=Region_No);
schisto=matrix(0,nrow=TimeStep,ncol=Region_No);
malaria=matrix(0,nrow=TimeStep,ncol=Region_No);
dfbs=matrix(0,nrow=1,ncol=Region_No);
dfch=matrix(0,nrow=1,ncol=Region_No);
smbs=matrix(0,nrow=1,ncol=Region_No);
smch=matrix(0,nrow=1,ncol=Region_No);
malbs=matrix(0,nrow=1,ncol=Region_No);
malch=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  dfbs[1,r]=Best_Guess(datalist[[64]]$V2[r]);
  dfch[1,r]=Best_Guess(datalist[[65]]$V2[r]);
  smbs[1,r]=Best_Guess(datalist[[160]]$V2[r]);
  smch[1,r]=Best_Guess(datalist[[161]]$V2[r]);
  malbs[1,r]=Best_Guess(datalist[[120]]$V2[r]);
  malch[1,r]=Best_Guess(datalist[[121]]$V2[r]);
}
dfnl=Best_Guess(datalist[[66]]$V1);
vbel=Best_Guess(datalist[[176]]$V1);
smnl=Best_Guess(datalist[[162]]$V1);
malnl=Best_Guess(datalist[[122]]$V1);
ImpactVectorBornelDiseases<-function(population,temp,income){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  ypc90=gdp90[1,]/pop90[1,]*1000;
  dengue[t,]<<-dfbs[1,]*population[t,]*dfch[1,]*(temp[t,]^dfnl)*((ypc/ypc90)^vbel);
  schisto[t,]<<-smbs[1,]*population[t,]*smch[1,]*(temp[t,]^smnl)*((ypc/ypc90)^vbel);
  schisto[t,]<<-ifelse(schisto[t,]<(0-smbs[1,]*population[t,]*((ypc/ypc90)^vbel)),(0-smbs[1,]*population[t,]*((ypc/ypc90)^vbel)),schisto[t,])
  malaria[t,]<<-malbs[1,]*population[t,]*malch[1,]*(temp[t,]^malnl)*((ypc/ypc90)^vbel);
}

#计算对心血管疾病的影响
basecardvasc=matrix(0,nrow=TimeStep,ncol=Region_No);
baseresp=matrix(0,nrow=TimeStep,ncol=Region_No);
cardheat=matrix(0,nrow=TimeStep,ncol=Region_No);
resp=matrix(0,nrow=TimeStep,ncol=Region_No);
cardcold=matrix(0,nrow=TimeStep,ncol=Region_No);
cardvasc90=matrix(0,nrow=1,ncol=Region_No);
resp90=matrix(0,nrow=1,ncol=Region_No);
chplbm=matrix(0,nrow=1,ncol=Region_No);
chmlbm=matrix(0,nrow=1,ncol=Region_No);
chpqbm=matrix(0,nrow=1,ncol=Region_No);
chmqbm=matrix(0,nrow=1,ncol=Region_No);
rlbm=matrix(0,nrow=1,ncol=Region_No);
rqbm=matrix(0,nrow=1,ncol=Region_No);
ccplbm=matrix(0,nrow=1,ncol=Region_No);
ccmlbm=matrix(0,nrow=1,ncol=Region_No);
ccpqbm=matrix(0,nrow=1,ncol=Region_No);
ccmqbm=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  cardvasc90[1,r]=Best_Guess(datalist[[17]]$V2[r]);
  resp90[1,r]=Best_Guess(datalist[[140]]$V2[r]);
  chplbm[1,r]=Best_Guess(datalist[[38]]$V2[r]);
  chmlbm[1,r]=Best_Guess(datalist[[36]]$V2[r]);
  chpqbm[1,r]=Best_Guess(datalist[[39]]$V2[r]);
  chmqbm[1,r]=Best_Guess(datalist[[37]]$V2[r]);
  rlbm[1,r]=Best_Guess(datalist[[142]]$V2[r]);
  rqbm[1,r]=Best_Guess(datalist[[144]]$V2[r]);
  ccplbm[1,r]=Best_Guess(datalist[[25]]$V2[r]);
  ccmlbm[1,r]=Best_Guess(datalist[[23]]$V2[r]);
  ccpqbm[1,r]=Best_Guess(datalist[[26]]$V2[r]);
  ccmqbm[1,r]=Best_Guess(datalist[[24]]$V2[r]);
}
cvlin=Best_Guess(datalist[[52]]$V1);
rlin=Best_Guess(datalist[[143]]$V1);
maxcardvasc=Best_Guess(datalist[[123]]$V1);

ImpactCardiovascularComponent<-function(population,temp,plus,urbpop){
  t=clock_Current(year_current);
  basecardvasc[t,]<<-cardvasc90[1,]+cvlin*(plus[t,]-plus90[1,]);
  basecardvasc[t,]<<-ifelse(basecardvasc[t,]>1,1,basecardvasc[t,]);
  baseresp[t,]<<-resp90[1,]+rlin*(plus[t,]-plus90[1,]);
  baseresp[t,]<<-ifelse(baseresp[t,]>1,1,baseresp[t,]);
  s_cardheat=(chplbm[1,]*plus[t,]+chmlbm[1,]*(1-plus[t,]))*temp[t,]+
    (chpqbm[1,]*plus[t,]+chmqbm[1,]*(1-plus[t,]))*(temp[t,r]^2);
  cardheat[t,]<<-s_cardheat*urbpop[t,]*population[t,]*10;
  cardheat[t,]<<-ifelse(cardheat[t,]>1000*maxcardvasc*basecardvasc[t,]*urbpop[t,]*population[t,],1000*maxcardvasc*basecardvasc[t,]*urbpop[t,]*population[t,],cardheat[t,]);
  cardheat[t,]<<-ifelse(cardheat[t,]<0,0,cardheat[t,]);
  resp[t,]<<-rlbm[1,]*temp[t,]+rqbm[1,]*(temp[t,]^2);
  resp[t,]<<-resp[t,]*urbpop[t,]*population[t,]*10;
  resp[t,]<<-ifelse(resp[t,]>1000*maxcardvasc*baseresp[t,]*urbpop[t,]*population[t,],1000*maxcardvasc*baseresp[t,]*urbpop[t,]*population[t,],resp[t,]);
  resp[t,]<<-ifelse(resp[t,]<0,0,resp[t,]);
  s_cardcold=(ccplbm[1,]*plus[t,]+ccmlbm[1,]*(1-plus[t,]))*temp[t,]+
    (ccpqbm[1,]*plus[t,]+ccmqbm[1,]*(1-plus[t,]))*(temp[t,]^2);
  cardcold[t,]<<-s_cardcold*population[t,]*10;
  cardcold[t,]<<-ifelse(cardcold[t,] < (0-1000*maxcardvasc*basecardvasc[t,]*population[t,]),(0-1000*maxcardvasc*basecardvasc[t,]*population[t,]),cardcold[t,]);
  cardcold[t,]<<-ifelse(cardcold[t,]>0,0,cardcold[t,]);
}

#计算温升导致死亡率变化
dead=matrix(0,nrow=TimeStep,ncol=Region_No);
yll=matrix(0,nrow=TimeStep,ncol=Region_No);
yld=matrix(0,nrow=TimeStep,ncol=Region_No);
deadcost=matrix(0,nrow=TimeStep,ncol=Region_No);
morbcost=matrix(0,nrow=TimeStep,ncol=Region_No);
vsl=matrix(0,nrow=TimeStep,ncol=Region_No);
vmorb=matrix(0,nrow=TimeStep,ncol=Region_No);
d2ld=matrix(0,nrow=1,ncol=Region_No);
d2ls=matrix(0,nrow=1,ncol=Region_No);
d2lm=matrix(0,nrow=1,ncol=Region_No);
d2lc=matrix(0,nrow=1,ncol=Region_No);
d2lr=matrix(0,nrow=1,ncol=Region_No);
d2dd=matrix(0,nrow=1,ncol=Region_No);
d2dm=matrix(0,nrow=1,ncol=Region_No);
d2dc=matrix(0,nrow=1,ncol=Region_No);
d2dr=matrix(0,nrow=1,ncol=Region_No);
d2ds=matrix(0,nrow=1,ncol=Region_No);
for(r in 1:Region_No){
  d2lc[1,r]=Best_Guess(datalist[[58]]$V2[r]);
  d2ld[1,r]=Best_Guess(datalist[[59]]$V2[r]);
  d2ls[1,r]=Best_Guess(datalist[[62]]$V2[r]);
  d2lr[1,r]=Best_Guess(datalist[[61]]$V2[r]);
  d2lm[1,r]=Best_Guess(datalist[[60]]$V2[r]);
  d2dr[1,r]=Best_Guess(datalist[[56]]$V2[r]);
  d2dm[1,r]=Best_Guess(datalist[[55]]$V2[r]);
  d2dc[1,r]=Best_Guess(datalist[[53]]$V2[r]);
  d2dd[1,r]=Best_Guess(datalist[[54]]$V2[r]);
  d2ds[1,r]=Best_Guess(datalist[[57]]$V2[r]);
}
vslbm=Best_Guess(datalist[[180]]$V1);
vslel=Best_Guess(datalist[[181]]$V1);
vmorbbm=Best_Guess(datalist[[177]]$V1);
vmorbel=Best_Guess(datalist[[178]]$V1);
vslypc0=Best_Guess(datalist[[182]]$V1);
vmorbypc0=Best_Guess(datalist[[179]]$V1);
ImpactDeathMorbidityComponent<-function(population,income,dengue,schisto,malaria,cardheat,cardcold,resp,diadead,hurrdead,extratropicalstormsdead,diasick){
  t=clock_Current(year_current);
  ypc=income[t,]/population[t,]*1000;
  dead[t,]<<-dengue[t,]+schisto[t,]+malaria[t,]+cardheat[t,]+cardcold[t,]+resp[t,]+diadead[t,]+hurrdead[t,]+extratropicalstormsdead[t,];
  dead[t,]<<-ifelse(dead[t,]>population[t,]*1000000,population[t,]*1000000,dead[t,]);
  yll[t,]<<-d2ld[1,]*dengue[t,]+d2ls[1,]*schisto[t,]+d2lm[1,]*malaria[t,]+d2lc[1,]*cardheat[t,]+d2lc[1,]*cardcold[t,]+d2lr[1,]*resp[t,];
  yld[t,]<<-d2dd[1,]*dengue[t,]+d2ds[1,]*schisto[t,]+d2dm[1,]*malaria[t,]+d2dc[1,]*cardheat[t,]+d2dc[1,]*cardcold[t,]+d2dr[1,]*resp[t,]+diasick[t,];
  vsl[t,]<<-vslbm*((ypc/vslypc0)^vslel);
  deadcost[t,]<<-vsl[t,]*dead[t,]/1000000000;
  vmorb[t,]<<-vmorbbm*((ypc/vmorbypc0)^vmorbel);
  morbcost[t,]<<-vmorb[t,]*yld[t,]/1000000000;
}


#将损失损害加总
switchoffwater=FALSE;
switchoffforests=FALSE;
switchoffheating=FALSE;
switchoffcooling=FALSE;
switchoffagcost=FALSE;
switchoffdrycost=FALSE;
switchoffprotcost=FALSE;
switchoffentercost=FALSE;
switchoffhurrdam=FALSE;
switchoffextratropicalstromsdam=FALSE;
switchoffspecies=FALSE;
switchoffdeadcost=FALSE;
switchoffmorbcost=FALSE;
switchoffwetcost=FALSE;
switchoffleavecost=FALSE;
eloss=matrix(0,nrow=TimeStep,ncol=Region_No);
sloss=matrix(0,nrow=TimeStep,ncol=Region_No);
loss=matrix(0,nrow=TimeStep,ncol=Region_No);
for(r in 1:Region_No){
  eloss[t0,r]=0;
  sloss[t0,r]=0;
}
ImpactAggregationComponent<-function(income,heating,cooling,agcost,species,water,hurrdam,extratropicalstormsbasedam,forests,drycost,protcost,entercost,deadcost,morbcost,wetcost,leavecost){
  t=clock_Current(year_current);
  
  for(r in 1:Region_No){
    eloss[t,r]<<-min(0-ifelse(switchoffwater==TRUE,0,water[t,r])
                     -ifelse(switchoffforests==TRUE,0,forests[t,r])
                     -ifelse(switchoffheating==TRUE,0,heating[t,r])
                     -ifelse(switchoffcooling==TRUE,0,cooling[t,r])
                     -ifelse(switchoffagcost==TRUE,0,agcost[t,r])
                     +ifelse(switchoffdrycost==TRUE,0,drycost[t,r])
                     +ifelse(switchoffprotcost==TRUE,0,protcost[t,r])
                     +ifelse(switchoffentercost==TRUE,0,entercost[t,r])
                     +ifelse(switchoffhurrdam==TRUE,0,hurrdam[t,r])
                     +ifelse(switchoffextratropicalstromsdam==TRUE,0,extratropicalstormsdam[t,r]),income[t,r])
    
    sloss[t,r]<<-(0+ifelse(switchoffspecies==TRUE,0,species[t,r])
                  +ifelse(switchoffdeadcost==TRUE,0,deadcost[t,r])
                  +ifelse(switchoffmorbcost==TRUE,0,morbcost[t,r])
                  +ifelse(switchoffwetcost==TRUE,0,wetcost[t,r])
                  +ifelse(switchoffleavecost==TRUE,0,leavecost[t,r]));
    loss[t,r]<<-(eloss[t,r]+sloss[t,r])*1000000000;
  }
}

library(compiler);
enableJIT(1);

Playground_Model<-function(YearTorun){     # CALCULATE DAMAGE
  for(t in 1:YearTorun){
    year_current<<-t;
    if(t==1){
      ClimateRegionComponent(s_temp)
      ImpactWaterResourceComponent(population,income,temp)
      ImpactDiarrhoeaComponent(population,income,regtemp)
      ImpactTropicalStromComponent(population,income,acco2)
      ImpactExtratropicalStormsComponent(population,income,acco2)
      ImpactVectorBornelDiseases(population,temp,income )
    }
    else{
      ClimateRegionComponent(s_temp)
      GeographyComponent(landloss)
      PopulationComponent(pgrowth,leave,enter,dead)
      SocialEconomicComponemt(ypcgrowth,pgrowth,eloss,sloss,mitigationcost,Area,globalpopulation,population,population1)
      
      OceanComponent(s_temp)
      BioDiversityComponent(s_temp)
      ImpactBiodiversityComponent(temp,nospieces,income,population)
      ImpactHeatingComponent(temp,population,income,cumaeei)
      ImpactCoolingComponent(population,income,temp,cumaeei)
      ImpactAgricultureComponent(population,income,temp,acco2)
      ImpactWaterResourceComponent(population,income,temp)
      ImpactDiarrhoeaComponent(population,income,regtemp)
      ImpactTropicalStromComponent(population,income,acco2)
      ImpactExtratropicalStormsComponent(population,income,acco2)
      ImpactSeaLevelRiseComponent(population,income,sea,Area)
      ImpactVectorBornelDiseases(population,temp,income )
      ImpactForests(population,income,temp,acco2)
      ImpactCardiovascularComponent(population,temp,plus,urbpop)
      ImpactDeathMorbidityComponent(population,income,dengue,schisto,malaria,cardheat,cardcold,resp,diadead,hurrdead,extratropicalstormsdead,diasick)
      ImpactAggregationComponent(income,heating,cooling,agcost,species,water,hurrdam,extratropicalstormsbasedam,forests,drycost,protcost,entercost,deadcost,morbcost,wetcost,leavecost)
    }
  }
}
