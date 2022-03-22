# PAGE model R version: authorized by Tianpeng Wang
#install.packages("triangle");
library("triangle");

a=list.files("IAMs_R_version/PAGE09_model/PAGE_base");
model_choose="MontelCarlo";
datalist=lapply(a,function(name){
  read.table(paste("IAMs_R_version/PAGE09_model/PAGE_base/",name,sep=""),sep=",",header = TRUE)
})
#emissionperiod<-61;
impulselength<-10;
options(digits = 15);
file_num=53;
t0=1; #起始年份，模型默认为1950年
TimeStep=10; #时间跨度
Region_No=8; #区域数量
Sector_No=15;
MarginalEmission_CO2=FALSE;
MarginalEmission_N2O=FALSE;
MarginalEmission_CH4=FALSE;
MarginalEmission_SF6=FALSE;
library(stringr);
clock_Current<-function(year_current){
  t<<-year_current; 
}
y_year=c(2009,2010,2020,2030,2040,2050,2075,2100,2150,2200);

y_year_0=2008;
# 编写best_guess函数
Best_Guess<-function(a)
{
  if (model_choose=="MontelCarlo"){
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


# CO2 emissions part
e_globalCO2emissions=matrix(nrow =TimeStep,ncol = 1 );
e0_baselineCO2emissions=matrix(nrow =1,ncol = Region_No );
e_regionalCO2emissions=matrix(nrow =TimeStep,ncol = Region_No);
er_CO2emissionsgrowth=matrix(nrow =TimeStep,ncol = Region_No);
for(r in 1:Region_No){
  e0_baselineCO2emissions[r]=Best_Guess(datalist[[9]]$e_co2_0[r]);
  e_globalCO2emissions[t0]=sum(e0_baselineCO2emissions[1,]);
  e_regionalCO2emissions[t0,r]=e0_baselineCO2emissions[r];
}
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    er_CO2emissionsgrowth[t,r]=Best_Guess(datalist[[14]][t,r+1]);
  }
}
CO2_emissions<-function(){
  t=clock_Current(year_current);
  for(r in 1:Region_No){
    e_regionalCO2emissions[t,r]<<-er_CO2emissionsgrowth[t,r]*e0_baselineCO2emissions[1,r]/100;
  }
  e_globalCO2emissions[t]<<-sum(e_regionalCO2emissions[t,]);
}

# CO2 cycle
c_CO2concentration=matrix(nrow =TimeStep,ncol = 1 );
pic_preindustconcCO2=278000;
#exc_excessconcCO2;
c0_CO2concbaseyr=395000;
re_remainCO2=matrix(nrow =TimeStep,ncol = 1 );
#re_remainCO2base;
renoccf_remainCO2wocc=matrix(nrow =TimeStep,ncol = 1 );
air_CO2fractioninatm=Best_Guess(datalist[[101]]$Value);
stay_fractionCO2emissionsinatm=Best_Guess(datalist[[144]]$Value);
tea_CO2emissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
teay_CO2emissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
ccf_CO2feedback=Best_Guess(datalist[[105]]$Value);
ccfmax_maxCO2feedback=Best_Guess(datalist[[106]]$Value);
cea_cumCO2emissionsatm=matrix(nrow =TimeStep,ncol = 1 );
tea_CO2emissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
teay_CO2emissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
ce_0_basecumCO2emissions=2050000;
#y_year=matrix(nrow =TimeStep,ncol = 1 );
#y_year_0;
res_CO2atmlifetime=Best_Guess(datalist[[136]]$Value);
den_CO2density=7.8;
rt_g0_baseglobaltemp=0.735309967925382;
e0_globalCO2emissions=38191.0315797948;
rt_g_globaltemperature=matrix(nrow =TimeStep,ncol = 1 );
# gain=ccf_CO2feedback*rt_g0_baseglobaltemp
# tea0=e0_globalCO2emissions*air_CO2fractioninatm/100

# for(t in 1:TimeStep){
# tea_CO2emissionstoatm[t]=(e_globalCO2emissions[t])*air_CO2fractioninatm/100
# teay_CO2emissionstoatm[t]=(tea_CO2emissionstoatm[t]+tea0)/2
# }
# exc_excessconcCO2=c0_CO2concbaseyr-pic_preindustconcCO2;
# re_remainCO2base=exc_excessconcCO2*den_CO2density;
#PAGE 2009 initial remaining emissions without CO2 feedback
# renoccf0_remainCO2wocc=re_remainCO2base/(1+gain/100)
#eq. 8 from Hope (2006) - baseline cumulative emissions to atmosphere
# ceabase=ce_0_basecumCO2emissions*air_CO2fractioninatm/100
#eq.9 from Hope(2006) - cumulative emissions in atmosphere

CO2_cycle<-function(){
  t=clock_Current(year_current);
  if (t==1){
    #CO2 emissions gain calculated based on PAGE 2009
    gain=ccf_CO2feedback*rt_g0_baseglobaltemp
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea0=e0_globalCO2emissions*air_CO2fractioninatm/100
    tea_CO2emissionstoatm[t]<<-(e_globalCO2emissions[t])*air_CO2fractioninatm/100
    teay_CO2emissionstoatm[t]<<-(tea_CO2emissionstoatm[t]+tea0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcCO2<<-c0_CO2concbaseyr-pic_preindustconcCO2
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainCO2base<<-exc_excessconcCO2*den_CO2density
    #PAGE 2009 initial remaining emissions without CO2 feedback
    renoccf0_remainCO2wocc=re_remainCO2base/(1+gain/100)
    #eq. 8 from Hope (2006) - baseline cumulative emissions to atmosphere
    ceabase=ce_0_basecumCO2emissions*air_CO2fractioninatm/100
    #eq.9 from Hope(2006) - cumulative emissions in atmosphere
    cea_cumCO2emissionsatm[t]<<-ceabase+teay_CO2emissionstoatm[t]
    #eq.11 from Hope (2006) - anthropogenic remaining emissions
    renoccf_remainCO2wocc[t]<<-stay_fractionCO2emissionsinatm*ceabase*
      (1-exp(-(y_year[t]-y_year_0)/
               res_CO2atmlifetime))+renoccf0_remainCO2wocc*
      exp(-(y_year[t]-y_year_0)/res_CO2atmlifetime)+
      teay_CO2emissionstoatm[t]*exp(-(y_year[t]-y_year_0)/
                                      (2*res_CO2atmlifetime))
    #Hope 2009 - remaining emissions with CO2 feedback
    re_remainCO2[t]<<-renoccf_remainCO2wocc[t]*(1+gain/100)
  }
  else{
    #CO2 emissions gain calculated based on PAGE 2009
    gain=min(ccf_CO2feedback*rt_g_globaltemperature[t-1],ccfmax_maxCO2feedback)
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_CO2emissionstoatm[t]<<-(e_globalCO2emissions[t])*air_CO2fractioninatm/100
    #eq.7 from Hope (2006) - total emissions over time period
    teay_CO2emissionstoatm[t]<<-(tea_CO2emissionstoatm[t]+tea_CO2emissionstoatm[t-1])*
      (y_year[t]-y_year[t-1])/2
    #eq.9 from Hope(2006) - cumulative emissions in atmosphere
    cea_cumCO2emissionsatm[t]<<-cea_cumCO2emissionsatm[t-1]+teay_CO2emissionstoatm[t]
    #eq.11 from Hope (2006) - anthropogenic remaining emissions
    renoccf_remainCO2wocc[t]<<-stay_fractionCO2emissionsinatm*cea_cumCO2emissionsatm[t-1]*
      (1-exp(-(y_year[t]-y_year[t-1])/
               res_CO2atmlifetime))+renoccf_remainCO2wocc[t-1]*
      exp(-(y_year[t]-y_year[t-1])/res_CO2atmlifetime)+
      teay_CO2emissionstoatm[t]*exp(-(y_year[t]-y_year[t-1])/
                                      (2*res_CO2atmlifetime))
    #Hope 2009 - remaining emissions with CO2 feedback
    re_remainCO2[t]<<-renoccf_remainCO2wocc[t]*(1+gain/100)
  }
  #eq.11 from Hope(2006) - CO2 concentration
  c_CO2concentration[t]<<-pic_preindustconcCO2+exc_excessconcCO2 * re_remainCO2[t]/re_remainCO2base
  
}

# CO2 forcing
f0_CO2baseforcing=1.735;
fslope_CO2forcingslope=5.5;
c0_baseCO2conc=395000;
f_CO2forcing=matrix(nrow =TimeStep,ncol = 1 );
CO2_forcing<-function(){
  f_CO2forcing[t]<<-f0_CO2baseforcing+fslope_CO2forcingslope*log(c_CO2concentration[t]/c0_baseCO2conc);
}


# CH4 emissions
e_globalCH4emissions=matrix(nrow =TimeStep,ncol = 1 );
e0_baselineCH4emissions=matrix(nrow =1,ncol = Region_No );
e_regionalCH4emissions=matrix(nrow =TimeStep,ncol = Region_No );
er_CH4emissionsgrowth=matrix(nrow =TimeStep,ncol = Region_No );
for(r in 1:Region_No){
  e0_baselineCH4emissions[r]=Best_Guess(datalist[[8]]$e_ch4_0[r]);
  e_globalCH4emissions[t0]=sum(e0_baselineCH4emissions[1,]);
  e_regionalCH4emissions[t0,r]=e0_baselineCH4emissions[r];
}
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    er_CH4emissionsgrowth[t,r]=Best_Guess(datalist[[13]][t,r+1]);
  }
}
CH4_emissions<-function(){
  t=clock_Current(year_current);
  for(r in 1:Region_No){
    e_regionalCH4emissions[t,r]<<-er_CH4emissionsgrowth[t,r]*e0_baselineCH4emissions[r]/100;
  }
  e_globalCH4emissions[t]<<-sum(e_regionalCH4emissions[t,]);
}

# CH4 cycle
pic_preindustconcCH4 = 700.
den_CH4density = 2.78
stim_CH4emissionfeedback= 0.
air_CH4fractioninatm = 100.
res_CH4atmlifetime = 10.5
c0_CH4concbaseyr = 1860.
rtl_g0_baselandtemp = 0.9258270139190647
e_0globalCH4emissions = 363.00000000000006
nte_natCH4emissions=matrix(nrow =TimeStep,ncol = 1 );
re_remainCH4=matrix(nrow =TimeStep,ncol = 1 );
nte_natCH4emissions=matrix(nrow =TimeStep,ncol = 1 );
tea_CH4emissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
teay_CH4emissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
rtl_g_landtemperature=matrix(nrow =TimeStep,ncol = 1 );
CH4_cycle<-function(){
  t=clock_Current(year_current);
  if (t==1){
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    nte_0<<-stim_CH4emissionfeedback*rtl_g0_baselandtemp;
    nte_natCH4emissions[t]<<-stim_CH4emissionfeedback*rtl_g0_baselandtemp;
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_CH4emissionstoatm[t]<<-(e_globalCH4emissions[t]+nte_natCH4emissions[t])*air_CH4fractioninatm/100
    tea_0=(e_0globalCH4emissions+nte_0)*air_CH4fractioninatm/100
    teay_CH4emissionstoatm[t]<<-(tea_CH4emissionstoatm[t]+tea_0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcCH4<<-c0_CH4concbaseyr-pic_preindustconcCH4
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainCH4base<<-exc_excessconcCH4*den_CH4density
    re_remainCH4[t]<<-re_remainCH4base*exp(-(y_year[t]-y_year_0)/res_CH4atmlifetime)+
      teay_CH4emissionstoatm[t]*res_CH4atmlifetime*(1-exp(-(y_year[t]-y_year_0)/res_CH4atmlifetime))/(y_year[t]-y_year_0)
  }
  else{
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    #Here assume still using area-weighted average regional temperatures (i.e. land temperatures) for natural emissions feedback
    nte_natCH4emissions[t]<<-stim_CH4emissionfeedback*rtl_g_landtemperature[t-1] #askChrisHope
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_CH4emissionstoatm[t]<<-(e_globalCH4emissions[t]+nte_natCH4emissions[t])*air_CH4fractioninatm/100
    #eq.7 from Hope (2006) - average emissions to atm over time period
    teay_CH4emissionstoatm[t]<<-(tea_CH4emissionstoatm[t]+tea_CH4emissionstoatm[t-1])*(y_year[t]-y_year[t-1])/2
    #eq.10 from Hope (2006) - remaining emissions in atmosphere
    re_remainCH4[t]<<-re_remainCH4[t-1]*exp(-(y_year[t]-y_year[t-1])/res_CH4atmlifetime)+
      teay_CH4emissionstoatm[t]*res_CH4atmlifetime*(1-exp(-(y_year[t]-y_year[t-1])/res_CH4atmlifetime))/(y_year[t]-y_year[t-1])
    
  }
  #eq.11 from Hope(2006) - CH4 concentration
  c_CH4concentration[t]<<-pic_preindustconcCH4+exc_excessconcCH4*re_remainCH4[t]/re_remainCH4base;
}

#CH4 forcing
fslope_CH4forcingslope = 0.036
f0_CH4baseforcing = 0.550
c0_baseN2Oconc = 322.
c0_baseCH4conc = 1860.
c_N2Oconcentration = matrix(nrow =TimeStep,ncol = 1 );
c_CH4concentration=matrix(nrow =TimeStep,ncol = 1 );
f_CH4forcing=matrix(nrow =TimeStep,ncol = 1 );
over=matrix(nrow =TimeStep,ncol = 1 );
#CH4forcing=matrix(nrow =TimeStep,ncol = 1 );
CH4_forcing<-function(){
  t=clock_Current(year_current);
  # if (t==1){
  #calculate baseline forcing overlap in first time period
  over_baseoverlap<<- -0.47*log(1+2.0e-5*(c0_baseN2Oconc*c0_baseCH4conc)^0.75+5.3e-15*c0_baseCH4conc*(c0_baseCH4conc*c0_baseN2Oconc)^1.52)
  # }
  
  over[t]<<- -0.47*log(1+2.0e-5*(c_CH4concentration[t]*c0_baseN2Oconc)^0.75+5.3e-15*c_CH4concentration[t]*(c0_baseN2Oconc*c_CH4concentration[t])^1.52)
  f_CH4forcing[t]<<-f0_CH4baseforcing+fslope_CH4forcingslope*(sqrt(c_CH4concentration[t])-sqrt(c0_baseCH4conc))+over[t]-over_baseoverlap;
  
}

#N2O emissions
e_globalN2Oemissions=matrix(nrow =TimeStep,ncol = 1 );
e0_baselineN2Oemissions=matrix(nrow =1,ncol = Region_No );
e_regionalN2Oemissions=matrix(nrow =TimeStep,ncol = Region_No );
er_N2Oemissionsgrowth=matrix(nrow =TimeStep,ncol = Region_No );
for(r in 1:Region_No){
  e0_baselineN2Oemissions[1,r]=Best_Guess(datalist[[11]]$e_n2o_0[r]);
  e_globalN2Oemissions[t0,1]=sum(e0_baselineN2Oemissions[1,]);
  e_regionalN2Oemissions[t0,r]=e0_baselineN2Oemissions[1,r];
}
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    er_N2Oemissionsgrowth[t,r]=Best_Guess(datalist[[16]][t,r+1]);
  }
}
N2O_emissions<-function(){
  t=clock_Current(year_current);
  for(r in 1:Region_No){
    e_regionalN2Oemissions[t,r]<<-er_N2Oemissionsgrowth[t,r]*e0_baselineN2Oemissions[r]/100;
  }
  e_globalN2Oemissions[t]<<-sum(e_regionalN2Oemissions[t,]);
}

# N2O cycle
pic_preindustconcN2O = 270.
den_N2Odensity = 7.8
stim_N2Oemissionfeedback = 0.
air_N2Ofractioninatm = 100.
res_N2Oatmlifetime = 114.
c0_N2Oconcbaseyr = 322.
rtl_g0_baselandtemp = 0.9258270139190647
e_0globalN2Oemissions = 11.046520000000001
re_remainN2O=matrix(nrow =TimeStep,ncol = 1 );
nte_natN2Oemissions=matrix(nrow =TimeStep,ncol = 1 );
tea_N2Oemissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
teay_N2Oemissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
#rtl_g_landtemperature=matrix(nrow =TimeStep,ncol = 1 );

N2O_cycle<-function(){
  t=clock_Current(year_current )
  if (t==1){
    #eq.3 from Hope (2006) - natural emissions feedback, using global temperatures calculated in ClimateTemperature component
    nte_0<<-stim_N2Oemissionfeedback*rtl_g0_baselandtemp
    nte_natN2Oemissions[t]<<-stim_N2Oemissionfeedback*rtl_g0_baselandtemp
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_N2Oemissionstoatm[t]<<-(e_globalN2Oemissions[t]+nte_natN2Oemissions[t])*air_N2Ofractioninatm/100
    tea_0<<-(e_0globalN2Oemissions+nte_0)*air_N2Ofractioninatm/100
    teay_N2Oemissionstoatm[t]<<-(tea_N2Oemissionstoatm[t]+tea_0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcN2O<<-c0_N2Oconcbaseyr-pic_preindustconcN2O
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainN2Obase<<-exc_excessconcN2O*den_N2Odensity
    re_remainN2O[t]<<-re_remainN2Obase*exp(-(y_year[t]-y_year_0)/res_N2Oatmlifetime)+
      teay_N2Oemissionstoatm[t]*res_N2Oatmlifetime*(1-exp(-(y_year[t]-y_year_0)/res_N2Oatmlifetime))/(y_year[t]-y_year_0)
  }
  else{
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    #Here assume still using area-weighted average regional temperatures (i.e. land temperatures) for natural emissions feedback
    nte_natN2Oemissions[t]<<-stim_N2Oemissionfeedback*rtl_g_landtemperature[t-1]
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_N2Oemissionstoatm[t]<<-(e_globalN2Oemissions[t]+nte_natN2Oemissions[t])*air_N2Ofractioninatm/100
    #eq.7 from Hope (2006) - average emissions to atm over time period
    teay_N2Oemissionstoatm[t]<<-(tea_N2Oemissionstoatm[t]+tea_N2Oemissionstoatm[t-1])*(y_year[t]-y_year[t-1])/2
    #eq.10 from Hope (2006) - remaining emissions in atmosphere
    re_remainN2O[t]<<-re_remainN2O[t-1]*exp(-(y_year[t]-y_year[t-1])/res_N2Oatmlifetime)+
      teay_N2Oemissionstoatm[t]*res_N2Oatmlifetime*(1-exp(-(y_year[t]-y_year[t-1])/res_N2Oatmlifetime))/(y_year[t]-y_year[t-1])
  }
  
  #eq.11 from Hope(2006) - N2O concentration
  c_N2Oconcentration[t]<<-pic_preindustconcN2O+exc_excessconcN2O*re_remainN2O[t]/re_remainN2Obase
  
}

#N2O forcing
fslope_N2Oforcingslope = 0.12
f0_N2Obaseforcing = 0.180
c0_baseN2Oconc = 322.
c0_baseCH4conc = 1860.
f_N2Oforcing=matrix(nrow =TimeStep,ncol = 1 );
N2O_forcing<-function(){
  t=clock_Current(year_current)
  # if (t==1){
  #calculate baseline forcing overlap in first time period
  
  #calculate baseline forcing overlap in first time period
  over_baseoverlap= -0.47*log(1+2.01e-5*(c0_baseN2Oconc*c0_baseCH4conc)^0.75+5.31e-15*c0_baseCH4conc*(c0_baseCH4conc*c0_baseN2Oconc)^1.52)
  
  
  over[t]= -0.47*log(1+2.01e-5*(c0_baseCH4conc*c_N2Oconcentration[t])^0.75+5.31e-15*c0_baseCH4conc*(c0_baseCH4conc*c_N2Oconcentration[t])^1.52)
  f_N2Oforcing[t]<<- f0_N2Obaseforcing+fslope_N2Oforcingslope*(sqrt(c_N2Oconcentration[t])-sqrt(c0_baseN2Oconc))+over[t]-over_baseoverlap
  
  
}

#LG emissions
e_globalLGemissions=matrix(nrow =TimeStep,ncol = 1 );
e0_baselineLGemissions=matrix(nrow =1,ncol = Region_No );
e_regionalLGemissions=matrix(nrow =TimeStep,ncol = Region_No );
er_LGemissionsgrowth=matrix(nrow =TimeStep,ncol = Region_No );
for(r in 1:Region_No){
  e0_baselineLGemissions[1,r]=Best_Guess(datalist[[10]]$e_glin_0[r]);
  e_globalLGemissions[t0,1]=sum(e0_baselineN2Oemissions[1,]);
  e_regionalLGemissions[t0,r]=e0_baselineN2Oemissions[1,r];
}
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    er_LGemissionsgrowth[t,r]=Best_Guess(datalist[[15]][t,r+1]);
  }
}
LG_emissions<-function(){
  t=clock_Current(year_current);
  for(r in 1:Region_No){
    e_regionalLGemissions[t,r]<<-er_LGemissionsgrowth[t,r]*e0_baselineLGemissions[r]/100;
  }
  e_globalLGemissions[t]<<-sum(e_regionalLGemissions[t,]);
}

#LG cycle
pic_preindustconcLG = 0.
den_LGdensity = 100000.
stim_LGemissionfeedback = 0.
air_LGfractioninatm = 100.
res_LGatmlifetime = 1000.
c0_LGconcbaseyr = 0.11
rtl_g0_baselandtemp = 0.9258270139190647
e_0globalLGemissions = 557.2112715473608
c_LGconcentration=matrix(nrow =TimeStep,ncol = 1 );
re_remainLG=matrix(nrow =TimeStep,ncol = 1 );
nte_natLGemissions=matrix(nrow =TimeStep,ncol = 1 );
tea_LGemissionstoatm=matrix(nrow =TimeStep,ncol = 1 );
teay_LGemissionstoatm=matrix(nrow =TimeStep,ncol = 1 );

LG_cycle<-function(){
  t=clock_Current(year_current)
  if (t==1){
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    nte0<<- stim_LGemissionfeedback* rtl_g0_baselandtemp
    nte_natLGemissions[t]<<- stim_LGemissionfeedback* rtl_g0_baselandtemp
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea0<<-( e_0globalLGemissions+nte0)* air_LGfractioninatm/100
    tea_LGemissionstoatm[t]<<-( e_globalLGemissions[t]+ nte_natLGemissions[t])* air_LGfractioninatm/100
    teay_LGemissionstoatm[t]<<-( tea_LGemissionstoatm[t]+tea0)/2
    #adapted from eq.1 in Hope(2006) - calculate excess concentration in base year
    exc_excessconcLG<<- c0_LGconcbaseyr- pic_preindustconcLG
    #Eq. 2 from Hope (2006) - base-year remaining emissions
    re_remainLGbase<<- exc_excessconcLG* den_LGdensity
    re_remainLG[t]<<- re_remainLGbase*exp(-(  y_year[t]- y_year_0)/ res_LGatmlifetime)+
      teay_LGemissionstoatm[t]* res_LGatmlifetime*(1-exp(-(  y_year[t]- y_year_0)/ res_LGatmlifetime))/(  y_year[t]- y_year_0)
  }
  else{
    #eq.3 from Hope (2006) - natural emissions (carbon cycle) feedback, using global temperatures calculated in ClimateTemperature component
    #Here assume still using area-weighted average regional temperatures (i.e. land temperatures) for natural emissions feedback
    nte_natLGemissions[t]<<- stim_LGemissionfeedback* rtl_g_landtemperature[t-1]
    #eq.6 from Hope (2006) - emissions to atmosphere depend on the sum of natural and anthropogenic emissions
    tea_LGemissionstoatm[t]<<-( e_globalLGemissions[t]+ nte_natLGemissions[t])* air_LGfractioninatm/100
    #eq.7 from Hope (2006) - average emissions to atm over time period
    teay_LGemissionstoatm[t]<<-( tea_LGemissionstoatm[t]+ tea_LGemissionstoatm[t-1])*(  y_year[t]-  y_year[t-1])/2
    #eq.10 from Hope (2006) - remaining emissions in atmosphere
    re_remainLG[t]<<- re_remainLG[t-1]*exp(-(  y_year[t]-  y_year[t-1])/ res_LGatmlifetime)+
      teay_LGemissionstoatm[t]* res_LGatmlifetime*(1-exp(-(  y_year[t]-  y_year[t-1])/ res_LGatmlifetime))/(  y_year[t]-  y_year[t-1])
    end
  }
  #eq.11 from Hope(2006) - LG concentration
  c_LGconcentration[t]<<- pic_preindustconcLG+ exc_excessconcLG* re_remainLG[t]/ re_remainLGbase
}

# LG forcing
f0_LGforcingbase = 0.022
fslope_LGforcingslope = 0.2
c0_LGconcbaseyr = 0.11;
f_LGforcing=matrix(nrow =TimeStep,ncol = 1 );

LG_forcing<-function(){
  t=clock_Current(year_current );
  f_LGforcing[t]<<-f0_LGforcingbase+fslope_LGforcingslope*(c_LGconcentration[t]-c0_LGconcbaseyr);
}

#SulphateForcing
d_sulphateforcingbase = Best_Guess(datalist[[113]]$Value)
ind_slopeSEforcing_indirect = Best_Guess(datalist[[120]]$Value)
se0_sulphateemissionsbase=matrix(nrow =1,ncol = Region_No );
pse_sulphatevsbase=matrix(nrow =TimeStep,ncol = Region_No );
se_sulphateemissions=matrix(nrow =TimeStep,ncol = Region_No );
area=matrix(nrow =1,ncol = Region_No );
sfx_sulphateflux=matrix(nrow =TimeStep,ncol = Region_No );
nf_naturalsfx=matrix(nrow =1,ncol = Region_No );
fs_sulphateforcing=matrix(nrow =TimeStep,ncol = Region_No );

for(r in 1:Region_No){
  se0_sulphateemissionsbase[1,r]=Best_Guess(datalist[[45]]$se_0[r]);
  area[1,r]=Best_Guess(datalist[[1]]$area[r]);
  nf_naturalsfx[1,r]=Best_Guess(datalist[[32]]$Natural.S[r]);
} 
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    pse_sulphatevsbase[t,r]=Best_Guess(datalist[[37]][t,r+1]);
  }
}
SulphateForcing<-function(){
  bigSFX0 <<- se0_sulphateemissionsbase /area;
  t=clock_Current(year_current);
  for(r in 1:Region_No){
    se_sulphateemissions[t, r] <<-  se0_sulphateemissionsbase[r] *  pse_sulphatevsbase[t, r] / 100
    
    # Eq.17 from Hope (2006) - sulfate flux
    sfx_sulphateflux[t,r] <<- se_sulphateemissions[t,r] /  area[r]
    # Update for Eq. 18 from Hope (2009) - sulfate radiative forcing effect
    bigSFD0 <<- d_sulphateforcingbase * bigSFX0[r] / (sum(bigSFX0 *  area) / sum( area))
    fsd_term <<- bigSFD0 * sfx_sulphateflux[t,r] / bigSFX0[r]
    fsi_term <<-  ind_slopeSEforcing_indirect/log(2) * log(( nf_naturalsfx[r] + sfx_sulphateflux[t, r]) /  nf_naturalsfx[r])
    
    fs_sulphateforcing[t, r] <<- fsd_term + fsi_term;
  }
}

# total forcing
f_lineargasforcing=matrix(nrow =TimeStep,ncol = 1 );
exf_excessforcing=matrix(nrow =TimeStep,ncol = 1 );
ft_totalforcing=matrix(nrow =TimeStep,ncol = 1 );
for(t in 1:TimeStep){
  exf_excessforcing[t]=Best_Guess(datalist[[17]]$exf_a[t]);
}
Total_forcing<-function(){
  # ft_totalforcing[t] =  f_CO2forcing[t] +  f_CH4forcing[t] +  f_N2Oforcing[t] +  f_lineargasforcing[t] +  exf_excessforcing[t]
  ft_totalforcing[t] <<- f_CO2forcing[t] + f_CH4forcing[t] + f_N2Oforcing[t] + f_LGforcing[t] + exf_excessforcing[t]
  
}

# climate temperature
rlo_ratiolandocean = Best_Guess(datalist[[137]]$Value)
pole_polardifference = Best_Guess(datalist[[128]]$Value)
lat_g_meanlatitude =  30.21989459076828
fslope_CO2forcingslope = 5.5
tcr_transientresponse = Best_Guess(datalist[[146]]$Value)
frt_warminghalflife = Best_Guess(datalist[[117]]$Value)
et_equilibriumtemperature=matrix(nrow =TimeStep,ncol = Region_No );
rt_realizedtemperature=matrix(nrow =TimeStep,ncol = Region_No );
lat_latitude=matrix(nrow =1,ncol = Region_No );
rtl_0_realizedtemperature=matrix(nrow =1,ncol = Region_No );
rtl_realizedtemperature=matrix(nrow =TimeStep,ncol = Region_No );
rtl_g_landtemperature=matrix(nrow =TimeStep,ncol = 1 );
rto_g_oceantemperature=matrix(nrow =TimeStep,ncol = 1 );
rt_g_globaltemperature=matrix(nrow =TimeStep,ncol = 1 );
rt_0_realizedtemperature=matrix(nrow =1,ncol = Region_No );
rt_adj_temperatureadjustment=matrix(nrow =1,ncol = Region_No );
for(r in 1:Region_No){
  rtl_0_realizedtemperature[r]=Best_Guess(datalist[[44]]$rtl_0[r])
  lat_latitude[r]=Best_Guess(datalist[[31]]$Latitude[r])
}
ocean_prop_ortion = 1. - sum( area) / 510000000.

# Equation 21 from Hope (2006): initial global land temperature
rtl_g0_baselandtemp = sum( rtl_0_realizedtemperature *  area) / sum( area)
#待确认
# initial ocean and global temperatures
rto_g0_baseoceantemp =  rtl_g0_baselandtemp/ rlo_ratiolandocean
rt_g0_baseglobaltemp = ocean_prop_ortion * rto_g0_baseoceantemp + (1. - ocean_prop_ortion) *  rtl_g0_baselandtemp;
ClimateTemperature<-function(){
  t=clock_Current(year_current )
  if (t == 1) {    # only calculate once
    sens_climatesensitivity<<-tcr_transientresponse / (1. - ( frt_warminghalflife / 70.) * (1. - exp(-70. /  frt_warminghalflife)))
  }
  
  ## Adjustment for latitude and land
  ocean_prop_ortion <<- 1. - (sum( area) / 510000000.)
  for(r in 1:Region_No){
    rt_adj_temperatureadjustment[r]  <<- ( pole_polardifference / 90.) * (abs( lat_latitude[r]) -  lat_g_meanlatitude)
  }
  ## Unadjusted realized temperature
  
  # Equation 19 from Hope (2006): equilibrium temperature estimate
  for (r in 1:Region_No){
    #et_equilibriumtemperature[t, r] = ( sens_climatesensitivity / log(2.0)) * ( ft_totalforcing[t] +  fs_sulfateforcing[t, r]) /  fslope_CO2forcingslope
    et_equilibriumtemperature[t, r] <<- ( sens_climatesensitivity / log(2.0)) * ( ft_totalforcing[t] +  fs_sulphateforcing[t, r]) /  fslope_CO2forcingslope
    
  }
  # Equation 20 from Hope (2006): realized temperature estimate
  # Hope (2009) replaced OCEAN with FRT
  if (t == 1){
    # Calculate baseline realized temperature by subtracting off adjustment
    
    for (r in 1:Region_No){
      rt_0_realizedtemperature[r]  <<-  ( rtl_0_realizedtemperature[r] - rt_adj_temperatureadjustment[r]) * (1. + (ocean_prop_ortion /  rlo_ratiolandocean) - ocean_prop_ortion)
      rt_realizedtemperature[t, r] <<- rt_0_realizedtemperature[r] + (1 - exp(-(  y_year[t] -  y_year_0) /  frt_warminghalflife)) * ( et_equilibriumtemperature[t, r] - rt_0_realizedtemperature[r])
    }
  }
  else{
    for (r in 1:Region_No){
      rt_realizedtemperature[t, r] <<-  rt_realizedtemperature[t-1, r] + (1 - exp(-(  y_year[t] -   y_year[t-1]) /  frt_warminghalflife)) * ( et_equilibriumtemperature[t, r] -  rt_realizedtemperature[t-1, r])
    }
  }
  
  ## Adjusted realized temperature
  
  # Adding adjustment, from Hope (2009)
  for (r in 1:Region_No){
    rtl_realizedtemperature[t, r] <<-  rt_realizedtemperature[t, r] / (1. + (ocean_prop_ortion /  rlo_ratiolandocean) - ocean_prop_ortion) + rt_adj_temperatureadjustment[r]
  }
  
  # Equation 21 from Hope (2006): global realized temperature estimate
  rtl_g_landtemperature[t] <<- sum( rtl_realizedtemperature[t, ]*  area) / sum( area)
  
  # Ocean and global average temperature from Hope (2009)
  rto_g_oceantemperature[t] <<-  rtl_g_landtemperature[t] /  rlo_ratiolandocean
  rt_g_globaltemperature[t] <<- ocean_prop_ortion *  rto_g_oceantemperature[t] + (1. - ocean_prop_ortion) *  rtl_g_landtemperature[t]
  # rt_g_globaltemperature[t]<<-rt_g_globaltemperature[t]*0.5
  # rtl_realizedtemperature[t,]<<-rtl_realizedtemperature[t,]*0.5
}

# Sea Level rise
sltemp_SLtemprise = Best_Guess(datalist[[143]]$Value)
sla_SLbaselinerise = Best_Guess(datalist[[141]]$Value)
sltau_SLresponsetime = Best_Guess(datalist[[142]]$Value)
s0_initialSL = Best_Guess(datalist[[138]]$Value)
es_equilibriumSL=matrix(nrow =TimeStep,ncol = 1 );
s_sealevel=matrix(nrow =TimeStep,ncol = 1 );
expfs_exponential=matrix(nrow =TimeStep,ncol = 1 );
yp_timestep=matrix(nrow =TimeStep,ncol = 1 );
Sea_level_rise<-function(){
  t=clock_Current(year_current)
  if(t==1){
    yp_timestep[t]<<-y_year[1] - y_year_0
    es_equilibriumSL[t]<<-sltemp_SLtemprise*rt_g_globaltemperature[t] + sla_SLbaselinerise
    expfs_exponential[t]<<-exp(-yp_timestep[t]/sltau_SLresponsetime)
    s_sealevel[t]<<-s0_initialSL + (es_equilibriumSL[t] - s0_initialSL)*(1-expfs_exponential[t])
  }
  else{
    yp_timestep[t]<<-y_year[t] - y_year[t-1]
    es_equilibriumSL[t]<<-sltemp_SLtemprise*rt_g_globaltemperature[t] + sla_SLbaselinerise
    expfs_exponential[t]<<-exp(-yp_timestep[t]/sltau_SLresponsetime)
    s_sealevel[t]<<-s_sealevel[t-1] + (es_equilibriumSL[t] -s_sealevel[t-1])*(1-expfs_exponential[t])
  }
}

# GDP
save_savingsrate = Best_Guess(datalist[[139]]$Value) #pp33 PAGE09 documentation, "savings rate".
isat0_initialimpactfxnsaturation =  Best_Guess(datalist[[125]]$Value) #pp34 PAGE09 documentation
gdp=matrix(nrow =TimeStep,ncol = Region_No );
cons_consumption=matrix(nrow =TimeStep,ncol = Region_No );
cons_percap_consumption=matrix(nrow =TimeStep,ncol = Region_No );
cons_percap_consumption_0=matrix(nrow =1,ncol = Region_No );
yagg_periodspan=matrix(nrow =TimeStep,ncol = 1 );
grw_gdpgrowthrate=matrix(nrow =TimeStep,ncol = Region_No );
popgrw_populationgrowth=matrix(nrow =TimeStep,ncol = Region_No );
gdp_0=matrix(nrow =1,ncol = Region_No );
pop0_initpopulation=matrix(nrow =1,ncol = Region_No );
pop_population=matrix(nrow =TimeStep,ncol = Region_No );
isatg_impactfxnsaturation = isat0_initialimpactfxnsaturation * (1 - save_savingsrate/100);
for (r in 1:Region_No){
  gdp_0[r]=Best_Guess(datalist[[18]]$gdp_0[r]);
  pop0_initpopulation[r]=Best_Guess(datalist[[35]]$pop_0[r]);
  cons_percap_consumption_0[r] = (gdp_0[r] / pop0_initpopulation[r])*(1 - save_savingsrate / 100)
}
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    grw_gdpgrowthrate[t,r]=Best_Guess(datalist[[19]][t,r+1]);
    popgrw_populationgrowth[t,r]=Best_Guess(datalist[[36]][t,r+1]);
    #pop_population[t,r]=Best_Guess(datalist[[61]][t,r+1]);
  }
}

GDP<-function(){
  t=clock_Current(year_current);
  if (t == 1){
    ylo_periodstart <<- y_year_0
  }
  else{
    ylo_periodstart <<- (y_year[t] + y_year[t-1]) / 2
  }
  
  if (t == length(y_year)){
    yhi_periodend <<- y_year[t]
  }
  else{
    yhi_periodend <<- (y_year[t] + y_year[t+1]) / 2
  }
  
  yagg_periodspan[t] <<- yhi_periodend- ylo_periodstart
  
  for (r in 1:Region_No){
    #eq.28 in Hope 2002
    if (t == 1){
      gdp[t, r] <<- gdp_0[r] * (1 + (grw_gdpgrowthrate[t,r]/100))^(y_year[t] - y_year_0)
    }
    else{
      gdp[t, r] <<- gdp[t-1, r] * (1 + (grw_gdpgrowthrate[t,r]/100))^(y_year[t] - y_year[t-1])
    }
    cons_consumption[t, r] <<- gdp[t, r] * (1 - save_savingsrate / 100)
    cons_percap_consumption[t, r] <<- cons_consumption[t, r] / pop_population[t, r]
  }
}

# Market damage
tcal_CalibrationTemp= Best_Guess(datalist[[145]]$Value);
iben_MarketInitialBenefit = Best_Guess(datalist[[118]]$Value)
ipow_MarketIncomeFxnExponent = Best_Guess(datalist[[122]]$Value)
#save_savingsrate= Best_Guess(datalist[[139]]$Value)
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
pow_MarketImpactExponent=Best_Guess(datalist[[129]]$Value);
W_MarketImpactsatCalibrationTemp = Best_Guess(datalist[[148]]$Value);
atl_adjustedtolerableleveloftemprise=matrix(nrow =TimeStep,ncol = Region_No );
imp_actualreduction=matrix(nrow =TimeStep,ncol = Region_No );
i_regionalimpact=matrix(nrow =TimeStep,ncol = Region_No );
rcons_per_cap_SLRRemainConsumption=matrix(nrow =TimeStep,ncol = Region_No );
rgdp_per_cap_SLRRemainGDP=matrix(nrow =TimeStep,ncol = Region_No );
WINCF_weightsfactor=matrix(nrow =1,ncol = Region_No );
rcons_per_cap_MarketRemainConsumption=matrix(nrow =TimeStep,ncol = Region_No );
rgdp_per_cap_MarketRemainGDP=matrix(nrow =TimeStep,ncol = Region_No );
#ref_ImpactatReferenceGDPperCap=matrix(nrow =TimeStep,ncol = Region_No );
igdp_ImpactatActualGDPperCap=matrix(nrow =TimeStep,ncol = Region_No );
iref_ImpactatReferenceGDPperCap=matrix(nrow =TimeStep,ncol = Region_No );
impmax_maxtempriseforadaptpolicyM=matrix(nrow =1,ncol = Region_No );
isat_ImpactinclSaturationandAdaptation_market=matrix(nrow =TimeStep,ncol = Region_No );
isat_per_cap_ImpactperCapinclSaturationandAdaptation=matrix(nrow =TimeStep,ncol = Region_No );
i_regionalimpact_market=matrix(nrow =TimeStep,ncol = Region_No );

for(r in 1:Region_No){
  WINCF_weightsfactor[r]=Best_Guess(datalist[[152]][2,r]);
  impmax_maxtempriseforadaptpolicyM[r]=Best_Guess(datalist[[21]]$impmax_1_a[r]);
}
# for(t in 1:TimeStep){
#   for(r in 1:Region_No){
#     rgdp_per_cap_SLRRemainGDP[t,r]=Best_Guess(datalist[[58]][t,r+1]);
#     rcons_per_cap_SLRRemainConsumption[t,r]=Best_Guess(datalist[[64]][t,r+1]);
#   }
# }
Market_damage<-function(){
  t=clock_Current(year_current);
  for (r in 1:Region_No){
    #calculate tolerability
    imp_actualreduction[t,r]<<-Best_Guess(datalist[[59]][t,r+1]);
    
    atl_adjustedtolerableleveloftemprise[t,r]<<-Best_Guess(datalist[[54]][t,r+1]);
    if ((rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r]) < 0){
      i_regionalimpact[t,r] <<- 0
    }
    else{
      i_regionalimpact[t,r] <<- rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r]
    }
    
    iref_ImpactatReferenceGDPperCap[t,r]<<- WINCF_weightsfactor[r]*((W_MarketImpactsatCalibrationTemp + iben_MarketInitialBenefit * tcal_CalibrationTemp)*
                                                                      (i_regionalimpact[t,r]/tcal_CalibrationTemp)^pow_MarketImpactExponent - i_regionalimpact[t,r] * iben_MarketInitialBenefit)
    
    igdp_ImpactatActualGDPperCap[t,r]<<- iref_ImpactatReferenceGDPperCap[t,r]*
      (rgdp_per_cap_SLRRemainGDP[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_MarketIncomeFxnExponent
    
    if (igdp_ImpactatActualGDPperCap[t,r] < isatg_impactfxnsaturation){
      isat_ImpactinclSaturationandAdaptation_market[t,r] <<- igdp_ImpactatActualGDPperCap[t,r]
    }
    else{
      isat_ImpactinclSaturationandAdaptation_market[t,r] <<- isatg_impactfxnsaturation+
        ((100-save_savingsrate)-isatg_impactfxnsaturation)*
        ((igdp_ImpactatActualGDPperCap[t,r]-isatg_impactfxnsaturation)/
           (((100-save_savingsrate)-isatg_impactfxnsaturation)+
              (igdp_ImpactatActualGDPperCap[t,r]-
                 isatg_impactfxnsaturation)))
    }
    
    if (i_regionalimpact[t,r] < impmax_maxtempriseforadaptpolicyM[r]){
      isat_ImpactinclSaturationandAdaptation_market[t,r]<<-isat_ImpactinclSaturationandAdaptation_market[t,r]*(1-imp_actualreduction[t,r]/100)
    }
    else{
      isat_ImpactinclSaturationandAdaptation_market[t,r] <<- isat_ImpactinclSaturationandAdaptation_market[t,r] *
        (1-(imp_actualreduction[t,r]/100)* impmax_maxtempriseforadaptpolicyM[r] /
           i_regionalimpact[t,r])
    }
    i_regionalimpact_market[t,r]<<-i_regionalimpact[t,r];
    
    isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r] <<- (isat_ImpactinclSaturationandAdaptation_market[t,r]/100)*rgdp_per_cap_SLRRemainGDP[t,r]
    rcons_per_cap_MarketRemainConsumption[t,r] <<- rcons_per_cap_SLRRemainConsumption[t,r] - isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r]
    rgdp_per_cap_MarketRemainGDP[t,r] <<- rcons_per_cap_MarketRemainConsumption[t,r]/(1-save_savingsrate/100)
  }
}

# non_market damage
#tcal_CalibrationTemp= Best_Guess(datalist[[145]]$Value);
w_NonImpactsatCalibrationTemp = Best_Guess(datalist[[149]]$Value);
iben_NonMarketInitialBenefit = Best_Guess(datalist[[119]]$Value)
ipow_NonMarketIncomeFxnExponent = Best_Guess(datalist[[123]]$Value)
#save_savingsrate= 15.
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
pow_NonMarketExponent = Best_Guess(datalist[[130]]$Value)
impmax_maxtempriseforadaptpolicyNM=matrix(nrow =1,ncol = Region_No );
i_regionalimpact_nonmarket=matrix(nrow =TimeStep,ncol = Region_No );
isat_ImpactinclSaturationandAdaptation_nonmarket=matrix(nrow =TimeStep,ncol = Region_No );

for(r in 1:Region_No){
  impmax_maxtempriseforadaptpolicyNM[r]=Best_Guess(datalist[[22]]$impmax_2_a[r]);
}
rcons_per_cap_NonMarketRemainConsumption=matrix(nrow =TimeStep,ncol = Region_No );
rgdp_per_cap_NonMarketRemainGDP=matrix(nrow =TimeStep,ncol = Region_No );
# for(t in 1:TimeStep){
#   for(r in 1:Region_No){
#     #rcons_per_cap_NonMarketRemainConsumption[t,r]=Best_Guess(datalist[[63]][t,r+1]);
#   #  rgdp_per_cap_NonMarketRemainGDP[t,r]=Best_Guess(datalist[[57]][t,r+1]);
#   }
# }

non_market_damage<-function(){
  t=clock_Current(year_current);
  for (r in 1:Region_No){
    imp_actualreduction[t,r]<<-Best_Guess(datalist[[60]][t,r+1]);
    
    atl_adjustedtolerableleveloftemprise[t,r]<<-Best_Guess(datalist[[55]][t,r+1]);
    
    if (rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r] < 0){
      i_regionalimpact[t,r] <<- 0
    }
    else{
      i_regionalimpact[t,r] <<- rtl_realizedtemperature[t,r]-atl_adjustedtolerableleveloftemprise[t,r]
    }
    
    iref_ImpactatReferenceGDPperCap[t,r]<<- WINCF_weightsfactor[r]*
      ((w_NonImpactsatCalibrationTemp + iben_NonMarketInitialBenefit *tcal_CalibrationTemp)*
         (i_regionalimpact[t,r]/tcal_CalibrationTemp)^pow_NonMarketExponent - i_regionalimpact[t,r] * iben_NonMarketInitialBenefit)
    
    igdp_ImpactatActualGDPperCap[t,r]<<- iref_ImpactatReferenceGDPperCap[t,r]*
      (rgdp_per_cap_MarketRemainGDP[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_NonMarketIncomeFxnExponent
    
    if (igdp_ImpactatActualGDPperCap[t,r] < isatg_impactfxnsaturation){
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] <<- igdp_ImpactatActualGDPperCap[t,r]
    }
    else{
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] <<- isatg_impactfxnsaturation+
        ((100-save_savingsrate)-isatg_impactfxnsaturation)*
        ((igdp_ImpactatActualGDPperCap[t,r]-isatg_impactfxnsaturation)/
           (((100-save_savingsrate)-isatg_impactfxnsaturation)+
              (igdp_ImpactatActualGDPperCap[t,r]-
                 isatg_impactfxnsaturation)))
    }
    
    if (i_regionalimpact[t,r] < impmax_maxtempriseforadaptpolicyNM[r]){
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r]<<-isat_ImpactinclSaturationandAdaptation_nonmarket[t,r]*(1-imp_actualreduction[t,r]/100)
    }
    else{
      isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] <<- isat_ImpactinclSaturationandAdaptation_nonmarket[t,r] *
        (1-(imp_actualreduction[t,r]/100)* impmax_maxtempriseforadaptpolicyNM[r] /
           i_regionalimpact[t,r])
    }
    i_regionalimpact_nonmarket[t,r]<<-i_regionalimpact[t,r];
    
    isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r] <<- (isat_ImpactinclSaturationandAdaptation_nonmarket[t,r]/100)*rgdp_per_cap_MarketRemainGDP[t,r]
    rcons_per_cap_NonMarketRemainConsumption[t,r] <<- rcons_per_cap_MarketRemainConsumption[t,r] - isat_per_cap_ImpactperCapinclSaturationandAdaptation[t,r]
    rgdp_per_cap_NonMarketRemainGDP[t,r] <<- rcons_per_cap_NonMarketRemainConsumption[t,r]/(1-save_savingsrate/100)
  }
}

#Discontinuity

rand_discontinuity = runif(1,0,1);# rand_discontinuity为0-1均匀分布
wdis_gdplostdisc=Best_Guess(datalist[[151]]$Value);
ipow_incomeexponent=Best_Guess(datalist[[121]]$Value);
distau_discontinuityexponent=Best_Guess(datalist[[114]]$Value);
tdis_tolerabilitydisc=Best_Guess(datalist[[147]]$Value);
pdis_probability=Best_Guess(datalist[[127]]$Value);
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
isatg_saturationmodification=28.333333333333336;
irefeqdis_eqdiscimpact=matrix(nrow =1,ncol = Region_No );
igdpeqdis_eqdiscimpact=matrix(nrow =TimeStep,ncol = Region_No );
igdp_realizeddiscimpact=matrix(nrow =TimeStep,ncol = Region_No );
occurdis_occurrencedummy=matrix(nrow =TimeStep,ncol = 1 );
expfdis_discdecay=matrix(nrow =TimeStep,ncol = 1 );
idis_lossfromdisc=matrix(nrow =TimeStep,ncol = 1 );
isat_satdiscimpact=matrix(nrow =TimeStep,ncol = Region_No );
isat_per_cap_DiscImpactperCapinclSaturation=matrix(nrow =TimeStep,ncol = Region_No );
rcons_per_cap_DiscRemainConsumption=matrix(nrow =TimeStep,ncol = Region_No );

Discontinuity<-function(){
  t=clock_Current(year_current)
  idis_lossfromdisc[t] <<- max(0, rt_g_globaltemperature[t] - tdis_tolerabilitydisc)
  
  if (t == 1){
    if (idis_lossfromdisc[t]*(pdis_probability/100) > rand_discontinuity){
      occurdis_occurrencedummy[t] <<- 1
    }
    else{
      occurdis_occurrencedummy[t] <<- 0
    }
    expfdis_discdecay[t]<<-exp(-(y_year[t] - y_year_0)/distau_discontinuityexponent)
  }
  else{
    if (idis_lossfromdisc[t]*(pdis_probability/100) > rand_discontinuity){
      occurdis_occurrencedummy[t] <<- 1
    }
    else if(occurdis_occurrencedummy[t-1] == 1) {
      occurdis_occurrencedummy[t] <<- 1
    }
    else{
      occurdis_occurrencedummy[t] <<- 0
    }
    expfdis_discdecay[t]<<-exp(-(y_year[t] - y_year[t-1])/distau_discontinuityexponent)
  }
  
  for (r in 1:Region_No){
    irefeqdis_eqdiscimpact[r] <<- WINCF_weightsfactor[r]*wdis_gdplostdisc
    
    igdpeqdis_eqdiscimpact[t,r] <<- irefeqdis_eqdiscimpact[r] * (rgdp_per_cap_NonMarketRemainGDP[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_incomeexponent
    
    if (t==1){
      igdp_realizeddiscimpact[t,r]<<-occurdis_occurrencedummy[t]*(1-expfdis_discdecay[t])*igdpeqdis_eqdiscimpact[t,r]
    }
    else{
      igdp_realizeddiscimpact[t,r]<<-igdp_realizeddiscimpact[t-1,r]+occurdis_occurrencedummy[t]*(1-expfdis_discdecay[t])*(igdpeqdis_eqdiscimpact[t,r]-igdp_realizeddiscimpact[t-1,r])
    }
    
    if (igdp_realizeddiscimpact[t,r] < isatg_saturationmodification){
      isat_satdiscimpact[t,r] <<- igdp_realizeddiscimpact[t,r]
    }
    else{
      isat_satdiscimpact[t,r] <<- isatg_saturationmodification + (100-isatg_saturationmodification)*((igdp_realizeddiscimpact[t,r]-isatg_saturationmodification)/((100-isatg_saturationmodification)+(igdp_realizeddiscimpact[t,r] - isatg_saturationmodification)))
    }
    isat_per_cap_DiscImpactperCapinclSaturation[t,r] <<- (isat_satdiscimpact[t,r]/100)*rgdp_per_cap_NonMarketRemainGDP[t,r]
    rcons_per_cap_DiscRemainConsumption[t,r] <<- rcons_per_cap_NonMarketRemainConsumption[t,r] - isat_per_cap_DiscImpactperCapinclSaturation[t,r]
  }
}

#adaptation cost
automult_autonomouschange = Best_Guess(datalist[[102]]$Value)
cf_costregional=matrix(nrow =1,ncol = Region_No );
impmax_maximumadaptivecapacity=matrix(nrow =1,ncol = Region_No );
plateau_increaseintolerableplateaufromadaptation=matrix(nrow =1,ncol = Region_No );
pstart_startdateofadaptpolicy=matrix(nrow =1,ncol = Region_No );
pyears_yearstilfulleffect=matrix(nrow =1,ncol = Region_No );
impred_eventualpercentreduction=matrix(nrow =1,ncol = Region_No );
istart_startdate=matrix(nrow =1,ncol = Region_No );
iyears_yearstilfulleffect=matrix(nrow =1,ncol = Region_No );
atl_adjustedtolerablelevel=matrix(nrow =TimeStep,ncol = Region_No );
imp_adaptedimpacts=matrix(nrow =TimeStep,ncol = Region_No );
autofac_autonomouschangefraction=matrix(nrow =TimeStep,ncol = 1 );
acp_adaptivecostplateau=matrix(nrow =TimeStep,ncol = Region_No );
aci_adaptivecostimpact=matrix(nrow =TimeStep,ncol = Region_No );
ac_adaptivecosts=matrix(nrow =TimeStep,ncol = Region_No );

ac_adaptationcosts_economic=matrix(nrow =TimeStep,ncol = Region_No );
ac_adaptationcosts_noneconomic=matrix(nrow =TimeStep,ncol = Region_No );
ac_adaptationcosts_sealevelrise=matrix(nrow =TimeStep,ncol = Region_No );
for(r in 1:Region_No){
  cf_costregional[r]=Best_Guess(datalist[[107]][2,r]);
}

cp_costplateau_eu_SLR=Best_Guess(datalist[[100]]$Value);

ci_costimpact_eu_SLR=Best_Guess(datalist[[99]]$Value);

Adaptation_cost_sealevel<-function(){
  t=clock_Current(year_current );
  
  for(r in 1:Region_No){
    plateau_increaseintolerableplateaufromadaptation[r]<<-Best_Guess(datalist[[46]]$plateau_s_a[r]);
    pstart_startdateofadaptpolicy[r]<<-Best_Guess(datalist[[47]]$pstart_s_a[r]);
    pyears_yearstilfulleffect[r]<<-Best_Guess(datalist[[48]]$pyears_s_a[r]);
    impmax_maximumadaptivecapacity[r]<<-Best_Guess(datalist[[24]]$impmax_s_a[r]);
    impred_eventualpercentreduction[r]<<-Best_Guess(datalist[[49]]$impred_s_a[r]);
    istart_startdate[r]<<-Best_Guess(datalist[[50]]$istart_s_a[r]);
    iyears_yearstilfulleffect[r]<<-Best_Guess(datalist[[51]]$iyears_s_a[r]);
  }
  
  auto_autonomouschangepercent <<- (1 - automult_autonomouschange^(1/(y_year[TimeStep] - y_year_0)))*100 # % per year
  autofac_autonomouschangefraction[t] <<- (1 - auto_autonomouschangepercent/100)^(y_year[t] - y_year_0) # Varies by year

  for (r in 1:Region_No){
    #calculate adjusted tolerable level and max impact based on adaptation policy
    if ((y_year[t] - pstart_startdateofadaptpolicy[r]) < 0){
      atl_adjustedtolerablelevel[t,r]<<- 0
    }
    else if (((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r])<1.){
      atl_adjustedtolerablelevel[t,r]<<-
        ((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r]) *
        plateau_increaseintolerableplateaufromadaptation[r]
    }
    else{
      atl_adjustedtolerablelevel[t,r] <<- plateau_increaseintolerableplateaufromadaptation[r]
    }
    
    if ((y_year[t]- istart_startdate[r]) < 0){
      imp_adaptedimpacts[t,r] <<- 0
    }
    else if (((y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]) < 1){
      imp_adaptedimpacts[t,r] <<-
        (y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]*
        impred_eventualpercentreduction[r]
    }
    else{
      imp_adaptedimpacts[t,r] <<- impred_eventualpercentreduction[r]
    }
    
    # Hope (2009),  25, equations 1-2
    cp_costplateau_regional <<- cp_costplateau_eu_SLR * cf_costregional[r]
    ci_costimpact_regional <<- ci_costimpact_eu_SLR * cf_costregional[r]
    
    # Hope (2009),  25, equations 3-4
    acp_adaptivecostplateau[t, r] <<- atl_adjustedtolerablelevel[t, r] * cp_costplateau_regional * gdp[t, r] * autofac_autonomouschangefraction[t] / 100
    aci_adaptivecostimpact[t, r] <<- imp_adaptedimpacts[t, r] * ci_costimpact_regional * gdp[t, r] * impmax_maximumadaptivecapacity[r] * autofac_autonomouschangefraction[t] / 100
    
    # Hope (2009),  25, equation 5
    ac_adaptivecosts[t, r] <<- acp_adaptivecostplateau[t, r] + aci_adaptivecostimpact[t, r]
    ac_adaptationcosts_sealevelrise[t,r]<<- ac_adaptivecosts[t, r]
  }
  
}

cp_costplateau_eu_economic=Best_Guess(datalist[[96]]$Value);
ci_costimpact_eu_economic=Best_Guess(datalist[[95]]$Value);


Adaptation_cost_economic<-function(){
  t=clock_Current(year_current );
  
  for(r in 1:Region_No){
    impmax_maximumadaptivecapacity[r]<<-Best_Guess(datalist[[20]]$impmax_1_a[r]);
    plateau_increaseintolerableplateaufromadaptation[r]<<-Best_Guess(datalist[[33]]$plateau_1_a[r]);
    pstart_startdateofadaptpolicy[r]<<-Best_Guess(datalist[[38]]$pstart_1_a[r]);
    pyears_yearstilfulleffect[r]<<-Best_Guess(datalist[[40]]$pyears_1_a[r]);
    impred_eventualpercentreduction[r]<<-Best_Guess(datalist[[25]]$impred_1_a[r]);
    istart_startdate[r]<<-Best_Guess(datalist[[27]]$istart_1_a[r]);
    iyears_yearstilfulleffect[r]<<-Best_Guess(datalist[[29]]$iyears_1_a[r]);
  }
  auto_autonomouschangepercent <<- (1 - automult_autonomouschange^(1/(y_year[TimeStep] - y_year_0)))*100 # % per year
  autofac_autonomouschangefraction[t] <<- (1 - auto_autonomouschangepercent/100)^(y_year[t] - y_year_0) # Varies by year

  for (r in 1:Region_No){
    #calculate adjusted tolerable level and max impact based on adaptation policy
    if ((y_year[t] - pstart_startdateofadaptpolicy[r]) < 0){
      atl_adjustedtolerablelevel[t,r]<<- 0
    }
    else if (((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r])<1.){
      atl_adjustedtolerablelevel[t,r]<<-
        ((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r]) *
        plateau_increaseintolerableplateaufromadaptation[r]
    }
    else{
      atl_adjustedtolerablelevel[t,r] <<- plateau_increaseintolerableplateaufromadaptation[r]
    }
    
    if ((y_year[t]- istart_startdate[r]) < 0){
      imp_adaptedimpacts[t,r] <<- 0
    }
    else if (((y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]) < 1){
      imp_adaptedimpacts[t,r] <<-
        (y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]*
        impred_eventualpercentreduction[r]
    }
    else{
      imp_adaptedimpacts[t,r] <<- impred_eventualpercentreduction[r]
    }
    
    # Hope (2009),  25, equations 1-2
    cp_costplateau_regional <<- cp_costplateau_eu_economic * cf_costregional[r]
    ci_costimpact_regional <<- ci_costimpact_eu_economic * cf_costregional[r]
    
    # Hope (2009),  25, equations 3-4
    acp_adaptivecostplateau[t, r] <<- atl_adjustedtolerablelevel[t, r] * cp_costplateau_regional * gdp[t, r] * autofac_autonomouschangefraction[t] / 100
    aci_adaptivecostimpact[t, r] <<- imp_adaptedimpacts[t, r] * ci_costimpact_regional * gdp[t, r] * impmax_maximumadaptivecapacity[r] * autofac_autonomouschangefraction[t] / 100
    
    # Hope (2009),  25, equation 5
    ac_adaptivecosts[t, r] <<- acp_adaptivecostplateau[t, r] + aci_adaptivecostimpact[t, r]
    ac_adaptationcosts_economic[t,r]<<- ac_adaptivecosts[t, r]
    
  }
  
}

cp_costplateau_eu_noneconomic=Best_Guess(datalist[[98]]$Value);
ci_costimpact_eu_noneconomic=Best_Guess(datalist[[97]]$Value);

Adaptation_cost_noneconomic<-function(){
  t=clock_Current(year_current );
  for(r in 1:Region_No){
    impmax_maximumadaptivecapacity[r]<<-Best_Guess(datalist[[23]]$impmax_2_a[r]);
    plateau_increaseintolerableplateaufromadaptation[r]<<-Best_Guess(datalist[[34]]$plateau_2_a[r]);
    pstart_startdateofadaptpolicy[r]<<-Best_Guess(datalist[[39]]$pstart_2_a[r]);
    pyears_yearstilfulleffect[r]<<-Best_Guess(datalist[[41]]$pyears_2_a[r]);
    impred_eventualpercentreduction[r]<<-Best_Guess(datalist[[26]]$impred_2_a[r]);
    istart_startdate[r]<<-Best_Guess(datalist[[28]]$istart_2_a[r]);
    iyears_yearstilfulleffect[r]<<-Best_Guess(datalist[[30]]$iyears_2_a[r]);
  }
  auto_autonomouschangepercent <<- (1 - automult_autonomouschange^(1/(y_year[TimeStep] - y_year_0)))*100 # % per year
  autofac_autonomouschangefraction[t] <<- (1 - auto_autonomouschangepercent/100)^(y_year[t] - y_year_0) # Varies by year

  for (r in 1:Region_No){
    #calculate adjusted tolerable level and max impact based on adaptation policy
    if ((y_year[t] - pstart_startdateofadaptpolicy[r]) < 0){
      atl_adjustedtolerablelevel[t,r]<<- 0
    }
    else if (((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r])<1.){
      atl_adjustedtolerablelevel[t,r]<<-
        ((y_year[t]-pstart_startdateofadaptpolicy[r])/pyears_yearstilfulleffect[r]) *
        plateau_increaseintolerableplateaufromadaptation[r]
    }
    else{
      atl_adjustedtolerablelevel[t,r] <<- plateau_increaseintolerableplateaufromadaptation[r]
    }
    
    if ((y_year[t]- istart_startdate[r]) < 0){
      imp_adaptedimpacts[t,r] <<- 0
    }
    else if (((y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]) < 1){
      imp_adaptedimpacts[t,r] <<-
        (y_year[t]-istart_startdate[r])/iyears_yearstilfulleffect[r]*
        impred_eventualpercentreduction[r]
    }
    else{
      imp_adaptedimpacts[t,r] <<- impred_eventualpercentreduction[r]
    }
    
    # Hope (2009),  25, equations 1-2
    cp_costplateau_regional <<- cp_costplateau_eu_noneconomic * cf_costregional[r]
    ci_costimpact_regional <<- ci_costimpact_eu_noneconomic * cf_costregional[r]
    
    # Hope (2009),  25, equations 3-4
    acp_adaptivecostplateau[t, r] <<- atl_adjustedtolerablelevel[t, r] * cp_costplateau_regional * gdp[t, r] * autofac_autonomouschangefraction[t] / 100
    aci_adaptivecostimpact[t, r] <<- imp_adaptedimpacts[t, r] * ci_costimpact_regional * gdp[t, r] * impmax_maximumadaptivecapacity[r] * autofac_autonomouschangefraction[t] / 100
    
    # Hope (2009),  25, equation 5
    ac_adaptivecosts[t, r] <<- acp_adaptivecostplateau[t, r] + aci_adaptivecostimpact[t, r]
    ac_adaptationcosts_noneconomic[t,r]<<- ac_adaptivecosts[t, r]
    
  }
  
}

#SLR damage
pow_SLRImpactFxnExponent = Best_Guess(datalist[[131]]$Value)
ipow_SLRIncomeFxnExponent = Best_Guess(datalist[[124]]$Value)
iben_SLRInitialBenefit = 0.00
scal_calibrationSLR = Best_Guess(datalist[[140]]$Value)
GDP_per_cap_focus_0_FocusRegionEU= 27934.244777382406
W_SatCalibrationSLR = Best_Guess(datalist[[150]]$Value) #pp33 PAGE09 documentation, "Sea level impact at calibration sea level rise"
#save_savingsrate = 15.00 #pp33 PAGE09 documentation, "savings rate".
tct_per_cap_totalcostspercap=matrix(nrow =TimeStep,ncol = Region_No );
act_percap_adaptationcosts=matrix(nrow =TimeStep,ncol = Region_No );
impmax_maxSLRforadaptpolicySLR=matrix(nrow =1,ncol = Region_No );
cons_percap_aftercosts=matrix(nrow =TimeStep,ncol = Region_No );
gdp_percap_aftercosts=matrix(nrow =TimeStep,ncol = Region_No );
atl_adjustedtolerablelevelofsealevelrise=matrix(nrow =TimeStep,ncol = Region_No );
imp_actualreductionSLR=matrix(nrow =TimeStep,ncol = Region_No );
i_regionalimpactSLR=matrix(nrow =TimeStep,ncol = Region_No );
iref_ImpactatReferenceGDPperCapSLR=matrix(nrow =TimeStep,ncol = Region_No );
igdp_ImpactatActualGDPperCapSLR=matrix(nrow =TimeStep,ncol = Region_No );
isat_ImpactinclSaturationandAdaptationSLR=matrix(nrow =TimeStep,ncol = Region_No );
isat_per_cap_SLRImpactperCapinclSaturationandAdaptation=matrix(nrow =TimeStep,ncol = Region_No );
for(r in 1:Region_No){
  impmax_maxSLRforadaptpolicySLR[r]=Best_Guess(datalist[[52]]$Sea.level.max.rise[r]);
}
for(t in 1:TimeStep){
  for(r in 1:Region_No){
    atl_adjustedtolerablelevelofsealevelrise[t,r]=Best_Guess(datalist[[65]][t,r+1]);
    #  tct_per_cap_totalcostspercap[t,r]=Best_Guess(datalist[[66]][t,r+1]);
    #  act_percap_adaptationcosts[t,r]=Best_Guess(datalist[[67]][t,r+1]);
    imp_actualreductionSLR[t,r]=Best_Guess(datalist[[68]][t,r+1]);
  }
}
SLR_damage<-function(){
  t=clock_Current(year_current );
  for (r in 1:Region_No){
    cons_percap_aftercosts[t, r] <<- cons_percap_consumption[t, r] - tct_per_cap_totalcostspercap[t, r] - act_percap_adaptationcosts[t, r]
    gdp_percap_aftercosts[t,r]<<-cons_percap_aftercosts[t, r]/(1 - save_savingsrate/100)
    
    if ((s_sealevel[t]-atl_adjustedtolerablelevelofsealevelrise[t,r]) < 0){
      i_regionalimpactSLR[t,r] <<- 0
    }
    else{
      i_regionalimpactSLR[t,r] <<- s_sealevel[t]-atl_adjustedtolerablelevelofsealevelrise[t,r]
    }
    
    iref_ImpactatReferenceGDPperCapSLR[t,r]<<- WINCF_weightsfactor[r]*((W_SatCalibrationSLR + iben_SLRInitialBenefit * scal_calibrationSLR)*
                                                                         (i_regionalimpactSLR[t,r]/scal_calibrationSLR)^pow_SLRImpactFxnExponent - i_regionalimpactSLR[t,r] * iben_SLRInitialBenefit)
    
    igdp_ImpactatActualGDPperCapSLR[t,r]<<- iref_ImpactatReferenceGDPperCapSLR[t,r]*
      (gdp_percap_aftercosts[t,r]/GDP_per_cap_focus_0_FocusRegionEU)^ipow_SLRIncomeFxnExponent
    
    if (igdp_ImpactatActualGDPperCapSLR[t,r] < isatg_impactfxnsaturation){
      isat_ImpactinclSaturationandAdaptationSLR[t,r] <<- igdp_ImpactatActualGDPperCapSLR[t,r]
    }
    else{
      isat_ImpactinclSaturationandAdaptationSLR[t,r] <<- isatg_impactfxnsaturation+
        ((100-save_savingsrate)-isatg_impactfxnsaturation)*
        ((igdp_ImpactatActualGDPperCapSLR[t,r]-isatg_impactfxnsaturation)/
           (((100-save_savingsrate)-isatg_impactfxnsaturation)+
              (igdp_ImpactatActualGDPperCapSLR[t,r]- isatg_impactfxnsaturation)))
    }
    if (i_regionalimpactSLR[t,r] < impmax_maxSLRforadaptpolicySLR[r]){
      isat_ImpactinclSaturationandAdaptationSLR[t,r]<<-isat_ImpactinclSaturationandAdaptationSLR[t,r]*(1-imp_actualreductionSLR[t,r]/100)
    }
    else{
      isat_ImpactinclSaturationandAdaptationSLR[t,r]<<-isat_ImpactinclSaturationandAdaptationSLR[t,r]*(1-(imp_actualreductionSLR[t,r]/100)* impmax_maxSLRforadaptpolicySLR[r] /
                                                                                                         i_regionalimpactSLR[t,r])
    }
    
    isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t,r] <<- (isat_ImpactinclSaturationandAdaptationSLR[t,r]/100)*gdp_percap_aftercosts[t,r]
    rcons_per_cap_SLRRemainConsumption[t,r] <<- cons_percap_aftercosts[t,r] - isat_per_cap_SLRImpactperCapinclSaturationandAdaptation[t,r]
    rgdp_per_cap_SLRRemainGDP[t,r] <<- rcons_per_cap_SLRRemainConsumption[t,r]/(1-save_savingsrate/100)
  } 
}

# Abatement cost
q0propmult_cutbacksatnegativecostinfinalyear = Best_Guess(datalist[[134]]$Value)
qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear = Best_Guess(datalist[[135]]$Value)
c0mult_mostnegativecostinfinalyear = Best_Guess(datalist[[104]]$Value)
curve_below_curvatureofMACcurvebelowzerocost = Best_Guess(datalist[[112]]$Value)
curve_above_curvatureofMACcurveabovezerocost = Best_Guess(datalist[[111]]$Value)
cross_experiencecrossoverratio = Best_Guess(datalist[[110]]$Value)
learn_learningrate = Best_Guess(datalist[[126]]$Value)
automult_autonomoustechchange = Best_Guess(datalist[[102]]$Value)
equity_prop_equityweightsproportion = 1.

er_emissionsgrowth=matrix(nrow =TimeStep,ncol = Region_No );
e0_baselineemissions=matrix(nrow =1,ncol = Region_No );
emitf_uncertaintyinBAUemissfactor=matrix(nrow =1,ncol = Region_No );
q0f_negativecostpercentagefactor=matrix(nrow =1,ncol = Region_No );
cmaxf_maxcostfactor=matrix(nrow =1,ncol = Region_No );
bau_businessasusualemissions=matrix(nrow =TimeStep,ncol = Region_No );
yagg_periodspan=matrix(nrow =TimeStep,ncol = 1 );
emit_UncertaintyinBAUEmissFactor=matrix(nrow =1,ncol = Region_No );
q0propinit_CutbacksinNegativeCostinBaseYear=matrix(nrow =1,ncol = Region_No );
cmaxinit_MaxCutbackCostinBaseYear=matrix(nrow =1,ncol = Region_No );
zc_zerocostemissions=matrix(nrow =TimeStep,ncol = Region_No );
cb_reductionsfromzerocostemissions=matrix(nrow =TimeStep,ncol = Region_No );
cbe_absoluteemissionreductions=matrix(nrow =TimeStep,ncol = Region_No );
cumcbe_cumulativereductionssincebaseyear=matrix(nrow =TimeStep,ncol = Region_No );
cumcbe_g_totalreductions=matrix(nrow =TimeStep,ncol = 1 );
learnfac_learning=matrix(nrow =TimeStep,ncol = Region_No );
autofac=matrix(nrow =TimeStep,ncol = 1 );
c0=matrix(nrow =TimeStep,ncol = 1 );
q0prop=matrix(nrow =TimeStep,ncol = Region_No );
q0_absolutecutbacksatnegativecost=matrix(nrow =TimeStep,ncol = Region_No );
qmax_maxreferencereductions=matrix(nrow =TimeStep,ncol = Region_No );
cmax=matrix(nrow =TimeStep,ncol = Region_No );
blo=matrix(nrow =TimeStep,ncol = Region_No );
alo=matrix(nrow =TimeStep,ncol = Region_No );
bhi=matrix(nrow =TimeStep,ncol = Region_No );
ahi=matrix(nrow =TimeStep,ncol = Region_No );
mc_marginalcost=matrix(nrow =TimeStep,ncol = Region_No );
tcq0=matrix(nrow =TimeStep,ncol = Region_No );
tc_totalcost=matrix(nrow =TimeStep,ncol = Region_No );
bau_co2emissions=matrix(nrow =TimeStep,ncol = Region_No );
bau_ch4emissions=matrix(nrow =TimeStep,ncol = Region_No );
bau_n2oemissions=matrix(nrow =TimeStep,ncol = Region_No );
bau_linemissions=matrix(nrow =TimeStep,ncol = Region_No );
for(r in 1:Region_No){
  emitf_uncertaintyinBAUemissfactor[r]=Best_Guess(datalist[[115]][2,r]);
  q0f_negativecostpercentagefactor[r]=Best_Guess(datalist[[133]][2,r]);
  cmaxf_maxcostfactor[r]=Best_Guess(datalist[[109]][2,r]);
}
for(t in 1:TimeStep){
  #  yagg_periodspan[t]=Best_Guess(datalist[[69]][t,2]);
  for(r in 1:Region_No){
    bau_co2emissions[t,r]=Best_Guess(datalist[[3]][t,r+1]);
    bau_ch4emissions[t,r]=Best_Guess(datalist[[2]][t,r+1]);
    bau_n2oemissions[t,r]=Best_Guess(datalist[[5]][t,r+1]);
    bau_linemissions[t,r]=Best_Guess(datalist[[4]][t,r+1]);
    
  }
}
tc_totalcosts_co2=matrix(nrow =TimeStep,ncol = Region_No );
tc_totalcosts_ch4=matrix(nrow =TimeStep,ncol = Region_No );
tc_totalcosts_n2o=matrix(nrow =TimeStep,ncol = Region_No );
tc_totalcosts_linear=matrix(nrow =TimeStep,ncol = Region_No );
emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CO2=Best_Guess(datalist[[79]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CO2=Best_Guess(datalist[[81]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_CO2= Best_Guess(datalist[[77]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CO2=Best_Guess(datalist[[82]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CO2 =Best_Guess(datalist[[78]]$Value)
ies_InitialExperienceStockofCutbacks_CO2 =Best_Guess(datalist[[80]]$Value)
Abatement_cost_CO2<-function(){
  
  er_emissionsgrowth<<-er_CO2emissionsgrowth;
  e0_baselineemissions<<-e0_baselineCO2emissions;
  bau_businessasusualemissions<<-bau_co2emissions;
  t=clock_Current(year_current)
  for (r in 1:Region_No){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CO2 *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CO2 *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CO2 *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_CO2* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CO2 * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  
  for (r in 1:Region_No){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_CO2)/  ies_InitialExperienceStockofCutbacks_CO2)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_co2[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_co2[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}


emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CH4= Best_Guess(datalist[[73]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CH4 = Best_Guess(datalist[[75]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_CH4 = Best_Guess(datalist[[71]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CH4 = Best_Guess(datalist[[76]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CH4 = Best_Guess(datalist[[72]]$Value)
ies_InitialExperienceStockofCutbacks_CH4 = Best_Guess(datalist[[74]]$Value)

Abatement_cost_CH4<-function(){
    er_emissionsgrowth=er_CH4emissionsgrowth;
  e0_baselineemissions=e0_baselineCH4emissions;
  bau_businessasusualemissions=bau_ch4emissions;
  
  t=clock_Current(year_current)
  for (r in 1:Region_No){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_CH4 *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_CH4 *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_CH4 *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_CH4* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_CH4 * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  
  for (r in 1:Region_No){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_CH4)/  ies_InitialExperienceStockofCutbacks_CH4)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_ch4[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_ch4[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}


emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_N2O<-Best_Guess(datalist[[91]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_N2O<-Best_Guess(datalist[[93]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_N2O<- Best_Guess(datalist[[89]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_N2O<-Best_Guess(datalist[[94]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_N2O <-Best_Guess(datalist[[90]]$Value)
ies_InitialExperienceStockofCutbacks_N2O <-Best_Guess(datalist[[92]]$Value)

Abatement_cost_N2O<-function(){
  er_emissionsgrowth<<-er_N2Oemissionsgrowth;
  e0_baselineemissions<<-e0_baselineN2Oemissions;
  bau_businessasusualemissions<<-bau_n2oemissions;
  t=clock_Current(year_current)
  for (r in 1:Region_No){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_N2O *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_N2O *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_N2O *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_N2O* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_N2O * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  
  for (r in 1:Region_No){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_N2O)/  ies_InitialExperienceStockofCutbacks_N2O)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_n2o[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_n2o[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}

emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_LG = Best_Guess(datalist[[85]]$Value)
q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_LG=Best_Guess(datalist[[87]]$Value)
c0init_MostNegativeCostCutbackinBaseYear_LG=Best_Guess(datalist[[83]]$Value)
qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_LG=Best_Guess(datalist[[88]]$Value)
cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_LG=Best_Guess(datalist[[84]]$Value)
ies_InitialExperienceStockofCutbacks_LG=Best_Guess(datalist[[86]]$Value)
Abatement_cost_LG<-function(){
  
  
  er_emissionsgrowth<<-er_LGemissionsgrowth;
  e0_baselineemissions<<-e0_baselineLGemissions;
  bau_businessasusualemissions<<-bau_linemissions;
  t=clock_Current(year_current)
  for (r in 1:Region_No){
    emit_UncertaintyinBAUEmissFactor[r] <<-  emit_UncertaintyinBAUEmissFactorinFocusRegioninFinalYear_LG *
      emitf_uncertaintyinBAUemissfactor[r]
    q0propinit_CutbacksinNegativeCostinBaseYear[r] <<-  q0propinit_CutbacksinNegativeCostinFocusRegioninBaseYear_LG *
      q0f_negativecostpercentagefactor[r]
    cmaxinit_MaxCutbackCostinBaseYear[r] <<-  cmaxinit_MaximumCutbackCostinFocusRegioninBaseYear_LG *
      cmaxf_maxcostfactor[r]
    
    zc_zerocostemissions[t,r] <<- (1+ emit_UncertaintyinBAUEmissFactor[r]/100 * ( y_year[t]- y_year_0)/( y_year[TimeStep]- y_year_0)) *  bau_businessasusualemissions[t,r]
    
    cb_reductionsfromzerocostemissions[t,r] <<- max( zc_zerocostemissions[t,r] -  er_emissionsgrowth[t,r], 0)
    
    cbe_absoluteemissionreductions[t,r] <<- cb_reductionsfromzerocostemissions[t,r]*  e0_baselineemissions[r]/100
    
    if (t==1){
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- 0.
    }
    else{
      cumcbe_cumulativereductionssincebaseyear[t,r] <<- cumcbe_cumulativereductionssincebaseyear[t-1, r] +  cbe_absoluteemissionreductions[t-1, r] *  yagg_periodspan[t-1]
    }
  }
  cumcbe_g_totalreductions[t] <<- sum( cumcbe_cumulativereductionssincebaseyear[t,])
  
  auto <<- (1- automult_autonomoustechchange^(1/( y_year[TimeStep]- y_year_0)))*100
  autofac[t] <<- (1- auto/100)^( y_year[t] -  y_year_0)
  
  c0g <<- ( c0mult_mostnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  c0[t] <<-  c0init_MostNegativeCostCutbackinBaseYear_LG* (1+ c0g/100)^( y_year[t]- y_year_0)
  
  qmaxminusq0propg <<- ( qmax_minus_q0propmult_maxcutbacksatpositivecostinfinalyear ^(1/( y_year[TimeStep]- y_year_0))- 1)* 100
  qmaxminusq0prop <<-  qmaxminusq0propinit_MaxCutbackCostatPositiveCostinBaseYear_LG * (1+  qmaxminusq0propg/100)^( y_year[t]- y_year_0)
  
  q0propg <<- ( q0propmult_cutbacksatnegativecostinfinalyear^(1/( y_year[TimeStep]- y_year_0))-1)*100
  
  for (r in 1:Region_No){
    learnfac_learning[t,r] <<- (( cross_experiencecrossoverratio * cumcbe_g_totalreductions[t]+ (1- cross_experiencecrossoverratio)* cumcbe_cumulativereductionssincebaseyear[t,r] +  ies_InitialExperienceStockofCutbacks_LG)/  ies_InitialExperienceStockofCutbacks_LG)^ -(log(1/(1- learn_learningrate))/log(2))
    
    q0prop[t,r] <<-  q0propinit_CutbacksinNegativeCostinBaseYear[r]* (1+ q0propg/100)^( y_year[t]- y_year_0)
    
    q0_absolutecutbacksatnegativecost[t,r]<<- ( q0prop[t,r]/100)* ( zc_zerocostemissions[t,r]/100) *  e0_baselineemissions[r]
    
    qmax_maxreferencereductions[t,r] <<- ( qmaxminusq0prop/100) * ( zc_zerocostemissions[t,r]/100)*  e0_baselineemissions[r] +  q0_absolutecutbacksatnegativecost[t,r]
    
    cmax[t,r] <<-  cmaxinit_MaxCutbackCostinBaseYear[r] *  learnfac_learning[t,r]*  autofac[t]
    
    blo[t,r] <<- -2*log((1+ curve_below_curvatureofMACcurvebelowzerocost)/(1- curve_below_curvatureofMACcurvebelowzerocost))/  q0_absolutecutbacksatnegativecost[t,r]
    alo[t,r] <<-  c0[t]/(exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])-1)
    bhi[t,r] <<- 2*log((1+ curve_above_curvatureofMACcurveabovezerocost)/(1- curve_above_curvatureofMACcurveabovezerocost))/ ( qmax_maxreferencereductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r])
    ahi[t,r] <<-  cmax[t,r]/ (exp( bhi[t,r]*( qmax_maxreferencereductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1)
    
    if  (cbe_absoluteemissionreductions[t,r]<  q0_absolutecutbacksatnegativecost[t,r]){
      mc_marginalcost[t,r] <<-  alo[t,r]* (exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    else{
      mc_marginalcost[t,r] <<-  ahi[t,r]*(exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))-1)
    }
    
    if  (q0_absolutecutbacksatnegativecost[t,r] == 0.){
      tcq0[t,r] <<- 0.
    }
    else{
      tcq0[t,r] <<- ( alo[t,r]/ blo[t,r])*(1-exp(- blo[t,r]*  q0_absolutecutbacksatnegativecost[t,r]))-  alo[t,r]* q0_absolutecutbacksatnegativecost[t,r]
    }
    
    if  (cbe_absoluteemissionreductions[t,r]< q0_absolutecutbacksatnegativecost[t,r]){
      tc_totalcosts_linear[t,r] <<- ( alo[t,r]/ blo[t,r])*(exp( blo[t,r]*( cbe_absoluteemissionreductions[t,r]-  q0_absolutecutbacksatnegativecost[t,r]))- exp(- blo[t,r]* q0_absolutecutbacksatnegativecost[t,r])) -  alo[t,r]* cbe_absoluteemissionreductions[t,r]
    }
    else{
      tc_totalcosts_linear[t,r] <<- ( ahi[t,r]/ bhi[t,r])* (exp( bhi[t,r]*( cbe_absoluteemissionreductions[t,r]- q0_absolutecutbacksatnegativecost[t,r]))-1) -  ahi[t,r]*( cbe_absoluteemissionreductions[t,r] -  q0_absolutecutbacksatnegativecost[t,r]) +  tcq0[t,r]
    }
  }
  
}

# TotalAbatementCosts

tct_totalcosts=matrix(nrow =TimeStep,ncol = Region_No );
#tct_per_cap_totalcostspercap=matrix(nrow =TimeStep,ncol = Region_No );

TotalAbatementCosts<-function(){
  t=clock_Current(year_current)
  for (r in 1:Region_No){
    tct_totalcosts[t,r] <<- tc_totalcosts_co2[t,r] + tc_totalcosts_n2o[t,r] + tc_totalcosts_ch4[t,r] + tc_totalcosts_linear[t,r]
    tct_per_cap_totalcostspercap[t,r] <<- tct_totalcosts[t,r]/pop_population[t,r]
    # tct_totalcosts[t,r] <<- 0;
    # tct_per_cap_totalcostspercap[t,r] <<-0;
     }
}

# TotalAdaptationCosts

act_adaptationcosts_total=matrix(nrow =TimeStep,ncol = Region_No );
TotalAdaptationCosts<-function(){
  t=clock_Current(year_current)
  for (r in 1:Region_No){
   act_adaptationcosts_total[t,r] <<- ac_adaptationcosts_economic[t,r] + ac_adaptationcosts_sealevelrise[t,r] + ac_adaptationcosts_noneconomic[t,r]
    # act_adaptationcosts_total[t,r] <<- 0
    act_percap_adaptationcosts[t,r] <<- act_adaptationcosts_total[t,r]/pop_population[t,r]
  }
}



# population
# 
#pop_population=matrix(nrow = TimeStep,ncol = Region_No)

Population<-function(){
  t=clock_Current(year_current);
  for (r in 1:Region_No){
    # Eq.28 in Hope 2002 (defined for GDP, but also applies to population)
    if (t == 1){
      pop_population[t, r] <<- pop0_initpopulation[r] * (1 + popgrw_populationgrowth[t, r]/100)^(y_year[t] - y_year_0)
    }
    else{
      pop_population[t, r] <<- pop_population[t-1, r] * (1 + popgrw_populationgrowth[t, r]/100)^(y_year[t] - y_year[t-1])
    }
  }
}


# equlity weight
ptp_timepreference = Best_Guess(datalist[[132]]$Value) # <0.1,1, 2>
equity_proportion = 1.0
emuc_utilityconvexity = Best_Guess(datalist[[116]]$Value) ;
civvalue_civilizationvalue = Best_Guess(datalist[[108]]$Value)

#tct_percap_totalcosts_total=matrix(nrow = TimeStep,ncol = Region_No)
# act_adaptationcosts_total=matrix(nrow = TimeStep,ncol = Region_No)
# act_percap_adaptationcosts=matrix(nrow = TimeStep,ncol = Region_No)
wtct_percap_weightedcosts=matrix(nrow = TimeStep,ncol = Region_No)
eact_percap_weightedadaptationcosts=matrix(nrow = TimeStep,ncol = Region_No)
wact_percap_partiallyweighted=matrix(nrow = TimeStep,ncol = Region_No)
wact_partiallyweighted=matrix(nrow = TimeStep,ncol = Region_No)
pct_percap_partiallyweighted=matrix(nrow = TimeStep,ncol = Region_No)
pct_partiallyweighted=matrix(nrow = TimeStep,ncol = Region_No)
pct_g_partiallyweighted_global=matrix(nrow = TimeStep,ncol = 1)
dr_discountrate=matrix(nrow = TimeStep,ncol = Region_No)
yp_yearsperiod=matrix(nrow = TimeStep,ncol = 1)
dfc_consumptiondiscountrate=matrix(nrow = TimeStep,ncol = Region_No)
df_utilitydiscountrate=matrix(nrow = TimeStep,ncol = 1)
pcdt_partiallyweighted_discounted=matrix(nrow = TimeStep,ncol = Region_No)
pcdt_g_partiallyweighted_discountedglobal=matrix(nrow = TimeStep,ncol = 1)
pcdat_partiallyweighted_discountedaggregated=matrix(nrow = TimeStep,ncol = Region_No)
wacdt_partiallyweighted_discounted=matrix(nrow = TimeStep,ncol = Region_No)
#rcons_percap_dis=matrix(nrow = TimeStep,ncol = Region_No)
wit_equityweightedimpact=matrix(nrow = TimeStep,ncol = Region_No)
widt_equityweightedimpact_discounted=matrix(nrow = TimeStep,ncol = Region_No)
addt_equityweightedimpact_discountedaggregated=matrix(nrow = TimeStep,ncol = Region_No)
aact_equityweightedadaptation_discountedaggregated=matrix(nrow = TimeStep,ncol = Region_No)
# for(t in 1:TimeStep){
#   for(r in 1:Region_No){
#     rcons_percap_dis[t,r]=Best_Guess(datalist[[70]][t,r+1]);
#   }
# }


equalityweighting<-function(){
  t=clock_Current(year_current);
  if (t == 1){
    tpc_totalaggregatedcosts <<- 0
    addt_gt_equityweightedimpact_discountedglobal <<- 0
    tac_totaladaptationcosts <<- 0
    te_totaleffect <<- 0
  }
  
  df_utilitydiscountrate[t] <<- (1 + ptp_timepreference / 100)^(-(y_year[t] - y_year_0))
  
  for (r in 1:Region_No){
    
    ## Gas Costs Accounting
    # Weighted costs (Page 23 of Hope 2009)
    wtct_percap_weightedcosts[t, r] <<- ((cons_percap_consumption_0[1]^emuc_utilityconvexity) / (1 - emuc_utilityconvexity)) * (cons_percap_consumption[t, r]^(1 - emuc_utilityconvexity) - (cons_percap_consumption[t, r] - tct_per_cap_totalcostspercap[t, r])^(1 - emuc_utilityconvexity))
    
    # Add these into consumption
    eact_percap_weightedadaptationcosts[t, r] <<- ((cons_percap_consumption_0[1]^emuc_utilityconvexity) / (1 - emuc_utilityconvexity)) * (cons_percap_consumption[t, r]^(1 - emuc_utilityconvexity) - (cons_percap_consumption[t, r] - act_percap_adaptationcosts[t, r])^(1 - emuc_utilityconvexity))
    
    # Do partial weighting
    if (equity_proportion == 0){
      pct_percap_partiallyweighted[t, r] <<- tct_per_cap_totalcostspercap[t, r]
      wact_percap_partiallyweighted[t, r] <<- act_percap_adaptationcosts[t, r]
    }
    else{
      pct_percap_partiallyweighted[t, r] <<- (1 - equity_proportion) * tct_per_cap_totalcostspercap[t, r] + equity_proportion * wtct_percap_weightedcosts[t, r]
      wact_percap_partiallyweighted[t, r] <<- (1 - equity_proportion) * act_percap_adaptationcosts[t, r] + equity_proportion * eact_percap_weightedadaptationcosts[t, r]
    }
    
    pct_partiallyweighted[t, r] <<- pct_percap_partiallyweighted[t, r] * pop_population[t, r]
    wact_partiallyweighted[t, r] <<- wact_percap_partiallyweighted[t, r] * pop_population[t, r]
    
    # Discount rate calculations
    dr_discountrate[t, r] <<- ptp_timepreference + emuc_utilityconvexity * (grw_gdpgrowthrate[t, r] - popgrw_populationgrowth[t, r])
    if (t == 1){
      yp_yearsperiod[1] <<- y_year[1] - y_year_0
    }
    else{
      yp_yearsperiod[t] <<- y_year[t] - y_year[t-1]
    }
    
    if (t == 1){
      dfc_consumptiondiscountrate[1, r] <<- (1 + dr_discountrate[1, r] / 100)^(-yp_yearsperiod[1])
    }
    else{
      dfc_consumptiondiscountrate[t, r] <<- dfc_consumptiondiscountrate[t - 1, r] * (1 + dr_discountrate[t, r] / 100)^(-yp_yearsperiod[t])
    }
    
    # Discounted costs
    if (equity_proportion == 0){
      pcdt_partiallyweighted_discounted[t, r] <<- pct_partiallyweighted[t, r] * dfc_consumptiondiscountrate[t, r]
      wacdt_partiallyweighted_discounted[t, r] <<- act_adaptationcosts_total[t, r] * dfc_consumptiondiscountrate[t, r]
    }
    else{
      pcdt_partiallyweighted_discounted[t, r] <<- pct_partiallyweighted[t, r] * df_utilitydiscountrate[t]
      wacdt_partiallyweighted_discounted[t, r] <<- wact_partiallyweighted[t, r] * df_utilitydiscountrate[t]
    }
    
    pcdat_partiallyweighted_discountedaggregated[t, r] <<- pcdt_partiallyweighted_discounted[t, r] * yagg_periodspan[t]
    
    ## Equity weighted impacts (end of page 28, Hope 2009)
    wit_equityweightedimpact[t, r] <<- ((cons_percap_consumption_0[1]^emuc_utilityconvexity) / (1 - emuc_utilityconvexity)) * (cons_percap_aftercosts[t, r]^(1 - emuc_utilityconvexity) - rcons_per_cap_DiscRemainConsumption[t, r]^(1 - emuc_utilityconvexity)) * pop_population[t, r]
    
    widt_equityweightedimpact_discounted[t, r] <<- wit_equityweightedimpact[t, r] * df_utilitydiscountrate[t]
    
    addt_equityweightedimpact_discountedaggregated[t, r] <<- widt_equityweightedimpact_discounted[t, r] * yagg_periodspan[t]
    aact_equityweightedadaptation_discountedaggregated[t, r] <<- wacdt_partiallyweighted_discounted[t, r] * yagg_periodspan[t]
  }
  
  pct_g_partiallyweighted_global[t] <<- sum(pct_partiallyweighted[t, ])
  pcdt_g_partiallyweighted_discountedglobal[t] <<- sum(pcdt_partiallyweighted_discounted[t, ])
  tpc_totalaggregatedcosts <<- tpc_totalaggregatedcosts + sum(pcdat_partiallyweighted_discountedaggregated[t, ])
  
  addt_gt_equityweightedimpact_discountedglobal <<- addt_gt_equityweightedimpact_discountedglobal + sum(addt_equityweightedimpact_discountedaggregated[t, ])
  
  tac_totaladaptationcosts <<- tac_totaladaptationcosts + sum(aact_equityweightedadaptation_discountedaggregated[t, ])
  
  td_totaldiscountedimpacts <<- min(addt_gt_equityweightedimpact_discountedglobal, civvalue_civilizationvalue)
  
  # Total effect of climate change
  te_totaleffect <<- min(td_totaldiscountedimpacts + tpc_totalaggregatedcosts + tac_totaladaptationcosts, civvalue_civilizationvalue)
  
}


playground_model<-function(){
  for(i in 1:10){
    year_current<<-i;
    CO2_emissions();
    CO2_cycle();
    CO2_forcing();
    CH4_emissions();
    CH4_cycle();
    CH4_forcing();
    N2O_emissions();
    N2O_cycle();
    N2O_forcing();
    LG_emissions();
    LG_cycle();
    LG_forcing();
    SulphateForcing();
    Total_forcing();
    ClimateTemperature();
    Sea_level_rise();
    
    Population();
    GDP();
    
    Abatement_cost_CO2();
    Abatement_cost_CH4();
    Abatement_cost_N2O();
    Abatement_cost_LG();
    
    Adaptation_cost_sealevel();
    Adaptation_cost_economic();
    Adaptation_cost_noneconomic();
    
    TotalAbatementCosts();
    TotalAdaptationCosts();
    
    SLR_damage();
    
    Market_damage();
    non_market_damage();
    
    Discontinuity();
    
    
    
    equalityweighting();
  }
}

Extra_emission_model<-function(){
  for(i in 1:10){
    year_current<<-i;
    if(i==1){
      CO2_emissions();
      e_globalCO2emissions[t]<<-e_globalCO2emissions[t]+100000;
      CO2_cycle();
      CO2_forcing();
      CH4_emissions();
      CH4_cycle();
      CH4_forcing();
      N2O_emissions();
      N2O_cycle();
      N2O_forcing();
      LG_emissions();
      LG_cycle();
      LG_forcing();
      SulphateForcing();
      Total_forcing();
      ClimateTemperature();
      Sea_level_rise();
      
      Population();
      GDP();
      
      Abatement_cost_CO2();
      Abatement_cost_CH4();
      Abatement_cost_N2O();
      Abatement_cost_LG();
      
      Adaptation_cost_sealevel();
      Adaptation_cost_economic();
      Adaptation_cost_noneconomic();
      
      TotalAbatementCosts();
      TotalAdaptationCosts();
      
      SLR_damage();
      
      Market_damage();
      non_market_damage();
      
      Discontinuity();
    }
    else{
      CO2_emissions();
      CO2_cycle();
      CO2_forcing();
      CH4_emissions();
      CH4_cycle();
      CH4_forcing();
      N2O_emissions();
      N2O_cycle();
      N2O_forcing();
      LG_emissions();
      LG_cycle();
      LG_forcing();
      SulphateForcing();
      Total_forcing();
      ClimateTemperature();
      Sea_level_rise();
      
      Population();
      GDP();
      
      Abatement_cost_CO2();
      Abatement_cost_CH4();
      Abatement_cost_N2O();
      Abatement_cost_LG();
      
      Adaptation_cost_sealevel();
      Adaptation_cost_economic();
      Adaptation_cost_noneconomic();
      
      TotalAbatementCosts();
      TotalAdaptationCosts();
      
      SLR_damage();
      
      Market_damage();
      non_market_damage();
      
      Discontinuity();
    }
    
    
    equalityweighting();
  }
}


