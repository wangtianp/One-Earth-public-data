clock_Current<-function(year_current){
  t=year_current; 
}

Time_periods = 100;
simulation_year=2;
fosslim=6000;
tstep=5;
t0=1;
ifopt=0;
#discount rate
elasmu=1.45;
prstp=0.015;

# elasmu=0;
# prstp=0.05; 

scale1 = 0.0302455265681763;
scale2 = -10993.704;

# Population and technology
gama=0.300;
pop0=7403;
popadj=0.134;
popasym=11500;
dk=0.1;
q0=105.5

k0=223;  

a0=5.115;
ga0=0.076;
dela=0.005;

# Emissions parameters
gsigma1=-0.0152 ;
dsig=-0.001;
eland0=2.6;
deland=0.115;
e0=35.85;
miu0=0.03;
# ** Carbon cycle
# * Initial Conditions
mat0=851;
mu0=460;
ml0=1740;
mateq=588;
mueq=360;
mleq=1720;
# * Flow paramaters
b12=0.12 ;
b23=0.007;
# ** Climate model parameters
eqmat = 588.000;
t2xco2=3.1;
fex0=0.5;
fex1=1.0;
tocean0=0.0068;
tatm0=0.85;
c1=0.1005;
c3=0.088;
c4=0.025;
fco22x=3.6813;
# ** Climate damage parameters
a10=0;
a1=0;
a2=0.00236;
a3=2.00;
usedamadj=TRUE
#** Abatement cost
expcost2=2.6;
pback=550;
gback=0.025
limmiu=1.2;
tnopol=45;
cprice0=2;
gcprice=0.02;

#########################
b11=1 - b12;
b21=b12*mateq/mueq;
b22 = 1 - b21 - b23;
b32 = b23*mueq/mleq;
b33 = 1 - b32 ;
sig0=e0/(q0*(1-miu0))
a20 = a2;
sig0 = e0/(q0*(1-miu0));
lam = fco22x/ t2xco2;

# tfirst(t), tlast(t), tearly(t), tlate(t);
l=matrix(0,nrow = Time_periods,ncol = 1)
al=matrix(0,nrow = Time_periods,ncol = 1)
sigma=matrix(0,nrow = Time_periods,ncol = 1)
rr=matrix(0,nrow = Time_periods,ncol = 1)
ga=matrix(0,nrow = Time_periods,ncol = 1)
forcoth=matrix(0,nrow = Time_periods,ncol = 1)
gl=matrix(0,nrow = Time_periods,ncol = 1)
gsig=matrix(0,nrow = Time_periods,ncol = 1)
etree=matrix(0,nrow = Time_periods,ncol = 1)
cumetree=matrix(0,nrow = Time_periods,ncol = 1)
cost1=matrix(0,nrow = Time_periods,ncol = 1)
gfacpop=matrix(0,nrow = Time_periods,ncol = 1)
pbacktime=matrix(0,nrow = Time_periods,ncol = 1)
scc=matrix(0,nrow = Time_periods,ncol = 1)
cpricebase=matrix(0,nrow = Time_periods,ncol = 1)
photel=matrix(0,nrow = Time_periods,ncol = 1)
ppm=matrix(0,nrow = Time_periods,ncol = 1)
atfrac=matrix(0,nrow = Time_periods,ncol = 1)
atfrac2010=matrix(0,nrow = Time_periods,ncol = 1)
EIND=matrix(0, nrow =Time_periods,ncol=1)

l[t0,1]=pop0;
ga[t0,1]=ga0;
al[t0,1]=a0;
gsig[t0,1]=gsigma1;
sigma[t0,1]=sig0;
cumetree[t0,1]= 100;

optlrsav = (dk + 0.004)/(dk + 0.004*elasmu + prstp)*gama;

E=matrix(0, nrow =Time_periods,ncol=1)
CCA=matrix(0, nrow =Time_periods,ncol=1)
CCATOT=matrix(0, nrow =Time_periods,ncol=1)
FORC=matrix(0, nrow =Time_periods,ncol=1)
DAMFRAC=matrix(0, nrow =Time_periods,ncol=1)
DAMAGES=matrix(0, nrow =Time_periods,ncol=1)
ABATECOST=matrix(0, nrow =Time_periods,ncol=1)
MCABATE=matrix(0, nrow =Time_periods,ncol=1)
CPRICE=matrix(0, nrow =Time_periods,ncol=1)
MAT=matrix(0, nrow =Time_periods,ncol=1)
ML=matrix(0, nrow =Time_periods,ncol=1)
MU=matrix(0, nrow =Time_periods,ncol=1)
TATM=matrix(0, nrow =Time_periods,ncol=1)
TOCEAN=matrix(0, nrow =Time_periods,ncol=1)
YGROSS=matrix(0, nrow =Time_periods,ncol=1)
YNET=matrix(0, nrow =Time_periods,ncol=1)
Y=matrix(0, nrow =Time_periods,ncol=1)
C=matrix(0, nrow =Time_periods,ncol=1)
CPC=matrix(0, nrow =Time_periods,ncol=1)
I=matrix(0, nrow =Time_periods,ncol=1)
K=matrix(0, nrow =Time_periods,ncol=1)
RI=matrix(0, nrow =Time_periods,ncol=1)
CEMUTOTPER=matrix(0, nrow =Time_periods,ncol=1)
PERIODU=matrix(0, nrow =Time_periods,ncol=1)

K[t0,1]=k0;
MAT[t0,1]=mat0;
CCA[t0,1]= 400;
MU[t0,1]= mu0;
ML[t0,1]= ml0;
TATM[t0,1]= tatm0;
TOCEAN[t0,1] = tocean0;
FORC[t0,1]= fco22x * ((log((MAT[t0,1]/588.000))/log(2))) + forcoth[t0,1];

## load socio-economic data and emissions parametes from 
etree=t(DICE_data[1,-1]);
MIU=t(DICE_data[2,-1]);
sigma=t(DICE_data[3,-1]);
al=t(DICE_data[4,-1]);
l=t(DICE_data[5,-1]);
forcoth=t(DICE_data[6,-1]);
cost1=t(DICE_data[7,-1]);
partfract=t(DICE_data[8,-1]);
pbacktime=t(DICE_data[9,-1]);
rr=matrix( nrow = Time_periods, ncol = 1);
for(t in 1:Time_periods){
  rr[t]=1/((1+prstp)**(tstep*(t-1)))
}

S=t(DICE_data[11,-1]);


Emission_component<-function(){
  t=clock_Current(year_current);
  EIND[t] <<- sigma[t] * YGROSS[t] * (1- MIU[t])
  
  #Define function for E
  E[t] <<- EIND[t] + etree[t]
  
  #Define function for CCA
  if (t>1){
    CCA[t] <<- CCA[t-1] + EIND[t-1] * 5/3.666
  }
}


# tfirst(t), tlast(t), tearly(t), tlate(t);
# K = matrix(nrow = Time_periods,ncol = 1)   #Capital stock (trillions 2005 US dollars)
# YGROSS = matrix(nrow = Time_periods,ncol = 1)   #Gross world product GROSS of abatement and damages (trillions 2005 USD per year)
# al= matrix(nrow = Time_periods,ncol = 1)  #Level of total factor productivity
# I = matrix(nrow = Time_periods,ncol = 1)   #Investment (trillions 2005 USD per year)
# l = matrix(nrow = Time_periods,ncol = 1)

Gross_economy<-function(){
  t=clock_Current(year_current)
  
  t=clock_Current(year_current)
  if(t==1){
    K[t]<<-k0;
  }
  else{
    K[t] <<- (1 - dk)^5 * K[t-1] + 5 * I[t-1]
    
  }
  YGROSS[t] <<- (al[t] * (l[t]/1000)^(1-gama)) * (K[t]^gama)
}



MAT = matrix(nrow = Time_periods,ncol = 1)    #Carbon concentration increase in atmosphere (GtC from 1750)
ML  = matrix(nrow = Time_periods,ncol = 1)    #Carbon concentration increase in lower oceans (GtC from 1750)
MU  = matrix(nrow = Time_periods,ncol = 1)    #Carbon concentration increase in shallow oceans (GtC from 1750)
#E   = matrix(nrow = Time_periods,ncol = 1)

CO2_cycle<-function(){
  t=clock_Current(year_current)
  
  if (t==1){
    MAT[t] <<- mat0
  }
  else{
    MAT[t] <<- MAT[t-1] * b11 + MU[t-1] * b21 + (E[t-1]*(5/3.666))
  }
  
  #Define function for MU
  if (t==1){
    MU[t] <<- mu0
  }
  else{
    MU[t] <<- MAT[t-1] * b12 + MU[t-1] * b22 + ML[t-1] * b32
  }
  
  #Define function for ML
  if (t==1){
    ML[t] <<- ml0
  }
  else{
    ML[t] <<- ML[t-1] * b33 + MU[t-1] * b23
  }
}


FORC = matrix(nrow = Time_periods,ncol = 1)   #Increase in radiative forcing (watts per m2 from 1900)
#forcoth = matrix(nrow = Time_periods,ncol = 1)   #Exogenous forcing for other greenhouse gases

Radiativeforcing<-function(){
  t=clock_Current(year_current)
  FORC[t] <<- fco22x * (log((MAT[t] / eqmat)) / log(2)) + forcoth[t];
  
}

TATM    = matrix(nrow = Time_periods,ncol = 1)   #Increase in temperature of atmosphere (degrees C from 1900)
TOCEAN  = matrix(nrow = Time_periods,ncol = 1)   #Increase in temperature of lower oceans (degrees C from 1900)
#FORC    = matrix(nrow = Time_periods,ncol = 1)

Climatedynamics<-function(){
  t=clock_Current(year_current)
  
  if (t==1){
    TATM[t] <<- tatm0
  }
  else{
    TATM[t] <<- TATM[t-1] + c1 * ((FORC[t] - (fco22x/t2xco2) * TATM[t-1]) - (c3 * (TATM[t-1] - TOCEAN[t-1])))
  }
  
  #Define function for TOCEAN
  if (t==1){
    TOCEAN[t] <<- tocean0
  }
  else{
    TOCEAN[t] <<- TOCEAN[t-1] + c4 * (TATM[t-1] - TOCEAN[t-1])
  }
  
}

DAMAGES = matrix(nrow = Time_periods,ncol = 1)     #Damages (trillions 2005 USD per year)
DAMFRAC = matrix(nrow = Time_periods,ncol = 1)    #Increase in temperature of atmosphere (degrees C from 1900)

Damages_component<-function(){
  t=clock_Current(year_current)
  
  DAMFRAC[t] <<- a1 * TATM[t] + a2 * TATM[t]^a3 ;
  
  #Define function for DAMAGES
  if (usedamadj==TRUE){
    # Excel version
    DAMAGES[t] <<- YGROSS[t] * DAMFRAC[t]
  }
  else{
    # GAMS Version
    DAMAGES[t] <<- YGROSS[t] * 1/(1 + DAMFRAC[t])
  }
}

Discount_rate<-matrix(nrow=Time_periods-1,ncol=1);

Neteconomy<-function(){
  t=clock_Current(year_current)
  
  YNET[t] <<-  YGROSS[t] -  DAMAGES[t]
  
  #Define function for ABATECOST
  ABATECOST[t] <<-  YGROSS[t] *  cost1[t] * ( MIU[t]^ expcost2) * ( partfract[t]^(1 -  expcost2))
  
  #Define function for MCABATE (equation from GAMS version)
  MCABATE[t] <<-  pbacktime[t] *  MIU[t]^( expcost2 - 1)
  
  #Define function for Y
  Y[t] <<-  YNET[t] -  ABATECOST[t]
  
  #Define function for I
  I[t] <<-  S[t] *  Y[t]
  
  #Define function for C
  C[t] <<- Y[t] -  I[t]
  
  #Define function for CPC
  CPC[t] <<- 1000 *  C[t] /  l[t]
  
  #Define function for CPRICE (equation from GAMS version of DICE2013)
  CPRICE[t] <<-  pbacktime[t] * ( MIU[t] /  partfract[t])^( expcost2 - 1)
  
  
  #discounted rate
  
}

Neteconomy_extarC<-function(){
  t=clock_Current(year_current)
  
  YNET[t] <<-  YGROSS[t] -  DAMAGES[t]
  
  #Define function for ABATECOST
  ABATECOST[t] <<-  YGROSS[t] *  cost1[t] * ( MIU[t]^ expcost2) * ( partfract[t]^(1 -  expcost2))
  
  #Define function for MCABATE (equation from GAMS version)
  MCABATE[t] <<-  pbacktime[t] *  MIU[t]^( expcost2 - 1)
  
  #Define function for Y
  Y[t] <<-  YNET[t] -  ABATECOST[t]
  
  #Define function for I
  I[t] <<-  S[t] *  Y[t]
  
  #Define function for C
  C[t] <<- Y[t] -  I[t]+margin_value
  
  #Define function for CPC
  CPC[t] <<- 1000 *  C[t] /  l[t]
  
  #Define function for CPRICE (equation from GAMS version of DICE2013)
  CPRICE[t] <<-  pbacktime[t] * ( MIU[t] /  partfract[t])^( expcost2 - 1)
  
  
  #discounted rate
  
}


CEMUTOTPER  = matrix(nrow = Time_periods,ncol = 1)     #Period utility
CUMCEMUTOTPER = matrix(nrow = Time_periods,ncol = 1)     #Cumulative period utility
PERIODU  = matrix(nrow = Time_periods,ncol = 1)    #One period utility function
CPC = matrix(nrow = Time_periods,ncol = 1)    #Per capita consumption (thousands 2005 USD per year)
#l = matrix(nrow = Time_periods,ncol = 1)    #Level of population and labor
#rr  = matrix(nrow = Time_periods,ncol = 1)    #Average utility social discount rate

Welfare_component<-function(){
  t=clock_Current(year_current)
  PERIODU[t] <<- ( CPC[t] ^ (1 -  elasmu) - 1) / (1 -  elasmu) - 1
  
  #Define function for CEMUTOTPER
  CEMUTOTPER[t] <<-  PERIODU[t] *  l[t] *  rr[t]
  
  #Define function for CUMCEMUTOTPER
  if (t ==1){
    CUMCEMUTOTPER[t] <<-  CEMUTOTPER[t]
  }
  else{
    CUMCEMUTOTPER[t] <<-  CUMCEMUTOTPER[t-1] +  CEMUTOTPER[t]
  }
  
  #Define function for UTILITY
  if (t==Time_periods){
    UTILITY <<- 5 *  scale1 *  CUMCEMUTOTPER[Time_periods] +  scale2
  }
  
}

Discount_factor=matrix(nrow = Time_periods,ncol = 1);
Discount_factor[t0]=1;

Playground_model<-function(){
  for(t in 1:Time_periods){
    year_current<<-t;
    Gross_economy();
    Emission_component();
    CO2_cycle();
    Radiativeforcing();
    Climatedynamics();
    Damages_component();
    Neteconomy();
    Welfare_component();
  }
  for(t in 1:(Time_periods-1)){
    Discount_rate[t]<<-(1+prstp) * (CPC[t+1]/CPC[t])**(elasmu/tstep) - 1;
    Discount_factor[t+1]<<-Discount_factor[t]*((1/(1+Discount_rate[t]))^tstep);
  }
}

Extra_emission_model<-function(){
  for(t in 1:Time_periods){
    year_current<<-t;
    if(t==simulation_year){
      Gross_economy();
      Emission_component();
      E[t] <<- E[t]+margin_value;
      CO2_cycle();
      Radiativeforcing();
      Climatedynamics();
      Damages_component();
      Neteconomy();
      Welfare_component();
    }
    else{
      Gross_economy();
      Emission_component();
      CO2_cycle();
      Radiativeforcing();
      Climatedynamics();
      Damages_component();
      Neteconomy();
      Welfare_component();
    }
  }
}
CC_marginal<-function(){
  for(t in 1:Time_periods){
    year_current<<-t;
    if(t==simulation_year){
      Gross_economy();
      Emission_component();
      CO2_cycle();
      Radiativeforcing();
      Climatedynamics();
      Damages_component();
      Neteconomy_extarC();
      Welfare_component();
    }
    else{
      Gross_economy();
      Emission_component();
      CO2_cycle();
      Radiativeforcing();
      Climatedynamics();
      Damages_component();
      Neteconomy();
      Welfare_component();
    }
    for(t in 1:(Time_periods-1)){
      Discount_rate[t]<<-(1+prstp) * (CPC[t+1]/CPC[t])**(elasmu/tstep) - 1;
      Discount_factor[t+1]<<-Discount_factor[t]*((1/(1+Discount_rate[t]))^tstep);
    }
  }
}



