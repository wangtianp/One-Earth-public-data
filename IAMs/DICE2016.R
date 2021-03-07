clock_Current<-function(year_current){
  t=year_current; 
}
Time_periods = 39;
simulation_year=2;
fosslim=6000;
tstep=5;
t0=1;
ifopt=0;
#discount rate
elasmu=1.45;
prstp=0.03;

elasmu=0;
prstp=0.05; 

# Population and technology
gama=0.300;
pop0=7403;
popadj=0.134;
popasym=11500;
dk=0.1;
q0=105.5

###capital stock is recalibrated based on the IMF capital stock database
k0=143;  #$2005 market exchange rate

a0=5.115;
ga0=0.076;
dela=0.005;

#**economic parameters
saving_rate = 0.2;  #fixed saving rate, same with the FUND and PAGE model

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
t2xco2=3;
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

## load socio-economic data and emissions data from SSPs
EIND<-t(Dice_data[4,3:61]);
etree=t(Dice_data[1,3:61])*44/12;
forcoth=t(Dice_data[5,3:61]);  # other gases forcing simulated by the Hector model
YGROSS_base = matrix(nrow = Time_periods,ncol = 1)   #Gross world product GROSS of abatement and damages (trillions 2005 USD per year)
YGROSS_base=t(Dice_data[3,3:61]);
l = t(Dice_data[2,3:61]);

## calculate parameter al and K;
al[t0] = (YGROSS_base[t0]/(K[t0]^gama))/((l[t0]/1000)^(1-gama));
K_base = K;
C_base = (1-saving_rate)*YGROSS_base;

for(t in 1:(Time_periods-1)){
  K_base[t+1]  = (1-dk)**tstep * K_base[t] + tstep * YGROSS_base[t] * saving_rate
  al[t+1] = (YGROSS_base[t+1]/(K_base[t+1]^gama))/((l[t+1]/1000)^(1-gama));
}

for(t in 1:(Time_periods-1)){
  rr[t]=1/((1+prstp)**(tstep*(t-simulation_year)+1));
}

###main componenet of DICE model
Emission_component<-function(){
  t=clock_Current(year_current)
  E[t,1]<<- (EIND[t,1] + etree[t,1])*1;
}

Climatedynamics<-function(){
  #*Emissions and Damages
  #  EIND[t,1]=sigma[t,1] * YGROSS[t,1] * (1-(para[t]));
  t=clock_Current(year_current)
  MAT[t+1,1]<<- MAT[t,1]*b11 + MU[t,1]*b21 + (E[t,1]*(5/3.666));
  FORC[t+1,1]<<- fco22x * ((log((MAT[t+1,1]/588.000))/log(2))) + forcoth[t+1,1];
  ML[t+1,1] <<- ML[t,1]*b33  + MU[t,1]*b23;
  MU[t+1,1] <<- MAT[t,1]*b12 + MU[t,1]*b22 + ML[t,1]*b32;
  TATM[t+1,1] <<- TATM[t,1] + c1 * ((FORC[t+1,1]-(fco22x/t2xco2)*TATM[t,1])-(c3*(TATM[t,1]-TOCEAN[t,1])));
  TOCEAN[t+1,1] <<- TOCEAN[t,1] + c4*(TATM[t,1]-TOCEAN[t,1]);
}

DAMAGES = matrix(nrow = Time_periods,ncol = 1)     #Damages (trillions 2005 USD per year)
DAMFRAC = matrix(nrow = Time_periods,ncol = 1)    #Increase in temperature of atmosphere (degrees C from 1900)

Damages_component<-function(){
  t=clock_Current(year_current)
  DAMFRAC[t] <<- 1-1/(1+a1 * TATM[t] + a2 * TATM[t]^a3) ;
  DAMFRAC[t] <<- min(0.99,DAMFRAC[t]);
  #Define function for DAMAGES
  # GAMS Version
}

Neteconomy<-function(){
  t=clock_Current(year_current)
  YGROSS[t]<<- al[t]*((l[t]/1000)^(1-gama))*(K[t]^gama)*(1-DAMFRAC[t]);
  C[t] <<- (1-saving_rate)*YGROSS[t];
  K[t+1] <<- K[t]*(1-dk)^tstep + saving_rate*YGROSS[t]*tstep;
}


Playground_model<-function(){
  for(t in 1:(Time_periods-1)){
    year_current<<-t;
    Emission_component();
    Climatedynamics();
    Damages_component();
    Neteconomy();
  }
}


Extra_emission_model<-function(){
  for(t in 1:(Time_periods-1)){
    year_current<<-t;
    if(t==simulation_year){
      Emission_component();
      E[t] <<- E[t]+20;
      Climatedynamics();
      Damages_component();
      Neteconomy();
    }
    else{
      Emission_component();
      Climatedynamics();
      Damages_component();
      Neteconomy();
    }
  }
}
