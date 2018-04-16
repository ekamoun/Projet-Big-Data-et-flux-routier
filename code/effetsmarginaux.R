setwd("C:/Users/Weld ommou/Desktop/weldou/Gendarmerie_Projet/donnees")

emna=read.csv2("emna.csv", header=TRUE, sep=";")
names(emna)


  agg0= rep(0.206,dim(emna)[1] )
  agg1= rep(-0.008, dim(emna)[1])
  agg2= rep(-0.146, dim(emna)[1])
  agg3= rep(-0.052, dim(emna)[1])

  sexe0= rep(0.027,dim(emna)[1] )
  sexe1= rep(-0.003, dim(emna)[1])
  sexe2= rep(-0.019, dim(emna)[1])
  sexe3= rep(-0.006, dim(emna)[1])
  
  age0= rep(0.0009,dim(emna)[1])
  age1= rep(-0.00008, dim(emna)[1])
  age2= rep(-0.0006, dim(emna)[1])
  age3= rep(-0.0001, dim(emna)[1])
  
  traj10= rep(-0.069,dim(emna)[1])
  traj11= rep(0.002, dim(emna)[1])
  traj12= rep(0.05, dim(emna)[1])
  traj13= rep(0.017, dim(emna)[1])
  
  
  traj20= rep(-0.082,dim(emna)[1])
  traj21= rep(Inf, dim(emna)[1])
  traj22= rep(0.06, dim(emna)[1])
  traj23= rep(0.022, dim(emna)[1])
  
  
  traj40= rep(0.216,dim(emna)[1])
  traj41= rep(-0.052, dim(emna)[1])
  traj42= rep(-0.134, dim(emna)[1])
  traj43= rep(-0.030, dim(emna)[1])
  
  traj50=rep(-0.082,dim(emna)[1])
  traj51= rep(0.004, dim(emna)[1])
  traj52= rep(0.058, dim(emna)[1])
  traj53= rep(0.019, dim(emna)[1])
  
  catusag0=rep(0.269, dim(emna)[1])
  catusag1=rep(0.065, dim(emna)[1])
  catusag2=rep(-0.2, dim(emna)[1])
  catusag3=rep(-0.134, dim(emna)[1])
  
  atmos0=rep(0.018, dim(emna)[1])
  atmos1=rep(-0.002, dim(emna)[1])
  atmos2=rep(-0.013, dim(emna)[1])
  atmos3=rep(-0.004, dim(emna)[1])
  
  lumi0=rep(0.025,dim(emna)[1] )
  lumi1=rep(-0.002, dim(emna)[1])
  lumi2=rep(-0.018, dim(emna)[1])
  lumi3=rep(-0.005, dim(emna)[1])
  
  
  emna_effets= read.csv2("emna_download.csv",header=TRUE, sep=",")
  head(emna_effets)
  names(emna_effets)
emna_effets=emna_effets[,-49]
  emna_effets=data.frame(emna_effets, agg0, agg1, agg2, agg3)  
names(emna_effets)  


emna_effets=data.frame(emna_effets, sexe0, sexe1, sexe2, sexe3)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, age0, age1, age2, age3)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj10, traj11, traj12, traj13)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj20, traj21, traj22, traj23)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj40, traj41, traj42, traj43)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj50, traj51, traj52, traj53)  
names(emna_effets) 


emna_effets=data.frame(emna_effets, catusag0, catusag1, catusag2, catusag3)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, atmos0, atmos1, atmos2, atmos3)  
names(emna_effets) 


emna_effets=data.frame(emna_effets, lumi0, lumi1, lumi2, lumi3)  
names(emna_effets) 


write.csv2(emna_effets, "emna_effects_all.csv", row.names = FALSE)


zone1=which((as.numeric(as.character(emna[,"lat"]))<= 48.02201) & (as.numeric(as.character(emna[,"lat"]))>=47.96627) & (as.numeric(as.character(emna[,"long"]))<=2.77849) & (as.numeric(as.character(emna[,"long"]))>=2.66017))

agg0[zone1]=0.168
agg1[zone1]=-0.051
agg2[zone1]= -0.097
agg3[zone1]= -0.020 

#sexe0[zone1]= 
#sexe1[zone1]= 
#sexe2[zone1]= 
#sexe3[zone1]= 

age0[zone1]= 0.003
age1[zone1]= -0.001
age2[zone1]= -0.001
age3[zone1]= -0.0002

#traj10[zone1]= 
#traj11[zone1]= 
#traj12[zone1]= 
#traj13[zone1]= 


#traj20[zone1]= 
#traj21[zone1]= 
#traj22[zone1]= 
#traj23[zone1]= 


#traj40[zone1]=
traj41[zone1]= -0.137
traj42[zone1]= -0.082
traj43[zone1]= -0.010

#traj50[zone1]=
#traj51[zone1]= 
#traj52[zone1]= 
#traj53[zone1]= 

catusag0[zone1]=0.322
#catusag1[zone1]=
catusag2[zone1]=-0.231
catusag3[zone1]=-0.077

#atmos0[zone1]=
#atmos1[zone1]=
#atmos2[zone1]=
#atmos3[zone1]=

lumi0[zone1]=0.035
lumi1[zone1]=-0.016
lumi2[zone1]=-0.017
lumi3[zone1]=-0.003


zone2=which((as.numeric(as.character(emna[,"lat"]))<= 47.96291) & (as.numeric(as.character(emna[,"lat"]))>=47.839124) & (as.numeric(as.character(emna[,"long"]))<=1.9643214) & (as.numeric(as.character(emna[,"long"]))>=1.8149166))

agg0[zone2]=0.126
agg1[zone2]=-0.027
agg2[zone2]= -0.085
agg3[zone2]= -0.014

#sexe0[zone2]= 
#sexe1[zone2]= 
#sexe2[zone2]= 
#sexe3[zone2]= 

age0[zone2]= 0.002
age1[zone2]= -0.0006
age2[zone2]= -0.001
age3[zone2]= -0.0001

#traj10[zone2]= 
traj11[zone2]= 0.015
#traj12[zone2]= 
#traj13[zone2]= 


#traj20[zone2]= 
#traj21[zone2]= 
#traj22[zone2]= 
#traj23[zone2]= 


#traj40[zone2]=
traj41[zone2]= -0.079
traj42[zone2]= -0.090
traj43[zone2]= -0.009

traj50[zone2]=-0.060
traj51[zone2]= 0.018
traj52[zone2]= 0.038
traj53[zone2]= 0.005

catusag0[zone2]=0.346
catusag1[zone2]=0.029
catusag2[zone2]=-0.284
catusag3[zone2]=-0.090

#atmos0[zone2]=
#atmos1[zone2]=
#atmos2[zone2]=
#atmos3[zone2]=

lumi0[zone2]=0.026
lumi1[zone2]=-0.008
lumi2[zone2]=-0.015
lumi3[zone2]=-0.002

emna_effets= read.csv2("emna_download.csv",header=TRUE, sep=",")
head(emna_effets)
names(emna_effets)
emna_effets=emna_effets[,-49]
emna_effets=data.frame(emna_effets, agg0, agg1, agg2, agg3)  
names(emna_effets)  


emna_effets=data.frame(emna_effets, sexe0, sexe1, sexe2, sexe3)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, age0, age1, age2, age3)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj10, traj11, traj12, traj13)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj20, traj21, traj22, traj23)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj40, traj41, traj42, traj43)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, traj50, traj51, traj52, traj53)  
names(emna_effets) 


emna_effets=data.frame(emna_effets, catusag0, catusag1, catusag2, catusag3)  
names(emna_effets) 

emna_effets=data.frame(emna_effets, atmos0, atmos1, atmos2, atmos3)  
names(emna_effets) 


emna_effets=data.frame(emna_effets, lumi0, lumi1, lumi2, lumi3)  
names(emna_effets) 

write.csv2(emna_effets, "emna_effects_all_zones.csv", row.names = FALSE)
