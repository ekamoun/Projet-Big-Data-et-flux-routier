setwd("C:/Users/Weld ommou/Desktop/weldou/Gendarmerie_Projet/donnees")
carac=read.csv("new.csv", skip=0, sep=";")
#carac=read.csv("caracteristiques_all.csv", skip=0, sep=",")
# sum(is.na(carac[, "lat"]))
# sum(is.na(carac[, "long"]))### 0
# length(which(carac[,"long"]==""))
# length(which(carac[,"lat"]==""))####0 


# carac=data.frame(carac,rep(1,dim(carac)[1]))
# names(carac)[dim(carac)[2]]="Y"
head(carac)
tail(carac)
summary(carac)
dim(carac)


carac1=matrix(Inf, nrow=dim(carac)[1], ncol=dim(carac)[2])
colnames(carac1)=colnames(carac)
head(carac1)
a=1
b=0

for(i in 5:16){
  I=which(carac[,"an"]==i)
  b=a+length(I)-1
  for (j in 1:dim(carac)[2]){
  carac1[a:b,j]=as.vector(as.matrix(carac[I,])[,j])
  }
  a=b+1
}
head(carac1)
tail(carac1)
head(carac1[which(carac1[,"an"]==8),])
head(carac)

usag=read.csv("usagers_all.csv", header=FALSE, sep=",")
usag=data.frame(usag, as.factor((usag[,1]%/%100000000)%%2000))
colnames(usag)= c("Num_Acc","place","catu","grav","sexe","trajet","secu","locp","actp","etatp","an_nais","num_veh", "an")
head(usag)
tail(usag)
summary(usag)
dim(usag)
levels(usag[,"an"])
sum(is.na(usag[,"an_nais"]))

k=1
occ=matrix(Inf, nrow=dim(carac1)[1], ncol=2)
occ[,1]=carac1[,"Num_Acc"]
occ[,2]=1
occ1=as.numeric(occ[,2])
for (i in 2:dim(usag)[1]){
  if (usag[i,"Num_Acc"]!= usag[i-1,"Num_Acc"]){
    k=k+1
  }else{
    occ1[k]=occ1[k]+1
  }
}
sum(occ1)
dim(usag)[1]
occ[,2]=occ1



usag1=matrix(Inf, nrow=dim(usag)[1], ncol=(18))
colnames(usag1)=c("Num_Acc", "an", "mois", "jour","hrmn","Jour_semaine","lum","agg","atm","adr","long","lat","dep","catu","grav","sexe","trajet", "age")
head(usag1)
usag1[,"Num_Acc"]=usag[,"Num_Acc"]
usag1[,"catu"]=usag[,"catu"]
usag1[,"grav"]=usag[,"grav"]
usag1[,"sexe"]=usag[,"sexe"]
usag1[,"trajet"]=usag[,"trajet"]### des donnees qui maquent 0
usag1[,"an"]=usag[,"an"]
usag1[,"age"]=as.numeric(usag[,"an"])+4+2000-as.numeric(usag[,"an_nais"])
tail(usag1)


usag1[1,"mois"]=carac1[1,"mois"]
  usag1[1,"jour"]=carac1[1,"jour"]
  usag1[1,"hrmn"]=carac1[1,"hrmn"]
  usag1[1,"Jour_semaine"]=carac1[1,"Jour_semaine"]
  usag1[1,"lum"]=carac1[1,"lum"]
  usag1[1,"agg"]=carac1[1,"agg"]
  usag1[1,"atm"]=carac1[1,"atm"]
  usag1[1,"adr"]=carac1[1,"adr"]
  usag1[1,"lat"]=carac1[1,"lat"]
  usag1[1,"long"]=carac1[1,"long"]
  usag1[1,"dep"]=carac1[1,"dep"]


  k=1
  
for (i in 2:dim(usag)[1]){
  if (usag1[i,"Num_Acc"]!= usag1[i-1,"Num_Acc"]){
    k=k+1
  }
  usag1[i,"mois"]=carac1[k,"mois"]
  usag1[i,"jour"]=carac1[k,"jour"]
  usag1[i,"hrmn"]=carac1[k,"hrmn"]
  usag1[i,"Jour_semaine"]=carac1[k,"Jour_semaine"]
  usag1[i,"lum"]=carac1[k,"lum"]
  usag1[i,"agg"]=carac1[k,"agg"]
  usag1[i,"atm"]=carac1[k,"atm"]
  usag1[i,"adr"]=carac1[k,"adr"]
  usag1[i,"lat"]=carac1[k,"lat"]
  usag1[i,"long"]=carac1[k,"long"]
  usag1[i,"dep"]=carac1[k,"dep"]
}
  hist(usag1[,"trajet"])
  head(usag1)
  tail(usag1)
  dim(usag1)
  usag1frame=data.frame(usag1)
  dim(usag1frame)
  head(usag1frame)
  tail(usag1frame)
  write.table(usag1frame, "usag11.txt", row.names=FALSE, col.names = TRUE, quote=FALSE, sep=";")
d=read.table("usag11.txt", sep=";")
head(d)
dim(d)
d[1025,]
  k=1
  occcc=matrix(Inf, nrow=dim(carac1)[1], ncol=2)
  occcc[,1]=carac1[,"Num_Acc"]
  occcc[,2]=0
  occ3=as.numeric(occcc[,2])
  if (usag1[1,"catu"]==1){
    occ3[1]=occ3[1]+1
  }
  
  for (i in 2:dim(usag1)[1]){
    if (usag1[i,"Num_Acc"]!= usag1[i-1,"Num_Acc"]){
      k=k+1
    }
    if (usag1[i,"catu"]==1){
      occ3[k]=occ3[k]+1
    }
  }
  occcc[,2]=occ3
  sum(occ3)

  
  

# grav=matrix(Inf, nrow=dim(carac)[1], ncol=3)
# grav[1,1]=usag[1,"Num_Acc"]
# grav[1,2]=usag[1,"grav"]
# k=2
# for (i in 2:dim(usag)[1]){
#   if (usag[i,"Num_Acc"]!= usag[i-1,"Num_Acc"]){
#     grav[k,1]=usag[i,"Num_Acc"]
#     grav[k,2]=usag[i,"grav"]
#     k=k+1
#   }
# }
# grav[,3]= (grav[,1]%/%100000000)%%2000
# dim(grav)
# 
# grav1=rep(Inf, dim(carac)[1])
# length(grav1)
# 
# a=1
# b=0
# for(i in 5:16){
# I=which(carac[,"an"]==i)
# b=a+length(I)-1
# grav1[I]=grav[a:b,2]
# a=b+1
# }
# 
# 
# carac=data.frame(carac, as.factor(grav1))
# names(carac)[dim(carac)[2]]="gravite"
# grav2=matrix(Inf, nrow=dim(carac)[1], ncol=1)
# grav2=grav1
# which(grav1==2)
# grav2[-which(grav1==2)]=1 ### 2 tué 1 autres
# grav2=grav2-1  ### 1 tué 0 autres
# carac=data.frame(carac, as.factor(grav2))
# names(carac)[dim(carac)[2]]="gravite2"
# head(carac)
# levels(carac[,"gravite2"])
# 
# sum(is.na(carac[,"atm"]))
# carac[is.na(carac[,"atm"]), "atm"]=1
  # sum(is.na(carac[,"atm"]))
  # carac[is.na(carac[,"atm"]), "atm"]=1
  



lieu=read.csv("lieux_all.csv", header=FALSE, sep=",")
lieu=data.frame(lieu, as.factor((lieu[,1]%/%100000000)%%2000))
colnames(lieu)= c("Num_Acc","catr","voie","v1","v2","circ","nbv","pr","pr1","vosp","prof","plan","lartpc","larrout","surf","infra","situ","env1", "an")
head(lieu)
tail(lieu)
summary(lieu)
dim(lieu)


vehic=read.csv("vehicules_all.csv", header=FALSE, sep=",")
vehic=data.frame(vehic, as.factor((vehic[,1]%/%100000000)%%2000))
colnames(vehic)= c("Num_Acc","senc","catv","occutc","obs","obsm","choc","manv","num_veh", "an")
head(vehic)
tail(vehic)
summary(vehic)
dim(vehic)

k=1
occc=matrix(Inf, nrow=dim(carac1)[1], ncol=2)
occc[,1]=carac1[,"Num_Acc"]
occc[,2]=1
occ2=as.numeric(occc[,2])
for (i in 2:dim(vehic)[1]){
  if (vehic[i,"Num_Acc"]!= vehic[i-1,"Num_Acc"]){
    k=k+1
  }else{
    occ2[k]=occ2[k]+1
  }
}
sum(occ2)
dim(vehic)[1]
occc[,2]=occ2

length(occc[,1])
length(occcc[,1])
sum(occc[,1]==occcc[,1])##memes accidentsdans les deux bases vehic et usag1

occc[1,2] ###nombre de véhicules associés à l'accident 1 
occcc[1,2] ## nombre de conducteurs associés à l'accident 1
 occ2[1]
 occ3[1]
 sum(occ2)
 sum(occ3)
length(which(occ2!=occ3))
vehic1=vehic
vehic1[-which(vehic1[,"Num_Acc"]==usag1[which(occ2!=occ3), "Num_Acc"]),]



moto= which(((vehic[,"catv"]<=6)&(vehic[,"catv"]>=1))|((vehic[,"catv"]<=36)&(vehic[,"catv"]>=30)))
voiture=which((vehic[,"catv"]==7)|(vehic[,"catv"]==10))
big_vehic=which(((vehic[,"catv"]<=17)&(vehic[,"catv"]>=11))|(vehic[,"catv"]==8)|(vehic[,"catv"]==9)|(vehic[,"catv"]==20)|(vehic[,"catv"]==21))
transp_com=which((vehic[,"catv"]==18)|(vehic[,"catv"]==19)|(vehic[,"catv"]==37)|(vehic[,"catv"]==38)|(vehic[,"catv"]==39)|(vehic[,"catv"]==40))
autres=which(vehic[,"catv"]==99)
vect=c(length(moto),length(voiture),length(big_vehic),length(transp_com),length(autres))/length(vehic[,"catv"])

length(vehic[,"catv"])
length(usag[,"catu"])
length(which(usag[,"catu"]==1))+length(which(usag[,"catu"]==4))+length(which(usag[,"catu"]==2))



gravite=matrix(Inf, nrow=dim(usag1)[1], ncol=1)
gravite=as.numeric(as.vector(usag1[,"grav"]))
# gravite[which((gravite==2)|(gravite==3))]=2
# gravite[-which((gravite==2)|(gravite==3))]=1 ### 2 tué 1 autres
gravite[which(gravite==2)]=2
gravite[-which(gravite==2)]=1
gravite=as.numeric(gravite)-1  ### 1 tué 0 autres
usag1=data.frame(usag1, as.factor(gravite))
names(usag1)[dim(usag1)[2]]="gravite"
head(usag1)
levels(usag1[,"gravite"])
usag1[,"gravite"]=as.factor(gravite)

sum(is.na(usag1[,"atm"]))
usag1[is.na(usag1[,"atm"]), "atm"]=1
I1=which(usag1[,"atm"]==" 1")
usag1[I1,"atm"]="1"

I1=which(usag1[,"atm"]==" 2")
usag1[I1,"atm"]="2"

I1=which(usag1[,"atm"]==" 3")
usag1[I1,"atm"]="3"

I1=which(usag1[,"atm"]==" 4")
usag1[I1,"atm"]="4"

I1=which(usag1[,"atm"]==" 5")
usag1[I1,"atm"]="5"

I1=which(usag1[,"atm"]==" 6")
usag1[I1,"atm"]="6"

I1=which(usag1[,"atm"]==" 7")
usag1[I1,"atm"]="7"

I1=which(usag1[,"atm"]==" 8")
usag1[I1,"atm"]="8"

I1=which(usag1[,"atm"]==" 9")
usag1[I1,"atm"]="9"

sum(is.na(usag1[,"lum"]))
sum(is.na(usag1[,"agg"]))
sum(is.na(usag1[,"mois"]))
sum(is.na(usag1[,"jour"]))
sum(is.na(usag1[,"hrmn"]))
sum(is.na(usag1[,"Jour_semaine"]))
# t=matrix(Inf, nrow=dim(usag)[1], ncol=(16))
# t=usag1
# colnames(t)=c("Num_Acc", "an", "mois", "jour","hrmn","lum","agg","atm","long","lat","dep","catu","grav","sexe","trajet", "age")
# head(t)
# t=t[-I1,]
sum(is.na(usag1[,"dep"]))
I1=which(is.na(usag1[,"dep"])==TRUE)
#usag1=na.omit(usag1[I1,])
usag1=usag1[-I1,]
sum(is.na(usag1[,"sexe"]))
sum(is.na(usag1[,"trajet"]))
I2=which(is.na(usag1[,"trajet"])==TRUE)
usag1=usag1[-I2,]
length(which(usag1[,"trajet"]=="0"))

sum(is.na(usag1[,"catu"]))
sum(is.na(usag1[,"grav"]))
sum(is.na(usag1[,"age"]))
I3=which(is.na(usag1[,"age"])==TRUE)
usag1[I3, "age"]=round(mean(as.numeric(usag1[-I3,"age"])))
dim(usag1)
names(usag1)
I4=which(usag1[,"adr"]=="")
usag1=usag1[-I4,]


usag2=matrix(Inf, nrow=dim(usag1)[1], ncol=dim(usag1)[2])
colnames(usag2)=c("Num_Acc", "an", "mois", "jour","hrmn","Jour_semaine", "lum","agg","atm","adr","long","lat","dep","catu","grav","sexe","trajet", "age")
usag2=usag1
head(usag2)
I5=which(usag2[,"trajet"]=="0")
usag2=usag2[-I5,]
dim(usag2)

usag2[which(usag2[,"atm"]==" 1"),"atm"]=1
usag2[which(usag2[,"atm"]==" 2"),"atm"]=2
usag2[which(usag2[,"atm"]==" 3"),"atm"]=3
usag2[which(usag2[,"atm"]==" 4"),"atm"]=4
usag2[which(usag2[,"atm"]==" 5"),"atm"]=5
usag2[which(usag2[,"atm"]==" 6"),"atm"]=6
usag2[which(usag2[,"atm"]==" 7"),"atm"]=7
usag2[which(usag2[,"atm"]==" 8"),"atm"]=8
usag2[which(usag2[,"atm"]==" 9"),"atm"]=9
levels(usag2[,"atm"])

l=levels(as.factor(usag2[,"dep"]))
tab= matrix(Inf, nrow=length(l), ncol=3)
tab[,1]=l
for (i in 1:length(l)){
  I=which(as.factor(usag1[, "dep"])==l[i])
  tab[i,2]=length(I)
  II=which(as.factor(usag2[, "dep"])==l[i])
  tab[i,3]=length(II)
}


sum(is.na(usag2[,"dep"]))
length(which(usag2[,"adr"]==""))
tab[,2]=as.numeric(tab[,2])
l[which.max(as.numeric(tab[-76,2]))]
s=sort(as.numeric(tab[,2]),decreasing = TRUE )[1:11]

l[which.max(as.numeric(tab[-76,3]))]
s=sort(as.numeric(tab[,3]),decreasing = TRUE )
s
w=which(tab[,3]=="39607")
I=which(usag2[,"dep"]==tab[w,1]) ##dep 72
length(I)
usag2[I,"adr"]

depart=which(usag2[,"dep"]==tab[w,1])
length(depart)
liste_adr=usag2[depart,"adr"]
liste_adr=paste(liste_adr, "39 Jura France", sep=",")
length(liste_adr)

write.table(liste_adr, "liste_adr_39.txt", row.names=FALSE, col.names = FALSE, quote=FALSE)

library(dismo)
library(XML)
gps=0
c.gps=matrix(Inf, nrow=length(liste_adr), ncol=2)
for (i in (1:207)){
  gps<- geocode(liste_adr[i], oneRecord=TRUE)
  c.gps[i,1]= as.numeric(gps["longitude"])
  c.gps[i,2]= as.numeric(gps["latitude"])
}
i=30
i ## la prochaine fois on part de i=208
sum(is.na(c.gps[,2]))
usag2[depart, "longitude"]=c.gps[,1]
usag2[depart, "latitude"]=c.gps[,2]
head(usag2[depart,])

write.table(c.gps, "liste_gps_39(1).txt", row.names=FALSE, col.names = FALSE, quote=FALSE)
d=read.table("liste_gps_39.txt")



c.gps[1:500,]

require(devtools)  
devtools::install_github(repo = 'rCarto/photon') 


# res.glm=glm(gravite~as.factor(sexe)+as.factor(lum)+as.factor(mois)+as.numeric(age)+as.factor(Jour_semaine),family=binomial(link = "probit"),data=usag2[depart,])
# s=step (res.glm, scope=list(upper=~as.factor(atm)+as.factor(catu)+as.factor(sexe)+as.factor(lum)+as.factor(mois)+as.numeric(age)+as.factor(Jour_semaine),lower=~1), direction="both", k = 2) 
# 
# 
# res.glm=glm(gravite ~ as.factor(sexe)+as.factor(trajet)+as.factor(agg)+as.factor(lum) + as.factor(mois) + as.numeric(age) + 
#               as.factor(catu)+as.factor(Jour_semaine),family=binomial(link = "logit"),data=usag1[depart,])

######### effet fixe 
X= cbind(as.factor(usag2[,"Num_Acc"]),as.factor(usag2[,"dep"]),as.factor(usag2[,"sexe"]), as.factor(usag2[,"trajet"]), usag2[,"age"],
         as.factor(usag2[,"catu"]), as.factor(usag2[,"Jour_semaine"]), as.factor(usag2[,"agg"]), as.factor(usag2[,"lum"]), as.factor(usag2[,"mois"]))
Y=as.factor(usag2[,"grav"])
library(nlme)
lme1=lme(Y~Num_Acc+dep, fixed = TRUE, data=usag2[depart,])
########### ologit 
library(rms)
ologit=lrm(Y~as.factor(usag2[,"Num_Acc"])+as.factor(usag2[,"dep"])+as.factor(usag2[,"sexe"])+ as.factor(usag2[,"trajet"])+ usag2[,"age"],
           as.factor(usag2[,"catu"])+ as.factor(usag2[,"Jour_semaine"])+ as.factor(usag2[,"agg"])+ as.factor(usag2[,"lum"])+ as.factor(usag2[,"mois"]),data=usag2[depart,] )
print(ologit)
########### oprobit 
gravit=rep(0,nrow=dim(usag2)[1])
gravit[which(usag2[,"grav"]==1)]=0
gravit[which(usag2[,"grav"]==2)]=3
gravit[which(usag2[,"grav"]==3)]=2
gravit[which(usag2[,"grav"]==4)]=1
usag2=data.frame(usag2, as.factor(gravit))
names(usag2)[dim(usag2)[2]]="gravit"
heure= as.numeric(usag2[,"hrmn"])%/%100
usag2=data.frame(usag2, as.factor(heure))
names(usag2)[dim(usag2)[2]]="heure"
weekend=rep(0,dim(usag2)[1])
weekend[which((usag2[,"Jour_semaine"]=="Dimanche")|(usag2[,"Jour_semaine"]=="Samedi"))]=1
usag2=data.frame(usag2, as.factor(weekend))
names(usag2)[dim(usag2)[2]]="weekend"

grnds_vacs=matrix(0, nrow=dim(usag2)[1], ncol=1)
grnds_vacs[which((usag2[,"mois"]==8)|(usag2[,"mois"]==6)|(usag2[,"mois"]==7))]=1

fev=matrix(0, nrow=dim(usag2)[1], ncol=1)
fev[which(usag2[,"mois"]==2)]=1

oct=matrix(0, nrow=dim(usag2)[1], ncol=1)
oct[which(usag2[,"mois"]==10)]=1

usag2[,'sexe']=as.numeric(usag2[,"sexe"])-1
usag2[,'agg']=as.numeric(usag2[,"agg"])-1

traj1=matrix(0, nrow=dim(usag2)[1], ncol=1)
traj1[which(usag2[,"trajet"]==1)]=1

traj2=matrix(0, nrow=dim(usag2)[1], ncol=1)
traj2[which(usag2[,"trajet"]==2)]=1

traj3=matrix(0, nrow=dim(usag2)[1], ncol=1)
traj3[which(usag2[,"trajet"]==3)]=1

traj4=matrix(0, nrow=dim(usag2)[1], ncol=1)
traj4[which(usag2[,"trajet"]==4)]=1

traj5=matrix(0, nrow=dim(usag2)[1], ncol=1)
traj5[which(usag2[,"trajet"]==5)]=1


catusag=matrix(0, nrow=dim(usag2)[1], ncol=1)
catusag[which((usag2[,"catu"]==1)|(usag2[,"catu"]==2))]=1

atmos=matrix(0, nrow=dim(usag2)[1], ncol=1)
atmos[which(usag2[,"atm"]==2)]=1
atmos[which(usag2[,"atm"]==3)]=2
atmos[(usag2[,"atm"]==4)]=3

lumi=matrix(0, nrow=dim(usag2)[1], ncol=1)
lumi[which(usag2[,"lum"]==5)]=1
lumi[which(usag2[,"lumm"]==2)]=2
lumi[which(usag2[,"lum"]==1)]=3



      
depart=which(usag2[,"dep"]==720)

write.csv(usag2[depart,], "usag2_72.csv", row.names=FALSE)

install.packages("MASS")
library(MASS)

oprobit <- polr(as.ordered(gravit)~
                  as.factor(mois)+as.factor(Jour_semaine)+
                  as.factor(sexe)+as.factor(trajet)+as.numeric(age) +
                  as.factor(catu)+as.factor(agg),data=usag2[depart,])
fitted=predict(oprobit, newdata=usag2[depart,], arg="class")
#colMeans(fitted)
usag2[depart, "grav"][246]
summary(oprobit)

library(nlme)
lme1=lme(as.factor(gravite)~Num_Acc+dep, fixed = TRUE, data=usag1[depart,])

######### glm 
gravite=matrix(Inf, nrow=dim(usag1)[1], ncol=1)
gravite=as.numeric(as.vector(usag1[,"grav"]))
# gravite[which((gravite==2)|(gravite==3))]=2
# gravite[-which((gravite==2)|(gravite==3))]=1 ### 2 tué 1 autres
gravite[which((usag1[,"grav"]==2)|(usag1[,"grav"]==3))]=2
gravite[-which((usag1[,"grav"]==2)|(usag1[,"grav"]==3))]=1
gravite=as.numeric(gravite)-1  ### 1 tué 0 autres
usag1=data.frame(usag1, as.factor(gravite))
names(usag1)[dim(usag1)[2]]="gravite"
usag1[,"gravite"]=gravite
depart=which(usag1[,"dep"]==720)

weekend=rep(0,dim(usag1)[1])
weekend[which((usag1[,"Jour_semaine"]=="Dimanche")|(usag1[,"Jour_semaine"]=="Samedi"))]=1
usag1=data.frame(usag1, as.factor(weekend))
names(usag1)[dim(usag1)[2]]="weekend"
depart=which(usag1[,"dep"]==720)
write.csv(usag1[depart,], "usag1_72.csv", row.names=FALSE)

res.glm=glm(gravite ~ as.factor(mois)+as.factor(Jour_semaine)+
              as.factor(sexe)+as.factor(trajet)+as.numeric(age) +
              as.factor(catu)+as.factor(agg),family=binomial(link = "probit"),data=usag1[depart,])

summary(res.glm)

A1=as.numeric((res.glm$fitted.values)>0.5)
sum(as.numeric(res.glm$fitted.values)>0.5)
length(A1)
A2=as.numeric(usag1[depart,"gravite"])
sum(as.numeric(usag1[depart,"gravite"])-1)
length(A2)
max(A1-A2)

(1-length(which(A2==A1))/length(A2))*100
B1=as.numeric(predict(res.glm,newdata=usag1[1:100,],type="response")>0.5)
B2=usag1[1:100,"gravite"]
(1-length(which(B2==B1))/length(B2))*100

# polr stands for proportional odds logistic regression (ordered logit)

coeffs <- oprobit$coefficients
cutpoints <- oprobit$zeta
ocoeffs <- c(coeffs,cutpoints)
ocovmat <- solve(ologit$Hessian)


library(car)
scatterplot(carac)

# carac=as.matrix(carac)
# carac=na.omit(carac)
# I=which(as.numeric(carac[,"long"])!=0)
# hist(as.numeric(carac[I,"long"]))
# 
# I=which(as.numeric(carac[,"lat"])!=0)
# hist(as.numeric(carac[I,"lat"]))
# 
# 
# hist(c(as.numeric(carac[I,"lat"]), as.numeric(carac[I,"long"])))

# l1=levels(as.factor(carac[,"an"]))
# for (j in (1:length(l1))){
#   I1= which(as.factor(carac[,"an"])==l1[j])
#   l=levels(as.factor(carac[I1,"lum"]))
#   W=matrix(Inf, nrow=length(l), ncol=3)
#   W[,1]=rep(l1[j], length(I1))
#   W[,2]=l
#   for(i in (1:length(l))){
#     W[i,2]=length(which(as.factor(carac[,"lum"])==l[i]))
#   }
#   
# }


# library(scatter)
# axe=c(l2)




l1=levels(as.factor(usag[,"an"]))
l2=levels(as.factor(usag[,"grav"]))
t=table(as.factor(usag[,"an"]), as.factor(usag[,"grav"]))
plot(l2, as.vector(t[1,]), type="p")
for (j in (2:length(l2))){
lines(l2, as.vector(t[j,]), type="p")
}

min(t)
max(t)

plot((as.numeric(l1)+2000), as.vector(t[,1]), type="l", ylim=c(2000,90000), xlab="Année", ylab="Nombre d'accidents", lwd=2)
for (j in (2:length(l2))){
  lines((as.numeric(l1)+2000), as.vector(t[,j]), type="l", col=j, lwd=2)

}

legend("topright",legend=c("Indemne","Tué", "Blessé hospitalisé", "Blessé léger"),
       col=1:length(l2), lty=1, cex=0.8, lwd=2)
title("Évolution du nombre de victimes")
m=apply(t,2,mean)
n=apply(t, 2, sum)
s=(n-1)/n*apply(t,2,var)

###### test d'égalité des moyennes :

m=apply(t,2,mean)
n=apply(t, 2, sum)
s=(n-1)/n*apply(t,2,var)

#### test de student
i=1
  j=3
  
t.test(t[,i], t[,j],  alternative=c("two.sided"))


# Si la valeur absolue de t (|t|) est supérieure à la valeur critique, 
# alors la différence est significative. Dans le cas contraire, elle, 
# ne l'est pas. Le degré de siginificativité ou p-value correspond 
# au risque indiqué par la table de Student pour la valeur |t|

# si p-value < alpha =5%, on va rejeter l'hypothèse H0(en faveur de H1)
# si p-value > alpha =5%, on va rejeter H1 (en faveur de H0).

#### test de welch
i=2
  j=4
  
stat= (m[i]-m[j])/ sqrt(s[i]/n[i]+s[j]/n[j])
d=(s[i]/n[i]+s[j]/n[j])*(s[i]/n[i]+s[j]/n[j])/(s[i]*s[i]/(n[i]*n[i]*(n[j]-1))+ s[j]*s[j]/(n[j]*n[j]*(n[i]-1)))
qt(0.05, d) 
# Le test t de Welch est une adaptation du test t de Student 
# permettant de comparer deux groupes d'échantillons indépendants, 
# lorsque les variances sont différentes.




###### test d'indépendance :
t=table(as.factor(carac[,"atm"]), as.factor(carac[,"lum"]))
I=which((usag[,"catu"]==1))
age =2000+as.numeric(usag[I,"an"])+4- as.numeric(usag[I,"an_nais"])
plot(as.factor(usag[I,"grav"]), age, xlab="Gravité de l'accident", ylab="Âge du conducteur", main="Relation entre gravité de l'accident et âge du conducteur")
grid()
library(ggplot2)
t=table(as.factor(usag[I,"sexe"]), as.factor(usag[I,"grav"]))
plot(as.factor(usag[I,"sexe"]), as.factor(usag[I,"grav"]), xlab="Sexe", ylab="Gravité", main="gravité vs. sexe")
#???ggplot(as.factor(usag[I,"sexe"]), as.factor(usag[I,"grav"]), aes(x=as.factor(usag[I,"sexe"]), y=as.factor(usag[I,"grav"]))
I=which((usag[,"catu"]==1))
heure= as.numeric(carac[,"hrmn"])%/%100
heure1=rep(Inf, length(I))
for (i in I){
  I1= which(carac[,"an"]==usag[i,"an"])
  j=which(carac[I1,"Num_Acc"]==usag[i,"Num_Acc"])
  heure1[i]=heure[j]
}

plot(as.factor(usag[I,"traj"]), heure1, xlab="trajet", ylab="heure", main="trajet vs. heure")


t=table(as.factor(heure), as.factor(usag[,"trajet"]))
chisq.test(t)
# En général on accepte l'hypothèse d'indépendance lorsque p-value 
# est supérieure à 5 % (0,05).



l=levels(as.factor(carac[,"dep"]))
tab= matrix(Inf, nrow=length(l), ncol=3)
tab[,1]=l
for (i in 1:length(l)){
  I=which(as.factor(carac[, "dep"])==l[i])
  tab[i,2]=length(I)
  tab[i,3]=length(which(carac[I, "adr"]!=""))
}


sum(is.na(carac[,"dep"]))
length(which(carac[,"adr"]==""))
tab[,2]=as.numeric(tab[,2])
l[which.max(as.numeric(tab[-76,2]))]
s=sort(as.numeric(tab[,2]),decreasing = TRUE )[1:11]
sum(as.numeric(tab[,2]))
dim(carac)
sum(is.na(carac[,"dep"]))

s=sort(as.numeric(tab[,2]),decreasing = FALSE )
w=rep(Inf, 101)
for (i in 1:101){
w[i]=which(as.numeric(tab[,2])==s[i])
}
########### sort le tableau des adresses 
tab[w,]
names(tab)[1:3]=c("num département", "occurrence", "nombre d'adresses")
write.csv(tab[w,], "departements.csv",row.names=FALSE, col.names = FALSE, quote=FALSE)
I=which(carac[, "dep"]==650)
length(I)
#J=which((as.character(carac[I, "adr"]))!="")
#length(J)
#length(which(carac[J, "adr"]!=""))
#liste_adr=carac[which(carac[J, "adr"]!=""),"adr"]
liste_adr=carac[I,"adr"]
liste_adr=paste(liste_adr, "65 Hautes-Pyrénées France", sep=" ")
length(liste_adr)
output <- liste_adr;
capture.output(output, file="liste_adr.txt")

write.csv(liste_adr, "liste_adr.csv")
write.table(liste_adr, "liste_adr.txt", row.names=FALSE, col.names = FALSE, quote=FALSE)

length(liste_adr)

library(dismo)
library(XML)
gps=0
c.gps=matrix(Inf, nrow=length(liste_adr), ncol=2)
for (i in (1:length(liste_adr))){
gps<- geocode(liste_adr[i], oneRecord=TRUE)
c.gps[i,1]= as.numeric(gps["longitude"])
c.gps[i,2]= as.numeric(gps["latitude"])
}

geocode("BAGNERES (RUE DE) 65 Hautes-Pyrénées France", oneRecord=TRUE)

c.gps[1:500,]

require(devtools)  
devtools::install_github(repo = 'rCarto/photon') 


test2=read.csv("lozere.txt", header =FALSE, sep=";")
head(test2)
dim(test2)
as.matrix(test2)[1,1]
dim(test)
geocode(liste_adr[1], limit = 1, key = "place")

address <- c("19 rue Michel Bakounine, 29600 Morlaix, France",
             "5 rue Proudhon, 34130 Mauguio France",
             "2 Emma Goldmanweg, Tilburg, Netherlands",
             "36 Strada Panait Israti, Bucarest, Romania")
place <- geocode(address, limit = 1, key = "place")
place


hist(as.factor(carac[, "dep"]))
I=which((carac[,"mois"]==8)& (carac[,"an"]==16))
length(I)

hist(carac[I, "jour"], main="Histogramme des accidents par jour", xlab="jour")
hist(carac[, "mois"], main="Histogramme des accidents par jour", xlab="mois")
title("Histogramme des accidents par jour")
hist(carac[, "lum"], main="Histogramme de la variable lumière", xlab = "niveaux de lumière")
hist(usag[, "sexe"])
hist(usag[, "grav"])
h=hist(usag[,"trajet"], main="Histogramme des trajets", xlab = "trajets")

library(RColorBrewer)

cc =colors()
cc= cc[c(550,552,574,589,652,52,47,31,24,27,116,496,260,142,55,640,18,19,514,530,569)] # ou plus simplement
dd = colors()[c(550,552,574,589,652,52,47,31,24,27,116,496,260,142,55,640,18,19,514,530,569)]
palette(dd) 

palette(grey(0:10/10)) 
################## nombre d'accidents par mois pour chaque année


I=matrix(Inf, nrow=12, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==(l+4)))
  for(k in 1:12){
    I[k,l]= length(which(carac[I1, "mois"]==k))
  }
}

plot(1:12,apply(I,1,mean), xlab="Mois", ylab="Nombre d'accidents", type="l", col="blue")
title("Profil moyen sur les années 2005-2016 des accidents par mois")
grid()

plot(2005:2016,apply(I,2,mean), xlab="mois", ylab="nombre d'accidents", type="l", col="blue")
title("Profil moyen des accidents par mois")
mean(I)#nombre d'accidents moyen par mois

I= which(carac[, "an"]==5)
II=rep(Inf, 12)
for (i in 1:12){
  II[i]= length(which(carac[I, "mois"]== i))
}

sum(is.na(carac[I,"mois"]))
hist(carac[I,"mois"], xlim=c(1,12))$count

plot(1:12, II, type="l", ylim=c(3000,9000), col=5, xlab="mois", ylab="nombre d'accidents")
plot(c("jan", "fev", "mars", "avr", "mai", "juin", "juil", "aout", "sep", "oct", "nov", "dec"), II, type="l", ylim=c(3000,9000), col=5, xlab="mois", ylab="nombre d'accidents")

A <- gl(12,1,12,labels=c("jan", "fév", "mars", "avr", "mai", "juin", "juil", "août", "sep", "oct", "nov", "déc"))
rownames(data) <- levels(A)
barplot(II,names.arg=levels(A), ylim=c(0,9000), col="blue", ylab="Nombre d'accidents", xlab="Mois", main="Nombre d'accidents par mois en 2009")
j=9
for (j in 6:16){
  I= which(carac[, "an"]==j)
  II=rep(Inf, 12)
  for (i in 1:12){
    II[i]= length(which(carac[I, "mois"]== i))
  }
  lines(1:12, II, type="l", col=j+1)
}

legend("topright",legend=5:16,
       col=5:16, lty=1, cex=0.8)
title("nombre d'accidents par mois pour chaque année")
j=9
############################ nombre d'accidents par jour pour chaque mois pour l'année 5
I= which(carac[, "an"]==15)
II= rep(Inf, 28)
for (i in 1:28){
  II[i]= length(which((carac[I, "jour"]== i)& (carac[I, "mois"]== 1)))
}
##A <- gl(28,1,28,labels=c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28))
A1=gl(28,1,28,labels=1:28)
barplot(II, names.arg=levels(A1),ylim=c(0,300), col="blue", ylab="Nombre d'accidents", xlab="Jour", main="Nombre d'accidents par jour en janvier 2006")

III=which(carac[I,"mois"]==12)
length(III)/(31)

III=which(carac[I,"mois"]==2)
length(III)/(28)

plot(1:31, II, type="l", ylim=c(0,300), col=1)
for (j in 2:12){
  II=rep(Inf, 31)
  for (i in 1:31){
    II[i]= length(which((carac[I, "jour"]== i)& (carac[I, "mois"]== j)))
  }
  lines(1:31, II, type="l", col=j)
}


########################  nombre d'accidents (moyen sur les années) par jour pour chaque mois
I= which(carac[, "mois"]==1)
II= matrix(Inf, nrow=31, ncol=16)
III= matrix(Inf, nrow=31, ncol=12)
for (j in 5:16){
  for (i in 1:31){
    II[i,j]= length(which((carac[I, "jour"]== i)& (carac[I, "an"]== j)))
  }
}

plot(1:31,apply(II[,-(1:4)],1,mean), col=1, type="l", ylim=c(0,300))
III[,1]= apply(II[,-(1:4)],1,mean)
for (k in 2:12){
  I= which(carac[, "mois"]==k)
  II= matrix(Inf, nrow=31, ncol=16)
  for (j in 5:16){
    for (i in 1:31){
      II[i,j]= length(which((carac[I, "jour"]== i)& (carac[I, "an"]== j)))
    }
  }
  III[,k]=apply(II[,-(1:4)],1,mean)
  lines(1:31,III[,k], col=k)
}

legend("topright",legend=1:12,
       col=1:12, lty=1, cex=0.8)

mean(apply(III,1,mean)) ##nombre d'accidents moyen par jour 


 ###################################### Nombre d'accidents moyen par heure (moyenne sur
######################################## sur année, mois et jour=)
IIII=matrix(Inf, nrow=24, ncol=1)
for (i in 1:24){
  IIII[i]= length(which(heure==i))
}
IIII=IIII/(12*12*30)
head(IIII)

plot(IIII, type="l")

IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==l))
III=matrix(Inf, nrow=24, ncol=12)
for(k in 1:12){
I= which(carac[I1, "mois"]==k)
II=matrix(Inf, nrow=24, ncol=31)
for (j in 1:31){
for (i in 1:24){
  II[i,j]= length(which((heure[I]== i)&(carac[I,"jour"]==j)))
}
}
III[,k]=apply(II[,-31],1,mean)
}
IIII[,l]=apply(III,1,mean)
}

plot(1:24,apply(IIII,1,mean), xlab="Heure de la journée", ylab="Nombre d'accidents", type="l")
title("Nombre d'accidents moyen en une journée par heure")
grid()

mean(apply(IIII,1,mean)) ##nombre d'accidents moyen par heure 

IIII=matrix(Inf, nrow=24, ncol=1)
for (i in 1:24){
  IIII[i]= length(which(heure==i))
}
IIII=IIII/(12*12*31)
head(IIII)

plot(1:24,IIII, xlab="Heure de la journée", ylab="Nombre d'accidents", type="l")
title("Nombre d'accidents moyen en une journée par heure")
grid()

IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==(l+4)))
  III=matrix(Inf, nrow=24, ncol=12)
  for(k in 1:12){
    I= which(carac[I1, "mois"]==k)
    II=matrix(0, nrow=24, ncol=1)
      for (i in 0:23){
        II[i+1]= length(which((heure[I]== i)&((carac[I,"Jour_semaine"]=="Dimanche")|(carac[I,"Jour_semaine"]=="Samedi"))))
        }
    III[,k]=II/8
    }
  IIII[,l]=apply(III,1,mean)
}

IIIII=matrix(Inf, nrow=24, ncol=12)
IIIII[1:19,]=IIII[-(1:5),]
IIIII[20:24,]=IIII[(1:5),]
A=gl(24,1,24,labels=c("5h","6h","7h","8h","9h","10h","11h","12h","13h","14h","15h","16h","17h","18h","19h","20h","21h","22h","23h","00h","1h","2h","3h","4h"))
barplot(apply(IIIII,1,mean), names.arg=levels(A), ylim=c(0,16), ylab="Nombre d'accidents", xlab="Heure", main="Évolution moyenne par heure du nombre d'accidents le weekend (2005-2016)", col="blue")

IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==(l+4)))
  III=matrix(Inf, nrow=24, ncol=12)
  for(k in 1:12){
    I= which(carac[I1, "mois"]==k)
    II=matrix(Inf, nrow=24, ncol=1)
    for (i in 0:23){
      #I2= -which((carac[I,"Jour_semaine"]=="Dimanche")|(carac[I,"Jour_semaine"]=="Samedi"))
      II[i+1]= length(which((heure[I]== i)&((carac[I,"Jour_semaine"]!="Dimanche")&(carac[I,"Jour_semaine"]!="Samedi"))))
    }
    III[,k]=II/25
  }
  IIII[,l]=apply(III,1,mean)
}

IIIII=matrix(Inf, nrow=24, ncol=12)
IIIII[1:19,]=IIII[-(1:5),]
IIIII[20:24,]=IIII[(1:5),]
barplot(apply(IIIII,1,mean), names.arg=levels(A), ylim=c(0,16), ylab="Nombre d'accidents", xlab="Heure", main="Évolution moyenne par heure du nombre d'accidents le reste de la semaine (2005-2016)", col="blue")


 l= 5
k=1
lines(0:23,apply(IIII,1,mean), col="red")
plot(0:23,apply(IIII,1,mean), ylim=c(0,18), xlab="Heure de la journée", ylab="Nombre d'accidents", type="l")
title("Nombre d'accidents moyen en une journée par heure")
grid()
mean(apply(IIII,1,mean)) ##nombre d'accidents moyen par heure 
legend("topright",legend=c("Weekend","Reste de la semaine" ),
       col=c("black", "red"), lty=1, cex=0.8)

I1= which((carac[, "an"]==8)& (carac[, "mois"]==1)&(carac[,"jour"]==30)& (heure==1))

########################### Nombre d'accidents total par heure 
I= which(carac[, "an"]==5)
II=matrix(Inf, nrow=24, ncol=12)
for (i in 1:24){
  II[i,1]= length(which(heure[I]== i))
}

plot(1:24, II[,1], type="l", ylim=c(0,10000), col=5)
for (j in 2:12){
  I= which(carac[, "an"]==(j+4))
  for (i in 1:24){
    II[i,j]= length(which(heure[I]== i))
  }
  lines(1:24, II[,j], type="l", col=j)
}

legend("topright",legend=5:16,
       col=5:16, lty=1, cex=0.8)

plot(1:24, apply(II, 1, sum))


heure= as.numeric(carac[,"hrmn"])%/%100
hist(heure)
hist(carac[, "lum"])


########################### agglomeration/hors agglomeration
h=matrix(Inf, nrow=12, ncol=20)
for (i in 1:12){
  I=which(carac[,"an"]==(i+4))
  h[i,]=hist(carac[I,"agg"])$counts
}

hh=apply(h, 2,mean)


######################## fin decembre

IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==l))
    I= which(carac[I1, "mois"]==12)
    II=matrix(Inf, nrow=24, ncol=(31-24+1))
    for (j in 1:8){
      for (i in 1:24){
        II[i,j]= length(which((heure[I]== i)&(carac[I,"jour"]==(j+23))))
      }
    }
  IIII[,l]=apply(II,1,mean)
}

plot(1:24,apply(IIII,1,mean))
mean(apply(IIII,1,mean)) ##nombre d'accidents moyen par heure 

##################################### 31 decembre
IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==l))
  I= which(carac[I1, "mois"]==12)
  II=rep(Inf, 24)
    for (i in 1:24){
      II[i]= length(which((heure[I]== i)&(carac[I,"jour"]==(31))))
    }
  IIII[,l]=II
}

plot(1:24,apply(IIII,1,mean))

################################ 1 janvier
IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==(l+4)))
  I= which(carac[I1, "mois"]==1)
  II=rep(Inf, 24)
  for (i in 0:23){
    II[i+1]= length(which((heure[I]== i)&(carac[I,"jour"]==(1))))
  }
  IIII[,l]=II
}


IIII1=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==(l+4)))
  I= which(carac[I1, "mois"]==10)
  II=rep(Inf, 24)
  for (i in 0:23){
    II[i+1]= length(which((heure[I]== i)&(carac[I,"jour"]==(30))))
  }
  IIII1[,l]=II
}
IIIII=matrix(Inf, nrow=24, ncol=12)
IIIII[1:19,]=IIII[-(1:5),]
IIIII[20:24,]=IIII[(1:5),]

IIIII1=matrix(Inf, nrow=24, ncol=12)
IIIII1[1:19,]=IIII1[-(1:5),]
IIIII1[20:24,]=IIII1[(1:5),]
A=gl(24,1,24,labels=c("5h","6h","7h","8h","9h","10h","11h","12h","13h","14h","15h","16h","17h","18h","19h","20h","21h","22h","23h","00h","01h","02h","03h","04h"))
barplot(apply(IIIII,1,mean), names.arg=levels(A), ylim=c(0,14), ylab="Nombre d'accidents", xlab="Heure", main="Profil moyen du nombre d'accidents au 1er janvier (2005-2016)", col="blue")
plot(A,apply(IIIII,1,mean), type="l",main="Profil moyen sur les années 2005-2016 des accidents par heure", xlab="Heure", ylab="Nombre d'accidents", col="blue", lwd = 4, ylim=c(1,20))
lines(A,apply(IIIII1,1,mean), col="red1", lwd = 4)
grid()
legend("topright",legend=c("1 Janvier","30 Octobre" ),
       col=c("blue", "red"), lty=1, lwd = 4,cex=0.8)


#####################################. 14 juillet
IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==l))
  I= which(carac[I1, "mois"]==7)
  II=rep(Inf, 24)
  for (i in 1:24){
    II[i]= length(which((heure[I]== i)&(carac[I,"jour"]==(14))))
  }
  IIII[,l]=II
}

plot(1:24,apply(IIII,1,mean))


##################################### 15 juillet
IIII=matrix(Inf, nrow=24, ncol=12)
for (l in 1:12){
  I1= which((carac[, "an"]==l))
  I= which(carac[I1, "mois"]==7)
  II=rep(Inf, 24)
  for (i in 1:24){
    II[i]= length(which((heure[I]== i)&(carac[I,"jour"]==(15))))
  }
  IIII[,l]=II
}

plot(1:24,apply(IIII,1,mean))


res.glm=glm(Y~lum+atm+mois+jour,family=binomial(link="logit"),data=carac[1:100,])
summary(res.glm)

ologit=lrm(Y~lum+atm+mois+jour)
res.glm2= 


plot( carac[,"long"],carac[,"lat"], ylim=c(4880000,4890000),xlim=c(2200000,2400000),type="p")

length(which(carac[,"gps"]=="C"))
      