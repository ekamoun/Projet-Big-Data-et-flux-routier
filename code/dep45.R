###############################
setwd("C:/Users/Weld ommou/Desktop/weldou/Gendarmerie_Projet/donnees")

carac11=read.csv2("carac11.csv", header=TRUE, sep=";")
head(carac11)

carac1=read.csv2("carac1.csv", header=TRUE, sep=";")
head(carac1)

carac22=read.csv2("carac22.csv", header=TRUE, sep=";") ###sans paris et corse
head(carac22)

ddd=which(carac11[,"dep"]==450)
length(ddd)

ddd1=which(carac1[,"dep"]==450)
length(ddd1)

usag11=read.csv2("usag11.csv", header=TRUE, sep=";")
head(usag11)

ddd=which(usag11[,"dep"]==450)
length(ddd)

ddd1=which(usag1[,"dep"]==450)
length(ddd1)

length(ddd1)/length(ddd)

length(which(usag11[ddd1,"grav"]==1))/length(ddd1) *100 ### %39.01
length(which(usag11[ddd1,"grav"]==4))/length(ddd1) *100 ### %36.18
length(which(usag11[ddd1,"grav"]==3))/length(ddd1) *100 ### %21.80
length(which(usag11[ddd1,"grav"]==2))/length(ddd1) *100 ### %3.00


usag1=read.csv2("usag1.csv", header=TRUE, sep=";")
head(usag1)

length(which(usag1[ddd1,"grav"]==1))/length(ddd1) *100 ### %39.68
length(which(usag1[ddd1,"grav"]==4))/length(ddd1) *100 ### %31.62
length(which(usag1[ddd1,"grav"]==3))/length(ddd1) *100 ### %24.12
length(which(usag1[ddd1,"grav"]==2))/length(ddd1) *100 ### %4.5

usag2=read.csv2("usag2.csv", header=TRUE, sep=";")
head(usag2)

usag22=read.csv2("usag22.csv", header=TRUE, sep=";")
head(usag22)

length(which(usag22[,"grav"]==1))/dim(usag22)[1] *100 ### %39.79973
length(which(usag22[,"grav"]==4))/dim(usag22)[1] *100 ### %32.5192
length(which(usag22[,"grav"]==3))/dim(usag22)[1] *100 ### %24.27087
length(which(usag22[,"grav"]==2))/dim(usag22)[1] *100 ### %3.410197

usag3=read.csv2("usag3.csv", header=TRUE, sep=";")
head(usag3)

dim(usag2)
dim(usag3)

departement45_acc=read.csv2("departement45_acc.csv", header=TRUE, sep=";")
departement45=read.csv2("departement45_vic.csv", header=TRUE, sep=";")
head(departement45_acc)
head(departement45)
dim(departement45_acc)
dim(departement45)
write.csv2(departement45[,"adr"], "liste_adrs45.csv", row.names = FALSE)
liste_adr_acc45= departement45_acc[,"adr"]
liste_adr_acc45=paste(liste_adr_acc45, " 45000 France", sep="")
write.csv2(liste_adr_acc45, "liste_adrs_acc45.csv", row.names = FALSE)
write.csv2(liste_adr_acc45[1:1000], "liste_adrs_acc45(1).csv", row.names = FALSE)
write.csv2(liste_adr_acc45[1001:2000], "liste_adrs_acc45(2).csv", row.names = FALSE)
write.csv2(liste_adr_acc45[2001:3000], "liste_adrs_acc45(3).csv", row.names = FALSE)
write.csv2(liste_adr_acc45[3001:4000], "liste_adrs_acc45(4).csv", row.names = FALSE)
write.csv2(liste_adr_acc45[4001:dim(departement45_acc)[1]], "liste_adrs_acc45(5).csv", row.names = FALSE)

# liste_adr= read.csv2("liste_adrs_acc45(4).csv", header=TRUE, sep=";")
# head(liste_adr)
# library(dismo)
# library(XML)
# convert_gps=0
# c.gps=matrix(Inf, nrow=length(liste_adr[,"x"]), ncol=2)
# for (i in (1:length(liste_adr[,"x"]))){
#   convert_gps<- geocode(liste_adr[i, "x"], oneRecord=TRUE)
#   c.gps[i,2]= as.numeric(convert_gps["longitude"])
#   c.gps[i,1]= as.numeric(convert_gps["latitude"])
# }
# colnames(c.gps)= c("lat", "long")
# head(c.gps)
# tail(c.gps)
# 
# c.gps1= matrix(Inf, nrow=length(liste_adr[,"x"]), ncol=2)
# colnames(c.gps1)= c("lat", "long")
# c.gps1[,1]=as.character(c.gps[,1])
# c.gps1[,2]=as.character(c.gps[,2])
# write.csv2(c.gps1, "liste_convert_gps_acc45(5).csv", row.names = FALSE)
# 
# gps=matrix(Inf, ncol=3, nrow=dim(departement45))
# colnames(gps)=c("lat", "long", "gravit")
# gps[,1]=as.character(departement45[,"lat"])
# gps[,2]=as.character(departement45[,"long"])
# gps[,3]=as.character(departement45[,"gravit"])
# write.csv2(gps, "liste_gps45.csv", row.names = FALSE)

liste_adr_c_gps= read.csv2("liste_convert_gps_acc45.csv", header=TRUE, sep=";")

ndepartement45=departement45
# ndepartement45=ndepartement45[,-12]
# ndepartement45=ndepartement45[,-11]
ndepartement45=data.frame(ndepartement45, rep(Inf, dim(departement45)[1]))
names(ndepartement45)[dim(ndepartement45)[2]]="lat2"
ndepartement45=data.frame(ndepartement45, rep(Inf, dim(departement45)[1]))
names(ndepartement45)[dim(ndepartement45)[2]]="long2"
k=1

for (i in 2:dim(departement45)[1]){
  if (departement45[i,"Num_Acc"]!= departement45[i-1,"Num_Acc"]){
    k=k+1
  }
  
  ndepartement45[i,"long2"]=as.character(liste_adr_c_gps[k,"long"])
  ndepartement45[i,"lat2"]=as.character(liste_adr_c_gps[k,"lat"])
}


#length(which((is.na(departement45_acc[,'lat'])=TRUE | (departement45_acc[,'lat']==Inf))& (is.na(departement45_acc[,'long'])=TRUE | (departement45_acc[,'long']==Inf)) & (is.na(liste_adr_c_gps[,'lat'])=TRUE | (liste_adr_c_gps[,'lat']==Inf)) &(is.na(liste_adr_c_gps[,'long'])=TRUE | (liste_adr_c_gps[,'long']==Inf))))
k=0
for (i in 1:dim(ndepartement45)[1]){
  if ((ndepartement45[i,'lat']==Inf) | (is.na(ndepartement45[i,'lat'])==TRUE))
    if ((ndepartement45[i,'lat2']==Inf) | (is.na(ndepartement45[i,'lat2'])==TRUE))
      k=k+1
}
index=rep(Inf, k)
k=0
for (i in 1:dim(ndepartement45)[1]){
  if ((ndepartement45[i,'lat']==Inf) | (is.na(ndepartement45[i,'lat'])==TRUE))
    if ((ndepartement45[i,'lat2']==Inf) | (is.na(ndepartement45[i,'lat2'])==TRUE)){
      k=k+1
      index[k]=i
    }
}
#which(((departement45_acc[,'lat']==Inf) || (is.na(departement45_acc[,'lat'])==TRUE))& ((liste_adr_c_gps[,'lat']==Inf) || (is.na(liste_adr_c_gps[,'lat'])==TRUE)))
k=601
nouveau45vic= ndepartement45[-index[1:k],]
#nouveau45listcgps=liste_adr_c_gps[-index[1:k],]

liste= matrix(Inf, nrow=dim(nouveau45vic)[1], ncol=2)
colnames(liste)=c('lat', 'long')
  
for (i in 1:dim(nouveau45vic)[1]){
  if ((nouveau45vic[i,'lat']!=Inf) & (is.na(nouveau45vic[i,'lat'])==FALSE) & (nouveau45vic[i,'long']!=Inf) & (is.na(nouveau45vic[i,'long'])==FALSE) & (nouveau45vic[i,'lat']!=0)&(nouveau45vic[i,'long']!=0)){
    liste[i,1]= as.character(nouveau45vic[i,'lat'])
      liste[i,2]= as.character(nouveau45vic[i,'long'])
  }
  else{
    liste[i,1]= as.character(nouveau45vic[i,'lat2'])
    liste[i,2]= as.character(nouveau45vic[i,'long2'])
  }
}

I=which((is.na(liste[,1])==TRUE) |(is.na(liste[,2])==TRUE))
liste1=liste[-I,]
nouveau45vic1=nouveau45vic[-I,]
dim(nouveau45vic1)

l=0
for (i in 1:dim(liste1)[1]){
  if ((trunc(as.numeric(liste1[i,1])) >48) | (trunc(as.numeric(liste1[i,1])) <47))
    l=l+1
  else if ((trunc(as.numeric(liste1[i,2])) >3) | (trunc(as.numeric(liste1[i,1])) <1))
    l=l+1
}

indexl=rep(Inf, l)
k=0
for (i in 1:dim(liste1)[1]){
  if ((trunc(as.numeric(liste1[i,1])) >48) | (trunc(as.numeric(liste1[i,1])) <47)){
    k=k+1
    indexl[k]=i
  }
  else if ((trunc(as.numeric(liste1[i,2])) >3) | (trunc(as.numeric(liste1[i,2])) <1)){
    k=k+1
    indexl[k]=i
  }
}

nouveau45vic2= nouveau45vic1[-indexl,]
liste2=liste1[-indexl,]

# length(which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])>=48.374057)))
# length(which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])<=47.446218)))
# length(which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])<=1.495529)))
# length(which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])>=3.187423)))
# length(which(as.numeric(liste2[,2])>3.187423))
# length(which(as.numeric(liste2[,1])>48.374057))
# length(which(as.numeric(liste2[,1])<47.446218))
# length(which(as.numeric(liste2[,2])<1.495529))
# 
# indexx=rep(Inf, (61+19+19+52)) ##151
# 
# indexx[1:61]= which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])>=48.374057))
# indexx[62:(61+19)]= which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])<=47.446218))
# indexx[(61+19+1):(61+19+19)]= which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])<=1.495529))
# indexx[(61+19+19+1):(61+19+19+52)]=which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])>=3.187423))
# 
# nouveau45acc3= nouveau45acc2[-indexx,]
# liste3=liste2[-indexx,]
# 
# dim(liste3)
# head(liste3)
# dim(nouveau45acc3) ##3764   17
# #nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
# #nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
# nouveau45acc4=data.frame(nouveau45acc3, liste3)
# head(nouveau45acc4)
# write.csv2(liste3, "liste_finale_gps_acc45.csv", row.names = FALSE)
# write.csv2(nouveau45acc4, "nouveau_acc45.csv", row.names = FALSE)

l1=length(which(as.numeric(liste2[,2])>3.187423))
l2=length(which(as.numeric(liste2[,1])>48.374057))
l3=length(which(as.numeric(liste2[,1])<47.446218))
l4=length(which(as.numeric(liste2[,2])<1.495529))

indexx=rep(Inf, l1+l2+l3+l4)
indexx[1:l1]= which(as.numeric(liste2[,2])>3.187423)
indexx[(l1+1):(l1+l2)]= which(as.numeric(liste2[,1])>48.374057)
indexx[(l1+l2+1):(l1+l2+l3)]= which(as.numeric(liste2[,1])<47.446218)
indexx[(l1+l2+l3+1):(l1+l2+l3+l4)]=which(as.numeric(liste2[,2])<1.495529)

nouveau45vic3= nouveau45vic2[-indexx,]
liste3=liste2[-indexx,]

dim(liste3)
head(liste3)
dim(nouveau45vic3) ##8418   34
#nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
#nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
# nouveau45acc4=data.frame(nouveau45acc3, liste3)
# head(nouveau45acc4)
names(nouveau45vic3)
nouveau45vic4=nouveau45vic3
nouveau45vic4=nouveau45vic4[,-17]
nouveau45vic4=nouveau45vic4[,-15]
nouveau45vic4=nouveau45vic4[,-14]
nouveau45vic4=nouveau45vic4[,-13]
nouveau45vic4=nouveau45vic4[,-12]
nouveau45vic4=nouveau45vic4[,-11]
nouveau45vic4=nouveau45vic4[,-10]
nouveau45vic4=nouveau45vic4[,-9]
nouveau45vic4=nouveau45vic4[,-7]
nouveau45vic4=nouveau45vic4[,-25]
nouveau45vic4=nouveau45vic4[,-24]
nouveau45vic4=data.frame(nouveau45vic4, liste3)
names(nouveau45vic4)
dim(nouveau45vic4)
#write.csv2(liste3, "liste_finale_gps_acc45.csv", row.names = FALSE)
write.csv2(nouveau45vic4, "nouveau_vic45.csv", row.names = FALSE)


head(nouveau45vic4)
head(departement45)

departement4533=read.csv2("departement45_vic(31).csv", header=TRUE)
names(departement4533)
I=which((departement4533[,"adr"]=="")&((departement4533[,'lat']==0)|(departement4533[,'long']==0)))
departement4533=departement4533[-I,]
which((departement4533[,"adr"]=="")&((departement4533[,'lat']==0)|(departement4533[,'long']==0)))
I=which((departement4533[,"adr"]=="")&((is.na(departement4533[,'lat'])==TRUE)|(is.na(departement4533[,'long'])==TRUE)))
departement4533=departement4533[-I,]
head(departement4533)
head(departement45)
tail(departement4533)
tail(departement45)
dim(departement4533)
departement4533[,'lat']=departement45[,'lat']
departement4533[,'long']=departement45[,'long']

ndepartement4533=departement4533

liste_adr_c_gps= read.csv2("liste_convert_gps_acc45.csv", header=TRUE, sep=";")

# ndepartement45=ndepartement45[,-12]
# ndepartement45=ndepartement45[,-11]
ndepartement4533=data.frame(ndepartement4533, rep(Inf, dim(departement4533)[1]))
names(ndepartement4533)[dim(ndepartement4533)[2]]="lat2"
ndepartement4533=data.frame(ndepartement4533, rep(Inf, dim(departement4533)[1]))
names(ndepartement4533)[dim(ndepartement4533)[2]]="long2"
k=1

for (i in 2:dim(departement4533)[1]){
  if (departement4533[i,"Num_Acc"]!= departement4533[i-1,"Num_Acc"]){
    k=k+1
  }
  
  ndepartement4533[i,"long2"]=as.character(liste_adr_c_gps[k,"long"])
  ndepartement4533[i,"lat2"]=as.character(liste_adr_c_gps[k,"lat"])
}


#length(which((is.na(departement45_acc[,'lat'])=TRUE | (departement45_acc[,'lat']==Inf))& (is.na(departement45_acc[,'long'])=TRUE | (departement45_acc[,'long']==Inf)) & (is.na(liste_adr_c_gps[,'lat'])=TRUE | (liste_adr_c_gps[,'lat']==Inf)) &(is.na(liste_adr_c_gps[,'long'])=TRUE | (liste_adr_c_gps[,'long']==Inf))))
k=0
for (i in 1:dim(ndepartement4533)[1]){
  if ((ndepartement4533[i,'lat']==Inf) | (is.na(ndepartement4533[i,'lat'])==TRUE))
    if ((ndepartement4533[i,'lat2']==Inf) | (is.na(ndepartement4533[i,'lat2'])==TRUE))
      k=k+1
}
index=rep(Inf, k)
k=0
for (i in 1:dim(ndepartement4533)[1]){
  if ((ndepartement4533[i,'lat']==Inf) | (is.na(ndepartement4533[i,'lat'])==TRUE))
    if ((ndepartement4533[i,'lat2']==Inf) | (is.na(ndepartement4533[i,'lat2'])==TRUE)){
      k=k+1
      index[k]=i
    }
}
#which(((departement45_acc[,'lat']==Inf) || (is.na(departement45_acc[,'lat'])==TRUE))& ((liste_adr_c_gps[,'lat']==Inf) || (is.na(liste_adr_c_gps[,'lat'])==TRUE)))
k=601
nouveau45vehic= ndepartement4533[-index[1:k],]
#nouveau45listcgps=liste_adr_c_gps[-index[1:k],]

liste= matrix(Inf, nrow=dim(nouveau45vehic)[1], ncol=2)
colnames(liste)=c('lat', 'long')

for (i in 1:dim(nouveau45vehic)[1]){
  if ((nouveau45vehic[i,'lat']!=Inf) & (is.na(nouveau45vehic[i,'lat'])==FALSE) & (nouveau45vehic[i,'long']!=Inf) & (is.na(nouveau45vehic[i,'long'])==FALSE) & (nouveau45vehic[i,'lat']!=0)&(nouveau45vehic[i,'long']!=0)){
    liste[i,1]= as.character(nouveau45vehic[i,'lat'])
    liste[i,2]= as.character(nouveau45vehic[i,'long'])
  }
  else{
    liste[i,1]= as.character(nouveau45vehic[i,'lat2'])
    liste[i,2]= as.character(nouveau45vehic[i,'long2'])
  }
}

I=which((is.na(liste[,1])==TRUE) |(is.na(liste[,2])==TRUE))
liste1=liste[-I,]
nouveau45vehic1=nouveau45vehic[-I,]
dim(nouveau45vehic1)
dim(nouveau45vic1)
l=0
for (i in 1:dim(liste1)[1]){
  if ((trunc(as.numeric(liste1[i,1])) >48) | (trunc(as.numeric(liste1[i,1])) <47))
    l=l+1
  else if ((trunc(as.numeric(liste1[i,2])) >3) | (trunc(as.numeric(liste1[i,1])) <1))
    l=l+1
}

indexl=rep(Inf, l)
k=0
for (i in 1:dim(liste1)[1]){
  if ((trunc(as.numeric(liste1[i,1])) >48) | (trunc(as.numeric(liste1[i,1])) <47)){
    k=k+1
    indexl[k]=i
  }
  else if ((trunc(as.numeric(liste1[i,2])) >3) | (trunc(as.numeric(liste1[i,2])) <1)){
    k=k+1
    indexl[k]=i
  }
}

nouveau45vehic2= nouveau45vehic1[-indexl,]
liste2=liste1[-indexl,]

# length(which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])>=48.374057)))
# length(which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])<=47.446218)))
# length(which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])<=1.495529)))
# length(which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])>=3.187423)))
# length(which(as.numeric(liste2[,2])>3.187423))
# length(which(as.numeric(liste2[,1])>48.374057))
# length(which(as.numeric(liste2[,1])<47.446218))
# length(which(as.numeric(liste2[,2])<1.495529))
# 
# indexx=rep(Inf, (61+19+19+52)) ##151
# 
# indexx[1:61]= which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])>=48.374057))
# indexx[62:(61+19)]= which((as.numeric(liste2[,2])<=3.187423)& (as.numeric(liste2[,2])>=1.495529) & (as.numeric(liste2[,1])<=47.446218))
# indexx[(61+19+1):(61+19+19)]= which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])<=1.495529))
# indexx[(61+19+19+1):(61+19+19+52)]=which((as.numeric(liste2[,1])<=48.374057)& (as.numeric(liste2[,1])>=47.446218) & (as.numeric(liste2[,2])>=3.187423))
# 
# nouveau45acc3= nouveau45acc2[-indexx,]
# liste3=liste2[-indexx,]
# 
# dim(liste3)
# head(liste3)
# dim(nouveau45acc3) ##3764   17
# #nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
# #nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
# nouveau45acc4=data.frame(nouveau45acc3, liste3)
# head(nouveau45acc4)
# write.csv2(liste3, "liste_finale_gps_acc45.csv", row.names = FALSE)
# write.csv2(nouveau45acc4, "nouveau_acc45.csv", row.names = FALSE)

l1=length(which(as.numeric(liste2[,2])>3.187423))
l2=length(which(as.numeric(liste2[,1])>48.374057))
l3=length(which(as.numeric(liste2[,1])<47.446218))
l4=length(which(as.numeric(liste2[,2])<1.495529))

indexx=rep(Inf, l1+l2+l3+l4)
indexx[1:l1]= which(as.numeric(liste2[,2])>3.187423)
indexx[(l1+1):(l1+l2)]= which(as.numeric(liste2[,1])>48.374057)
indexx[(l1+l2+1):(l1+l2+l3)]= which(as.numeric(liste2[,1])<47.446218)
indexx[(l1+l2+l3+1):(l1+l2+l3+l4)]=which(as.numeric(liste2[,2])<1.495529)

nouveau45vehic3= nouveau45vehic2[-indexx,]
liste3=liste2[-indexx,]

dim(liste3)
head(liste3)
dim(nouveau45vehic3) ##8418   44
#nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
#nouveau45acc3=nouveau45acc3[,-dim(nouveau45acc3)[2]]
# nouveau45acc4=data.frame(nouveau45acc3, liste3)
# head(nouveau45acc4)
names(nouveau45vehic3)
nouveau45vehic4=nouveau45vehic3
# nouveau45vic4=nouveau45vic4[,-17]
# nouveau45vic4=nouveau45vic4[,-15]
nouveau45vehic4=nouveau45vehic4[,-44]
nouveau45vehic4=nouveau45vehic4[,-43]
nouveau45vehic4=nouveau45vehic4[,-12]
nouveau45vehic4=nouveau45vehic4[,-11]
# nouveau45vic4=nouveau45vic4[,-10]
# nouveau45vic4=nouveau45vic4[,-9]
# nouveau45vic4=nouveau45vic4[,-7]
# nouveau45vic4=nouveau45vic4[,-25]
# nouveau45vic4=nouveau45vic4[,-24]
nouveau45vehic4=data.frame(nouveau45vehic4, liste3)
names(nouveau45vehic4)
dim(nouveau45vic4)
#write.csv2(liste3, "liste_finale_gps_acc45.csv", row.names = FALSE)
write.csv2(nouveau45vehic4, "nouveau_vehic45.csv", row.names = FALSE)


head(nouveau45vic4)
head(departement45)




emna=read.csv2("nouveau_vic45.csv", header=TRUE, sep=";")

age_cond_interv1=rep(0, dim(emna)[1])
age_cond_interv2=rep(0, dim(emna)[1])
age_cond_interv3=rep(0, dim(emna)[1])
age_cond_interv4=rep(0, dim(emna)[1])
age_cond_interv5=rep(0, dim(emna)[1])
for (i in 1:dim(emna)[1]){
  if (emna[i,"catusag"]==1){
    if (emna[i,"age"]<16)
      age_cond_interv1[i]=1
    else if ((emna[i,"age"]<=17)&(emna[i,"age"]>=16)) 
      age_cond_interv2[i]=1
    else if ((emna[i,"age"]<=24)&(emna[i,"age"]>=18)) 
      age_cond_interv3[i]=1
    else if ((emna[i,"age"]<=65)&(emna[i,"age"]>=25)) 
      age_cond_interv4[i]=1
    if (emna[i,"age"]>65)
      age_cond_interv5[i]=1
  }
}

emna=data.frame(emna, age_cond_interv1)
names(emna)[dim(emna)[2]]="age_cond1"
emna=data.frame(emna, age_cond_interv2)
names(emna)[dim(emna)[2]]="age_cond2"
emna=data.frame(emna, age_cond_interv3)
names(emna)[dim(emna)[2]]="age_cond3"
emna=data.frame(emna, age_cond_interv4)
names(emna)[dim(emna)[2]]="age_cond4"
emna=data.frame(emna, age_cond_interv5)
names(emna)[dim(emna)[2]]="age_cond5"
names(emna)

age_vic_interv1=rep(0, dim(emna)[1])
age_vic_interv2=rep(0, dim(emna)[1])
age_vic_interv3=rep(0, dim(emna)[1])
age_vic_interv4=rep(0, dim(emna)[1])
age_vic_interv5=rep(0, dim(emna)[1])
for (i in 1:dim(emna)[1]){
    if (emna[i,"age"]<20)
      age_vic_interv1[i]=1
    else if ((emna[i,"age"]<=39)&(emna[i,"age"]>=20)) 
      age_vic_interv2[i]=1
    else if ((emna[i,"age"]<=59)&(emna[i,"age"]>=40)) 
      age_vic_interv3[i]=1
    else if ((emna[i,"age"]<=79)&(emna[i,"age"]>=60)) 
      age_vic_interv4[i]=1
    if (emna[i,"age"]>79)
      age_vic_interv5[i]=1
}

emna=data.frame(emna, age_vic_interv1)
names(emna)[dim(emna)[2]]="age_vic1"
emna=data.frame(emna, age_vic_interv2)
names(emna)[dim(emna)[2]]="age_vic2"
emna=data.frame(emna, age_vic_interv3)
names(emna)[dim(emna)[2]]="age_vic3"
emna=data.frame(emna, age_vic_interv4)
names(emna)[dim(emna)[2]]="age_vic4"
emna=data.frame(emna, age_vic_interv5)
names(emna)[dim(emna)[2]]="age_vic5"
names(emna)

heure_interv1=rep(0, dim(emna)[1])
heure_interv2=rep(0, dim(emna)[1])
heure_interv3=rep(0, dim(emna)[1])
heure_interv4=rep(0, dim(emna)[1])
heure_interv5=rep(0, dim(emna)[1])
heure_interv6=rep(0, dim(emna)[1])
heure_interv7=rep(0, dim(emna)[1])
heure_interv8=rep(0, dim(emna)[1])

for (i in 1:dim(emna)[1]){
  if ((emna[i,"heure"]<=6)&(emna[i,"heure"]>=5)) 
    heure_interv1[i]=1
  else if ((emna[i,"heure"]<=9)&(emna[i,"heure"]>=7)) 
    heure_interv2[i]=1
  else if ((emna[i,"heure"]<=11)&(emna[i,"heure"]>=10)) 
    heure_interv3[i]=1
  else if ((emna[i,"heure"]<=13)&(emna[i,"heure"]>=12)) 
    heure_interv4[i]=1
  else if ((emna[i,"heure"]<=15)&(emna[i,"heure"]>=14)) 
    heure_interv5[i]=1
  else if ((emna[i,"heure"]<=19)&(emna[i,"heure"]>=16)) 
    heure_interv6[i]=1
  else if ((emna[i,"heure"]<=23)&(emna[i,"heure"]>=20)) 
    heure_interv7[i]=1
  else if ((emna[i,"heure"]<=4)&(emna[i,"heure"]>=0)) 
    heure_interv8[i]=1
}


emna=data.frame(emna, heure_interv1)
names(emna)[dim(emna)[2]]="heure1"
emna=data.frame(emna, heure_interv2)
names(emna)[dim(emna)[2]]="heure2"
emna=data.frame(emna, heure_interv3)
names(emna)[dim(emna)[2]]="heure3"
emna=data.frame(emna, heure_interv4)
names(emna)[dim(emna)[2]]="heure4"
emna=data.frame(emna, heure_interv5)
names(emna)[dim(emna)[2]]="heure5"
emna=data.frame(emna, heure_interv6)
names(emna)[dim(emna)[2]]="heure6"
emna=data.frame(emna, heure_interv7)
names(emna)[dim(emna)[2]]="heure7"
emna=data.frame(emna, heure_interv8)
names(emna)[dim(emna)[2]]="heure8"
names(emna)

nouveau45_acc4=read.csv2("nouveau_acc45.csv", header=TRUE, sep=";")

nb_acc_452= dim(nouveau45_acc4)[1]
nmat452=matrix(0,nrow=nb_acc_452, ncol=5 )
colnames(nmat452)=c("Num_Acc", "grav0", "grav1", "grav2", 'grav3')

nmat452[1,1]= as.character(emna[1,"Num_Acc"])
if (emna[1, "gravit"]==0)
  nmat452[1,2]=1
if (emna[1, "gravit"]==1)
  nmat452[1,3]=1
if (emna[1, "gravit"]==2)
  nmat452[1,4]=1
if (emna[1, "gravit"]==3)
  nmat452[1,5]=1


k=1
for (i in 2:dim(emna)[1]){
  if (emna[i,1]!=emna[i-1,1]){
    k=k+1
    nmat452[k,1]=as.character(emna[i,1])
  }
  if (emna[i, "gravit"]==0)
    nmat452[k,2]=as.numeric(nmat452[k,2])+1
  if (emna[i, "gravit"]==1)
    nmat452[k,3]=as.numeric(nmat452[k,3])+1
  if (emna[i, "gravit"]==2)
    nmat452[k,4]=as.numeric(nmat452[k,4])+1
  if (emna[i, "gravit"]==3)
    nmat452[k,5]=as.numeric(nmat452[k,5])+1
}

##mat45[,1]=carac1[which(carac1[,"dep"]==450), "Num_Acc"]
which((nmat452[,3]==0)&(nmat452[,4]==0)&(nmat452[,5]==0))

write.csv2(nmat452, "nouveau_gravite_mat45.csv", row.names = FALSE)
max_gravit=rep(Inf, dim(emna)[1])
k=0
for (i in 1:dim(nmat452)[1]){
  if (as.numeric(nmat452[i,5])!=0)
    max_grav=3
  else if (as.numeric(nmat452[i,4])!=0)
    max_grav=2
  else if (as.numeric(nmat452[i,3])!=0)
    max_grav=1
  else if (as.numeric(nmat452[i,2])!=0)
    max_grav=0
  k=k+sum(as.numeric(nmat452[i,2:5]))
  max_gravit[(k-sum(as.numeric(nmat452[i,2:5]))+1):k]=max_grav
}

which(max_gravit==0)##aucun accident avec que des indemnes 

emna=data.frame(emna, max_gravit)
names(emna)[dim(emna)[2]]="max_gravit"
names(emna)
emna=emna[,-15]
emna=emna[,-14]
write.csv2(emna, "emna.csv", row.names = FALSE)

mm=read.csv2("nouveau_vehic45.csv", header=TRUE, sep=";")
names(mm)
emna=data.frame(emna, mm[,"cat1"])
names(emna)[dim(emna)[2]]="catv1"
emna=data.frame(emna, mm[,"cat2"])
names(emna)[dim(emna)[2]]="catv2"
emna=data.frame(emna, mm[,"cat3"])
names(emna)[dim(emna)[2]]="catv3"
emna=data.frame(emna, mm[,"cat4"])
names(emna)[dim(emna)[2]]="catv4"
emna=data.frame(emna, mm[,"cat5"])
names(emna)[dim(emna)[2]]="catv5"
emna=data.frame(emna, mm[,"cat6"])
names(emna)[dim(emna)[2]]="catv6"
names(emna)

write.csv2(emna, "emna.csv", row.names = FALSE)
emna=read.csv2("emna.csv", header=TRUE, sep=";")
head(emna)
emna=data.frame(emna, rep(450, dim(emna)[1]))
names(emna)[dim(emna)[2]]="dep"
names(emna)

write.csv2(emna, "emna.csv", row.names = FALSE)

#######################################
departement45_acc1=read.csv2("departement45_acc(1).csv", header=TRUE, sep=";")
departement451=read.csv2("departement45_vic(1).csv", header=TRUE)
head(departement45_acc1)
head(departement451)


mat45=read.csv2("mat45.csv", header=TRUE, sep=";")
head(mat45)


departement45_acc2=read.csv2("departement45_acc(2).csv", header=TRUE)
departement452=read.csv2("departement45_vic(2).csv", header=TRUE)
dim(departement452)
dim(departement45_acc2)

mat452=read.csv2("mat45(2).csv", header=TRUE, sep=";")
head(mat452)

departement45_acc3=read.csv("departement45_acc(3).csv", header=TRUE)
departement453=read.csv2("departement45_vic(3).csv", header=TRUE)
head(departement453)
head(departement45_acc3)

######################################

####### dep 45
dep452=which(usag22[,"dep"]==450)
length(dep452)


departement452=usag22[dep452,]
head(departement452)
dim(departement452)


write.csv2(departement452, "departement45_vic(2).csv", row.names = FALSE)

dep45_acc2=which(carac11[,"dep"]==450)
departement45_acc2=carac11[dep45_acc2,]
head(departement45_acc2)
dim(departement45_acc2)

write.csv2(departement45_acc2, "departement45_acc(2).csv", row.names = FALSE)


##########################################################
####### ajout de la meteo 
I45=which(carac11[,"dep"]==450)

dates= ((as.numeric(carac11[I45,"an"])+2000)*100+as.numeric(carac11[I45,"mois"]))*100+ as.numeric(carac11[I45,"jour"])
write.table(dates, "dates_45.txt", row.names=FALSE, col.names = FALSE, quote=FALSE)

setwd("C:/Users/Weld ommou/Desktop/weldou/Gendarmerie_Projet/donnees")
m45=read.table("meteo_45.txt", header = TRUE)
head(m45)

sum(is.na(m45[,"temper"]))
I=which(m45[,"temper"]==Inf)
m45[I,"temper"]=median(m45[-I,"temper"])
sum(is.na(m45[,"humid"]))
I=which(m45[,"humid"]==Inf)
m45[I,"humid"]=median(m45[-I,"humid"])
sum(is.na(m45[,"precipit"]))
I=which((m45[,"precipit"]==Inf)|(is.na(m45[,"precipit"])==TRUE))
m45[I,"precipit"]=median(m45[-I,"precipit"])

I=which(m45[,"h_neige"]==Inf)
neige=m45[-I,"h_neige"]
I1=which(is.na(neige)==TRUE)
neige=neige[-I1]
median(neige)
m45[I,"h_neige"]=median(neige)
I2= which(is.na(m45[,"h_neige"])==TRUE)
m45[I2,"h_neige"]=median(neige)

departement45_acc2=data.frame(departement45_acc2, m45)

write.csv(departement45_acc2, "departement45_acc(3).csv", row.names = FALSE)

departement452[,"temper"]=rep(Inf, nrow=dim(departement452)[1], ncol=1)
departement452[,"humid"]=rep(Inf, nrow=dim(departement452)[1], ncol=1)
departement452[,"precipit"]=rep(Inf, nrow=dim(departement452)[1], ncol=1)
departement451[,"h_neige"]=rep(Inf, nrow=dim(departement452)[1], ncol=1)

departement452[1,"temper"]=m45[1,1]
departement452[1,"humid"]=m45[1,2]
departement452[1,"precipit"]=m45[1,3]
departement452[1,"h_neige"]=m45[1,4]


k=1

for (i in 2:dim(departement452)[1]){
  if (departement452[i,"Num_Acc"]!= departement452[i-1,"Num_Acc"]){
    k=k+1
  }
  departement452[i,"temper"]=m45[k,1]
  departement452[i,"humid"]=m45[k,2]
  departement452[i,"precipit"]=m45[k,3]
  departement452[i,"h_neige"]=m45[k,4]
}

head(departement452)
tail(departement452)
write.csv2(departement452, "departement45_vic(3).csv", row.names = FALSE)


###### nombre de victimes par accident pou le dep 45
nb_acc_452= length(which(carac11[,"dep"]==450))
mat452=matrix(0,nrow=nb_acc_452, ncol=5 )
colnames(mat452)=c("Num_Acc", "grav0", "grav1", "grav2", 'grav3')

mat452[1,1]= as.character(departement452[1,"Num_Acc"])
if (departement452[1, "gravit"]==0)
  mat452[1,2]=1
if (departement452[1, "gravit"]==1)
  mat452[1,3]=1
if (departement452[1, "gravit"]==2)
  mat452[1,4]=1
if (departement452[1, "gravit"]==3)
  mat452[1,5]=1


k=1
for (i in 2:length(dep452)){
  if (departement452[i,1]!=departement452[i-1,1]){
    k=k+1
    mat452[k,1]=as.character(departement452[i,1])
  }
  if (departement452[i, "gravit"]==0)
    mat452[k,2]=as.numeric(mat452[k,2])+1
  if (departement452[i, "gravit"]==1)
    mat452[k,3]=as.numeric(mat452[k,3])+1
  if (departement452[i, "gravit"]==2)
    mat452[k,4]=as.numeric(mat452[k,4])+1
  if (departement452[i, "gravit"]==3)
    mat452[k,5]=as.numeric(mat452[k,5])+1
}

##mat45[,1]=carac1[which(carac1[,"dep"]==450), "Num_Acc"]
which((mat452[,3]==0)&(mat452[,4]==0)&(mat452[,5]==0))

write.csv2(mat452, "mat45(2).csv", row.names = FALSE)

nb_acc_452= length(which(carac11[,"dep"]==450))
length(which(mat452[,2]!=0))/nb_acc_452 ##poucentage des accidents contenant au moins une victime de grav0
length(which(mat452[,3]!=0))/nb_acc_452 ##poucentage des accidents contenant au moins une victime de grav1
length(which(mat452[,4]!=0))/nb_acc_452 ##poucentage des accidents contenant au moins une victime de grav2 
length(which(mat452[,5]!=0))/nb_acc_452 ##poucentage des accidents contenant au moins une victime de grav3


nb_acc_45= length(which(carac1[,"dep"]==450))
length(which(mat45[,2]!=0))/nb_acc_45 ##poucentage des accidents contenant au moins une victime de grav0  %68.56221
length(which(mat45[,3]!=0))/nb_acc_45 ##poucentage des accidents contenant au moins une victime de grav1  %55.11192
length(which(mat45[,4]!=0))/nb_acc_45 ##poucentage des accidents contenant au moins une victime de grav2  %47.30792
length(which(mat45[,5]!=0))/nb_acc_45 ##poucentage des accidents contenant au moins une victime de grav3  %9.417221


###### nombre de victimes par accident en globalite sans traitement d'adresses 
nb_acc= dim(carac22)[1]
mat=matrix(0,nrow=nb_acc, ncol=5 )
colnames(mat)=c("Num_Acc", "grav0", "grav1", "grav2", 'grav3')

mat[1,1]= as.character(usag22[1,"Num_Acc"])
if (usag22[1, "gravit"]==0)
  mat[1,2]=1
if (usag22[1, "gravit"]==1)
  mat[1,3]=1
if (usag22[1, "gravit"]==2)
  mat[1,4]=1
if (usag22[1, "gravit"]==3)
  mat[1,5]=1


k=1
for (i in 2:dim(usag22)[1]){
  if (usag22[i,1]!=usag22[i-1,1]){
    k=k+1
    mat[k,1]=as.character(usag22[i,1])
  }
  if (usag22[i, "gravit"]==0)
    mat[k,2]=as.numeric(mat[k,2])+1
  if (usag22[i, "gravit"]==1)
    mat[k,3]=as.numeric(mat[k,3])+1
  if (usag22[i, "gravit"]==2)
    mat[k,4]=as.numeric(mat[k,4])+1
  if (usag22[i, "gravit"]==3)
    mat[k,5]=as.numeric(mat[k,5])+1
}



##mat45[,1]=carac1[which(carac1[,"dep"]==450), "Num_Acc"]
length(which((mat[,3]==0)&(mat[,4]==0)&(mat[,5]==0)))[1]

write.csv2(mat, "mat.csv", row.names = FALSE)

nb_acc= dim(carac22)[1]
length(which(mat[,2]!=0))/nb_acc ##poucentage des accidents contenant au moins une victime de grav0
length(which(mat[,3]!=0))/nb_acc ##poucentage des accidents contenant au moins une victime de grav1
length(which(mat[,4]!=0))/nb_acc ##poucentage des accidents contenant au moins une victime de grav2 
length(which(mat[,5]!=0))/nb_acc ##poucentage des accidents contenant au moins une victime de grav3

###########



liste_adr45= departement45[,"adr"]
########### base departement45
write.table(liste_adr45, "liste_adr_45.txt", row.names=FALSE, col.names = FALSE, quote=FALSE)
# departement45=departement45[,-12]
# departement45=departement45[,-11]
# departement45=departement45[,-10]
# names(departement45)


######### changement d'heure
chang_heure_date= matrix(Inf, nrow=12, ncol=3)
chang_heure_date[,1]=2005:2016
chang_heure_date[,2]=c(27,26,25,30,29,28,27,25,31,30,29,27) ## passage à l'heure l'heure
chang_heure_date[,3]=c(30,29,28,26,25,31,30,28,27,26,25,30)

nb_1jour_apres=matrix(Inf, nrow=12, ncol=11)
colnames(nb_1jour_apres)=c("Num_Acc", "Passag_Nbre_Acc", "Nbre_Vict_grav0", "Nbre_Vict_grav1","Nbre_Vict_grav2", "Nbre_Vict_grav3", "Suppres_Nbre_Acc", "Nbre_Vict_grav0", "Nbre_Vict_grav1","Nbre_Vict_grav2", "Nbre_Vict_grav3")
nb_1jour_apres[,1]=2005:2016
I=which(carac1[,"dep"]==450)
for (i in 1:12){
  nb_1jour_apres[i,2]=length(which((carac1[I,"an"]==(4+i))&(as.numeric(carac1[I,"mois"])==3)&(as.numeric(carac1[I,"jour"])==chang_heure_date[i,2])))
  
  nb_1jour_apres[i,3]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==3)&(departement45[,"jour"]==chang_heure_date[i,2])&(departement45[,"gravit"]==0)))
  nb_1jour_apres[i,4]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==3)&(departement45[,"jour"]==chang_heure_date[i,2])&(departement45[,"gravit"]==1)))
  nb_1jour_apres[i,5]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==3)&(departement45[,"jour"]==chang_heure_date[i,2])&(departement45[,"gravit"]==2)))
  nb_1jour_apres[i,6]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==3)&(departement45[,"jour"]==chang_heure_date[i,2])&(departement45[,"gravit"]==3)))
  
  nb_1jour_apres[i,7]=length(which((carac1[I,"an"]==(4+i))&(as.numeric(carac1[I,"mois"])==10)&(as.numeric(carac1[I,"jour"])==chang_heure_date[i,3])))
  
  nb_1jour_apres[i,8]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==10)&(departement45[,"jour"]==chang_heure_date[i,3])&(departement45[,"gravit"]==0)))
  nb_1jour_apres[i,9]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==10)&(departement45[,"jour"]==chang_heure_date[i,3])&(departement45[,"gravit"]==1)))
  nb_1jour_apres[i,10]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==10)&(departement45[,"jour"]==chang_heure_date[i,3])&(departement45[,"gravit"]==2)))
  nb_1jour_apres[i,11]=length(which((departement45[,"an"]==(2005+i-1))&(departement45[,"mois"]==10)&(departement45[,"jour"]==chang_heure_date[i,3])&(departement45[,"gravit"]==3)))
}

##### nbre d'accidents/victimes moyen par jour pour chaque année
moyenne_jour=matrix(Inf, nrow=12, ncol=2)
moyenne_jour[,1]=2005:2016
for (l in 1:12){
  an=which((carac1[,"an"]==(4+l))&(carac1[,"dep"]==450))
  m_an=matrix(Inf, nrow=12, ncol=31)
  for (j in 1:12){
    for (i in 1:31){
      m_an[j,i]=length(which((as.numeric(carac1[an,"mois"])==j)&(as.numeric(carac1[an,"jour"])==i)))
    }
  }
  moyenne_jour[l,2]=sum(apply(m_an, 1,mean)*(apply(m_an,1,sum)/sum(m_an)))
}

######### 3 jours representatifs 
repres_jour=matrix(Inf, nrow=12, ncol=4)
repres_jour[,1]=2005:2016
for (l in 1:12){
  an=which((carac1[,"an"]==(4+l))&(carac1[,"dep"]==450))
  repres_jour[l,2]=length(which((as.numeric(carac1[an,"mois"])==5)&(as.numeric(carac[an,"jour"])==5)))
  repres_jour[l,3]=length(which((as.numeric(carac1[an,"mois"])==7)&(as.numeric(carac[an,"jour"])==27)))
  repres_jour[l,4]=length(which((as.numeric(carac1[an,"mois"])==11)&(as.numeric(carac[an,"jour"])==10)))
}


names(departement45)
write.csv(departement45, "departement45.csv", row.names=FALSE)

######## base propre departement45
departement45_1=matrix(Inf, nrow=dim(departement45)[1], ncol=dim(departement45)[2])
colnames(departement45_1)=colnames(departement45)
departement45_1=departement45
names(departement45_1)
departement45_1=departement45_1[,-14]
departement45_1=departement45_1[,-12]
departement45_1=departement45_1[,-11]
departement45_1=departement45_1[,-9]
departement45_1=departement45_1[,-7]
departement45_1=departement45_1[,-5]
departement45_1=departement45_1[,-4]
departement45_1=departement45_1[,-3]

dim(departement45_1)
names(departement45_1)
head(departement45_1)

############ modèle 

install.packages("glmmML")

library(glmmML)



## a pooled ordered logit

pooled.ologit <- pglm(as.factor(gravit) ~ as.factor(weekend)+as.numeric(lumi)+as.factor(agg)
                      +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                      +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                      +as.factor(traj4)+as.factor(traj5), 
                      model=("pooling"), effect=("individual"), index=c("Num_Acc"), family=ordinal(link="logit"), data=departement4533)
summary(pooled.ologit)

library(tree)
arbre=tree(as.factor(gravit) ~as.factor(weekend)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
           +as.numeric(temper)+as.numeric(humid)
           +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
           +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
           +as.factor(traj4)+as.factor(traj5)
           +as.factor(cat1)+as.factor(cat2)+as.factor(cat3)
           +as.factor(cat4)+as.factor(cat5)+as.factor(cat6), data=departement4533)
print(arbre)
plot(arbre)
text(arbre, cex=0.5, pretty=0)
summary(arbre)

library(rpart)
arbre=rpart(as.factor(gravit) ~as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
           +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
           +as.factor(traj1)+ as.factor(traj2)
           +as.factor(traj4)+as.factor(traj5)
           +as.factor(cat1)+as.factor(cat2)+as.factor(cat3)
           +as.factor(cat4)+as.factor(cat5)+as.factor(cat6),control = rpart.control(cp = 0.0000001),data=departement4533)
print(arbre)
plot(arbre)
text(arbre, cex=0.5, pretty=0)
summary(arbre)
pred=predict(arbre, newdata=departement4533, type="class")
length(which(pred==3))
length(which(departement4533[,"gravit"]==3))
length(which(pred==departement4533[,"gravit"]))/length(departement4533[,"gravit"])

install.packages("pglm")
library(pglm)

departement4533=read.csv2("departement45_vic(31).csv", header=TRUE)
names(departement4533)

stata=departement4533
stata=stata[,-17]
stata=stata[,-15]
stata=stata[,-14]
stata=stata[,-12]
stata=stata[,-11]
stata=stata[,-10]
stata=stata[,-9]
stata=stata[,-7]
stata=stata[,-5]

#type(stata[,32])
write.csv2(as.numeric(stata1[,24]), "stata1.csv", row.names = FALSE)

pooled.oprobit= pglm(as.ordered(gravit) ~as.factor(weekend)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
                      +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                      +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                      +as.factor(traj4)+as.factor(traj5)
                      +as.factor(cat1)+as.factor(cat2)+as.factor(cat3)
                      +as.factor(cat4)+as.factor(cat5)+as.factor(cat6)+as.numeric(temper)+as.numeric(humid), 
                      x=TRUE, model=("pooling"), effect=("individual"), index=c("Num_Acc"), family=ordinal(link="probit"), data=stata)
summary(pooled.oprobit)
ordinalprobit= clm(as.ordered(gravit) ~as.factor(weekend)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
                    +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                    +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                    +as.factor(traj4)+as.factor(traj5)
                    +as.factor(cat1)+as.factor(cat5)+as.numeric(humid), 
                    link="logit", data=stata)
summary(ordinalprobit)
prediction(ordinalprobit, stata,
           type = "class", category=3)

predict(ordinalprobit, arg="class")
margins(ordinalprobit, data=stata, type="response", vcov=vcov(ordinalprobit), vce="delta")
dydx(stata,ordinalprobit, "age", type="response" )


marginal_effects(stata,ordinalprobit, "agg", type="response")
allEffects(ordinalprobit)
gravit1=rep(Inf, dim(stata)[1])
for (i in 1:dim(stata)[1]){
  if (stata[i,"gravit"]==0)
  gravit1[i]="Indemne"
  else if (stata[i,"gravit"]==1)
    gravit1[i]="Bles.leger"
  else if (stata[i,"gravit"]==2)
  gravit1[i]="Bles.Hosp"
  else
    gravit1[i]="Tue"
}

stata=data.frame(stata, gravit1)
names(stata)[dim(stata)[2]]="gravit1"
oprobit= polr(as.factor(gravit) ~as.factor(weekend)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
              +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
              +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
              +as.factor(traj4)+as.factor(traj5)
              +as.factor(cat1)
              +as.factor(cat5)+as.numeric(humid), 
               method="probit", data=stata)

oprobit= polr(as.factor(gravit) ~weekend+lumi+agg+atmos
              +catusag+age+sexe
              +traj1+ traj2+traj3
              +traj4+traj5
              +cat1
              +cat5+humid, 
              method="logistic", data=stata)

summary(oprobit)
prediction(oprobit, stata,
           type = "class", category=3)

predict(oprobit, arg="class")
margins(oprobit, data=stata, vcov=vcov(oprobit), vce="delta")
dydx(stata,oprobit, "age", type="response" )

dydx(stata, oprobit, "gravit",fwrap = TRUE)

marginal_effects(stata,oprobit, "agg", type="response")
allEffects(oprobit)

probit= glm(as.ordered(gravit) ~as.factor(weekend)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
                    +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                    +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                    +as.factor(traj4)+as.factor(traj5)
                    +as.factor(cat1)+as.factor(cat2)+as.factor(cat3)
                    +as.factor(cat4)+as.factor(cat5)+as.factor(cat6)+as.numeric(temper)+as.numeric(humid), 
                     family=binomial(link="probit"),x=TRUE, data=stata)


summary(probit)
predict.glm(pooled.ologit, type="response")
margins(oprobit)
dydx(stata,oprobit, "agg", type="response" )
#dydx(stata,pooled.ologit, "agg" )
dydx(stata, oprobit, "age",fwrap = TRUE)

marginal_effects(stata,oprobit, "agg", type="response")
allEffects(oprobit)

library(margins)
library(plm)
if (!require("ghit")) {
  install.packages("ghit")
  library("ghit")
}

# building vignettes takes a moment, so for a quicker install set:
install_github("leeper/margins", build_vignettes = FALSE)
library(margins)

dydx.(pooled.ologit)

install.packages("MASS")
library(MASS)


oprobit <- polr(as.ordered(gravit)~
                  as.factor(weekend)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
                +as.numeric(temper)+as.numeric(humid)
                +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                +as.factor(traj4)+as.factor(traj5)
                +as.factor(cat1)+as.factor(cat2)+as.factor(cat3)
                +as.factor(cat4)+as.factor(cat5)+as.factor(cat6),data=departement4533)

summary(oprobit)

oprobit <- polr(as.ordered(gravit)~
                  as.factor(weekend)+as.factor(grnds_vacs) 
                +as.factor(fev)+as.factor(oct)+as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
                +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                +as.factor(traj4)+as.factor(traj5),data=departement452)

summary(oprobit)

pooled.ologit <- pglm(gravit ~
                     as.numeric(lumi)+as.factor(agg)+as.numeric(atmos)
                      +as.factor(catusag)+as.numeric(age)+as.factor(sexe)
                      +as.factor(traj1)+ as.factor(traj2)+as.factor(traj3)
                      +as.factor(traj4), 
                      model=("pooling"), effect=("individual"),family=ordinal(link="probit"), data=departement453)
summary(pooled.ologit)



#####################################################