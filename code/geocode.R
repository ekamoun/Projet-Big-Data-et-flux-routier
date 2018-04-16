setwd("C:/Users/Weld ommou/Desktop/weldou/Gendarmerie_Projet/donnees")
liste_adr= read.csv2("liste_adrs_acc45(4).csv", header=TRUE, sep=";")
head(liste_adr)
library(dismo)
library(XML)
convert_gps=0
c.gps=matrix(Inf, nrow=length(liste_adr[,"x"]), ncol=2)
for (i in (1:length(liste_adr[,"x"]))){
  convert_gps<- geocode(liste_adr[i, "x"], oneRecord=TRUE)
  c.gps[i,2]= as.numeric(convert_gps["longitude"])
  c.gps[i,1]= as.numeric(convert_gps["latitude"])
}
colnames(c.gps)= c("lat", "long")
head(c.gps)
tail(c.gps)

c.gps1= matrix(Inf, nrow=length(liste_adr[,"x"]), ncol=2)
colnames(c.gps1)= c("lat", "long")
c.gps1[,1]=as.character(c.gps[,1])
c.gps1[,2]=as.character(c.gps[,2])
write.csv2(c.gps1, "liste_convert_gps_acc45(4).csv", row.names = FALSE)

liste_adr=matrix(Inf, ncol=2, nrow= 4959)
liste_adr[1:1000,]= as.matrix(read.csv2("liste_convert_gps_acc45(1).csv", header=TRUE, sep=";"))
liste_adr[1001:2000,]= as.matrix(read.csv2("liste_convert_gps_acc45(2).csv", header=TRUE, sep=";"))
liste_adr[2001:3000,]= as.matrix(read.csv2("liste_convert_gps_acc45(3).csv", header=TRUE, sep=";"))
liste_adr[3001:4000,]= as.matrix(read.csv2("liste_convert_gps_acc45(4).csv", header=TRUE, sep=";"))
liste_adr[4001:4959,]= as.matrix(read.csv2("liste_convert_gps_acc45(5).csv", header=TRUE, sep=";"))
colnames(liste_adr)= c("lat", "long")
head(liste_adr)
tail(liste_adr)

write.csv2(liste_adr, "liste_convert_gps_acc45.csv", row.names = FALSE)


