##Pujada dades
library(stringr)
library(lubridate)
dd<-read.delim2("C:/Users/becari/Downloads/Cloperal-text.txt",header = F)
head(dd)
dd<-as.data.frame(dd)

##Declaració Variables
Granja<-c()
Animal<-c()
Ciclo<-c()
Sexo<-c()
Raza<-c()
Fecn<-c()
Origen<-c()
Fece<-c()
Baja<-c()
Fecb<-c()
Razon<-c()
Rep<-c()
Fecha.Cub<-c()
Fecha.Cub2<-c()
Fecha.Cub3<-c()
Fecha.Cub4<-c()
Fecha.Cub5<-c()
Macho<-c()
Macho2<-c()
Macho3<-c()
Macho4<-c()
Macho5<-c()
Tipo.Cubr<-c()
Tipo.Cubr2<-c()
Tipo.Cubr3<-c()
Tipo.Cubr4<-c()
Tipo.Cubr5<-c()
Fecha.Parto<-c()
Tipo.Parto<-c()
Viv<-c()
Mue<-c()
Momi<-c()
Ado<-c()
Ret<-c()
FECHA.DESTETE<-c()
NUM.LECHONES<-c()

nrow(dd)
for (i in 1:nrow(dd)) {
  x <- gsub("[[]]", "", dd[i,])
  xs <- unlist(strsplit(x, ""))  
  if(xs[13]==0&xs[12]==0){
    Granja<-c(Granja,str_c(xs[1:5],collapse = ""))
    Animal<-c(Animal,str_c(xs[6:11],collapse = ""))
    Ciclo<-c(Ciclo,str_c(xs[12:13],collapse = ""))
    Sexo<-c(Sexo,xs[14])
    Raza<-c(Raza,str_c(xs[15:16],collapse = ""))
    Fecn<-c(Fecn,str_c(xs[17:22],collapse = ""))
    Origen<-c(Origen,str_c(xs[23:27],collapse = ""))
    Fece<-c(Fece,str_c(xs[28:33],collapse = ""))
    Baja<-c(Baja,xs[34])
    Fecb<-c(Fecb,str_c(xs[35:40],collapse = ""))
    Razon<-c(Razon,str_c(xs[41:42],collapse = ""))
    Rep<-c(Rep,as.numeric(""))
    Fecha.Cub<-c(Fecha.Cub,"")
    Fecha.Cub2<-c(Fecha.Cub2,"")
    Fecha.Cub3<-c(Fecha.Cub3,"")
    Fecha.Cub4<-c(Fecha.Cub4,"")
    Fecha.Cub5<-c(Fecha.Cub5,"")
    Macho<-c(Macho,"")
    Macho2<-c(Macho2,"")
    Macho3<-c(Macho3,"")
    Macho4<-c(Macho4,"")
    Macho5<-c(Macho5,"")
    Tipo.Cubr<-c(Tipo.Cubr,"")
    Tipo.Cubr2<-c(Tipo.Cubr2,"")
    Tipo.Cubr3<-c(Tipo.Cubr3,"")
    Tipo.Cubr4<-c(Tipo.Cubr4,"")
    Tipo.Cubr5<-c(Tipo.Cubr5,"")
    Fecha.Parto<-c(Fecha.Parto,"")
    Tipo.Parto<-c(Tipo.Parto,"")
    Viv<-c(Viv,"")
    Mue<-c(Mue,"")
    Momi<-c(Momi,"")
    Ado<-c(Ado,"")
    Ret<-c(Ret,"")
    FECHA.DESTETE<-c(FECHA.DESTETE,"")
    NUM.LECHONES<-c(NUM.LECHONES,"")
    
  }
  else{
    Granja<-c(Granja,str_c(xs[1:5],collapse = ""))
    Animal<-c(Animal,str_c(xs[6:11],collapse = ""))
    Ciclo<-c(Ciclo,str_c(xs[12:13],collapse = ""))
    Sexo<-c(Sexo,tail(Sexo,1))
    Raza<-c(Raza,tail(Raza,1))
    Fecn<-c(Fecn,tail(Fecn,1)) # Fecn[length(Fecn)]
    Origen<-c(Origen,tail(Origen,1))
    Fece<-c(Fece,tail(Fece,1))
    Baja<-c(Baja,tail(Baja,1))
    Fecb<-c(Fecb,tail(Fecb,1))
    Razon<-c(Razon,tail(Razon,1))
    Rep<-c(Rep,as.numeric(str_c(xs[14],collapse = "")))
    Fecha.Cub<-c(Fecha.Cub,str_c(xs[15:20],collapse = ""))
    Fecha.Cub2<-c(Fecha.Cub2,str_c(xs[21:26],collapse = ""))
    Fecha.Cub3<-c(Fecha.Cub3,str_c(xs[27:32],collapse = ""))
    Fecha.Cub4<-c(Fecha.Cub4,str_c(xs[33:38],collapse = ""))
    Fecha.Cub5<-c(Fecha.Cub5,str_c(xs[39:44],collapse = ""))
    Macho<-c(Macho,str_c(xs[45:50],collapse = ""))
    Macho2<-c(Macho2,str_c(xs[51:56],collapse = ""))
    Macho3<-c(Macho3,str_c(xs[57:62],collapse = ""))
    Macho4<-c(Macho4,str_c(xs[63:68],collapse = ""))
    Macho5<-c(Macho5,str_c(xs[69:74],collapse = ""))
    Tipo.Cubr<-c(Tipo.Cubr,xs[75])
    Tipo.Cubr2<-c(Tipo.Cubr2,xs[76])
    Tipo.Cubr3<-c(Tipo.Cubr3,xs[77])
    Tipo.Cubr4<-c(Tipo.Cubr4,xs[78])
    Tipo.Cubr5<-c(Tipo.Cubr5,xs[79])
    Fecha.Parto<-c(Fecha.Parto,str_c(xs[80:85],collapse = ""))
    Tipo.Parto<-c(Tipo.Parto,xs[86])
    Viv<-c(Viv,str_c(xs[87:88],collapse = ""))
    Mue<-c(Mue,str_c(xs[89:90],collapse = ""))
    Momi<-c(Momi,xs[91])
    Ado<-c(Ado,str_c(xs[92:93],collapse = ""))
    Ret<-c(Ret,str_c(xs[94:95],collapse = ""))
    FECHA.DESTETE<-c(FECHA.DESTETE,str_c(xs[96:101],collapse = ""))
    NUM.LECHONES<-c(NUM.LECHONES,str_c(xs[102:103],collapse = ""))
    
  }
}
df<-data.frame(Granja,Animal,Ciclo,Sexo,Raza,Fecn,
               Origen,Fece,Baja,Fecb,Razon,Rep,Fecha.Cub,Fecha.Cub2,Fecha.Cub3,Fecha.Cub4,Fecha.Cub5,Macho,Macho2,Macho3, Macho4,Macho5, Tipo.Cubr,
               Tipo.Cubr2,Tipo.Cubr3,Tipo.Cubr4,Tipo.Cubr5,Fecha.Parto,Tipo.Parto,Viv,Mue,Momi,Ado,Ret,FECHA.DESTETE,NUM.LECHONES)
head(df)
nrow(df)
nrow(dd)

####dates
#falta afegir l'any a cada data
#Variables :Fecn, Fece , Fecb, Fecha.Cub,Fecha.Cub2,Fecha.Cub3,Fecha.Cub4,Fecha.Cub5,Fecha.Parto, FECHA.DESTETE

##Fecn ###
AFecn<-substr(Fecn, start = 1, stop = 4)
BFecn<-substr(Fecn, start = 5, stop = 6)
BFecn<-str_remove_all(BFecn, " ")
AFecn<-str_remove_all(AFecn, " ")


for (i in 1:length(BFecn)) {
  if(BFecn[i]!=("")){
    if(as.numeric(BFecn[i])>50){
      BFecn[i]<-as.numeric(BFecn[i])+1900
    }
    else{
      BFecn[i]<-as.numeric(BFecn[i])+2000
    }
  }
}
ABFecn<-str_c(AFecn, BFecn)
df$Fecn<-dmy(ABFecn)
table(is.na(df$Fecn))
table(AFecn)

##Fece 
AFece<-substr(Fece, start = 1, stop = 4)
BFece<-substr(Fece, start = 5, stop = 6)
BFece<-str_remove_all(BFece, " ")
AFece<-str_remove_all(AFece, " ")

for (i in 1:length(BFece)) {
  if(BFece[i]!=""){
    if(as.numeric(BFece[i])>50){
      BFece[i]<-as.numeric(BFece[i])+1900
    }
    else{
      BFece[i]<-as.numeric(BFece[i])+2000
    }
  }
}
ABFece<-str_c(AFece, BFece)

df$Fece<-dmy(ABFece)
table(is.na(df$Fece))
table(AFece)

##Fecb
AFecb<-substr(Fecb, start = 1, stop = 4)
BFecb<-substr(Fecb, start = 5, stop = 6)
AFecb<-str_remove_all(AFecb, " ")
BFecb<-str_remove_all(BFecb, " ")

for (i in 1:length(BFecb)) {
  if(BFecb[i]!=""){
  if(as.numeric(BFecb[i])>50){
    BFecb[i]<-as.numeric(BFecb[i])+1900
  }
    else{
      BFecb[i]<-as.numeric(BFecb[i])+2000
    }
}
}
ABFecb<-str_c(AFecb, BFecb)

df$Fecb<-dmy(ABFecb)
table(is.na(df$Fecb))
table(AFecb)

##Fecha.Cub
AFecha.Cub<-substr(Fecha.Cub, start = 1, stop = 4)
BFecha.Cub<-substr(Fecha.Cub, start = 5, stop = 6)
BFecb<-str_remove_all(BFecb, " ")
AFecb<-str_remove_all(AFecb, " ")

for (i in 1:length(BFecha.Cub)) {
  if(BFecha.Cub[i]!=""){
    if(as.numeric(BFecha.Cub[i])>50){
      BFecha.Cub[i]<-as.numeric(BFecha.Cub[i])+1900
    }
    else{
      BFecha.Cub[i]<-as.numeric(BFecha.Cub[i])+2000
    }
  }
}
ABFecha.Cub<-str_c(AFecha.Cub, BFecha.Cub)

df$Fecha.Cub<-dmy(ABFecha.Cub)
table(is.na(df$Fecha.Cub))
table(AFecha.Cub)

##Fecha.Cub2
AFecha.Cub2<-substr(Fecha.Cub2, start = 1, stop = 4)
BFecha.Cub2<-substr(Fecha.Cub2, start = 5, stop = 6)
BFecha.Cub2<-str_remove_all(BFecha.Cub2, "  ")
AFecha.Cub2<-str_remove_all(AFecha.Cub2, "  ")

for (i in 1:length(BFecha.Cub2)) {
  if(BFecha.Cub2[i]!=""){
    if(as.numeric(BFecha.Cub2[i])>50){
      BFecha.Cub2[i]<-as.numeric(BFecha.Cub2[i])+1900
    }
    else{
      BFecha.Cub2[i]<-as.numeric(BFecha.Cub2[i])+2000
    }
  }
}
ABFecha.Cub2<-str_c(AFecha.Cub2, BFecha.Cub2)

df$Fecha.Cub2<-dmy(ABFecha.Cub2)
table(is.na(df$Fecha.Cub2))
table(AFecha.Cub2)

##Fecha.Cub3
AFecha.Cub3<-substr(Fecha.Cub3, start = 1, stop = 4)
BFecha.Cub3<-substr(Fecha.Cub3, start = 5, stop = 6)
BFecha.Cub3<-str_remove_all(BFecha.Cub3, "  ")
AFecha.Cub3<-str_remove_all(AFecha.Cub3, "  ")

for (i in 1:length(BFecha.Cub3)) {
  if(BFecha.Cub3[i]!=""){
    if(as.numeric(BFecha.Cub3[i])>50){
      BFecha.Cub3[i]<-as.numeric(BFecha.Cub3[i])+1900
    }
    else{
      BFecha.Cub3[i]<-as.numeric(BFecha.Cub3[i])+2000
    }
  }
}
ABFecha.Cub3<-str_c(AFecha.Cub3, BFecha.Cub3)

df$Fecha.Cub3<-dmy(ABFecha.Cub3)
table(is.na(df$Fecha.Cub3))
table(AFecha.Cub3)

##Fecha.Cub4
AFecha.Cub4<-substr(Fecha.Cub4, start = 1, stop = 4)
BFecha.Cub4<-substr(Fecha.Cub4, start = 5, stop = 6)
BFecha.Cub4<-str_remove_all(BFecha.Cub4, "  ")
AFecha.Cub4<-str_remove_all(AFecha.Cub4, "  ")

for (i in 1:length(BFecha.Cub4)) {
  if(BFecha.Cub4[i]!=""){
    if(as.numeric(BFecha.Cub4[i])>50){
      BFecha.Cub4[i]<-as.numeric(BFecha.Cub4[i])+1900
    }
    else{
      BFecha.Cub4[i]<-as.numeric(BFecha.Cub4[i])+2000
    }
  }
}
ABFecha.Cub4<-str_c(AFecha.Cub4, BFecha.Cub4)

df$Fecha.Cub4<-dmy(ABFecha.Cub4)
table(is.na(df$Fecha.Cub4))
table(AFecha.Cub4)

##Fecha.Cub5
AFecha.Cub5<-substr(Fecha.Cub5, start = 1, stop = 4)
BFecha.Cub5<-substr(Fecha.Cub5, start = 5, stop = 6)
AFecha.Cub5<-str_remove_all(BFecha.Cub5, "  ")
BFecha.Cub5<-str_remove_all(BFecha.Cub5, "  ")

for (i in 1:length(BFecha.Cub5)) {
  if(BFecha.Cub5[i]!=""){
    if(as.numeric(BFecha.Cub5[i])>50){
      BFecha.Cub5[i]<-as.numeric(BFecha.Cub5[i])+1900
    }
    else{
      BFecha.Cub5[i]<-as.numeric(BFecha.Cub5[i])+2000
    }
  }
}
ABFecha.Cub5<-str_c(AFecha.Cub5, BFecha.Cub5)

df$Fecha.Cub5<-dmy(ABFecha.Cub5)

table(is.na(df$Fecha.Cub5))
table(AFecha.Cub5)

##Fecha.Parto
AFecha.Parto<-substr(Fecha.Parto, start = 1, stop = 4)
BFecha.Parto<-substr(Fecha.Parto, start = 5, stop = 6)
AFecha.Parto<-str_remove_all(AFecha.Parto, "  ")
BFecha.Parto<-str_remove_all(BFecha.Parto, "  ")

for (i in 1:length(BFecha.Parto)) {
  if(BFecha.Parto[i]!=""){
    if(as.numeric(BFecha.Parto[i])>50){
      BFecha.Parto[i]<-as.numeric(BFecha.Parto[i])+1900
    }
    else{
      BFecha.Parto[i]<-as.numeric(BFecha.Parto[i])+2000
    }
  }
}
ABFecha.Parto<-str_c(AFecha.Parto, BFecha.Parto)

df$Fecha.Parto<-dmy(ABFecha.Parto)
table(is.na(df$Fecha.Parto))
table(AFecha.Parto)

##FECHA.DESTETE
AFECHA.DESTETE<-substr(FECHA.DESTETE, start = 1, stop = 4)
BFECHA.DESTETE<-substr(FECHA.DESTETE, start = 5, stop = 6)
AFECHA.DESTETE<-str_remove_all(AFECHA.DESTETE, "  ")
BFECHA.DESTETE<-str_remove_all(BFECHA.DESTETE, "  ")

for (i in 1:length(BFECHA.DESTETE)) {
  if(BFECHA.DESTETE[i]!=""){
    if(as.numeric(BFECHA.DESTETE[i])>50){
      BFECHA.DESTETE[i]<-as.numeric(BFECHA.DESTETE[i])+1900
    }
    else{
      BFECHA.DESTETE[i]<-as.numeric(BFECHA.DESTETE[i])+2000
    }
  }
}
ABFECHA.DESTETE<-str_c(AFECHA.DESTETE, BFECHA.DESTETE)

df$FECHA.DESTETE<-dmy(ABFECHA.DESTETE)
table(is.na(df$FECHA.DESTETE))
table(AFECHA.DESTETE)
