library(survey)
library(laeken)
library(convey)

IPM00<-"/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Script/data2.csv"
IPM10<-"/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Script/data10.csv"
IPM00<-read.csv(IPM00)
IPM10<-read.csv(IPM10)
#################2000
AF00<-svydesign(ids=~1,data=IPM00)
AF00<-convey_prep(AF00)
AFs00<-svyafc(~as.numeric(Esgoto)+as.numeric(Lixo),design=AF00,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
AFe00<-svyafc(~as.numeric(Frequencia)+as.numeric(Escoridade),design=AF00,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
AFc00<-svyafc(~as.numeric(Agua)+as.numeric(Iluminação)+as.numeric(Banheiros)+as.numeric(Geladeira)+as.numeric(Radio)+as.numeric(Telefone),design=AF00,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
AFr00<-svyafc(~as.numeric(Renda),design=AF00,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
################2010
AF10<-svydesign(ids=~1,data=IPM10)
AF10<-convey_prep(AF10)
AFs10<-svyafc(~as.numeric(Esgoto)+as.numeric(Lixo),design=AF10,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
AFe10<-svyafc(~as.numeric(Frequencia)+as.numeric(Escoridade),design=AF10,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
AFc10<-svyafc(~as.numeric(Agua)+as.numeric(Iluminação)+as.numeric(Banheiros)+as.numeric(Geladeira)+as.numeric(Radio)+as.numeric(Telefone),design=AF10,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
AFr10<-svyafc(~as.numeric(Renda),design=AF10,k=1/3,cutoffs=list(1,1),g=1,na.rm=TRUE)
