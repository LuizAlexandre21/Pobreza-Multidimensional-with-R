#####################################Pacotes
library(readxl)
library(foreach)
library(dplyr)
#################################Função
IPM=function(dados){
    colunas<-ncol(dados)
    cole=c(1:ncol(dados))
    colnames(dados)<-cole
    dados[is.na(dados)]<-0
    A=c(0)
    for(i in cole){
        n=A[i]+dados[i]
        A<-append(A,n)
    }
    b=data.frame(A)[colunas+1]
    b=b/colunas
    return(b)
}
###########################Importando resultados
IPM00<-"/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Script/data2.csv"
IPM10<-"/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Script/data10.csv"
IPM00<-read.csv(IPM00)
IPM10<-read.csv(IPM10)

#######################Subindices
#Censo00
#######Saúde
saude00<-IPM00%>%select(Esgoto,Lixo)
IPMS00<-IPM(saude)
######Condições
condições00<-IPM00%>%select(Agua,Iluminação,Banheiros,Geladeira,Radio,Telefone)#ormitorio,Automoveis,Condições)
IPMC00<-IPM(condições)
######Educação
educação00<-IPM00%>%select(Frequencia,Escolaridade)
IPME00<-IPM(educação)
######Renda
IPMR00<-IPM00$Renda
######Geral
geral00<-IPM00%>%select(Esgoto,Lixo,Aguas,Iluminação,Banheiros,Geladeira,Radio,Telefone,Dormitorio,Automoveis,Condições,Frequencia,Escolaridade,Renda)
IPMG00(geral)
ipm<-data.frame("IPMS"=IPMS,"IPMC"=IPMC,"IPME"=IPME,"IPMR"=IPMR,"IPMG"=IPMG)
data<-IPM00%>%select(V0102,V1002,V1003,V0103,V0104,V0105,V0300,V0400,V1004,AREAP,V1001,V1005,V1006)
merge(ipm,data)

##Censo10
saude10<-IPM10%>%select(Esgoto,Lixo)
IPMS10<-IPM(saude)
######Condições
condições10<-IPM10%>%select(Agua,Iluminação,Banheiros,Geladeira,Radio,Telefone)#ormitorio,Automoveis,Condições)
IPMC10<-IPM(condições)
######Educação
educação10<-IPM10%>%select(Frequencia,Escolaridade)
IPME10<-IPM(educação)
######Renda
IPMR10<-IPM01$Renda
######Geral
geral<-IPM10%>%select(Esgoto,Lixo,Aguas,Iluminação,Banheiros,Geladeira,Radio,Telefone,Dormitorio,Automoveis,Condições,Frequencia,Escolaridade,Renda)
IPMG10(geral)
ipm10<-data.frame("IPMS"=IPMS,"IPMC"=IPMC,"IPME"=IPME,"IPMR"=IPMR,"IPMG"=IPMG)
data<-IPM10%>%select(V0002,V1002,V1003,V0300,V1004)
merge(ipm10,data)


############Resultados Agregados
ceara<-data.frame("IPMS00"=mean(IPMS00),"IPMC00"=mean(IPMC00),"IPMR00"=mean(IPMR00),"IPME00"=mean(IPME00),"IPMG00"=mean(IPMG00),"IPMS10"=mean(IPMS10),"IPMC10"=mean(IPMC10),"IPMR10"=mean(IPMR10),"IPME10"=mean(IPME10),"IPMG10"=mean(IPMG10))
write.csv(ceara,IPM_resultado.csv)
