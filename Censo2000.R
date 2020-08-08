####################################Pacotes
library(fastDummies)
library(dplyr)
library(foreach)
library(plyr)
###################################Importando os Resultados
dom00<-read.csv("/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Databases/DOMF2000.csv",na="0")
pes00<-read.csv("/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Databases/PESF2000.csv",na="0")
dom00[is.na(dom00)]<-0
pes00[is.na(pes00)]<-0
####################################Ajustando
####################################Criando Dummies
Lixo<-dummy_columns(dom00$V0212)$.data_1+dummy_cols(dom00$V0212)$.data_2
Esgoto<-dummy_columns(dom00$V0211)$.data_1
Agua<-dummy_columns(dom00$V0207)$.data_1+dummy_cols(dom00$V0207)$.data_2
Iluminação<-dummy_columns(dom00$V0213)$.data_1
Banheiros<-1-dummy_columns(dom00$V0209)$.data_0
Geladeira<-dummy_columns(dom00$V0215)$.data_1
Radio<-dummy_columns(dom00$V0214)$.data_1
Telefone<-dummy_columns(dom00$V0219)$.data_1
#Televisão<-dummy_columns(dom00$V0221)$.data_1
Automoveis<-1-dummy_columns(dom00$V0222)$.data_0
Condições<-dummy_columns(dom00$V0205)$.data_1+dummy_cols(dom00$V0205)$.data_2
###################################Variaveis pessoas
Escola=pes00[,c('V4300','V0300')]
domicilio=pes00[,c('V0300',"V4300")]
Colunas=unique(pes00$V0300)
#Colunas=as.data.frame(Colunas)
Escolaridade=c()
for(i in Colunas){
    Controle=Escola %>% select('V4300','V0300') %>% filter(V0300==as.character(i))
    num=c()
     for(j in Controle$V4300){
         if(j>=0 ){
             print(j)
             num<-append(num,0)
         }
         else if(j<=7){
             num<-append(num,0)
        }
         else{
             num<-append(num,1)
         }
     }
     if(mean(num) > 0){
         Escolaridade<-append(Escolaridade,1)
     }
     else{
         Escolaridade<-append(Escolaridade,0)
     }
}
  

Freq=pes00%>%select('V0628','V0300')
domicilio=pes00[,c('V0300',"V0628")]
Colunas=unique(pes00$V0300)
#Colunas=as.data.frame(Colunas)
Frequencia=c()
for(i in Colunas){
    Controle=Freq %>% select('V0628','V0300') %>% filter(V0300==as.character(i))
    num=c()
     for(j in Controle$V0628){
         if(j>=0 ){
             print(j)
             num<-append(num,0)
         }
         else if(j<=7){
             num<-append(num,0)
        }
         else{
             num<-append(num,1)
         }
     }
     if(mean(num) > 0){
         Frequencia<-append(Frequencia,1)
     }
     else{
         Frequencia<-append(Frequencia,0)
     }
}
  
Ren=pes00%>%select('V4615','V0300')
domicilio=pes00[,c('V0300',"V4615")]
Colunas=unique(pes00$V0300)
#Colunas=as.data.frame(Colunas)
Renda=c()
for(i in Colunas){
    Controle=Ren %>% select('V4615','V0300') %>% filter(V0300==as.character(i))
    num=c()
     for(j in Controle$V4615){
         if(j>=0 ){
             print(j)
             num<-append(num,0)
         }
         else if(j<=7){
             num<-append(num,0)
        }
         else{
             num<-append(num,1)
         }
     }
     if(mean(num) > 0){
         Renda<-append(Renda,1)
     }
     else{
         Renda<-append(Renda,0)
     }
}
  
data=data.frame("Lixo"=Lixo,"Esgoto"=Esgoto,"Agua"=Agua,"Iluminação"=Iluminação,"Banheiros"=Banheiros,"Geladeira"=Geladeira,"Radio"=Radio,"Telefone"=Telefone,"Automoveis"=Automoveis,"Condições"=Condições),#"Escolaridade"=Escolaridade,"Renda"=Renda)#"Televisão"=Televisão,"Frequencia"=Frequencia)
data2=dom00%>%select(V0102,V1002,V1003, V0103,V0104,V0105,V0300, V0400, V1004, AREAP, V1001, V1005, V1006)
data2=join(data2,data)
