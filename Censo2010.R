library(fastDummies)
library(dplyr)
library(foreach)
dom10<-read.csv("/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Databases/DOMF2010.csv",na="0")
pes10<-read.csv("/home/alexandre/Documentos/Artigos/Indice de Pobreza Multidimensional - Ceará/Databases/PESF2010.csv",na="0")
dom10[is.na(dom10)]<-0
pes10[is.na(pes10)]<-0
Lixo<-dummy_columns(dom10$V0210)$.data_1+dummy_cols(dom10$V0210)$.data_2
Esgoto<-dummy_columns(dom10$V0207)$.data_1
#Agua<-dummy_columns(dom10$V0209)$.data_1+dummy_cols(dom10$V0209)$.data_2
Iluminação<-dummy_columns(dom10$V0211)$.data_1
Banheiros<-1-dummy_columns(dom10$V0205)$.data_0
#Geladeira<-dummy_columns(dom00$V021)$.data_1
Radio<-dummy_columns(dom10$V0213)$.data_1
Telefone<-dummy_columns(dom10$V0218)$.data_1
#Televisão<-dummy_columns(dom00$V0214)$.data_1
Automoveis<-1-dummy_columns(dom10$V0222)$.data_0
Condições<-dummy_columns(dom10$V0201)$.data_1+dummy_cols(dom10$V0205)$.data_2
Renda<-dummy_columns((1/510)*dom10$V6531)
###################################Variaveis pessoas
Escola=pes00[,c('V0663','V0300')]
domicilio=pes00[,c('V0300',"V4300")]
Colunas=unique(pes00$V0300)
#Colunas=as.data.frame(Colunas)
Escolaridade=c()
for(i in Colunas){
    Controle=Escola %>% select('V0633','V0300') %>% filter(V0300==as.character(i))
    num=c()
     for(j in Controle$V0633){
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


data2=dom10%>%select(V0002,V1002,V1003,V0300,V1004)
data2[c("Lixo","Esgoto","Agua","Iluminação","Banheiros","Geladeira","Radio","Telefone","Automoveis")]<-c(Lixo,Esgoto,Agua,Iluminação,Banheiros,Geladeira,Radio,Telefone,Automoveis)#"Condições"=Condições),#"Escolaridade"=Escolaridade,"Renda"=Renda)#"Televisão"=Televisão,"Frequencia"=Frequencia)
write.csv(data2,"data10.csv")

