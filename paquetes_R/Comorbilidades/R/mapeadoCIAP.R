Codigos_ciap_Charlton <- read_excel("F:/BARDENA/paquetes_R/Codigos_ciap_Charlton.xlsx", sheet = "Hoja2", col_types = c("text","skip", "text"))
Codigos_ciap_Charlton$Dtcos_CIAP[is.na(Codigos_ciap_Charlton$Dtcos_CIAP)==FALSE]->somecod
colnames(Codigos_ciap_Charlton)<-c("grupo_","CIAP2")


Codigos_ciap_Charlton$Dtcos_CIAP[is.na(Codigos_ciap_Charlton$Dtcos_CIAP)==FALSE]->somecod

load("~/BARDENA/paquetes_R/Comorbilidades/data/allcod.rda")
load("~/BARDENA/paquetes_R/Comorbilidades/data/dicod.rda")

allcod<-unlist(strsplit(x=append(allcod,unlist(strsplit(x = somecod,split = ";"))),split="\\^" ))#aquí hemos añadido los códigos CIAP2 a la lista, pero veo que hay solapamientos con los CIE10... T82 es obesidad en CIAP2 y complicacion de protesis en cie10 )

#trataré de traducir el ciap2 a cie9 y así nos los quitamos.(https://www.sanidad.gob.es/ca/estadEstudios/estadisticas/estadisticas/estMinisterio/SIAP/map_cie9mc_cie10_ciap2.htm)
Mapeo_CIE10ES2020_BDCAP_31_07_2020 <- read_excel("F:/BARDENA/IndiceCharlson/Mapeo_CIE10ES2020_BDCAP_31_07_2020.xlsx",  sheet = "Correspondencia(2)", col_types = c("text", "skip", "text", "skip"))
tripi<-as.data.frame(dicod)
Mapeo_CIE10 <- read_excel("F:/BARDENA/IndiceCharlson/Mapeo_CIE10ES2020_BDCAP_31_07_2020.xlsx",  sheet = "Correspondencia(2)")


colnames(Mapeo_CIE10ES2020_BDCAP_31_07_2020)<-c("CIAP2","cie10")


Mapeo_CIE10ES2020_BDCAP_31_07_2020%>%mutate(cie10=gsub(x =cie10 ,pattern="\\.",replacement = ""))->mapeo

esi<-data.frame(matrix(ncol=2))
colnames(esi)<-c("grupo","cie10")
for (i in colnames(tripi)){
  unlist(strsplit(x = gsub(pattern =  "\\^",replacement="",unlist(tripi[colnames(tripi)==i])),split = "\\|"))->esto
  data.frame(matrix(nrow=length(esto),ncol=2))->eso
  colnames(eso)<-c("grupo","cie10")
  eso$cie10<-esto
  eso$grupo<-i
  rbind(esi,eso)->esi
}

esi%>%filter(is.na(as.numeric(cie10)))%>%distinct(grupo,cie10)->grupos_y_cie10
left_join(grupos_y_cie10,mapeo,by="cie10")%>%filter(is.na(CIAP2)==FALSE)%>%distinct(grupo,CIAP2)->mapeado

library(tidyr)
library(dplyr)
library(data.table)
mapeado %>%
  mutate(CIAP2 = strsplit(as.character(CIAP2), "/")) %>%
  unnest(CIAP2)%>%
  mutate(CIAP2=ifelse(nchar(CIAP2)==2&nchar(lag(x = CIAP2))==3,paste0(strtrim(lag(x = CIAP2,default = ""),1),CIAP2),ifelse(nchar(CIAP2)==2&nchar(lag(x = CIAP2))==2,paste0(strtrim(lag(x = CIAP2,n=2,default = ""),1),CIAP2),CIAP2)))->mapeado

mapeado%>%mutate(new=paste0("^",CIAP2))%>%group_by(grupo)%>%mutate(n=1:n())%>%ungroup()%>%mutate(new=ifelse(n>1,paste0("|",new),new))%>%select(grupo,new)->ppp

lista=NULL
for(i in unique(ppp$grupo)){

  lele<-assign(as.list(ppp%>%filter(grupo==i)%>%transmute(i=new)),x = i)
  lista=append(lista,lele)
}

#


save(mapeado,file = "mapeado.rda")
