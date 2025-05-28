library(tidyr)
library(dplyr)
library(data.table)
library(readxl)
library(stringr)

# Codigos_ciap_Charlton <- read_excel("F:/BARDENA/paquetes_R/Codigos_ciap_Charlton.xlsx", sheet = "Hoja2", col_types = c("text","skip", "text"))
# Codigos_ciap_Charlton$Dtcos_CIAP[is.na(Codigos_ciap_Charlton$Dtcos_CIAP)==FALSE]->somecod
# colnames(Codigos_ciap_Charlton)<-c("grupo_","CIAP2")
#
#
# Codigos_ciap_Charlton$Dtcos_CIAP[is.na(Codigos_ciap_Charlton$Dtcos_CIAP)==FALSE]->somecod

load("~/BARDENA/paquetes_R/Comorbilidades/data/allcod.rda")
load("~/BARDENA/paquetes_R/Comorbilidades/data/dicod.rda")


# summary(as.factor(names(dicod)))#hay valores repetidos quitar el dicod[21] porque son exactamente los mismo valores
dicod[-21]->dicod
names(dicod)[c(8,30)]<-c("pud_ch","pud_hx")

dicod$lymph<-"^200|^201|^202|^2030|^2386|^C81|^C82|^C83|^C84|^C85|^C88|^C900|^C902|^C96"
#renombramos los de la úlcera porque son distintos valores para elixhauseer y para charlson

# hay valores faltantes en solidtum:C00.x - C26.x, C30.x - C34.x, C37.x - C41.x, C43.x, C45.x - C58.x, C60.x - C76.x, C97.x
# paste0("C",00:26)
# paste0("C",30:34)
# paste0("C",37:41)
# paste0("C",45:58)
# paste0("C",60:76)
# C43
# C97

faltan_solid<-"^C00|^C01|^C02|^C03|^C04|^C05|^C06|^C07|^C08|^C09|^C10|^C11|^C12|^C13|^C14|^C15|^C16|^C17|^C18|^C19|^C20|^C21|^C22|^C23|^C24|^C25|^C26|^C30|^C31|^C32|^C33|^C34|^C37|^C38|^C39|^C40|^C41|^C45|^C46|^C47|^C48|^C49|^C50|^C51|^C52|^C53|^C54|^C55|^C56|^C57|^C58|^C60|^C61|^C62|^C63|^C64|^C65|^C66|^C67|^C68|^C69|^C70|^C71|^C72|^C73|^C74|^C75|^C76|^C43|^C97"

dicod$solidtum<-paste0(dicod$solidtum,faltan_solid)


# allcod<-unlist(strsplit(allcod,split = "\\^"))#aquí había un error  porque uno de los códigos estaba con el simbolo ^, separo el valor
# allcod<-allcod
# unique(strtrim(allcod,3))->allstrimcod
#
# paste0("^",allstrimcod)





# allcod<-unlist(strsplit(x=append(allcod,unlist(strsplit(x = somecod,split = ";"))),split="\\^" ))#aquí hemos añadido los códigos CIAP2 a la lista, pero veo que hay solapamientos con los CIE10... T82 es obesidad en CIAP2 y complicacion de protesis en cie10 )

#trataré de traducir el ciap2 a cie10 y así nos los quitamos.(https://www.sanidad.gob.es/ca/estadEstudios/estadisticas/estadisticas/estMinisterio/SIAP/map_cie9mc_cie10_ciap2.htm)
#Mapeo_CIE10ES2020_BDCAP_31_07_2020 <- read_excel("F:/BARDENA/IndiceCharlson/Mapeo_CIE10ES2020_BDCAP_31_07_2020.xlsx",  sheet = "Correspondencia(2)", col_types = c("text", "skip", "text", "skip"))
tripi<-as.data.frame(dicod)
Mapeo_CIE10 <- read_excel("C:/Usuarios/D127022/Documentos/Github/BARDENA/paquetes_R/IndiceCharlson/Mapeo_CIE10ES2020_BDCAP_31_07_2020.xlsx",  sheet = "Correspondencia(2)")

Mapeo_CIE10_BDCAP_30_10_2024 %>% mutate(cie10=CIE10,CIAP2=BDCAP) %>% dplyr::select(CIAP2,cie10)->Mapeo_CIE10ES2020_BDCAP_31_07_2020
# colnames(Mapeo_CIE10ES2020_BDCAP_31_07_2020)<-c("CIAP2","cie10")


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


esi%>%
  filter(is.na(as.numeric(cie10))&is.na(grupo)==FALSE&(str_detect(string = cie10,pattern = "^V")==FALSE))%>%
  distinct(grupo,cie10)->grupos_y_cie10

left_join(grupos_y_cie10,mapeo,by="cie10")%>%
  filter(is.na(CIAP2)==FALSE)%>%
  distinct(grupo,CIAP2)->mapeado






mapeado %>%
  mutate(CIAP2 = strsplit(as.character(CIAP2), "/")) %>%
  unnest(CIAP2)%>%
  mutate(CIAP2=ifelse(nchar(CIAP2)==2&nchar(lag(x = CIAP2))==3,
                      paste0(strtrim(lag(x = CIAP2,default = ""),1),CIAP2),
                      ifelse(nchar(CIAP2)==2&nchar(lag(x = CIAP2))==2,
                             paste0(strtrim(lag(x = CIAP2,n=2,default = ""),1),CIAP2),CIAP2)))->mapeado



# para_hx <- c("chf" , "carit" , "valv" , "pcd" , "pvd" ,"hypunc" , "hypc" ,"para" ,"ond" ,"copd" ,"diabunc" ,"diabc" ,"hypothy","rf" ,"ld" , "pud_hx" ,"aids" ,"lymph" , "metacanc" ,"solidtum" , "rheumd_hx" , "coag" , "obes" ,"wloss" ,"fed" ,"blane" ,"dane" ,"alcohol","drug" ,"psycho" ,"depre")
# View(mapeado%>%filter(grupo%in%para_hx))
# para_charlson<-c("ami" , "chf" , "pvd" , "cevd" , "dementia" , "copd" , "rheumd_ch" , "pud_ch" , "mld" ,"msld","diabwoc" ,"diabwic" , "hp" , "rend" , "canc" , "msld" ,"metacanc","aids")
# mapeado%>%filter(grupo%in%para_charlson)




# HELIXHAWSER A98 de drug,B74 de solidtum,B80 de dane,D97 de alcohol,K28 de pvd y carit,K71 de chf y carit,K82 de copd,K84 de chf,alcohol,carit,K87 dechf y rf,K99 de ld,reum,fed,L84 de rheumd_hx esta rep,N99 de para,P15 de ond,P73 de depre,T89 y T90 de diabc,T91 de alcohol
# CHARLSON: quitamos A79 de metacanc, d97 de msld,quitamos K87 de chd, quitamos K99 de msld, reumd_ch,N99 dementia y hp,t89y t90 de diabwic
mapeado%>%
  mutate(CIAP2=ifelse(str_detect(string=CIAP2,pattern="(R78|R79|R96|R99|S99|L86|L83|N94|A98|K71|A94|D99|K28|F83|L99|D28|N71|U28|A89|YX70|A87)"),"",CIAP2))%>%
  mutate(CIAP2=ifelse((grupo=="metacanc" &CIAP2=="A79")|(grupo=="msld" &CIAP2=="D97")|(grupo=="msld" &CIAP2=="K99")|(grupo=="chd" &CIAP2=="K87")|(grupo=="chd" &CIAP2=="K87")|(grupo=="rheumd_ch" &CIAP2=="K99")|(grupo=="dementia" &CIAP2=="N99")|(grupo=="hp" &CIAP2=="N99")|(grupo=="diabwic" &CIAP2=="T89")|(grupo=="diabwic" &CIAP2=="T90"),"",CIAP2))%>%
  mutate(CIAP2=ifelse((grupo=="drug" &CIAP2=="A98")|
                        (grupo=="solidtum" &CIAP2=="B74")|
                        (grupo=="dane" &CIAP2=="B80")|
                        (grupo=="alcohol" &CIAP2=="D97")|
                        (grupo=="carit" &CIAP2=="K28")|
                        (grupo=="pvd" &CIAP2=="K28")|
                        (grupo=="rheumd_hx" &CIAP2=="K99")|
                        (grupo=="carit" &CIAP2=="K71")|
                        (grupo=="chf" &CIAP2=="K71")|
                        (grupo=="copd" &CIAP2=="K82")|
                        (grupo=="chf" &CIAP2=="K84")|
                        (grupo=="alcohol" &CIAP2=="K84")|
                        (grupo=="carit" &CIAP2=="K84")|
                        (grupo=="chf" &CIAP2=="K87")|
                        (grupo=="rf" &CIAP2=="K87")|
                        (grupo=="ld" &CIAP2=="K99")|
                        (grupo=="reum" &CIAP2=="K99")|
                        (grupo=="fed" &CIAP2=="K99")|
                        (grupo=="para" &CIAP2=="N99")|
                        (grupo=="ond" &CIAP2=="P15")|
                        (grupo=="depre" &CIAP2=="P73")|
                        (grupo=="diabc" &CIAP2=="T89")|
                        (grupo=="diabc" &CIAP2=="T90")|
                        (grupo=="alcohol" &CIAP2=="T91"),"",CIAP2))%>%
  distinct(grupo,CIAP2)%>%
  group_by(grupo)%>%
  mutate(n=n())%>%
  ungroup()%>%
  mutate(quitar=ifelse(n>1&CIAP2=="",1,0))%>%
  filter(quitar==0)%>%
  dplyr::select(-c("n","quitar"))->mapeado2



# mapeado%>%mutate(new=paste0("^",CIAP2))%>%group_by(grupo)%>%mutate(n=1:n())%>%ungroup()%>%mutate(new=ifelse(n>1,paste0("|",new),new))%>%select(grupo,new)->ppp #por si los necesitamos por separado
mapeado2%>%mutate(new=ifelse(CIAP2!="",paste0("|^_",CIAP2),CIAP2))%>%  dplyr::select(grupo,new)->ppp

#vars to leave out:


lista=NULL
for(i in unique(ppp$grupo)){
  dd<-ppp%>%filter(grupo==i)%>%dplyr::select(new)
  # ppp%>%distinct(grupo)->kk

  mu=NULL
  for(j in seq(nrow(dd))){
    mu=paste0(mu,dd[j,1])
  }
  as.data.frame(mu)->mu
  colnames(mu)<-i

  lista=append(lista,mu)
}



Map(paste0,dicod, lista)->tricod
save(mapeado2,file = "mapeado.rda")
save(tricod,file = "tricod.rda")


unique(strtrim(unlist(str_split(gsub(x = tricod,pattern ="(\\|)|(_)",replacement = ""),pattern = "\\^")),3))[-1]->allstrimcod
save(allstrimcod,file = "allstrimcod.rda")
Reduce(paste0,tricod)->regx_todoscods
