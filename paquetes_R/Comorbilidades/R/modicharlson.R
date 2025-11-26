library(dplyr)

load("~/Github/BARDENA/paquetes_R/Comorbilidades/codigos.rda")
lista11->charlson_tabla
charlson_tabla %>%
  filter((enfermedad=="chf"&codigo=="_K76")==FALSE) %>%
  filter((enfermedad=="pvd"&codigo=="_K99")==FALSE) %>%
  filter((enfermedad=="hp"&codigo=="_K76")==FALSE) %>%
  filter((enfermedad=="rend"&codigo=="_K87")==FALSE) %>%
  transmute(charlson=case_when(enfermedad=="ami"~"myocardial infarction",
                      enfermedad=="chf"~"congestive heart failure",
                      enfermedad=="pvd"~"peripheral vascular disease",
                      enfermedad=="cevd"~"cerebrovascular disease",
                      enfermedad=="dementia"~"dementia",
                      enfermedad=="pcd"~"chronic pulmonary disease",
                      enfermedad=="rheumd_ch"~"rheumatoid disease",
                      enfermedad=="pud_ch"~"peptic ulcer disease",
                      enfermedad=="mld"~"mild liver disease",
                      enfermedad=="diabwoc"~"diabetes without complications",
                      enfermedad=="diabwic"~"diabetes with complications",
                      enfermedad=="hp"~"hemiplegia or paraplegia",
                      enfermedad=="rend"~"renal disease",
                      enfermedad=="canc"~"cancer (any malignancy)",
                      enfermedad=="msld"~"moderate or severe liver disease",
                      enfermedad=="metacanc"~"metastatic solid tumour",
                      enfermedad=="aids"~"AIDS/HIV"),
            Diag_Cod=gsub(codigo,pattern="\\^",replacement="")) %>%
              mutate(Diag_Tipo=ifelse(startsWith(prefix = "_",x=Diag_Cod),"CIAP2",
                         ifelse(grepl(pattern = "^[0-9]",x=Diag_Cod),"CIE9","CIE10"))) %>%
  mutate(Diag_Cod=ifelse(Diag_Tipo=="CIAP2",gsub(x=Diag_Cod,"_",""),Diag_Cod)) %>% filter(!is.na(charlson))->charlson_gorricho



library(MorbfindR)
library(readxl)


# setwd("C:/Users/D127022/Desktop")
library(readr)
lista_pac<-read_delim("Indice_Charlson_20220407.txt", ";", escape_double = FALSE,
col_names = TRUE, locale = locale(encoding = "ISO-8859-1"),
col_types = cols(X1 = col_double(), X2 = col_character(),
X3 = col_character(), X4 = col_character()),
trim_ws = TRUE)
# setwd("C:/Users/D127022/Documents/Github/Asesoria/Farmacia/Amaya Echeverria/desprescripcion IBP")
colnames(lista_pac)[1]<-"Pac_Unif_Cod"

P_7_Retirada_IBP_Datos_finales_enviados_v2 <- read_excel("datos/P_7_Retirada_IBP_Datos_finales_enviados_v2.xlsx",
                                                         sheet = "Población")

lista_pac %>% filter(Pac_Unif_Cod %in%P_7_Retirada_IBP_Datos_finales_enviados_v2$Pac_Unif_Cod )->lista_pac_enf



MorbfindR(data_disease = lista_pac_enf,
          id = "Pac_Unif_Cod",
          deduce_from = "from_disease",
          Disease_code = "Diag_Cod",
          Disease_codification = "Diag_Tipo",
          Disease_group =  "charlson",
          Disease_date = "ResDiag_Inicio_Fecha",
          Custom_disease_def = charlson_gorricho)->nuevo_charlson
nuevo_charlson->nuevo_charlson_IBP

library(openxlsx)
write.xlsx(nuevo_charlson_IBP,file = "Charlson_nuevo_2.xlsx")

