library(lubridate)
library(dplyr)
library (janitor)
tabla<-read_delim("F:/Navarrabiomed/Methodology/Github/BARDENA/datos/Indice_Charlons.txt",
                    +                   "\t", escape_double = FALSE,col_names = FALSE,
                    +                   locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                    +                   trim_ws = TRUE)
tabla_CIAP<-read_delim("F:/Navarrabiomed/Methodology/Github/BARDENA/datos/Indice_Charlson_20220407.txt",
                       +                   ";", escape_double = FALSE,col_names = TRUE,
                       +                   locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                       +                   trim_ws = TRUE)
colnames(tabla)<-colnames(tabla_CIAP)
rbind(tabla,tabla_CIAP)->tablajunta

set.seed(1234)

tablajunta %>% mutate(fecha=ymd(tablajunta$ResDiag_Inicio_Fecha))->tablajunta

tablajunta %>% mutate(fecha2=if_else(is.na(fecha),excel_numeric_to_date(as.numeric(ResDiag_Inicio_Fecha)),fecha))->tablajunta2

tablajunta->toscramble
tablajunta$PAc_Unif_Cod[sample(1:nrow(tablajunta))]->toscramble$PAc_Unif_Cod
set.seed(3829)
tablajunta2$fecha2[sample(1:nrow(tablajunta2))]->toscramble$ResDiag_Inicio_Fecha

set.seed(9283)
sample(unique(toscramble$PAc_Unif_Cod),500)->lista_pacientes
toscramble %>% filter(PAc_Unif_Cod%in%lista_pacientes)->muestra_random
muestra_random %>% distinct(id) %>% mutate(orden=1:n())->muestra_random_3
muestra_random %>% left_join(muestra_random_3,by="id")->muestra_random_4
muestra_random_4[,c(5,2:4)]->muestra_random_5
colnames(muestra_random_5)<-c("id","cod","codtype","date")
class(muestra_random_5)<-"data.frame"
muestra_random_5->example

save(example,file="example.rda")
