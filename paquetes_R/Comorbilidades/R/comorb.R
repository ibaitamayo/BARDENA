#' Comorbilidades
#'
#' modified version of comorbidity to work simultaneously with icd9 and icd10, and Smoking group extraction
#'
#' @name comorb
#'
#' @param archivo Archivo refers to either the data frame or to the txt.
#' @param id The name of the column the patients identification codes are found.
#' @param code code refers to the name of the column where the disease codes will be found
#' @param type type refers to the name of the column where the type of codification that "code" is being codified. Usually this column will contain values such as "ICD9", "ICD10" or "CIAP2" or similar
#' @param fecha fecha refers to the name of the column where the date of the coded disease is found
#' @param granularidad This boolean parameter defines if mild and severe formas of the disease should be distinguished, and will be applied to liver disease, diabetes and cancer.
#' @param mini This numeric parameters  defines the number of rows from the "archivo" that the package should be passed on.
#' @return modified version of comorbidity to use  ICD9 , ICD10 and CIAP2 to return Charlson score, elixhauser score and first event date.
#'
#' @export
#'
comorb<-function(archivo,id="Pac_Unif_Cod",code="Diag_Cod", type="type",fecha="ResDiag_Inicio_Fecha",granularidad = TRUE,mini=NULL) {

  require(dplyr)
  require(tidyr)
  require(readr)
  require(data.table)
  require(stringr)
  require(DT)
  require(lubridate)
  require(tools)



  if(is.data.frame(archivo)){
    message("Loading Dataframe","\n")
    tabla<-archivo

  }else if(tools::file_ext(archivo)=="txt"){
    message("Loading txt file","\n")
    tabla<-read_delim(archivo,
                      ";", escape_double = FALSE,col_names = TRUE,
                      locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                      trim_ws = TRUE)
  }
  # else if(tools::file_ext(archivo)=="csv"){
  #   tabla<-read_delim(archivo,
  #                     ";", escape_double = FALSE, col_types = cols(ResDiag_Inicio_Fecha= col_character()),
  #                     locale = locale(encoding = "ISO-8859-1"),
  #                     trim_ws = TRUE)
  #   colnames(tabla)<-c(id,code,"type",fecha)
  #
  #
  # }
  if(is.null(mini)==FALSE){
    tabla<-head(tabla,mini)
  }

  colnames(tabla)<-c(id,code,"type",fecha)
  tabla <- tabla %>%mutate(ResDiag_Inicio_Fecha=lubridate::ymd(str_trunc(ResDiag_Inicio_Fecha,width = 10,ellipsis = "")))

  tabla%>%distinct(Pac_Unif_Cod)->allpeople
  allpeople$Pac_Unif_Cod->allpeople
  message(paste0("load complete...",paste0(length(allpeople)," patients detected")))

  tabla <- tabla %>% dplyr::mutate(Diag_Cod=gsub("\\.","",Diag_Cod))%>%filter(strtrim(Diag_Cod,3)%in%Comorbilidades:::allstrimcod)#corto a 3 digitos y lo comparo con los eventos cortados a 3 digitos,x velocidad

  tabla<-tabla%>%mutate(Diag_Cod=ifelse(str_starts(string=tolower(type),pattern = "(ciap)"),paste0("_",Diag_Cod),Diag_Cod))%>%filter(is.na(Pac_Unif_Cod)==FALSE)#esto es lo que he añadido para que lea los ciap2
  tabla<-tabla%>%dplyr::select(all_of(c(id,code,fecha)))
  colnames(tabla)<-c("id","code","fecha")
  # message("CIAP2 processed")


  ### 1. construye in identificador compuesto de id.fecha.codecie ####
  tabla_ <- tabla %>% dplyr::mutate(idfechacod=paste(as.character(id),as.character(fecha),code, sep=",")) %>% relocate(idfechacod, .before=id)  %>% dplyr::select(-all_of(c("id","fecha"))) %>% setDT()
  # message("information_joined")
  ## 2. se generan las diversas comorbilidades empleando los codigos del objeto Comorbilidades:::tricod ####

  for(i in seq(1,length(Comorbilidades:::tricod))) {
    dc=Comorbilidades:::tricod[[i]]
    varname=names(Comorbilidades:::tricod)[i]
    tabla_ <- tabla_ %>% mutate(!!varname:=as.numeric(str_detect(code,dc)))
  }
  message("disease matrix generated")

  mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[3:45]) %>% setDT()
  # message("diseases grouped")
  # mpp=mpp[value!=0]
  mpp=mpp[with(mpp,value!=0),]
  # message("empty disease groups eliminated")
  # mpp[, c("id","fecha","code"):=tstrsplit(x=idfechacod, "," , fixed=TRUE)]
  # mpp[, c("id","fecha","code"):=tstrsplit(idfechacod, "," , fixed=TRUE,names=TRUE)]## eliminar registros con codigos no incluidos
  tidyr::separate_wider_delim(data=mpp,cols="idfechacod",delim=",",names=c("id","fecha","code"),cols_remove = TRUE)->mpp
  # mpp=mpp[, -"idfechacod"]
  # message("disaggregating information")

  ## S1 17g+score generar el fichero previo (a falta del cálculo de pesos) de la S1

  ppf=mpp %>%
    pivot_wider(id_cols = id,names_from = variable,values_from = fecha,
    values_fn = ~min(.x,na.rm=TRUE))
  message("calculating first events")

  if(any(names(Comorbilidades:::tricod)%in%colnames(ppf)==FALSE)){
    colvacias<-data.frame(matrix(data=NA,nrow = dim(ppf)[1],ncol=sum((as.numeric(names(Comorbilidades:::tricod)%in%colnames(ppf)-1)^2))))
    colnames(colvacias)<-names(Comorbilidades:::tricod)[names(Comorbilidades:::tricod)%in%colnames(ppf)==FALSE]
    cbind(ppf,colvacias)->ppf
  }

  pps<- ppf %>% mutate(across(names(Comorbilidades:::tricod),function(x){x=ifelse(is.na(x),0,1)}))
  # message("preprocessing values for scores")

  data.table::setDF(pps)
  pps$score_ch <- with(pps, ami + chf + pvd + cevd + dementia +
                         copd + rheumd_ch + pud_ch + mld * ifelse(msld == 1 & granularidad,
                                                                  0, 1) + diabwoc * ifelse(diabwic == 1 & granularidad, 0, 1) +
                         diabwic + hp + rend + canc * ifelse(metacanc == 1 &
                                                               granularidad, 0, 1) + msld + metacanc + aids)
  pps$index_ch <- with(pps, cut(score_ch, breaks = c(0, 1, 2.5, 4.5,
                                                     Inf), labels = c("0", "1-2", "3-4",
                                                                      ">=5"), right = FALSE))
  pps$wscore_ch <- with(pps, ami + chf + pvd + cevd + dementia +
                          copd + rheumd_ch + pud_ch + mld * ifelse(msld == 1 & granularidad,
                                                                   0, 1) + diabwoc * ifelse(diabwic == 1 & granularidad, 0, 1) +
                          diabwic * 2 + hp * 2 + rend * 2 + canc * ifelse(metacanc ==
                                                                            1 & granularidad, 0, 2) + msld * 3 + metacanc * 6 + aids *
                          6)
  pps$windex_ch <- with(pps, cut(wscore_ch, breaks = c(0, 1, 2.5,
                                                       4.5, Inf), labels = c("0", "1-2", "3-4",
                                                                             ">=5"), right = FALSE))
  pps$score_hx <- with(pps, chf + carit + valv + pcd + pvd + hypunc *
                         ifelse(hypc == 1 & granularidad, 0, 1) + hypc + para +
                         ond + copd + diabunc * ifelse(diabc == 1 & granularidad,
                                                       0, 1) + diabc + hypothy + rf + ld + pud_hx + aids +
                         lymph + metacanc + solidtum * ifelse(metacanc ==
                                                                1 & granularidad, 0, 1) + rheumd_hx + coag + obes + wloss +
                         fed + blane + dane + alcohol + drug + psycho + depre)
  pps$index_hx <- with(pps, cut(score_hx, breaks = c(-Inf, 0, 1,
                                                     4.5, Inf), labels = c("<0", "0", "1-4",
                                                                           ">=5"), right = FALSE))
  pps$wscore_ahrq <- with(pps, chf * 9 + carit * 0 + valv *
                            0 + pcd * 6 + pvd * 3 + ifelse(hypunc == 1 | hypc ==
                                                             1, 1, 0) * (-1) + para * 5 + ond * 5 + copd * 3 +
                            diabunc * ifelse(diabc == 1 & granularidad, 0, 0) + diabc *
                            (-3) + hypothy * 0 + rf * 6 + ld * 4 + pud_hx * 0 +
                            aids * 0 + lymph * 6 + metacanc * 14 + solidtum *
                            ifelse(metacanc == 1 & granularidad, 0, 7) + rheumd_hx *
                            0 + coag * 11 + obes * (-5) + wloss * 9 + fed * 11 +
                            blane * (-3) + dane * (-2) + alcohol * (-1) + drug *
                            (-7) + psycho * (-5) + depre * (-5))
  pps$wscore_hxvw <- with(pps, chf * 7 + carit * 5 + valv * (-1) +
                            pcd * 4 + pvd * 2 + ifelse(hypunc == 1 | hypc ==
                                                         1, 1, 0) * 0 + para * 7 + ond * 6 + copd * 3 + diabunc *
                            ifelse(diabc == 1 & granularidad, 0, 0) + diabc * 0 +
                            hypothy * 0 + rf * 5 + ld * 11 + pud_hx * 0 + aids *
                            0 + lymph * 9 + metacanc * 12 + solidtum * ifelse(metacanc ==
                                                                                1 & granularidad, 0, 4) + rheumd_hx * 0 + coag * 3 + obes *
                            (-4) + wloss * 6 + fed * 5 + blane * (-2) + dane *
                            (-2) + alcohol * 0 + drug * (-7) + psycho * 0 + depre *
                            (-3))
  pps$windex_ahrq <- with(pps, cut(wscore_ahrq, breaks = c(-Inf,
                                                           0, 1, 4.5, Inf), labels = c("<0", "0",
                                                                                       "1-4", ">=5"), right = FALSE))
  pps$windex_vw <- with(pps, cut(wscore_hxvw, breaks = c(-Inf,
                                                         0, 1, 4.5, Inf), labels = c("<0", "0",
                                                                                     "1-4", ">=5"), right = FALSE))

  ppf%>%left_join(pps%>%dplyr::select(all_of(c("id","score_ch","index_ch","wscore_ch","windex_ch","score_hx","index_hx","wscore_ahrq","wscore_hxvw","windex_vw"))),by="id")->ppf
  message("scores calculated")
  if(any(allpeople%in%ppf$id==FALSE)){
    data.frame(matrix(data = NA,nrow = length(allpeople[allpeople%in%ppf$id==FALSE]),ncol=dim(ppf)[2]))->fillpatient
    colnames(fillpatient)<-colnames(ppf)
    fillpatient$id<-allpeople[allpeople%in%ppf$id==FALSE]
    fillpatient[,45:53]<-0#los pacientes que no presentan ninguna de las comorbilidades se les asigna un 0 en los valores de charlson y elixhauser
    rbind(ppf,fillpatient)->ppf

  }
  # message("fit people reincorporated")
  return(ppf)
  }
