#' Comorbilidades
#'
#' modified version of comorbidity to work simultaneously with icd9 and icd10, and Smoking group extraction
#'
#' @name comorb
#'
#' @param archivo. Archivo refers to the location of the csv file that needs 3 columns. pacUnifCOde (patient identifier),Diag_Cod(ICD9 or ICD10 of the pathology),ResDGP_Fecha_Presta(date of the comorbidity).
#' @param mini. This numeric parameters  defines the number of rows from the "archivo" that the package should be passed on.
#' @return modified version of comorbidity to use  ICD9 , ICD10 and CIAP2 to return Charlson score, elixhauser score and first event date.
#'
#'
#'
#' @export
#'
comorb<-function(archivo="//Centcs01srv03/TABLEAU/Coronavirus/R/PacDiag2021.csv",id="Pac_Unif_Cod",code="Diag_Cod", fecha="ResDiag_Inicio_Fecha",assign0 = TRUE,mini=NULL) {

  require(dplyr)
  require(readr)
  require(data.table)
  require(stringr)
  require(DT)
  require(lubridate)
  require(tools)

  if(tools::file_ext(archivo)=="txt"){
    tabla<-read_delim(archivo,
                      ";", escape_double = FALSE,col_names = TRUE,
                      locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                      trim_ws = TRUE)
    colnames(tabla)<-c(id,code,"type",fecha)
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

  tabla <- tabla %>%mutate(ResDiag_Inicio_Fecha=lubridate::ymd(str_trunc(ResDiag_Inicio_Fecha,width = 10,ellipsis = "")))

  tabla%>%distinct(Pac_Unif_Cod)->allpeople
  allpeople$Pac_Unif_Cod->allpeople

  tabla <- tabla %>% dplyr::mutate(Diag_Cod=gsub("\\.","",Diag_Cod))%>%filter(strtrim(Diag_Cod,3)%in%allstrimcod)#corto a 3 digitos y lo comparo con los eventos cortados a 3 digitos,x velocidad

  tabla<-tabla%>%mutate(Diag_Cod=ifelse(str_starts(string=tolower(type),pattern = "(ciap)"),paste0("_",Diag_Cod),Diag_Cod))%>%filter(is.na(Pac_Unif_Cod)==FALSE)#esto es lo que he añadido para que lea los ciap2
  tabla<-tabla%>%dplyr::select(id,code,fecha)
  colnames(tabla)<-c("id","code","fecha")


  ### 1. construye in identificador compuesto de id.fecha.codecie ####
  tabla_ <- tabla %>% dplyr::mutate(idfechacod=paste(as.character(id),as.character(fecha),code, sep=",")) %>% relocate(idfechacod, .before=id)  %>% select(-c(id,fecha)) %>% setDT()

  ## 2. se generan las diversas comorbilidades empleando los codigos del objeto tricod ####

  for(i in seq(1,length(tricod))) {
    dc=tricod[[i]]
    varname=names(tricod)[i]
    tabla_ <- tabla_ %>% mutate(!!varname:=as.numeric(str_detect(code,dc)))
  }

  mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[3:45])
  mpp=mpp[value!=0]  ## eliminar registros con codigos no incluidos
  mpp[, c("id","fecha","code"):=tstrsplit(idfechacod, "," , fixed=TRUE)]
  mpp=mpp[, -"idfechacod"]

  ## S1 17g+score generar el fichero previo (a falta del cálculo de pesos) de la S1

  ppf=dcast.data.table(
    mpp, id ~ variable,
    value.var = 'fecha',
    fun = list(min))

  if(any(names(tricod)%in%colnames(ppf)==FALSE)){
    colvacias<-data.frame(matrix(data=NA,nrow = dim(ppf)[1],ncol=sum((as.numeric(names(tricod)%in%colnames(ppf)-1)^2))))
    colnames(colvacias)<-names(tricod)[names(tricod)%in%colnames(ppf)==FALSE]
    cbind(ppf,colvacias)->ppf
  }

  pps<- ppf %>% mutate(across(names(tricod),function(x){x=ifelse(is.na(x),0,1)}))

  data.table::setDF(pps)
  pps$score_ch <- with(pps, ami + chf + pvd + cevd + dementia +
                         copd + rheumd_ch + pud_ch + mld * ifelse(msld == 1 & assign0,
                                                                  0, 1) + diabwoc * ifelse(diabwic == 1 & assign0, 0, 1) +
                         diabwic + hp + rend + canc * ifelse(metacanc == 1 &
                                                               assign0, 0, 1) + msld + metacanc + aids)
  pps$index_ch <- with(pps, cut(score_ch, breaks = c(0, 1, 2.5, 4.5,
                                                     Inf), labels = c("0", "1-2", "3-4",
                                                                      ">=5"), right = FALSE))
  pps$wscore_ch <- with(pps, ami + chf + pvd + cevd + dementia +
                          copd + rheumd_ch + pud_ch + mld * ifelse(msld == 1 & assign0,
                                                                   0, 1) + diabwoc * ifelse(diabwic == 1 & assign0, 0, 1) +
                          diabwic * 2 + hp * 2 + rend * 2 + canc * ifelse(metacanc ==
                                                                            1 & assign0, 0, 2) + msld * 3 + metacanc * 6 + aids *
                          6)
  pps$windex_ch <- with(pps, cut(wscore_ch, breaks = c(0, 1, 2.5,
                                                       4.5, Inf), labels = c("0", "1-2", "3-4",
                                                                             ">=5"), right = FALSE))
  pps$score_hx <- with(pps, chf + carit + valv + pcd + pvd + hypunc *
                         ifelse(hypc == 1 & assign0, 0, 1) + hypc + para +
                         ond + copd + diabunc * ifelse(diabc == 1 & assign0,
                                                       0, 1) + diabc + hypothy + rf + ld + pud_hx + aids +
                         lymph + metacanc + solidtum * ifelse(metacanc ==
                                                                1 & assign0, 0, 1) + rheumd_hx + coag + obes + wloss +
                         fed + blane + dane + alcohol + drug + psycho + depre)
  pps$index_hx <- with(pps, cut(score_hx, breaks = c(-Inf, 0, 1,
                                                     4.5, Inf), labels = c("<0", "0", "1-4",
                                                                           ">=5"), right = FALSE))
  pps$wscore_ahrq <- with(pps, chf * 9 + carit * 0 + valv *
                            0 + pcd * 6 + pvd * 3 + ifelse(hypunc == 1 | hypc ==
                                                             1, 1, 0) * (-1) + para * 5 + ond * 5 + copd * 3 +
                            diabunc * ifelse(diabc == 1 & assign0, 0, 0) + diabc *
                            (-3) + hypothy * 0 + rf * 6 + ld * 4 + pud_hx * 0 +
                            aids * 0 + lymph * 6 + metacanc * 14 + solidtum *
                            ifelse(metacanc == 1 & assign0, 0, 7) + rheumd_hx *
                            0 + coag * 11 + obes * (-5) + wloss * 9 + fed * 11 +
                            blane * (-3) + dane * (-2) + alcohol * (-1) + drug *
                            (-7) + psycho * (-5) + depre * (-5))
  pps$wscore_hxvw <- with(pps, chf * 7 + carit * 5 + valv * (-1) +
                            pcd * 4 + pvd * 2 + ifelse(hypunc == 1 | hypc ==
                                                         1, 1, 0) * 0 + para * 7 + ond * 6 + copd * 3 + diabunc *
                            ifelse(diabc == 1 & assign0, 0, 0) + diabc * 0 +
                            hypothy * 0 + rf * 5 + ld * 11 + pud_hx * 0 + aids *
                            0 + lymph * 9 + metacanc * 12 + solidtum * ifelse(metacanc ==
                                                                                1 & assign0, 0, 4) + rheumd_hx * 0 + coag * 3 + obes *
                            (-4) + wloss * 6 + fed * 5 + blane * (-2) + dane *
                            (-2) + alcohol * 0 + drug * (-7) + psycho * 0 + depre *
                            (-3))
  pps$windex_ahrq <- with(pps, cut(wscore_ahrq, breaks = c(-Inf,
                                                           0, 1, 4.5, Inf), labels = c("<0", "0",
                                                                                       "1-4", ">=5"), right = FALSE))
  pps$windex_vw <- with(pps, cut(wscore_hxvw, breaks = c(-Inf,
                                                         0, 1, 4.5, Inf), labels = c("<0", "0",
                                                                                     "1-4", ">=5"), right = FALSE))

  ppf%>%left_join(pps%>%select(id,score_ch,index_ch,wscore_ch,windex_ch,score_hx,index_hx,wscore_ahrq,wscore_hxvw,windex_vw),by="id")->ppf

  if(any(allpeople%in%ppf$id==FALSE)){
    data.frame(matrix(data = NA,nrow = length(allpeople[allpeople%in%ppf$id==FALSE]),ncol=dim(ppf)[2]))->fillpatient
    colnames(fillpatient)<-colnames(ppf)
    fillpatient$id<-allpeople[allpeople%in%ppf$id==FALSE]
    rbind(ppf,fillpatient)->ppf
  }
  return(ppf)
  }
