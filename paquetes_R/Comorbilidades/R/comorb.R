#' Comorbilidades
#'
#' modified version of comorbidity to work simultaneously with icd9 and icd10, and Smoking group extraction
#'
#' @name comorb
#'
#' @param tabla. Tabla refers to the location of the csv file that needs 3 columns. pacUnifCOde (patient identifier),Diag_Cod(ICD9 or ICD10 of the pathology),ResDGP_Fecha_Presta(date of the comorbidity).
#' @param modo. Determina la salida del paquete. modo= "pesos" es la variable por defecto (42g+score):  de charlson (17 primeros grupos) y elixhauser, con un operador numerico 1/0 segun se hayan dado o no en el periodo estudiado, más la puntuacion de los índices charlson t¡y elixhauser.modo="fechas" (42g+fecha): 42 grupos, con vacios (NA) para todas aquellas comorbilidades que no se dieron y en el resto la fecha de primera aparicion.modo="codigos"S (42g+cie)**:  42 grupos, igualmente vacios si no tuvieron y con un codigo diagnostico (el primero) en caso afirmativo.
#' @return modified version of comorbidity to use both ICD9 and ICD10 and generate CHarlson score, elixhauser score or first event date.
#'
#' @examples comorb(tabla="//Centcs01srv03/TABLEAU/Coronavirus/R/PacDiag2021.csv",id="Pac_Unif_Cod",code="Diag_Cod", fecha="ResDiag_Inicio_Fecha", modo="pesos",assign0 = TRUE)
#'
#'
#' @export
#'
## Pasos del programa: tres funciones


### 1. funcion01  noramliza codigo yconstruye in identificador compuesto de id.fecha.codecie


comorb<-function(tabla="//Centcs01srv03/TABLEAU/Coronavirus/R/PacDiag2021.csv",id="Pac_Unif_Cod",code="Diag_Cod", fecha="ResDiag_Inicio_Fecha", modo="pesos",assign0 = TRUE) {

require(dplyr)
require(readr)
require(data.table)
require(stringr)
require(DT)
require(lubridate)
require(tools)

  if(tools::file_ext(tabla)=="txt"){
    tabla<-read_delim(tabla,
                      "\t", escape_double = FALSE,col_names = FALSE,
                      locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                      trim_ws = TRUE)
    colnames(tabla)<-c("Pac_Unif_Cod","Diag_Cod","type","ResDiag_Inicio_Fecha")
    tabla <- tabla %>%mutate(ResDiag_Inicio_Fecha=ymd(str_trunc(ResDiag_Inicio_Fecha,width = 10,ellipsis = "")))


  }
  if(tools::file_ext(tabla)=="csv"){
    tabla<-read_delim(tabla,
                      ";", escape_double = FALSE, col_types = cols(ResDiag_Inicio_Fecha= col_character()),
                      locale = locale(encoding = "ISO-8859-1"),
                      trim_ws = TRUE)
    tabla <- tabla %>%mutate(ResDiag_Inicio_Fecha=dmy(str_trunc(ResDiag_Inicio_Fecha,width = 10,ellipsis = "")))
  }

  tabla <- tabla %>% dplyr::select(id,code,fecha)

  colnames(tabla) <- c("id","code","fecha")
  tabla%>%distinct(id)->allpeople
  allpeople$id->allpeople

  tabla <- tabla %>% dplyr::mutate(code=gsub("\\.","",code))%>%filter(code%in%allcod)# lo normalizo code y filtro

  ### 1. construye in identificador compuesto de id.fecha.codecie ####
  tabla_ <- tabla %>% dplyr::mutate(idfechacod=paste(as.character(id),as.character(fecha),code, sep=",")) %>% relocate(idfechacod, .before=id)  %>% select(-c(id,fecha)) %>% setDT()


    ## 2. se generan las diversas comorbilidades empleando los codigos del objeto dicod ####


    for(i in 1:44) {
      dc=dicod[[i]]
      varname=names(dicod)[i]
      tabla_ <- tabla_ %>% mutate(!!varname:=as.numeric(str_detect(code,dc)))
    }

    mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[3:44])
    mpp=mpp[value!=0]  ## eliminar registros con codigos no incluidos
    mpp[, c("id","fecha","code"):=tstrsplit(idfechacod, "," , fixed=TRUE)]
    mpp=mpp[, -"idfechacod"]

    ## S1 17g+score generar el fichero previo (a falta del cálculo de pesos) de la S1

    if (modo == "pesos") {
      pps=dcast.data.table(
        mpp, id ~ variable,
        value.var = 'value',
        fill=0)
      if(any(names(dicod)%in%colnames(pps)==FALSE)){
        colvacias<-data.frame(matrix(data=0,nrow = dim(pps)[1],ncol=sum((as.numeric(names(dicod)%in%colnames(pps)-1)^2))))
        colnames(colvacias)<-names(dicod)[names(dicod)%in%colnames(pps)==FALSE]
        cbind(pps,colvacias)->pps
      }
      pps= pps %>%  mutate_at(vars(ami:depre),function(x){x=ifelse(x>0,1,0)})

      # co_cols=names(pps)[-1]
      # ch_cols=c( "ami" ,"chf", "pvd", "cevd","dementia" ,"codp","rheumd_ch", "pud", "mld", "diabwoc" ,  "diabwic" ,"hp",  "rend","canc","msld","metacanc" , "aids")
      # hx_cols=c("chf","carit","valv", "pcd", "pvd", "hypunc"  ,  "hypc", "para","ond", "codp", "diabunc" ,  "diabc", "hypothy" ,  "rf",  "ld",  "lymph","solidtum",  "rheumd_hx", "coag", "obes", "wloss"  , "fed",  "blane","dane", "alcohol", "drug", "psycho", "depre","aids"  )

      data.table::setDF(pps)
      pps$score_ch <- with(pps, ami + chf + pvd + cevd + dementia +
                             copd + rheumd_ch + pud + mld * ifelse(msld == 1 & assign0,
                                                                   0, 1) + diabwoc * ifelse(diabwic == 1 & assign0, 0, 1) +
                             diabwic + hp + rend + canc * ifelse(metacanc == 1 &
                                                                   assign0, 0, 1) + msld + metacanc + aids)
      pps$index_ch <- with(pps, cut(score_ch, breaks = c(0, 1, 2.5, 4.5,
                                                         Inf), labels = c("0", "1-2", "3-4",
                                                                          ">=5"), right = FALSE))
      pps$wscore_ch <- with(pps, ami + chf + pvd + cevd + dementia +
                              copd + rheumd_ch + pud + mld * ifelse(msld == 1 & assign0,
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
                                                           0, 1) + diabc + hypothy + rf + ld + pud + aids +
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
                                (-3) + hypothy * 0 + rf * 6 + ld * 4 + pud * 0 +
                                aids * 0 + lymph * 6 + metacanc * 14 + solidtum *
                                ifelse(metacanc == 1 & assign0, 0, 7) + rheumd_hx *
                                0 + coag * 11 + obes * (-5) + wloss * 9 + fed * 11 +
                                blane * (-3) + dane * (-2) + alcohol * (-1) + drug *
                                (-7) + psycho * (-5) + depre * (-5))
      pps$wscore_hxvw <- with(pps, chf * 7 + carit * 5 + valv * (-1) +
                                pcd * 4 + pvd * 2 + ifelse(hypunc == 1 | hypc ==
                                                             1, 1, 0) * 0 + para * 7 + ond * 6 + copd * 3 + diabunc *
                                ifelse(diabc == 1 & assign0, 0, 0) + diabc * 0 +
                                hypothy * 0 + rf * 5 + ld * 11 + pud * 0 + aids *
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
      if(any(allpeople%in%pps$id==FALSE)){
        data.frame(matrix(data = 0,nrow = length(allpeople[allpeople%in%pps$id==FALSE]),ncol=dim(pps)[2]))->fillpatient
        colnames(fillpatient)<-colnames(pps)
        fillpatient$id<-allpeople[allpeople%in%pps$id==FALSE]
        rbind(pps,fillpatient)->pps
        print(dim(fillpatient))
      }
      return(pps)
    }

    if (modo == "fechas") {
      ## S2 17g+fecha ( formato fecha?)
      ppf=dcast.data.table(
        mpp, id ~ variable,
        value.var = 'fecha',
        fun = list(min))
      if(any(allpeople%in%ppf$id==FALSE)){
        data.frame(matrix(data = NA,nrow = length(allpeople[allpeople%in%ppf$id==FALSE]),ncol=dim(ppf)[2]))->fillpatient
        colnames(fillpatient)<-colnames(ppf)
        fillpatient$id<-allpeople[allpeople%in%ppf$id==FALSE]
        rbind(ppf,fillpatient)->ppf
        print(dim(fillpatient))
      }
      return(ppf)
    }

    if (modo == "codigos") {
      ppd=dcast.data.table(
        mpp, id ~ variable,
        value.var = 'code',
        fun = list(min))  ## no se como aplica y que consecuencias tiene, este minimo, podria hacer un descriptivo previo de distribucion de diagnosticos por grupo CIE
      return(ppd)
    }
}

