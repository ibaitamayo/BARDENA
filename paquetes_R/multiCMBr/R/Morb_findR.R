#' Comorbilidades
#'
#' modified version of comorbidity to work simultaneously with icd9 and icd10, and Smoking group extraction
#'
#' @name Morb_findR
#'
#' @param data_disease Archivo data in data frame or txt formatdata frame or to the txt.
#' @param id The name of the column the patients identification codes are found.
#' @param Disease_code Disease_code refers to the name of the column where the disease codes will be found
#' @param Disease_codification column where the type of codification that "Disease_code" is being codified. Usually this column will contain values such as "ICD9", "ICD10" or "CIAP2" or similar
#' @param Disease_date Disease_date refers to the name of the column where the date of the coded disease is found
#' @param granularidad This boolean parameter defines if mild and severe formas of the disease should be distinguished, and will be applied to liver disease, diabetes and cancer.
#' @param mini This numeric parameters  defines the number of rows from the "archivo" that the package should be passed on. For learning and debugging.
#' @return modified version of comorbidity to use  ICD9 , ICD10 and CIAP2 to return Charlson score, elixhauser score and first event date.
#'
#' @export
#'
Morb_findR<-function(data_disease,
                     data_drugs=NULL,
                     id="Pac_Unif_Cod",
                     deduce_from=c("from_disease","from_drugs","from_Disease&Drugs"),
                     Disease_code="Diag_Cod",
                     Disease_codification="type",
                     Disease_date="ResDiag_Inicio_Fecha",
                     Disease_group="snack",
                     Drug_code=NULL,
                     Drug_date=NULL,
                     granularidad = FALSE,
                     Custom_disease_def=NULL,
                     Custom_drug_def=NULL) {

  require(dplyr)
  require(tidyr)
  require(readr)
  require(data.table)
  require(stringr)
  require(DT)
  require(lubridate)
  require(tools)



  if(is.data.frame(data_disease)){
    message("Loading Dataframe","\n")
    tabla<-data_disease
    print(colnames(tabla))

  }else if(tools::file_ext(data_disease)=="txt"){
    message("Loading txt file","\n")
    tabla<-read_delim(data_disease,
                      ";", escape_double = FALSE,col_names = TRUE,
                      locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                      trim_ws = TRUE)
  }
  # else if(tools::file_ext(data_disease)=="csv"){
  #   tabla<-read_delim(data_disease,
  #                     ";", escape_double = FALSE, col_types = cols(Disease_date= col_character()),
  #                     locale = locale(encoding = "ISO-8859-1"),
  #                     trim_ws = TRUE)
  #   colnames(tabla)<-c(id,Disease_code,"Disease_codification",Disease_date)
  #
  #
  # }
  colnames(tabla)[grep(pattern=Disease_code,colnames(tabla))]<-"Disease_code"
  colnames(tabla)[grep(pattern=id,colnames(tabla))]<-"id"
  colnames(tabla)[grep(pattern=Disease_codification,colnames(tabla))]<-"Disease_codification"
  colnames(tabla)[grep(pattern=Disease_date,colnames(tabla))]<-"Disease_date"

  tabla <- tabla %>%mutate(Disease_date=lubridate::ymd(str_trunc(Disease_date,width = 10,ellipsis = "")))
  print(colnames(tabla))

  tabla%>%distinct(id)->allpeople
  allpeople$id->allpeople
  message(paste0("load complete...",paste0(length(allpeople)," patients detected in disease table")))

  if (deduce_from=="from_disease"){
    if(is.null(Custom_disease_def)){


    tabla <- tabla %>% dplyr::mutate(Disease_code=gsub("\\.","",Disease_code))%>%filter(strtrim(Disease_code,3)%in%Comorbilidades:::allstrimcod)#corto a 3 digitos y lo comparo con los eventos cortados a 3 digitos,x velocidad

    tabla<-tabla%>%dplyr::select(c("id","Disease_code","Disease_codification","Disease_date")) %>%
      mutate(Disease_code=ifelse(str_starts(string=tolower(Disease_codification),pattern = "(ciap)"),paste0("_",Disease_code),Disease_code))%>%
      filter(is.na(id)==FALSE)%>%
      dplyr::select(all_of(c("id","Disease_code","Disease_date")))#esto es lo que he añadido para que lea los ciap2
    # tabla<-tabla%>%dplyr::select(all_of(c(id,Disease_code,Disease_date)))
    # colnames(tabla)<-c("id","Disease_code","Disease_date")
    # message("CIAP2 processed")


    ### 1. construye in identificador compuesto de id.Disease_date.codecie ####
    tabla_ <- tabla %>%
      dplyr::mutate(idfechacod=paste(as.character(id),as.character(Disease_date),Disease_code, sep=",")) %>%
      relocate(idfechacod, .before=id)  %>%
      dplyr::select(-all_of(c("id","Disease_date"))) %>%
      setDT()

    ## 2. se generan las diversas comorbilidades empleando los codigos del objeto Comorbilidades:::tricod ####

    for(i in seq(1,length(Comorbilidades:::tricod))) {
      dc=Comorbilidades:::tricod[[i]]
      varname=names(Comorbilidades:::tricod)[i]
      tabla_ <- tabla_ %>% mutate(!!varname:=as.numeric(str_detect(Disease_code,dc)))
    }
    message("disease matrix generated")
    mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[3:45]) %>% setDT()
    mpp=mpp[with(mpp,value!=0),]
    tidyr::separate_wider_delim(data=mpp,cols="idfechacod",delim=",",names=c("id","Disease_date","Disease_code"),cols_remove = TRUE)->mpp

    ppf=mpp %>%
      pivot_wider(id_cols = id,names_from = variable,values_from = Disease_date,
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

    ppf%>%left_join(pps%>%dplyr::select(all_of(c("id","score_ch","index_ch","wscore_ch","windex_ch","score_hx","index_hx","wscore_ahrq","wscore_hxvw","windex_ahrq","windex_vw"))),by="id")->ppf
    message("scores calculated")
    if(any(allpeople%in%ppf$id==FALSE)){
      data.frame(matrix(data = NA,nrow = length(allpeople[allpeople%in%ppf$id==FALSE]),ncol=dim(ppf)[2]))->fillpatient
      colnames(fillpatient)<-colnames(ppf)
      fillpatient$id<-allpeople[allpeople%in%ppf$id==FALSE]
      fillpatient[,45:53]<-0#los pacientes que no presentan ninguna de las comorbilidades se les asigna un 0 en los valores de charlson y elixhauser
      rbind(ppf,fillpatient)->ppf
      }
    }
    if(!is.null(Custom_disease_def)){
      message("Generating custom disease search query","\n")
      traducir_tabla(Custom_disease_def,Disease_code,Disease_codification ,Disease_group )->custom_search_disease
      unique(strtrim(unlist(str_split(gsub(x = custom_search_disease,pattern ="(\\|)|(_)",replacement = ""),pattern = "\\^")),3))[-1]->custom_dis_strimcod
      message("calculating...","\n")

    tabla <- tabla %>% dplyr::mutate(Disease_code=gsub("\\.","",Disease_code))%>%filter(strtrim(Disease_code,3)%in%custom_dis_strimcod)#corto a 3 digitos y lo comparo con los eventos cortados a 3 digitos,x velocidad

    tabla<-tabla%>%
      mutate(Disease_code=ifelse(str_starts(string=tolower(Disease_codification),pattern = "(ciap)"),paste0("_",Disease_code),Disease_code))%>%
      filter(is.na(id)==FALSE)%>%dplyr::select(all_of(c("id","Disease_code","Disease_date")))#esto es lo que he añadido para que lea los ciap2
    # tabla<-tabla%>%dplyr::select(all_of(c(id,Disease_code,Disease_date)))
    # colnames(tabla)<-c("id","Disease_code","Disease_date")
    # message("CIAP2 processed")


    ### 1. construye in identificador compuesto de id.Disease_date.codecie ####
    tabla_ <- tabla %>%
      dplyr::mutate(idfechacod=paste(as.character(id),as.character(Disease_date),Disease_code, sep=",")) %>%
      relocate(idfechacod, .before=id)  %>%
      dplyr::select(-all_of(c("id","Disease_date"))) %>%
      setDT()
    # message("information_joined")
    ## 2. se generan las diversas comorbilidades empleando los codigos del objeto custom_search_disease ####

    for(i in seq(1,length(custom_search_disease))) {
      dc=custom_search_disease[[i]]
      varname=names(custom_search_disease)[i]
      tabla_ <- tabla_ %>% mutate(!!varname:=as.numeric(str_detect(Disease_code,dc)))
    }
    message("disease matrix generated")

    mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[3:length(custom_search_disease)]) %>% setDT()

    mpp=mpp[with(mpp,value!=0),]

    tidyr::separate_wider_delim(data=mpp,cols="idfechacod",delim=",",names=c("id","Disease_date","Disease_code"),cols_remove = TRUE)->mpp


    ppf=mpp %>%
      pivot_wider(id_cols = id,names_from = variable,values_from = Disease_date,
                  values_fn = ~min(.x,na.rm=TRUE))
    message("calculating first events")

    if(any(names(custom_search_disease)%in%colnames(ppf)==FALSE)){
      colvacias<-data.frame(matrix(data=NA,nrow = dim(ppf)[1],ncol=sum((as.numeric(names(custom_search_disease)%in%colnames(ppf)-1)^2))))
      colnames(colvacias)<-names(custom_search_disease)[names(custom_search_disease)%in%colnames(ppf)==FALSE]
      cbind(ppf,colvacias)->ppf
      }
    }
    }else if(deduce_from!="from_disease"& any(is.null(Custom_drug_defo),is.null(Drug_date),is.null(Drug_code))){
        message("Drug - disease relation table and column definitions needed!")
      }else if(deduce_from!="from_disease"& all(!is.null(Custom_drug_defo),!is.null(Drug_date),!is.null(Drug_code))){
        message("Generating drug search query","\n")
        traducir_tabla(Custom_drug_defo)->custom_search_drug
        unique(strtrim(unlist(str_split(gsub(x = custom_search_drug,pattern ="(\\|)|(_)",replacement = ""),pattern = "\\^")),3))[-1]->custom_drug_strimcod
        message("calculating...","\n")
        colnames(data_drugs)[grep(pattern=Drug_code,x=colnames(data_drugs))]<-"Drug_code"
        colnames(data_drugs)[grep(pattern=Drug_date,x=colnames(data_drugs))]<-"Drug_date"

        tabla <- data_drugs %>%
          mutate(Drug_date=lubridate::ymd(str_trunc(Drug_date,width = 10,ellipsis = ""))) %>%
          dplyr::select(all_of(c("id","Drug_code","Drug_date")))
        tabla%>%distinct(id)->allpeople
        allpeople$id->allpeople
        message(paste0("load complete...",paste0(length(allpeople)," patients detected in drugs table")))

        tabla <- tabla %>% dplyr::mutate(Drug_code=gsub("\\.","",Drug_code))%>%filter(strtrim(Drug_code,3)%in%custom_dis_strimcod)#corto a 3 digitos y lo comparo con los eventos cortados a 3 digitos,x velocidad

        # tabla<-tabla%>%mutate(Drug_code=ifelse(str_starts(string=tolower(Disease_codification),pattern = "(ciap)"),paste0("_",Drug_code),Drug_code))%>%filter(is.na(id)==FALSE)#esto es lo que he añadido para que lea los ciap2
        # tabla<-tabla%>%dplyr::select(all_of(c(id,Drug_code,Drug_date)))
        # colnames(tabla)<-c("id","Drug_code","Drug_date")
        # message("CIAP2 processed")


        ### 1. construye in identificador compuesto de id.Drug_date.codecie ####
        tabla_ <- tabla %>%
          dplyr::mutate(idfechacod=paste(as.character(id),as.character(Drug_date),Drug_code, sep=",")) %>%
          relocate(idfechacod, .before=id)  %>%
          dplyr::select(-all_of(c("id","Drug_date"))) %>% setDT()
        # message("information_joined")
        ## 2. se generan las diversas comorbilidades empleando los codigos del objeto custom_search_drug ####

        for(i in seq(1,length(custom_search_drug))) {
          dc=custom_search_drug[[i]]
          varname=names(custom_search_drug)[i]
          tabla_ <- tabla_ %>% mutate(!!varname:=as.numeric(str_detect(Drug_code,dc)))
        }
        message("disease matrix generated")

        mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[3:length(custom_search_drug)]) %>%
          setDT()

        mpp=mpp[with(mpp,value!=0),]

        tidyr::separate_wider_delim(data=mpp,cols="idfechacod",delim=",",names=c("id","Drug_date","Drug_code"),cols_remove = TRUE)->mpp


        ppf=mpp %>%
          pivot_wider(id_cols = id,names_from = variable,values_from = Drug_date,
                      values_fn = ~min(.x,na.rm=TRUE))
        message("calculating first events")

        if(any(names(custom_search_drug)%in%colnames(ppf)==FALSE)){
          colvacias<-data.frame(matrix(data=NA,nrow = dim(ppf)[1],ncol=sum((as.numeric(names(custom_search_drug)%in%colnames(ppf)-1)^2))))
          colnames(colvacias)<-names(custom_search_drug)[names(custom_search_drug)%in%colnames(ppf)==FALSE]
          cbind(ppf,colvacias)->ppfarm
        }
        if(deduce_from=="from_drugs"){
          ppfarm->ppf
        }else if(deduce_from!="from_Disease&Drugs"){
          rbind(ppf,ppfarm) %>%
            group_by(id) %>%
            mutate_if(is.Date,.funs=function(x) min(x,na.rm=TRUE)) %>%
            ungroup()->ppf

        }

    }



  # message("fit people reincorporated")
  return(ppf)
}
sample(seq(as.Date('1999/01/01'), as.Date('2024/01/01'), by="day"), nrow(synthetic_patients),replace=TRUE)->synthetic_patients$dates


Morb_findR(data_disease = synthetic_patients,id = "patient_id",deduce_from = "from_disease",Disease_code = "code",Disease_codification = "diccionary",Disease_date = "dates",Custom_disease_def = talbaa)
