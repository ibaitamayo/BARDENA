Morb_findR<-function(data_disease=NULL,
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
    message("Loading clinical history","\n")
    tabla<-data_disease
    # print(colnames(tabla))

  }else if(tools::file_ext(data_disease)=="txt"){
    message("Loading txt file","\n")
    tabla<-read_delim(data_disease,
                      ";", escape_double = FALSE,col_names = TRUE,
                      locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                      trim_ws = TRUE)
  }

  if (deduce_from=="from_disease"|deduce_from=="from_Disease&Drugs"){
    colnames(tabla)[grep(pattern=paste0("^",paste0(Disease_code,"$")),colnames(tabla))]<-"Disease_code"
    colnames(tabla)[grep(pattern=id,colnames(tabla))]<-"id"
    colnames(tabla)[grep(pattern=Disease_codification,colnames(tabla))]<-"Disease_codification"
    colnames(tabla)[grep(pattern=Disease_date,colnames(tabla))]<-"Disease_date"

    tabla <- tabla %>%mutate(Disease_date=lubridate::ymd(str_trunc(Disease_date,width = 10,ellipsis = "")))


    tabla%>%distinct(id)->allpeople
    allpeople$id->allpeople
    message(paste0("load complete...",paste0(length(allpeople)," patients detected in disease table")))
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
      mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[-grep(pattern="cod",x=names(tabla_))]) %>% setDT()
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
      # assign("lovalgo",tabla_,envir = globalenv())
      message("disease matrix generated")

      mpp=data.table::melt(tabla_, id.vars = "idfechacod",measure.vars = names(tabla_)[-grep(pattern="cod",x=names(tabla_))]) %>% setDT()
      # print(mpp)
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
      if(any(allpeople%in%ppf$id==FALSE)){
        data.frame(matrix(data = NA,nrow = length(allpeople[allpeople%in%ppf$id==FALSE]),ncol=dim(ppf)[2]))->fillpatient
        colnames(fillpatient)<-colnames(ppf)
        fillpatient$id<-allpeople[allpeople%in%ppf$id==FALSE]
        rbind(ppf,fillpatient)->ppf
        message("Done!")
        # print(ppf)
      }
    }
  }
  if(deduce_from!="from_disease"& any(is.null(Custom_drug_def),is.null(Drug_date),is.null(Drug_code))){
    message("Drug - disease relation table and column definitions needed!")
  }else if(deduce_from!="from_disease"& all(!is.null(Custom_drug_def),!is.null(Drug_date),!is.null(Drug_code))){
    if(is.data.frame(data_drugs)){
      message("Loading drug records","\n")
      tabla_drugs<-data_drugs
      # print(colnames(tabla))

    }else if(tools::file_ext(data_drugs)=="txt"){
      message("Loading txt file","\n")
      tabla_drugs<-read_delim(data_drugs,
                              ";", escape_double = FALSE,col_names = TRUE,
                              locale = locale(encoding = "ISO-8859-1"),col_types = cols(X1 = col_double(),X2 = col_character(),X3 = col_character(),X4 = col_character()),
                              trim_ws = TRUE)
    }
    colnames(tabla_drugs)[grep(pattern=Drug_code,x=colnames(tabla_drugs))]<-"Drug_code"
    colnames(tabla_drugs)[grep(pattern=Drug_date,x=colnames(tabla_drugs))]<-"Drug_date"


    tabla_drugs <- tabla_drugs %>%
      mutate(Drug_date=lubridate::ymd(str_trunc(Drug_date,width = 10,ellipsis = ""))) %>%
      dplyr::select(all_of(c("id","Drug_code","Drug_date")))

    tabla_drugs%>%distinct(id)->allpeople_drugs
    allpeople_drugs$id->allpeople_drugs
    message(paste0("load complete...",paste0(length(allpeople_drugs)," patients detected in drugs table")))

    message("Generating drug search query","\n")
    traducir_tabla(tabla=Custom_drug_def,Drug_code=Drug_code,Disease_group=Disease_group)->custom_search_drug
    unique(strtrim(unlist(str_split(gsub(x = custom_search_drug,pattern ="(\\|)|(_)",replacement = ""),pattern = "\\^")),3))[-1]->custom_drug_strimcod
    tabla_drugs <- tabla_drugs %>% dplyr::mutate(Drug_code=gsub("\\.","",Drug_code))%>%filter(strtrim(Drug_code,3)%in%custom_drug_strimcod)#corto a 3 digitos y lo comparo con los eventos cortados a 3 digitos,x velocidad

    message("calculating...","\n")

    ### 1. construye in identificador compuesto de id.Drug_date.codecie ####
    tabla_drugs_ <- tabla_drugs %>%
      dplyr::mutate(idfechacod=paste(as.character(id),as.character(Drug_date),Drug_code, sep=",")) %>%
      relocate(idfechacod, .before=id)  %>%
      dplyr::select(-all_of(c("id","Drug_date"))) %>% setDT()
    # message("information_joined")
    ## 2. se generan las diversas comorbilidades empleando los codigos del objeto custom_search_drug ####

    for(i in seq(1,length(custom_search_drug))) {
      dc=custom_search_drug[[i]]
      varname=names(custom_search_drug)[i]
      tabla_drugs_ <- tabla_drugs_ %>% mutate(!!varname:=as.numeric(str_detect(Drug_code,dc)))
    }
    message("drug disease matrix generated")

    mpp_drugs=data.table::melt(tabla_drugs_, id.vars = "idfechacod",measure.vars = names(tabla_drugs_)[-grep(pattern="cod",x=names(tabla_drugs_))]) %>%
      setDT()

    mpp_drugs=mpp_drugs[with(mpp_drugs,value!=0),]

    tidyr::separate_wider_delim(data=mpp_drugs,cols="idfechacod",delim=",",names=c("id","Drug_date","Drug_code"),cols_remove = TRUE)->mpp_drugs


    ppfarm=mpp_drugs %>%
      pivot_wider(id_cols = id,names_from = variable,values_from = Drug_date,
                  values_fn = ~min(.x,na.rm=TRUE))
    message("calculating first events")

    if(any(names(custom_search_drug)%in%colnames(ppfarm)==FALSE)){
      colvacias_drugs<-data.frame(matrix(data=NA,nrow = dim(ppfarm)[1],ncol=sum((as.numeric(names(custom_search_drug)%in%colnames(ppfarm)-1)^2))))
      colnames(colvacias_drugs)<-names(custom_search_drug)[names(custom_search_drug)%in%colnames(ppfarm)==FALSE]
      cbind(ppfarm,colvacias_drugs)->ppfarm
      # print(ppfarm)
    }
    if(any(allpeople_drugs%in%ppfarm$id==FALSE)){
      data.frame(matrix(data = NA,nrow = length(allpeople_drugs[allpeople_drugs%in%ppfarm$id==FALSE]),ncol=dim(ppfarm)[2]))->fillpatient_drugs
      colnames(fillpatient_drugs)<-colnames(ppfarm)
      fillpatient_drugs$id<-allpeople_drugs[allpeople_drugs%in%ppfarm$id==FALSE]
      rbind(ppfarm,fillpatient_drugs)->ppfarm
      message("Done!")
    }
    if(deduce_from=="from_drugs"){
      ppfarm->ppf
    }else if(deduce_from=="from_Disease&Drugs"){
      message("merging clinical and drug related disease groups and dates")
      suppressWarnings(rbind(ppf,ppfarm) %>%
                         slice_min(order_by=id) %>%
                         distinct(id,.keep_all = TRUE)%>%
                         ungroup())->ppf

    }

  }

  return(ppf)
}
