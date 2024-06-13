#' Comorbilidades
#'
#' Extraction of smoking group through regular expressions and data of variable.
#'
#' @name traducir_tabla
#'
#' @param tabla.  Tabla refers to the location of the csv file that needs 3 columns. pacUnifCOde,DGP_Cod=TABACO,ResDGP_Fecha_Presta. Usually corersponds to the extraction of THE LAST AVAILABLE INFORMATION ON SMOKE PER PATIENT.
#' @param exfumador_meses. exfumador_meses reffers to the minimum number of months since last time soking to consider one patient as ex_smoker.
#'
#' @return groups whatever data is stored in ATENEA or ANDIA DGP_Cod=TABACO into three smoker groups. Current smoker, ex-smoker, non-smoker
#'
#'
#'
#' @export


traducir_tabla<-function(tabla,
                         Disease_code="Diag_Cod",
                         Disease_codification="type",
                         Disease_group="snack",
                         Drug_code=NULL){

  if(is.null(Drug_code)){
    colnames(tabla)[grep(pattern=Disease_code,x=colnames(tabla))]<-"Disease_code"
    colnames(tabla)[grep(pattern=Disease_codification,x=colnames(tabla))]<-"Disease_codification"
    colnames(tabla)[grep(pattern=Disease_group,x=colnames(tabla))]<-"Disease_group"

    tabla %>% dplyr::select(c("Disease_code","Disease_codification","Disease_group")) %>%
      mutate(Disease_code=ifelse(stringr::str_starts(string=tolower(Disease_codification),pattern = "(ciap)"),paste0("_",Disease_code),Disease_code))%>%
      dplyr::select(-Disease_codification ) %>%
      mutate(Disease_code=gsub("\\.","",Disease_code)) %>%
      group_by(Disease_group) %>%
      tidyr::nest(result=Disease_code) %>%
      mutate(result = purrr::map_chr(result, ~paste0("^",paste(unlist(.x), collapse = "|^"))))->resultado

    as.data.frame(t(resultado))->para_lista

    colnames(para_lista)<-resultado$Disease_group

    return(as.list(para_lista[-1,]))

  }else if(!is.null(Drug_code)){
    colnames(tabla)[grep(pattern=Drug_code,x=colnames(tabla))]<-"Drug_code"
    colnames(tabla)[grep(pattern=Disease_group,x=colnames(tabla))]<-"Disease_group"

    tabla %>% dplyr::select(c("Drug_code","Disease_group"))  %>%
      mutate(Drug_code=gsub("\\.","",Drug_code)) %>%
      group_by(Disease_group) %>%
      tidyr::nest(result=Drug_code) %>%
      mutate(result = purrr::map_chr(result, ~paste0("^",paste(unlist(.x), collapse = "|^"))))->resultado

    as.data.frame(t(resultado))->para_lista

    colnames(para_lista)<-resultado$Disease_group

    return(as.list(para_lista[-1,]))

  }


}


data(example)


c("aja","euk","ue","facjakd")
sample(c("aja","euk","ue","facjakd"),1)


head(example,500)->af

# sample(c("aja","euk","ue","facjakd"),500)->af$diagnom

af$diagnom<-NA
for(i in seq(1,nrow(af),1)){

  sample(c("aja","euk","ue","facjakd"),1)->af[i,5]
}

af[,-c(1,4)]->yolvieha



traducir_tabla(yolvieha)
library(dplyr)


