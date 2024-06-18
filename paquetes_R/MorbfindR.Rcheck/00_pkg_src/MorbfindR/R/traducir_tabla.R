#' MorbfindR
#'
#' Generating search query form definitions
#'
#' @name traducir_tabla
#'
#' @param tabla. Dataframe with the disease definitions and codes (disease or drug) associated.
#' @param Disease_code Column name from the custom disease definition table where codes are found
#' @param Disease_codification Column name from the custom disease definition table where code types (ICD9, ICD10, CIAP) are found
#' @param Disease_group Column name from the custom  definition tables where custom Diseases definitions are found
#' @param Drug_code  Column name from the custom drug definition table where drug codes (usually ATCs) are found
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
    colnames(tabla)[grep(pattern=paste0("^",paste0(Disease_code,"$")),x=colnames(tabla))]<-"Disease_code"
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




