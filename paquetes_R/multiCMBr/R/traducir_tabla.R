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

library(Comorbilidades)
data(example)




head(example,500)->af

# sample(c("aja","euk","ue","facjakd"),500)->af$diagnom

af$diagnom<-NA
for(i in seq(1,nrow(af),1)){

  sample(c("aja","euk","ue","facjakd"),1)->af[i,5]
}




library(dplyr)
# tabla enfermedades
library(Comorbilidades)
data(example)
example->tabla_pacientes_cod_enferm
example %>% distinct(cod,.keep_all=TRUE) ->codigos_enf_cand
codigos_enf_cand$pat<-sample(c(0,1),nrow(codigos_enf_cand),replace=TRUE)
codigos_enf_cand %>% filter(pat==1) %>% dplyr::select(all_of(c("cod","codtype")))->codigos_enf_cand_1
codigos_enf_cand_1$pat<-sample(sample(c("aja","euk","ue","facjakd"),nrow(codigos_enf_cand_1),replace=TRUE))




# tabla_farmacos
load("example_dat.rda")
example->tabla_pacientes_farmacos
example %>% distinct(codATC,.keep_all=TRUE) %>% mutate(codATC=strtrim(codATC,5)) %>% distinct(codATC)->farmacos_cand
farmacos_cand$pat<-sample(c(0,1),nrow(farmacos_cand),replace=TRUE)
farmacos_cand %>% filter(pat==1)->farmacos_cand_1
farmacos_cand_1$pat<-sample(sample(c("aja","euk","ue","facjakd"),nrow(farmacos_cand_1),replace=TRUE))


save(tabla_pacientes_cod_enferm,tabla_pacientes_farmacos,codigos_enf_cand_1,farmacos_cand_1,file = "tablas_prueba_morbfindr.rda")

traducir_tabla(tabla = codigos_enf_cand_1,Disease_group = "pat",Disease_code = "cod",Disease_codification = "codtype")
traducir_tabla(tabla = farmacos_cand_1,Disease_group = "pat",Drug_code = "codATC")


