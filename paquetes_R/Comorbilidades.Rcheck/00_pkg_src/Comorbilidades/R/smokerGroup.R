#' Comorbilidades
#'
#' Extraction of smoking group through regular expressions and data of variable.
#'
#' @name smokerGroup
#'
#' @param tabla.  Tabla refers to the location of the csv file that needs 3 columns. pacUnifCOde,DGP_Cod=TABACO,ResDGP_Fecha_Presta. Usually corersponds to the extraction of THE LAST AVAILABLE INFORMATION ON SMOKE PER PATIENT.
#' @param exfumador_meses. exfumador_meses reffers to the minimum number of months since last time soking to consider one patient as ex_smoker.
#'
#' @return groups whatever data is stored in ATENEA or ANDIA DGP_Cod=TABACO into three smoker groups. Current smoker, ex-smoker, non-smoker
#'
#'
#'
#' @export


smokerGroup<-function(tabla="//Centcs01srv03/TABLEAU/Coronavirus/R/Ultimo_DGP_Tabaco.csv",Ultimo_DGP_Tabaco.csv,exfumador_meses=12){
  require(readr)
  require(dplyr)
  require(lubridate)
  require(stringr)
  Tabla<-read_delim(tabla,
                    ";", escape_double = FALSE, col_types = cols(DGP_Cod = col_skip(), ResDGP_Fecha_Presta = col_character()),
                    locale = locale(encoding = "ISO-8859-1"),
                    trim_ws = TRUE)
  names(Tabla)<-c("ID","valorDGP","fechaDGP")
  Tabla%>%mutate(valorDGP=tolower(valorDGP),fechaDGP=ymd(str_trunc(fechaDGP,width = 10,ellipsis = "")))%>%
    mutate(valorDGP=case_when(str_detect(string = valorDGP,pattern = "^(s|f)")~ "fumador" ,str_detect(string = valorDGP,pattern = "^n")~ "no fumador", str_detect(string = valorDGP,pattern = "<") & fechaDGP > (fechaDGP-months(exfumador_meses))~ "fumador", str_detect(valorDGP, pattern= ">")~ "exfumador", valorDGP=="exf"|valorDGP=="exf >"|valorDGP=="exf 1"|valorDGP=="exf 6"|valorDGP=="exf,"|valorDGP=="exf."|valorDGP=="exf+"|valorDGP=="ex" ~ "exfumador"))%>%
    filter(is.na(valorDGP)==FALSE)->tabDGP1a_TABACO
  return(tabDGP1a_TABACO)
}
