#' CalculoR0
#'
#' Calculates upper lower and mean values of R0
#'
#' @name calculoR0
#'
#' @param tabla
#'
#' @return R0 results total and health area specific.
#'
#' @examples
#'
#' @export





calculoR0<-function(tabla="F:/Navarrabiomed/Teletrabajo/Dropbox/ju/covid19/dat/Sergio/Nuevos positivos 20210112.xlsx",area=NULL){
  require(readxl)
  require(EpiEstim)
  require(dplyr)
  require(lubridate)
  require(tidyr)

  ## los positivos de sergio se refieren al dia anterior
  covid_positives<- read_excel(tabla)
  ## se ha incorporado el CT pero hay duplicados
  names(covid_positives)= c( "i" ,  "fdia" , "id", "edad", "gen" , "gma" , "ZBS", "abs" , "res","ct")

  ## eliminar duplicados!!?? coger primera fecha!
  covid_positives%>%arrange(fdia)%>%distinct(id,.keep_all = TRUE)->covid_positives

  covid_positives <- covid_positives %>% mutate(edad=as.numeric(edad), gen=factor(gen), gma=factor(gma), res=res=="SI") %>% mutate(fdia=ymd_hms(fdia)) %>%  mutate(fdia=as.Date(fdia))

  x=covid_positives


  abgac <- covid_positives %>% dplyr::group_by(fdia,abs) %>% dplyr::summarise(I=n()) %>% data.frame()
  names(abgac)=c( "day" ,"area" , "cvt")
  abgac <- abgac %>% mutate(wday=weekdays(day)) %>% arrange(day) %>% group_by(area) %>%  dplyr::mutate(cs=cumsum(cvt)) %>% data.frame()


  tbgac <- abgac %>% dplyr::group_by(day,wday) %>% dplyr::summarise(cvt=sum(cvt)) %>% data.frame()%>%arrange(day) %>%  mutate(cs=cumsum(cvt)) %>% data.frame()#para los positivos totales, no filtramos los AREA con NA
  bbgac<- data.frame(day=seq(min(tbgac$day),max(tbgac$day),by = 1))
  bbgac<- left_join(bbgac, tbgac %>% dplyr::select(day,cvt),by="day") %>% mutate(cvt=ifelse(is.na(cvt),0,cvt))
  names(bbgac)<- c("dates" ,"I" )
  if(is.null(area)){
    resultado=tr(bbgac)%>%mutate(area="todas_areas")
  }
  if(!is.null(area)){
    area_=tolower(area)
    abgac<- abgac %>% filter(!is.na(area)) %>% mutate(area=tolower(area)) %>% filter(area!="nulo/sin informar" & area!="limitrofe")#eliminamos los na en area
    cros=crossing(day=seq(min(abgac$day),max(abgac$day),by = 1), area = unique(abgac$area))
    badtc<- left_join(cros, abgac %>% select(day,area,cvt),by=c("day","area")) %>% mutate(cvt=ifelse(is.na(cvt),0,cvt))
    names(badtc)<- c("dates" , "area" ,"I" )

    resultado=tr(badtc%>%filter(area==area_)%>%dplyr::select(c("dates" ,"I" )))%>%mutate(area=area)
  }

  return(resultado)

}

