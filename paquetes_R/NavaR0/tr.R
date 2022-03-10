#' Estimate R0
#'
#' Estimates R0 through parametric method (EpiEstim) assuming mean serial interval=6 and standard dev of serial interval= 4.75
#'
#' @name tr
#'
#' @param x formated table.
#'
#' @return Estimated values of R0, upper and lower limits.
#'
#' @examples
#'
#' @export





tr= function(x) {
  require(EpiEstim)
  require(dplyr)
  tr=estimate_R(dplyr::select(x,dates,I),  method="parametric_si", config = make_config(list(mean_si = 6, std_si = 4.75)))
  datr=bind_cols(dates=tr$dates[8:length(tr$dates)],sdia=weekdays(tr$dates[8:length(tr$dates)]),inf2_5=round(tr$R$`Quantile.0.025(R)`,2),media=round(tr$R$`Mean(R)`,2) ,sup97_5=round(tr$R$`Quantile.0.975(R)`,2))
  names(datr)=c("cdia" ,"sdia" , "linf_R" , "media_R",   "lsup_R")
  datr<- datr %>% arrange(desc(cdia))
  datr$cdia=as.character(datr$cdia)
  return(datr)
}
