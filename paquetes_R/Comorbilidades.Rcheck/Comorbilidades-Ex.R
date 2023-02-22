pkgname <- "Comorbilidades"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
base::assign(".ExTimings", "Comorbilidades-Ex.timings", pos = 'CheckExEnv')
base::cat("name\tuser\tsystem\telapsed\n", file=base::get(".ExTimings", pos = 'CheckExEnv'))
base::assign(".format_ptime",
function(x) {
  if(!is.na(x[4L])) x[1L] <- x[1L] + x[4L]
  if(!is.na(x[5L])) x[2L] <- x[2L] + x[5L]
  options(OutDec = '.')
  format(x[1L:3L], digits = 7L)
},
pos = 'CheckExEnv')

### * </HEADER>
library('Comorbilidades')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("comorb")
### * comorb

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: comorb
### Title: Comorbilidades
### Aliases: comorb

### ** Examples

comorb(archivo="//Centcs01srv03/TABLEAU/Coronavirus/R/PacDiag2021.csv",id="Pac_Unif_Cod",code="Diag_Cod", fecha="ResDiag_Inicio_Fecha",assign0 = TRUE)


comorb(tabla="//Centcs01srv03/TABLEAU/Coronavirus/R/PacDiag2021.csv",id="Pac_Unif_Cod",code="Diag_Cod", fecha="ResDiag_Inicio_Fecha", modo="pesos",assign0 = TRUE)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("comorb", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
cleanEx()
nameEx("smokerGroup")
### * smokerGroup

flush(stderr()); flush(stdout())

base::assign(".ptime", proc.time(), pos = "CheckExEnv")
### Name: smokerGroup
### Title: Comorbilidades
### Aliases: smokerGroup

### ** Examples

smokerGroup(tabla="F:/Navarrabiomed/Teletrabajo/Dropbox/Intercambio_archivos_Juli_Ibai/Comorbilidades/Tabaco/Ultimo_DGP_Tabaco.csv",exfumador_meses=12)





base::assign(".dptime", (proc.time() - get(".ptime", pos = "CheckExEnv")), pos = "CheckExEnv")
base::cat("smokerGroup", base::get(".format_ptime", pos = 'CheckExEnv')(get(".dptime", pos = "CheckExEnv")), "\n", file=base::get(".ExTimings", pos = 'CheckExEnv'), append=TRUE, sep="\t")
### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
