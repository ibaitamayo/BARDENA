#' @title example data.
#'
#' @description of 500 patients with multiple disease codes in ICD9 ICD10 and CIAP2
#'
#' @format A data frame with 103640 rows and 4 variables:
#' \describe{
#'   \item{id}{Patient identification number.}
#'   \item{cod}{disease codes in ICD9, ICD10 or CIAP2 format}
#'   \item{codtype}{column defining if the cod variable refers to either ICD9, ICD10 or CIAP2}
#'   \item{date}{date when de disease was diagnosed}
#' }
"example"
