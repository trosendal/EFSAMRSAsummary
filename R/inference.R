##' ST2CC
##'
##' STs 9 and 8325 were classified as CC1 by PubMLST1.
##' ST22 was classified as CC22 by PubMLST1.
##'
##' @param ST
##' @return A vector of infered CC from ST
ST2CC <- function(ST) {
    ST2CC <- c("9" = 1,
               "8325" = 1,
               "22" = 22)
    ST2CC[ST]
}
