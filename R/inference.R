##' ST2CC
##'
##' STs 9 and 8325 were classified as CC1 by PubMLST1.
##' ST22 was classified as CC22 by PubMLST1.
##'
##' @param ST a vector of STs
##' @return A vector of inferred CC from ST
ST2CC <- function(ST) {
    ST2CC <- c("9" = 1,
               "8325" = 1,
               "22" = 22)
    ST2CC[ST]
}

##' spa2CC
##'
##' @param spa a vector of spa's
##' @return A vector of inferred CC from spa
spa2CC <- function(spa) {
    spa2CC <- c("11" = 398, "34" = 398, "108" = 398,
                "571" = 398, "588" = 398, "1255" = 398,
                "1451" = 398, "1456" = 398, "1580" = 398,
                "1793" = 398, "2011" = 398, "2330" = 398,
                "2346" = 398, "2576" = 398, "2922" = 398,
                "5452" = 398, "6228" = 398, "6575" = 398,
                "10485" = 398, "19248" = 398,
                ##(Battisti et al., 2010; EFSA 2009a ; Kinross et al.,
                ##2017 ; Köck et al., 2013 ; Pauly et al., 2019 ;Tkadlec
                ##et al., 2023).
                "899" = "CC1/CC398",
                ##If ST and/or CC was not specified, spa-type t899 was
                ##classified as CC1/CC398 (EFSA, 2009a ; Guardabassi et
                ##al., 2009 ; Larsen et al., 2016).
                "127" = 1,
                ## spa-type t127 was classified as CC1, LA-MRSA (EFSA,
                ## 2009 ; Merialdi et al., 2019).
                "2" = 5, "242" = 5,
                ## spa-types t002 and t242 were classified as CC5 (Asanin
                ## et al., 2019 ; Köck et al., 2013).
                "8" = 8, "9" = 8,
                ## spa-types t008 and t009 were classified as CC8 (Boost
                ## et al., 2012 ; Cuny et al., 2016).
                "1419" = 1, "1430" = 1, "10204" = 1,
                ## spa-types t1419, t1430 and t10204 were associated to
                ## ST9 (EFSA, 2009a ; Hasman et al., 2011 ; Köck et al.,
                ## 2013), and classified as CC1 (PubMLST1).
                "1422" = 692,
                ## spa-type t1422 was classified as CC692 (Silva et al.,
                ## 2020).
                "3512" = 2343 ## spa-type t3512 was
                ## classified as ST2343 (Chen
                ## et al., 2017) and CC1
                ## (PubMLST1).
                )
    spa2CC[spa]
}

##' spa2ST
##'
##' @param spa a vector of spa's
##' @return A vector of inferred ST from spa
spa2ST <- function(spa) {
    spa2ST <-
        c("1419" = 9, "1430" = 9, "10204" = 9)
    ## spa-types t1419, t1430 and t10204 were associated to ST9 (EFSA,
    ## 2009a ; Hasman et al., 2011 ; Köck et al., 2013), and
    ## classified as CC1 (PubMLST1).
    spa2ST[spa]
}

##' AB
##'
##' Translate antibiotic names to shortform
##'
##' @param ab A vector of longform antibiotic names
##' @return A vector of shortform names for antibiotics
AB <- function(ab) {
    AB <- c("Gentamicin" = "GEN",
            "Kanamycin" = "KAN",
            "Streptomycin" = "STR",
            "Chloramphenicol" = "CHL",
            "Rifampicin" = "RIF",
            "Ciprofloxacin" = "CIP",
            "Erythromycin" = "ERY",
            "Clindamycin" = "CLI",
            "Quinupristin/Dalfopristin" = "Q/D",
            "Linezolid" = "LZD",
            "Tiamulin" = "TIA",
            "Mupirocin" = "MUP",
            "Fusidic acid" = "FUS",
            "Sulfamethoxazole" = "SMX",
            "Trimethoprim" = "TMP",
            "Tetracycline" = "TET",
            "Vancomycin" = "VAN",
            "Cefoxitin" = "CEF",
            "Penicillin" = "PEN")
    stopifnot(all(ab %in% names(AB)))
    AB[ab]
}
