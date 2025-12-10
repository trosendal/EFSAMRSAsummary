##' tableE5
##'
##' Produce Annex E table 5: Methicillin-resistant Staphylococcus aureus in
##' food-producing animals, clinical investigations, 2024.
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param food logical if you want food (TRUE) or nonfood (FALSE)
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE5 <- function(df_prev = prev_by_samplingID(),
                    year =  2024,
                    food = TRUE,
                    path_csv = tempfile(fileext = ".csv")) {
    tableE5_E6_E7_inner(df_prev,
                        year,
                        food,
                        path_csv)
}

##' tableE6
##'
##' Produce Annex E table 6: Methicillin-resistant Staphylococcus aureus in
##' non-food-producing animals, clinical investigations, 2024.
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param food logical if you want food (TRUE) or nonfood (FALSE)
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE6 <- function(df_prev = prev_by_samplingID(),
                    year =  2024,
                    food = FALSE,
                    path_csv = tempfile(fileext = ".csv")) {
    tableE5_E6_E7_inner(df_prev,
                        year,
                        food,
                        path_csv)
}

##' tableE7
##'
##' Produce Annex E table 7: Methicillin-resistant Staphylococcus
##' aureus in non-food-producing animals, clinical investigations,
##' 2023.
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param food logical if you want food (TRUE) or nonfood (FALSE)
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE7 <- function(df_prev = prev_by_samplingID(),
                    year =  2023,
                    food = FALSE,
                    path_csv = tempfile(fileext = ".csv")) {
    tableE5_E6_E7_inner(df_prev,
                        year,
                        food,
                        path_csv)
}

##' tableE5_E6_E7_inner
##'
##' Produce a table
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param food logical if you want food (TRUE) or nonfood (FALSE)
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
tableE5_E6_E7_inner <- function(df_prev = prev_by_samplingID(),
                                year =  NULL,
                                food = FALSE,
                                path_csv = tempfile(fileext = ".csv")) {
    stopifnot(is.numeric(year))
    env <- environment()
    stopifnot(identical(length(year), 1L))
    nonfood <- c("Dogs", "Felidae", "Solipeds, domestic",
                 "Land game mammals")
    tab1 <- df_prev[
    {
        i <- source == "animal" &
             year == get("year", envir = env) &
             SAMPCONTEXT %in% c("Clinical investigations", "Outbreak investigation")
        if (get("food", envir = env)) j <- !(matrix %in% nonfood)
        else {j <- (matrix %in% nonfood)}
        i & j
    },
                    .(N = sum(N), n = sum(n), prop = sum(n) / sum(N)),
                    by = .(type = matrix,
                           country = country,
                           desc = matrix_txt,
                           unit = SAMPUNIT)]
    tab1 <- tab1[order(type, country, desc, unit)]
    tab1$result <- paste0(tab1$n, " (", round(tab1$prop * 100, 1), "%)")
    tab1 <- tab1[, c(1, 2, 3, 4, 5, 8)]
    names(tab1) <- c("Animal", "Country", "Production type",
                     "Sample unit", "Units tested",
                     "Positive for MRSA (%)")
    write.csv2(tab1,
               file = path_csv,
               row.names = FALSE)
    path_csv
}
