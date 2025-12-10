##' tableE1
##'
##' Produce Annex E Table 1: Methicillin-resistant Staphylococcus
##' aureus in food, 2024.
##'
##' In the 2024 data as of 2025-12-10 there is a mistake in the German
##' reports in the prevlance data. This has resulted in an incorrect
##' numerator (number of positives) for the German: broiler meat,
##' turkey meat and crustaceans. In order to correct this we have used these three numbers from the isolate based data like this:
##'
##' foo <- collapse_AMR(read_AMR())
##' setDT(foo)
##' foo[year == 2024,
##'     .(pos = sum(as.numeric(unique(totUnitsPositive))), tot = max(totUnitsTested)),
##'     by = .(source, matrix, country)]
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE1 <- function(df_prev = read_prev(),
                    year =  2024,
                    path_csv = tempfile(fileext = ".csv")) {
    table1_2_inner(df_prev,
                   year,
                   path_csv)
}

##' tableE2
##'
##' Produce Annex E Table 2: Methicillin-resistant Staphylococcus
##' aureus in food, 2023.
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param path_csv path to a csv file
##' @import data.table
##' @return path to a csv file
##' @export
tableE2 <- function(df_prev = read_prev(),
                    year =  2023,
                    path_csv = tempfile(fileext = ".csv")) {
    table1_2_inner(df_prev,
                   year,
                   path_csv)
}

##' table1_2_inner
##'
##' @param df_prev The data object
##' @param year the year to filter
##' @param path_csv path to a csv file
##' @return a file path
table1_2_inner <- function(df_prev = read_prev(),
                           year =  NULL,
                           path_csv = tempfile(fileext = ".csv")) {
    stopifnot(is.numeric(year))
    env <- environment()
    stopifnot(identical(length(year), 1L))
    tab1 <- df_prev[year == get("year", envir = env) &
                    source == "food",
                    .(N = sum(N), n = sum(n), prop = sum(n) / sum (N)),
                    by = .(type = matrix,
                           country = country,
                           desc = matrix_txt,
                           unit = SAMPUNIT)]
    tab1 <- tab1[order(type, country, desc, unit)]
    tab1$result <- paste0(tab1$n, " (", round(tab1$prop * 100, 1), "%)")
    tab1 <- tab1[, c(1, 2, 3, 4, 5, 8)]
    names(tab1) <- c("Food", "Country", "Production type", "Sample unit",
                     "Units tested", "Positive for MRSA (%)")
    write.csv2(tab1,
               file = path_csv,
               row.names = FALSE)
    path_csv
}
