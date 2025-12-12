##' table1
##'
##' Table 1 - CC398 only in main chapter
##' Produce Table 1 in the main chapter
##'
##' @param df_prev The object read_prev function
##' @param path_csv path to the output csv file
##' @import data.table
##' @importFrom utils write.csv2
##' @return A path to a csv file
##' @export
table1 <- function(df_prev= read_prev(),
                   path_csv = tempfile(fileext = ".csv")) {

    ## Replace the CC's with infered CC's
    df_prev$CC <- df_prev$CC_infer

    ## Just the CC398 spa types
    df_prev<- df_prev[CC %in% c("398", "CC1/CC398")]

    ## build the table
    table123_inner(df_prev,
                   path_csv = path_csv)
}

##' table2
##'
##' Table 2 - Other types that are not CC398
##' Produce Table 2 in the main chapter
##'
##' @param df_prev The object read_prev function
##' @param path_csv path to the output csv file
##' @import data.table
##' @importFrom utils write.csv2
##' @return A path to a csv file
##' @export
table2 <- function(df_prev = read_prev(),
                   path_csv = tempfile(fileext = ".csv")) {

    ## Replace the CC's with infered CC's
    df_prev$CC <- df_prev$CC_infer

    ## Just the non CC398 spa types
    df_prev <- df_prev[!(CC %in% c("398", "CC1/CC398")) &
                       !is.na(CC)]

    ## build the table
    table123_inner(df_prev,
                   path_csv = path_csv)
}

##' table3
##'
##' Table 3 - Other types that are not CC classified Produce Table 3
##' that is not in the main chapter but helps to understand where the
##' missing data is.
##'
##' @param df_prev The object read_prev function
##' @param path_csv path to the output csv file
##' @import data.table
##' @importFrom utils write.csv2
##' @return A path to a csv file
##' @export
table3 <- function(df_prev = read_prev(),
                   path_csv = tempfile(fileext = ".csv")) {

    ## Replace the CC's with infered CC's
    df_prev$CC <- df_prev$CC_infer

    ## Just the missing CC spa types
    df_prev <- df_prev[is.na(CC)]

    ## build the table
    table123_inner(df_prev,
                   path_csv = path_csv)
}

##' table123_inner
##'
##' @param data the data from read_prev()
##' @param path_csv path to the csv file
##' @return A table
table123_inner <- function(data = read_prev(),
                           path_csv) {
    data <- data[!is.na(T)]
    data[, REPYEAR := as.integer(REPYEAR)]
    data <- data[PROGSAMPSTRATEGY != "Suspect sampling"]
    data <- data[!(SAMPCONTEXT %in% c("Clinical investigations",
                                      "Control and eradication programmes",
                                      "Outbreak investigation"))]
    data <- data[, .(T, REPYEAR, MATRIX_L1, UNITSPOSITIVE, SPECIESTYPE, CC, samplingID)]
    data[, SPA := T]
    data[, T := NULL]
    data[, UNITSPOSITIVE := as.integer(UNITSPOSITIVE)]
    data <- data[, .(n = sum(UNITSPOSITIVE)),
                 by = .(samplingID, SPA, REPYEAR, SPECIESTYPE, MATRIX_L1)]
    data <- data[, .(n = sum(n)), by = .(SPA, REPYEAR, SPECIESTYPE, MATRIX_L1)]
    data <- data[, .(text = paste0(MATRIX_L1, "(", n, ")"), n = sum(n)),
                 by = .(SPA, REPYEAR, SPECIESTYPE, MATRIX_L1)][order(text), ]
    data <- data[, .(text = paste(text, collapse = ", "), n = sum(n)),
                 by = .(SPA, REPYEAR, SPECIESTYPE)]
    data <- dcast(data, SPA + REPYEAR ~ SPECIESTYPE, value.var = c("text", "n"))
    data[is.na(n_animal) == TRUE, n_animal := 0]
    data[is.na(n_food) == TRUE, n_food := 0]
    data[is.na(text_animal) == TRUE, text_animal := ""]
    data[is.na(text_food) == TRUE, text_food := ""]
    data[, sort_order := sum(n_animal + n_food), by = SPA]
    data <- data[order(-sort_order, SPA, -REPYEAR)]
    data[, sort_order := NULL]
    data[, SPA := sprintf("t%03d", as.numeric(SPA))]
    data <- data[, .(`spa-type` = SPA,
                     Year = REPYEAR,
                     `Animals (N)` = text_animal,
                     Total_animal = n_animal,
                     `Food (N)` = text_food,
                     Total_food = n_food)]
    setDF(data)
    write.csv2(data,
               file = path_csv,
               row.names = FALSE,
               quote = TRUE)
    path_csv
}
