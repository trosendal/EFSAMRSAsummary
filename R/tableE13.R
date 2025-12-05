##' tableE13
##'
##' Produce Annex E - Table 12: MDR patterns in methicillin-resistant
##' Staphylococcus aureus isolates from animals and food, 2023.
##'
##'
##' @param df_AMR The isolate based data object
##' @param year the year to filter
##' @param path_csv path to the output csv file
##' @import data.table
##' @return A path to a csv file
##' @export
tableE13 <- function(df_AMR = read_AMR(),
                     year = 2023,
                     path_csv = tempfile(fileext = ".csv")) {
    stopifnot(identical(length(year), 1L))
    df_AMR_collapse <- collapse_AMR(df_AMR)
    ## how much resistance
    subdf <- df_AMR_collapse[df_AMR_collapse$year == year  &
                             df_AMR_collapse$n > 2, ]
    patterns <- names(sort(table(subdf$fingerprint), decreasing = TRUE))
    subdf$fingerprint <- factor(subdf$fingerprint, levels = patterns)
    tab14 <- do.call("cbind", lapply(unique(subdf$country), function(x) {
        dfinner1 <- subdf[subdf$country == x, ]
        do.call("cbind", lapply(unique(dfinner1$matrix), function(z) {
            dfinner2 <- dfinner1[dfinner1$matrix == z, ]
            n <- table(dfinner2$fingerprint)
            N <- sum(n)
            mat_txt <- paste0(z, " (N=", N, ")")
            occ <- paste0(n, " (", round(n/N * 100, 1), ")")
            occ[n == 0] <- ""
            c(country = x, matrix = mat_txt, occ)
        }))
    }))
    foo <- as.numeric(sapply(strsplit(tab14, " "), "[", 1))
    dim(foo) <- dim(tab14)
    foo <- rowSums(foo, na.rm = TRUE)
    foo <- paste0(foo, " (", round(foo/sum(foo) * 100, 1), ")")
    foo[1:2] <- c("-", "Total")
    tab14 <- cbind(c("Country", "Matrix", patterns), tab14, foo)
    colnames(tab14) <- rep("", times = ncol(tab14))
    write.csv2(tab14,
               file = path_csv,
               row.names = FALSE,
               quote = TRUE)
    path_csv
}
