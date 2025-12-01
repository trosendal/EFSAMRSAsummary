## Figure 1 in the main chapter
source("R/functions.R")
df_prev <- read_prev()
tab1 <- df_prev[year %in% c(2023, 2024) & source == "food",
            .(N = sum(N), n = sum(n), prop = sum(n) / sum (N)),
            by = .(year = year,
                   type = matrix,
                   country = country,
                   desc = matrix_txt,
                   unit = SAMPUNIT)]
tab1 <- tab1[order(type, -year, country, desc, unit)]
tabgraph <- tab1[, .(N = sum(N),
                     n = sum(n),
                     p = sum(n)/sum(N)),
                 by = .(Food = type, country, year)]
tabgraph <- tabgraph[N > 10, ]
## order of foods in graph
foods <- tabgraph[, .(N = sum(N)), by = .(Food)][order(-N), Food]
tabgraph[, foodrank := match(Food, foods)]
tabgraph <- tabgraph[order(foodrank, country, -year),]
graphnames <- c("Meat from broilers (Gallus gallus)" = "Broiler meat",
              "Meat from bovine animals" = "Bovine meat",
              "Meat from pig" = "Pig meat",
              "Meat from turkey" = "Turkey meat",
              "Meat from sheep" = "Sheep meat",
              "Crustaceans" = "Crustaceans",
              "Meat from deer (venison)" = "Deer meat")
tabgraph$Food <- as.character(graphnames[tabgraph$Food])
countrynames <- c("Austria" = "AT",
                  "Germany" = "DE",
                  "Netherlands" = "NL",
                  "Norway" = "NO",
                  "Slovakia" = "SK")
tabgraph$country <- as.character(countrynames[tabgraph$country])
mat <- matrix(c(tabgraph$N, tabgraph$n),
              nrow = 2, byrow = TRUE)
colnames(mat) <- paste0(tabgraph$Food,
                        " (",
                        tabgraph$country,
                        ", ",
                        tabgraph$year, ")")
mat <- mat[, seq(ncol(mat), 1)]
pdf("tables_and_figures/figure1.pdf", height = 12, width = 8, timestamp = FALSE)
par(mar = c(8, 11, 0, 6))
cols <- c("#409fff", "#ffb740")
bp <- barplot(mat,
              horiz = TRUE,
              col = cols,
              las = 1,
              xlim = c(0, 1200),
              xlab = "No. of sample units tested")
labels <- paste0(round(mat[2,]/mat[1,] * 100, 1),
                 "% (N=", mat[1,], ")")
text(x = colSums(mat),
     y = bp,
     labels,
     pos = 4,
     xpd = TRUE)
legend(
    "bottom",
    legend = c("No. of sample units negative for MRSA (%)",
               "No. of sample units positive for MRSA (%)"),
    fill   = cols,
    horiz  = TRUE,
    inset = c(-100, -0.12),
    xpd = TRUE,
    cex = 0.9)
dev.off()
