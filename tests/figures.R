## Figure 1
obs <- EFSAMRSAsummary::figure1(check = TRUE)
ex <- structure(c(226, 40, 151, 7, 154, 7, 21, 5, 755, 354, 173, 28,
                  130, 20, 291, 20, 279, 22, 380, 39, 313, 45, 616,
                  42, 475, 36, 74, 5, 310, 6, 185, 18, 407, 28, 1103,
                  140, 325, 15),
                dim = c(2L, 19L),
                dimnames = list(NULL,
                c("Crustaceans (DE, 2024)", "Sheep meat (NL, 2023)",
                  "Sheep meat (NL, 2024)", "Turkey meat (NL, 2024)",
                  "Turkey meat (DE, 2024)", "Turkey meat (AT, 2024)",
                  "Pig meat (SK, 2023)", "Pig meat (NL, 2023)",
                  "Pig meat (NL, 2024)", "Pig meat (DE, 2023)",
                  "Pig meat (AT, 2023)", "Bovine meat (NL, 2023)",
                  "Bovine meat (NL, 2024)", "Bovine meat (DE, 2023)",
                  "Bovine meat (AT, 2023)", "Broiler meat (NL, 2023)",
                  "Broiler meat (NL, 2024)","Broiler meat (DE, 2024)",
                  "Broiler meat (AT, 2024)")))
stopifnot(identical(obs, ex))

## Figure 2
obs <- EFSAMRSAsummary::figure2(check = TRUE)
dput(obs)
ex <- structure(c(27, 5, 156, 8, 322, 3, 129, 1, 310, 166, 130, 27,
                  114, 81, 351, 91, 307, 11, 316, 2, 356, 84),
                dim = c(2L, 11L),
                dimnames = list(NULL,
                c("Turkeys (BE, 2023)", "Small ruminants (NL, 2023)",
                  "Broilers (DE, 2024)", "Broilers (BE, 2023)",
                  "Pigs (CH, 2023)", "Pigs (SK, 2023)",
                  "Pigs (NL, 2024)", "Pigs (DE, 2023)",
                  "Cattle (CH, 2023)", "Cattle (NL, 2023)",
                  "Cattle (BE, 2024)")))
stopifnot(identical(obs, ex))
