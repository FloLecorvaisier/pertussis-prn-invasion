## The data set from Lefrancq et al. 2022 [1] comes under the form of a .xlsx
## file, not practical to be used in R. We thus first cleaned by hand the file
## and converted it to a text file.

## We load the file.
data_prn <- read.table("data/raw/data-lefrancq-2022", header = T, sep = "\t")

## We filter the US strains.
data_prn <- data_prn[data_prn$country == "US", ]

## We only keep the strains for which we have the year and state of isolation
## and the PRN status.
data_prn <- na.omit(data_prn[, c("year_isolated", "region", "prn_def")])

## We only keep the strains isolated since the first PRN- strain was found.
data_prn <- data_prn[data_prn$year_isolated >= 
                                 min(data_prn$year_isolated[data_prn$prn_def == "-"]), ]

## We change "region" with "state" and "year_isolated" with "year".
colnames(data_prn)[grep("region", colnames(data_prn))] <- "state"
colnames(data_prn)[grep("year_isolated", colnames(data_prn))] <- "year"

## We edit the names of New York and Washington states.
data_prn$state[data_prn$state == "New York State"] <- "New York"
data_prn$state[data_prn$state == "Washington State"] <- "Washington"

## We load a file containing the two-letters code associated with each state to
## replace the names of the states in the data frame.
states_names <- read.table("files/states-names.txt", h = T, sep = "\t")
for (name in states_names$long) {
  data_prn$state[data_prn$state == name] <- states_names$short[states_names$long == name]
}

## We finally write the file in a standardised format quite matching the format
## used in the next script for the datasets of the cofactors.
data_prn <- data_prn[, c(2, 1, 3)]
data_prn <- data_prn[order(data_prn$year, data_prn$state), ]
write.table(data_prn, file = "data/data-prn", quote = F, row.names = F, sep = "\t")

## [1] https://www.doi.org/10.1126/scitranslmed.abn3253