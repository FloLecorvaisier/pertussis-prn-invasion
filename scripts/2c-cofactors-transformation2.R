## This script must run once all cofactors are ready.

## We import the data set with the strains.
data_prn <- read.table("data/data-prn", header = T, sep = "\t")

## We import the data sets with the cofactors.
list_data_cofac <- list()
for (cofac in list.files("data/cofactors")) {
  list_data_cofac[[gsub("data-", "", cofac)]] <- read.table(paste0("data/cofactors/", cofac), header = T, sep = "\t")
}

## As explained in the main text, we cope with the great heterogeneity in the
## data by transforming the values of the cofactors.
table_prn <- with(data_prn, as.data.frame(table(state, year, prn_def)))
table_prn$year <- as.numeric(as.character(table_prn$year))
data_analyzes <- data.frame(year = unique(data_prn$year),
                            PRNm = tapply(table_prn$Freq[table_prn$prn_def == "-"], table_prn$year[table_prn$prn_def == "-"], sum),
                            PRNp = tapply(table_prn$Freq[table_prn$prn_def == "+"], table_prn$year[table_prn$prn_def == "+"], sum))
for (df in names(list_data_cofac)) {
  data = list_data_cofac[[df]]
  colnames(data)[3] <- "value"
  x1 = x2 = 0
  for (k in unique(table_prn$year) + 1) {
    x1 = c(x1, (sum(table_prn$Freq[table_prn$year == k & table_prn$prn_def == "-"] * data$value[data$year == k]) + x1[length(x1)]) / 
             (sum(table_prn$Freq[table_prn$year == k & table_prn$prn_def == "-"]) + 1))
    x2 = c(x2, (sum(table_prn$Freq[table_prn$year == k & table_prn$prn_def == "+"] * data$value[data$year == k]) + x2[length(x2)]) / 
             (sum(table_prn$Freq[table_prn$year == k & table_prn$prn_def == "+"]) + 1))
  }
  Delta = x1 - x2 
  mu = (x1 + x2) / 2
  data_analyzes[paste0("Delta_", df)] <- Delta[-length(Delta)]
  data_analyzes[paste0("mu_", df)] <- mu[-length(mu)]
}

## We now have the final data frame that will be used in all the following
## analyzes. We save it.
write.table(data_analyzes, file = "data/data-analyzes2", quote = F, row.names = F, sep = "\t")
