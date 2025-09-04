library(doParallel)
library(data.table)
library(pbapply)

lf <- list.files("data/analyzes-outputs/precision-analyzes-2/", pattern = "-a-")

read_and_sum <- function(file, colnum) {
  df_out <- read.table(paste0("data/analyzes-outputs/precision-analyzes-2/", file))
  df = data.frame(IC2.5 = quantile(df_out[, colnum], probs = c(.025, .975))[1],
                  IC97.5 = quantile(df_out[, colnum], probs = c(.025, .975))[2],
                  med = median(df_out[, colnum]),
                  sd = sd(df_out[, colnum]))
  df$af <- gsub(".*-af(.*)-se.*", "\\1", file)
  df$se <- gsub(".*-se(.*)-sr2.*", "\\1", file)
  df$sr2 <- gsub(".*-sr2(.*)-tm.*", "\\1", file)
  df$tm <- gsub(".*-tm(.*)-x.*", "\\1", file)
  df$x <- gsub(".*-x(.*)-y.*", "\\1", file)
  df$y <- gsub(".*-y(.*)-z.*", "\\1", file)
  df$z <- gsub(".*-z(.*)-prop.*", "\\1", file)
  df$prop <- gsub(".*-prop(.*)-rep.*", "\\1", file)
  df$rep <- gsub(".*-rep(.*)", "\\1", file)
  return(df)
}

summ_af_data = rbindlist(pblapply(lf, read_and_sum, colnum = 2, cl = makeCluster(detectCores() - 1)))

write.table(summ_af_data, file = "data/analyzes-summaries/summary-precision-2", quote = F, sep = "\t", row.names = F)
