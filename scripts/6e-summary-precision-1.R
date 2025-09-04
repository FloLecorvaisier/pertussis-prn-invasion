library(doParallel)
library(data.table)
library(pbapply)

lf <- list.files("data/analyzes-outputs/precision-analyzes-1/", pattern = "-a-")

read_and_sum <- function(file, colnum) {
  df_out <- read.table(paste0("data/analyzes-outputs/precision-analyzes-1/", file))
  df = data.frame(IC2.5 = quantile(df_out[, colnum], probs = c(.025, .975))[1],
                  IC97.5 = quantile(df_out[, colnum], probs = c(.025, .975))[2],
                  med = median(df_out[, colnum]),
                  sd = sd(df_out[, colnum]))
  df$a0 <- gsub(".*-a0(.*)-af.*", "\\1", file)
  df$af <- gsub(".*-af(.*)-p0.*", "\\1", file)
  df$p0 <- gsub(".*-p0(.*)-tm.*", "\\1", file)
  df$tm <- gsub(".*-tm(.*)-n.*", "\\1", file)
  df$n <- gsub(".*-n(.*)-se.*", "\\1", file)
  df$se <- gsub(".*-se(.*)-sr2.*", "\\1", file)
  df$sr2 <- gsub(".*-sr2(.*)-prop.*", "\\1", file)
  df$prop <- gsub(".*-prop(.*)-rep.*", "\\1", file)
  df$rep <- gsub(".*-rep(.*)", "\\1", file)
  return(df)
}

sum_af = rbindlist(pblapply(lf, read_and_sum, colnum = 2, cl = makeCluster(detectCores() - 1)))
write.table(sum_af, file = "data/analyzes-summaries/summary-precision-1", quote = F, sep = "\t", row.names = F)
