## This script converts the hundreds of files of the robustness analyzes into
## one compact file summarizing the information.

library(doParallel)
library(data.table)
library(pbapply)

read_and_sum <- function(file, colnum) {
  df_out <- read.table(file)
  df <- data.frame(param = 1)
  df$param <- gsub(".*-glmm-(.*)-a0.*", "\\1", file)
  df$a0 <- gsub(".*-a0(.*)-af.*", "\\1", file)
  df$af <- gsub(".*-af(.*)-n.*", "\\1", file)
  df$n <- gsub(".*-n(.*)-p0.*", "\\1", file)
  df$p0 <- gsub(".*-p0(.*)-se.*", "\\1", file)
  df$se <- gsub(".*-se(.*)-tm.*", "\\1", file)
  df$tm <- gsub(".*-tm(.*)", "\\1", file)
  df$alpha <- nrow(df_out[df_out$V1 > df_out$V2, ]) / nrow(df_out)
  return(df)
}

#### GLMM ####

lf_glmm <- list.files("data/analyzes-outputs/robustness-glmm/")
summar_glmm <- rbindlist(pblapply(paste0("data/analyzes-outputs/robustness-glmm/", lf_glmm), 
                                  read_and_sum, cl = makeCluster(4)))
write.table(summar_glmm, file = "data/analyzes-summaries/summary-robust-glmm", quote = F, sep = "\t", row.names = F)
