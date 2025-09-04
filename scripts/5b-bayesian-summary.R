## This script converts the hundreds of files of the Bayesian analyzes into
## one compact file (per method of calculation of the transformed values of the
## cofactors) summarizing the information.

for (k in c("", 2)) {
  lf <- list.files(paste0("data/analyzes-outputs/bayesian-outputs", k, "/"), pattern = "DIC")
  lf_a <- list.files(paste0("data/analyzes-outputs/bayesian-outputs", k, "/"), pattern = "-a-")
  
  df <- data.frame(model = gsub("DIC-(.*)-.*", "\\1", lf),
                   dic = 0, a0 = 0, af = 0)
  
  for (i in 1:length(lf)) {
    data <- read.table(paste0("data/analyzes-outputs/bayesian-outputs", k, "/", lf[i]))
    data_a <- read.table(paste0("data/analyzes-outputs/bayesian-outputs", k, "/", lf_a[i]))
    df$dic[i] <- data$V1[1]
    df$a0[i] <- median(data_a$V1)
    df$af[i] <- median(data_a$V2)
    df$p[i] <- table(data_a$V2 < 0)[ifelse(median(data_a$V2) < 0, 1, 2)] / nrow(data_a) * 2
    if (is.na(df$p[i])) df$p[i] <- 0
  }
  
  df_plot <- data.frame(model = unique(df$model), 
                        dic = tapply(df$dic, df$model, mean),
                        sd = tapply(df$dic, df$model, sd),
                        sem_dic = tapply(df$dic, df$model, sd) / sqrt(tapply(df$dic, df$model, length)),
                        a0 = tapply(df$a0, df$model, mean),
                        sd_a0 = tapply(df$a0, df$model, sd),
                        sem_a0 = tapply(df$a0, df$model, sd) / sqrt(tapply(df$a0, df$model, length)),
                        af = tapply(df$af, df$model, mean),
                        sd_af = tapply(df$af, df$model, sd),
                        sem_af = tapply(df$af, df$model, sd) / sqrt(tapply(df$af, df$model, length)),
                        pval = tapply(df$p, df$model, mean))
  
  df_plot$DeltaDIC <- df_plot$dic - df_plot$dic[df_plot$model == "null"]
  df_plot2 <- df_plot[, c("model", "dic", "a0", "af", "DeltaDIC", "pval")]  
  df_plot2[df_plot2$model == "null", c("af", "pval", "DeltaDIC")] <- NA
  df_plot2 <- df_plot2[order(df_plot2$DeltaDIC), ]
  
  write.table(df_plot2, paste0("data/analysis-bayes", k), quote = F, sep = "\t", row.names = F)
}
