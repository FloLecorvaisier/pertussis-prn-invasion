## This script runs the bayesian analyzes for all the cofactors (including the
## null model) and for the two methods of calculation of the transformed
## cofactors.

library(jagsUI)

for (k in c("", 2)) {
  data_analyzes <- read.table(paste0("data/data-analyzes", k), header = T, sep = "\t")
  data_analyzes <- data_analyzes[1:11, ]
  data_analyzes$null <- 0 # To have the null model
  list_var <- unique(gsub(".*_", "", colnames(data_analyzes)[4:ncol(data_analyzes)]))
  
  n.adpt = 1e4
  n.iter = 1e6
  n.thin = 1e3
  n.burn = 1e4
  n.chns = 3
  
  for (num in 1:20) {
    for (i in 4:ncol(data_analyzes)) {
      data <- data_analyzes[, c(1:3, i)]
      
      values <- data[, 4]
      values <- (values[- 1] - mean(values[- 1])) / sd(values[- 1])
      values <- c(0, values) ## Actually, since the first value is not used in the model, it can be anything and the model would still work the same.
      
      n <- apply(data[, 2:3], 1, sum)
      success = data[, 2]
      
      converged = F
      
      while (converged == F) {
        out_jags = jags(data = list(tm = length(n) - 1,
                                    X = cbind(0, values), # To solve that if the data frame of values has just one column, it becomes a vector.
                                    n = n,
                                    nv = 1,
                                    S = success),
                        n.adapt = n.adpt, n.burnin = n.burn, n.iter = n.iter + n.burn, n.chains = n.chns, n.thin = n.thin,
                        model.file = "files/model.bugs",
                        parameters.to.save = c("a", "se", "epsilon", "p"))
        converged <- ifelse(all(na.omit(unlist(out_jags$Rhat)) < 1.1), T, F)
      }
      
      for (out in 1:length(out_jags$sims.list)) {
        write.table(out_jags$sims.list[out], 
                    paste0("data/analyzes-outputs/bayesian-outputs", k, "/outputs-", colnames(data_analyzes)[i], "-", 
                           names(out_jags$sims.list)[out], "-", num), 
                    quote = F, sep = "\t", row.names = F, col.names = F)
      }
      write.table(out_jags$DIC, paste0("data/analyzes-outputs/bayesian-outputs", k, "/DIC-", colnames(data_analyzes)[i], "-", num), 
                  quote = F, sep = "\t", row.names = F, col.names = F)
    }
  }
}

