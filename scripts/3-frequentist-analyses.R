## This script run the frequentist analyses using GLMMs.

library(lme4)
library(MuMIn)

for (k in c("", 2)) {
  data_analyses <- read.table(paste0("data/data-analyses", k), header = T, sep = "\t")
  
  colnames(data_analyses)[4:19] <- paste(rep(c("Delta", "mu"), times = 8), rep(c("age", "coverage", "density", "hospitals", "households", "income", "physicians", "temperature"), each = 2), sep = "_")
  
  data_analyses <- data_analyses[1:11, ]
  data_analyses <- cbind(data_analyses[, 1:3], "nul" = 0, cbind(data_analyses[, 4:ncol(data_analyses)]))
  
  ## Because our cofactors are expressed in a variety of units, we center and
  ## reduce the values. It helps the glmer() functions to converge and allows
  ## comparisons between coefficients.
  values <- data_analyses[, 4:ncol(data_analyses)]
  values <- values[-1, ] - matrix(data = apply(values[-1, ], 2, mean),
                                  nrow = nrow(values[-1, ]),
                                  ncol = ncol(values[-1, ]),
                                  byrow = T)
  values <- values / matrix(data = apply(values, 2, sd),
                            nrow = nrow(values),
                            ncol = ncol(values),
                            byrow = T)
  values <- rbind(0, values)
  
  ## As explained in the main text, we must use the cumulative values of the
  ## cofactors in the analysis.
  values <- apply(values, 2, cumsum)
  values <- as.data.frame(values)
  values$nul <- 0
  
  ## We run a GL(M)M with each one of the cofactors as an explaining variable,
  ## plus the time. We use a criterion to make the decision on which cofactor is
  ## the best to explain the structure of the data.
  time = 0:(nrow(data_analyses) - 1)
  p.val_glmm = LRT_glmm = criterion_glmm = logitp0_glmm = time_glmm = cofac_glmm = se_glmm = numeric(ncol(values))
  model_glmm = list()
  for (i in 1:ncol(values)) {
    model_glmm[[i]] = glmer(cbind(data_analyses$PRNm, data_analyses$PRNp) ~ time + values[, i] + (1|time), family = "binomial")
    logitp0_glmm[i] = fixef(model_glmm[[i]])[1]
    time_glmm[i] = fixef(model_glmm[[i]])[2]
    cofac_glmm[i] = fixef(model_glmm[[i]])[3]
    
    se_glmm[i] <- VarCorr(model_glmm[[i]])$time[1] ** .5
    
    criterion_glmm[i] = AICc(model_glmm[[i]])
    # LRT_glmm[i] = anova(model_glmm[[i]])$`Sum Sq`[2]
    LRT_glmm[i] = anova(model_glmm[[1]], model_glmm[[i]])$Chisq[2]
    p.val_glmm[i] = pchisq(LRT_glmm[i], 1, lower.tail = FALSE)
    
  }
  cofac <- colnames(data_analyses)[4:ncol(data_analyses)]
  data_glmm <- data.frame(model = cofac, logitp0 = logitp0_glmm, time = time_glmm, cofac = cofac_glmm, se = se_glmm,
                          criterion = criterion_glmm, lrt = LRT_glmm, p = p.val_glmm)
  data_glmm <- data_glmm[order(data_glmm$lrt, decreasing = T), ]
  
  ## We save the results of the analyses.
  write.table(data_glmm, file = paste0("data/analysis-glmm", k), quote = F, row.names = F, sep = "\t")
}

