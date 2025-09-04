## In this script, we run analyzes to check if the frequentist approach is
## robust (for the GLMM).

library(lme4)
library(performance)

## We obtain the parameters of the simulations from an outsider script 
## (written) in Bash.
args = commandArgs(trailingOnly = TRUE)
param = args[1]
value = as.numeric(args[2])
n_sim = as.numeric(args[3])

robustness_glmm <- function(tm, a0, af, se, n, p0) {
  repeat {
    xf <- c(0, rnorm(tm, 0, 1))
    eps <- c(0, rnorm(tm, 0, se))
    psi <- log(p0 / (1 - p0))
    logit <- psi + a0 * 0:tm + af * cumsum(xf) + cumsum(eps)
    p <- exp(logit) / (1 + exp(logit))
    success <- rbinom(tm + 1, n, p)
    success[1] <- max(success[1], 1)
    failure <- n - success
    xd <- c(0, rnorm(tm, 0, 1))
    Z <- 0:tm
    suppressMessages({
      modelC <- try(glmer(cbind(success, failure) ~ as.numeric(0:tm) + cumsum(xf) + (1|Z), family = "binomial"), silent = T)
      modelD <- try(glmer(cbind(success, failure) ~ as.numeric(0:tm) + cumsum(xf) + cumsum(xd) + (1|Z), family = "binomial"), silent = T)
    })
    if (class(modelC) != "try-error" & class(modelD) != "try-error") {
      chat = check_overdispersion(modelD)$dispersion_ratio
      # Chiobs = (deviance(modelC) - deviance(modelD))
      Chiobs = anova(modelD)$`Sum Sq`[3]
      Chiseuil = qchisq(.95, 1)
      df = data.frame(Chiobs, Chiseuil, chat)
      return(df)
    }
  }
}

## We set up the values of the parameters.
tm = ifelse(param == "tm", value, 12)
a0 = ifelse(param == "a0", value, .5)
af = ifelse(param == "af", value, .1) 
se = ifelse(param == "se", value, 0) 
n  = ifelse(param == "n", value, 100) 
p0 = ifelse(param == "p0", value, .01)

## This part is useful to avoid reproducing simulations that have already been
## conducted.
file <- paste0("data/analyzes-outputs/robustness-glmm/robust-glmm-", param, 
               "-a0", round(a0, 3), 
               "-af", round(af, 3),
               "-n", n,
               "-p0", round(p0, 3),
               "-se", round(se, 3),
               "-tm", tm)

if (!file.exists(file)) {
  ## This command produces the 'n_sim' simulations for the chosen value of
  ## parameter 'param', while values for other parameters are fixed to a
  ## default value.
  repli <- replicate(n_sim, robustness_glmm(tm, a0, af, se, n, p0),
                     simplify = "matrix")
  
  ## We then edit a little the object containing the outputs of the 
  ## simulations for exportation.
  repli <- as.data.frame(t(repli))
  for (cl in colnames(repli)) { # This loop is mandatory for the exportation to work.
    repli[, cl] <- as.numeric(repli[, cl])
  }
  
  ## We save the file containing the simulations for one value of one parameter.
  write.table(repli, file = paste0("data/analyzes-outputs/robustness-glmm/robust-glmm-", param, 
                                   "-a0", round(a0, 3),
                                   "-af", round(af, 3),
                                   "-n", n,
                                   "-p0", round(p0, 3),
                                   "-se", round(se, 3),
                                   "-tm", tm), 
              quote = F, sep = "\t", row.names = F, col.names = F)
}
