## This script runs the simulations for the first part of the precision analysis
## of the Bayesian model. Note that this script is ABSOLUTELY NOT meant to be
## executed in a reasonable time span. Simulations were run for dozens of days 
## on a computer cluster.

library(jagsUI)
library(doParallel)

## We list the files already existing. This helps if the simulations crashed
## before finishing so that simulations already done are not re-run.
lf <- list.files("data/analyzes-outputs/precision-analyzes-1/")

## We obtain the values of the parameters of the simulations from an
## outsider script written in Bash.
args <- commandArgs(trailingOnly = TRUE)
a0 <- as.numeric(args[1])
af <- as.numeric(args[2])
p0 <- as.numeric(args[3])
tm <- as.numeric(args[4])
n <- as.numeric(args[5])
se <- as.numeric(args[6])
sr2 <- as.character(args[7])
prop <- as.numeric(args[8])
nrep <- as.numeric(args[9])

## We set the values of the MCMC parameters.
n.adpt <- 1e3
n.iter <- 1e5
n.thin <- 1e3
n.burn <- 1e4
n.chns <- 3

## The simulations run in parallel, using as much cores as possibles by
## default.
registerDoParallel(cl <- makeCluster(detectCores()))

## We run the simulations.
foreach(nrep = 1:nrep, .packages = "jagsUI") %dopar% {
  
  ## We re-calculate the values of af and se if sr2 is given in the arguments.
  if (sr2 != "null") {
    sr2 <- as.numeric(sr)
    af <- sqrt(prop) * sqrt(sr2)
    se <- sqrt(sr2 - af ** 2)
    if (is.na(se)) se = 0 # Because R may state that sqrt(sr - af ** 2) approaches 0 but is < 0, so NA produced!!
  }
  
  ## We check if there is not already a file corresponding to the simulation we
  ## want to run.
  converged <- paste0("-jags-se-a0", a0, round(a0, 3), "-af", round(af, 3), 
                      "-p0", p0, "-tm", tm, "-n", n, "-se", round(se, 3), "-rep", nrep) %in% lf
  
  ## Simulations will be run until convergence of chains is reached.
  while (converged == FALSE) {
    
    ## We produce the values used in the simulation.
    xf <- c(0, rnorm(tm, 0, 1))
    eps <- c(0, rnorm(tm, 0, se))
    psi <- log(p0 / (1 - p0))
    logit <- psi + a0 * 0:tm + af * cumsum(xf) + cumsum(eps)
    p <- exp(logit) / (1 + exp(logit))
    success <- rbinom(tm + 1, n, p)
    
    ## We run the simulation.
    output <- jags(data = list(tm = tm,
                               X = cbind(0, xf),
                               n = rep(n, tm + 1),
                               nv = 1,
                               S = success), 
                   verbose = T, 
                   n.chains = n.chns, n.adapt = n.adpt, n.thin = n.thin, n.burnin = n.burn, n.iter = n.burn + n.iter, 
                   parameters.to.save = c("a", "se", "epsilon", "p"), model.file = "files/model.bugs")
    
    ## We check if the chains converged.
    if (all(unlist(output$Rhat) < 1.1)) {
      
      ## We save the results
      for (x in names(output$sims.list)) {
        write.table(output$sims.list[[x]],
                    file = paste0("data/analyzes-outputs/precision-analyzes-1/jags-", x, 
                                  "-a0", round(a0, 3), 
                                  "-af", round(af, 3), 
                                  "-p0", p0, 
                                  "-tm", tm, 
                                  "-n", n, 
                                  "-se", round(se, 3), 
                                  "-rep", nrep),
                    quote = F, sep = "\t", row.names = F, col.names = F)
      }
      converged <- TRUE
    }
  }
}

stopCluster(cl)
