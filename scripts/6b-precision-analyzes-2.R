## This script runs the simulations for the second part of the power analysis
## on the Bayesian model. Note that this script is ABSOLUTELY NOT meant to be
## executed in a reasonable time span. Simulations were run for dozens of days 
## on a computer cluster.
library(jagsUI)
library(doParallel)

## We obtain the values of the parameters of the simulations from an
## outsider script written in Bash.
args <- commandArgs(trailingOnly = TRUE)
tm <- as.numeric(args[1])
x <- as.numeric(args[2])
y <- as.numeric(args[3])
z <- as.numeric(args[4])
prop <- as.numeric(args[5])
nrep <- as.numeric(args[6])

## We fix the values of sr2, p0 and a0 based on a simulation we ran with the
## real data without any cofactor.
sr2 <- 0.355
p0 <- 0.162
a0 <- 0.457

## We change the sample size according to the factor changing the mean (x), the
## factor changing the variance (y), and the factor changing the sampling
## frequency (z).
data <- read.table("data/data-analyzes", header = T, sep = "\t")[1:11, ]
n <- data$PRNm + data$PRNp
tm <- nrow(data) - 1
n2 <- ((n * sqrt(1 / y) + mean(n) * (1 - sqrt(1 / y))) * x)
n2 <- rep(n2 / z, each = z)
n2 <- ceiling(n2)

## We change the values of sr2, a0 and t accordingly to the change in the
## sampling frequency in order to keep a coherent structure of the data.
sr2 <- sr2 / z
a0 <- a0 / z
tm <- tm * z + z - 1

## We set the values of the MCMC parameters.
n.adpt <- 1e3
n.iter <- 1e5
n.thin <- 1e3
n.burn <- 1e4
n.chns <- 3

## The simulations run in parallel, using as much cores as possibles by
## default.
registerDoParallel(cl <- makeCluster(detectCores()))
foreach(nrep = 1:nrep, .packages = "jagsUI") %dopar% {
  af <- sqrt(prop) * sqrt(sr2)
  se <- sqrt(sr2 - af ** 2)
  if (is.na(se)) se <- 0
  
  lf <- list.files("data/analyzes-outputs/power-data-outputs/")
  
  converged <- paste0("jags-a", 
                      "-af", round(af, 3), 
                      "-se", round(se, 3), 
                      "-sr2", round(sr2, 3),
                      "-t", tm, # The original tm, in years (normally 11)
                      "-x", x, 
                      "-y", round(y, 3),
                      "-z", round(z, 3),
                      "-prop", round(prop, 3),
                      "-rep", nrep) %in% lf
  
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
      for (out in names(output$sims.list)) {
        write.table(output$sims.list[[x]],
                    file = paste0("data/analyzes-outputs/power-analyzes-2/jags-", out, 
                                  "-af", round(af, 3), 
                                  "-se", round(se, 3), 
                                  "-sr2", round(sr2, 3),
                                  "-tm", tm,
                                  "-x", x,
                                  "-y", round(y, 3),
                                  "-z", round(z, 3),
                                  "-prop", round(prop, 3),
                                  "-rep", nrep),
                    quote = F, sep = "\t", row.names = F, col.names = F)
      }
      converged <- TRUE
    }
  }
}

stopCluster(cl)
