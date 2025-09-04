## This script is used to send SLURM jobs to a server/cluster for the script
## scripts/6a-precision-analyzes-1.R

## SBATCH parameters
nodes = 1
tasks = 8
mem = 256

nrep = 1000

list_values <- list()
list_values$a0 <- seq(0, 1.5, 0.25)
list_values$n <- c(seq(5, 30, 5), seq(50, 500, 50))
list_values$tm <- c(5:9, seq(15, 25, 5))
list_values$prop <- seq(0, 1, .1)

temp_batch <- tempfile()
temp_err <- tempfile()
temp_out <- tempfile()
for (param in names(list_values)) {
  for (val in list_values[[param]]) {
    a0 <- .5
    af <- "NA"
    p0 <- .1
    tm <- 10
    n <- 200
    prop <- 0
    se <- "NA"
    sr2 <- 1
    assign(param, val)
    
    ## To set up the time according to the value of tm (the higher the tm, the longer the time)
    time = paste0(.5 * tm, ":00:00")
    
    if (param == "prop") {
      for (sr2 in c(.1, .5, 1, 2)) {
        values <- c(a0, af, p0, tm, n, se, sr2, prop, nrep)
        
        title <- paste0("-a0", a0, "-af", af, "-p0", p0, "-tm", tm, "-n", n, "-se", se, "-sr2", "-prop", prop)
        
        cat(paste0("#!/bin/bash", "\n",
                   "#SBATCH --time=", time, "\n",
                   "#SBATCH --nodes=", nodes, "\n",
                   "#SBATCH --ntasks-per-node=", tasks, "\n",
                   "#SBATCH --mem=", mem, "mb", "\n",
                   "#SBATCH -o ", temp_out, "\n",
                   "#SBATCH -e ", temp_err, ".err", "\n",
                   "#SBATCH --job-name=\"pwr1", title, "\"", "\n\n",
                   paste("Rscript --vanilla scripts/6a-precision-analyzes-1-data.R",
                         paste(values, collapse = " "))),
            file = temp_batch)
        system(paste("sbatch", temp_batch))
      }
    } else {
      for (prop in seq(0, 1, .25)) {
        values <- c(a0, af, p0, tm, n, se, sr2, prop, nrep)
        
        title <- paste0("-a0", a0, "-af", af, "-p0", p0, "-tm", tm, "-n", n, "-se", se, "-sr2", "-prop", prop)
        
        cat(paste0("#!/bin/bash", "\n",
                   "#SBATCH --time=", time, "\n",
                   "#SBATCH --nodes=", nodes, "\n",
                   "#SBATCH --ntasks-per-node=", tasks, "\n",
                   "#SBATCH --mem=", mem, "mb", "\n",
                   "#SBATCH -o ", temp_out, "\n",
                   "#SBATCH -e ", temp_err, ".err", "\n",
                   "#SBATCH --job-name=\"pwr1", title, "\"", "\n\n",
                   paste("Rscript --vanilla scripts/6a-precision-analyzes-1-data.R",
                         paste(values, collapse = " "))),
            file = temp_batch)
        system(paste("sbatch", temp_batch))
      }
    }
  }
}