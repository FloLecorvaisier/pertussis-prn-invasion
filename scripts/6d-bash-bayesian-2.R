## This script is used to send SLURM jobs to a server/cluster for the script
## scripts/6a-precision-analyzes-2.R

## SBATCH parameters
nodes = 1
tasks = 8
mem = 256

nrep = 1000

lf <- list.files("data/analyzes-outputs/power-analyzes-2")

list_values <- list()
list_values$x <- c(1, 2, 4, 8)
list_values$y <- c(1, 2, 4, 8)
list_values$z <- c(1, 2, 3, 4)
list_values$prop <- seq(0, 1, .1)

temp_batch <- tempfile()
temp_err <- tempfile()
temp_out <- tempfile()
for (param in c("x", "y", "z")) {
  for (val in list_values[[param]]) {
    for (prop in seq(0, 1, .1)) {
      x <- 1
      y <- 1
      z <- 1
      assign(param, val)
      
      ## To set up the time according to the value of z (the higher the z, the longer the time)
      time = paste0(5 * z, ":00:00")
      
      values <- c(x, y, z, prop, nrep)
      
      cat(paste0("#!/bin/bash", "\n",
                 "#SBATCH --time=", time, "\n",
                 "#SBATCH --nodes=", nodes, "\n",
                 "#SBATCH --ntasks-per-node=", tasks, "\n",
                 "#SBATCH --mem=", mem, "mb", "\n",
                 "#SBATCH -o ", temp_out, "\n",
                 "#SBATCH -e ", temp_err, ".err", "\n",
                 "#SBATCH --job-name=\"pwr2", title, "\"", "\n\n",
                 paste("Rscript --vanilla scripts/6b-power-analyzes-2.R",
                       paste(values, collapse = " "))),
          file = temp_batch)
      system(paste("sbatch", temp_batch))
    }
  }
}