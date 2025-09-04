## This script is used to send SLURM jobs to a server/cluster for the script
## scripts/4a-robustness-glmm.R

n_sim = 1e4
precision = 0.01

time = "1:30:00" # Should be adapted to the number of simulations (n_sim) and the power of the server.
nodes = 1
tasks = 1
mem = 512

## Only parameter se was tested in the article but other parameters are shown
## for reference.
list_values = list()
# list_values$a0 = seq(0, 2, length = 1 / precision)
# list_values$af = seq(0, 2, length = 1 / precision)
list_values$se = seq(0, 2, length = 1 / precision)
# list_values$p0 = seq(.01, .1, length = 1 / precision)
# list_values$n = unique(round(seq(5, 500, length = 1 / precision)))
# list_values$tm = unique(round(seq(5, 25, length = 1 / precision)))

temp_batch <- tempfile()
temp_err <- tempfile()
temp_out <- tempfile()
for (param in names(list_values)) {
  values = list_values[[param]]
  for (value in values) {
    cat(paste0("#!/bin/bash\n",
               "#SBATCH --time=", time, "\n",
               "#SBATCH --nodes=", nodes, "\n",
               "#SBATCH --ntasks-per-node=", tasks, "\n",
               "#SBATCH --mem=", mem, "mb\n",
               "#SBATCH --job-name=\"glmm-robust\"\n\n",
               "#SBATCH -o ", temp_out, "\n",
               "#SBATCH -e ", temp_err, "\n",
               "Rscript --vanilla scripts/4a-robustness-glmm.R ", param, " ", value, " ", n_sim),
        file = temp_batch)
    system(paste("sbatch", temp_batch))
  }
}
