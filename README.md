This repository contains all the scripts and data for the work by Lecorvaisier et al. (in prep). The scripts are meant to be run on a server with SLURM installed but could easily be modified to run on a local device without SLURM. We nevertheless discourage it because of the high calculation time required to run all the analyzes and particularly the power analyzes.

## Code

The different scripts have the following roles:

### Part 1: Pertussis data

- `1-pertussis-editing.R`: This script filters the dataset published by Lefrancq et al. (2022) to only keep the strains that we used in the analyzes.

### Part3: Cofactors data

- `2a-cofactors-gathering.R`: This script gives the raw values of the cofactors used in the analyzes.

- `2b-cofactors-transformation.R`: This script calculates the transformed values of the cofactors as explained in the main text using the first method.

- `2c-cofactors-transformation2.R`: This script calculates the transformed values of the cofactors as explained in the main text using the second method.

### Part 3: Frequentist analyzes

- `3-frequentist-analyzes.R`: This script runs the frequentist analyzes, i.e., the GLMs and GLMMs, with the cofactors calculated with both methods presented in the main text.

### Part 4: Robustness analyzes

- `4a-robustness-glmm.R`: This script runs the robustness analysis of the GLMM approach. It is not meant to be run by the user.

- `4b-bash-glmm`: This script creates and runs the SLURM jobs to run the robustness analysis of the GLMM approach.

### Part 5: Bayesian analyzes

- `5a-bayesian-analyzes`: This script runs the Bayesian analyzes (time-consuming, count a few hours to run all the analyzes).

- `5a-bayesian-analyzes`: This script summarizes the results of the Bayesian analyzes.

### Part 6: Power analyzes

- `6a-power-analyzes-1.R`: This script runs the first power analysis. It is not meant to be run by the user.

- `6b-power-analyzes-2.R`: This script runs the second power analysis. It is not meant to be run by the user.

- `6c-bash-bayesian-1.R`: This script creates and runs the SLURM jobs to run the first power analysis. It is not recommended to run these scripts without a powerful computer cluster at disposal.

- `6d-bash-bayesian-2.R`: This script creates and runs the SLURM jobs to run the second power analysis. It is not recommended to run these scripts without a powerful computer cluster at disposal.

- `6e-summary-power-1.R`: This script summarizes the data of the hundreds of thousands of files created with the first power analysis.

- `6f-summary-power-2.R`: This script summarizes the data of the hundreds of thousands of files created with the second power analysis.

### Part 7: Figures

- `7-figures`: This script produces all the figures included in the article.

## Information

Files `analysis-glmm`, `analysis-glmm2`, `analysis-bayes` and `analysis-bayes2` in the `data/` folder contain the data used to produce Tables 2, 3, 4 and 5, respectively. Files `fig-prop.png`, `fig-isolates.png`, `fig-cofactors.png`, `fig.robust.png`, `fig-power.png` and `fig-power-data.png` in the `figures/` folder correspond to Figures 1, 2, 3, 4, 5 and 6, respectively.

