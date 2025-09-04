This repository contains all the scripts and data for the work by Lecorvaisier et al. (in prep). Some of these scripts are meant to be run on a server with SLURM installed but could easily be modified to run on a local device without SLURM. We nevertheless discourage it because of the high calculation time required to run all the analyzes and particularly the precision analyzes.

## Scripts

The different scripts, stored in the `scripts/` folder, have the following roles:

### Part 1: Pertussis data

-   `1-pertussis-editing.R`: This script filters the dataset published by [Lefrancq et al. (2022)](https://doi.org/10.1126/scitranslmed.abn3253) to only keep the strains that we used in the analyzes.

### Part3: Cofactors data

-   `2a-cofactors-gathering.R`: This script gives the raw values of the cofactors used in the analyzes. Some parts are highly time-consuming and running the script does not need to be run because the data it produces are already available in the `data/cofactors/` folder.

-   `2b-cofactors-transformation.R`: This script calculates the transformed values of the cofactors as explained in the main text using the first method. Here again, the script does not need to be run as its outputs are available in the `data/data-analyzes` file.

-   `2c-cofactors-transformation2.R`: This script calculates the transformed values of the cofactors as explained in the main text using the second method. The outputs are available in the `data/data-analyzes2` file.

### Part 3: Frequentist analyzes

-   `3-frequentist-analyzes.R`: This script runs the frequentist analyzes, i.e., the GLMM approach, with the cofactors calculated with both methods presented in the main text. The outputs of the analyzes are stored in the `data/analysis-glmm` and `data/analysis-glmm2` for the GLMM approach using the first and second method for the transformed values of the cofactors, respectively.

### Part 4: Robustness analyzes

-   `4a-robustness-glmm.R`: This script runs the robustness analysis of the GLMM approach. It is meant to be run on a server using SLURM (see script below) but can also be run locally. To run the script locally, you should open a terminal and run the following command:

    ``` bash
    Rscript --vanilla scripts/4a-robustness-glmm.R param value n_sim
    ```

    Where `param` is the parameter you want to check, `value` is the value you want the parameter to take and `n_sim` is the number of simulations you want to run with this value for this parameter. Argument `param` can take the values `tm`, `a0`, `af`, `se`, `n` or `p0`.

    The outputs of this scripts used in the article can be found in the `data/analyzes-outputs/robustness-glmm/` folder.

-   `4b-bash-glmm`: This script creates and runs the SLURM jobs to lead the robustness analysis of the GLMM approach.

-   `4c-summary-robust.R`: This script summarizes the results of the frequentist analyzes. Its outputs are stored in the `data/analyzes-summaries/summary-robust-glmm` file.

### Part 5: Bayesian analyzes

-   `5a-bayesian-analyzes`: This script runs the Bayesian analyzes. It is highly time-consuming (a few hours to run), but its outputs can be found in the `data/analyzes-outputs/bayesian-outputs` and `data/analyzes-outputs/bayesian-outputs2` for the Bayesian approach using the first and second method for the transformed values of the cofactors, respectively.

-   `5b-bayesian-summary`: This script summarizes the results of the Bayesian analyzes. The outputs are stored in the `data/analysis-bayes` and `data/analysis-bayes2` files for the first and second method for the transformed values of the cofactors, respectively.

### Part 6: Precision analyzes

-   `6a-precision-analyzes-1.R`: This script runs the simulations for the first precision analysis. It is meant to be run on a server using SLURM (see script below) but can also be run locally. It is nevertheless not advised to do so, as producing a reasonable number of simulations can take days. To run the script locally, you should open a terminal and run the following command:

    ``` bash
    Rscript --vanilla scripts/6a-power-analyzes-1-data.R a0 af p0 tm n se sr2 prop nrep
    ```

    Where `a0`, `af`, `p0`, `tm`, `n`, `se`, `sr2` and `prop` are the model parameters and `nrep` is the number of simulations.

    The outputs of this script can be found in the `precision-analyzes-1.zip` archive in thisXXX Zenodo record.

-   `6b-power-analyzes-2.R`: This script runs the simulations for the second precision analysis. It is meant to be run on a server using SLURM (see script below) but can also be run locally. It is nevertheless not advised to do so, as producing a reasonable number of simulations can take days. To run the script locally, you should open a terminal and run the following command:

    ``` bash
    Rscript --vanilla scripts/6b-power-analyzes-2.R x y z prop nrep
    ```

    Where `x`, `y`, `z` and `prop` are the model parameters and `nrep` is the number of simulations.

    The outputs of this script can be found in the `precision-analyzes-2.zip` archive in thisXXX Zenodo record.

-   `6c-bash-bayesian-1.R`: This script creates and runs the SLURM jobs to run the first precision analysis. It is not recommended to run these scripts without a powerful computer cluster available.

-   `6d-bash-bayesian-2.R`: This script creates and runs the SLURM jobs to run the second precision analysis. It is not recommended to run these scripts without a powerful computer cluster available.

-   `6e-summary-precision-1.R`: This script summarizes the data of the hundreds of thousands of files created with the first precision analysis. Its outputs are stored in the `data/analyzes-summaries/summary-precision-1` file.

-   `6f-summary-precision-2.R`: This script summarizes the data of the hundreds of thousands of files created with the second precision analysis. Its outputs are stored in the `data/analyzes-summaries/summary-precision-2` file.

### Part 7: Figures

-   `7-figures`: This script produces all the figures included in the article.

## Data

The `data/` folder has four main subfolders. These are:

-   `analyzes-outputs/`: It contains the outputs of the different script producing a large quantity of files (the Bayesian analyzes, the robustness analysis and the precision analyzes). Zip files obtained from the Zenodo record should be extracted in this folder.

-   `analyzes-summary/`: It contains the summaries of the thousands of files in the folder described just above.

-   `cofactors/`: It contains the "clean" values of the cofactors (before transformation).

-   `raw/`: It contains the raw values of the cofactors.

Additionally, the `data/` folder contains the files `analysis-glmm`, `analysis-glmm2`, `analysis-bayes` and `analysis-bayes2` used to produce Tables 2, 3, 4 and 5 of the article, respectively.

## Files

The `files/` folder contains supplementary files used in this work. The most important of these files is the one containing the code for running the Bayesian model, `model.bugs`.

## Figures

Files `fig-prop.png`, `fig-isolates.png`, `fig-cofactors.png`, `fig.robust.png`, `fig-precision-1.png`, `fig-precision-2.png`, `fig-sd-1.png` and `fig-sd-2.png` in the `figures/` folder correspond to Figures 1, 2, 3, 4, 5, 6, 7 and 8, respectively.
