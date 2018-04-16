# PH252D Causal Inference Final Project: The Average Treatment Effect of Vaccination on Subsequent Admissions to Hospitals
### By: Eric Kim, David Proudman, James Duncan, Denys Dukhovnov

In order to download this repository, run
```
git clone https://github.com/erickim/PH252D_final_project.git
```

In order to get the cleaned data set, please create a `data` directory, put the `TAMU_FINAL_SUBSET.csv` file in the `data` directory, and run in the terminal from the main directory:

```
rscript code/clean.R
```

## The g-computation estimator
In order to compute the g-computation estimator, please run

```
rscript code/g_computation.R bootstrap=FALSE
```

If `bootstrap=FALSE` not specified, then it will default to `FALSE`.

If you want to compute the g-computation estimator and have a non-parametric bootstrap estimate of the g-computation estimator and its standard error, please run

```
rscript code/g_computation.R bootstrap=TRUE B=1000 n=20000
```

Where you can alter `B` and `n` to change the number of bootstrap samples to take and the number of rows to sample in each run of the bootstrap. The bootstrap estimates will be saved into `data/g_comp_np_bootstrap_est.csv`

## The IPTW estimator
In order to compute the g-computation estimator, please run

```
rscript code/iptw.R bootstrap=FALSE
```

If `bootstrap=FALSE` not specified, then it will default to `FALSE`.

If you want to compute the g-computation estimator and have a non-parametric bootstrap estimate of the g-computation estimator and its standard error, please run

```
rscript code/iptw.R bootstrap=TRUE B=1000 n=20000
```

Where you can alter `B` and `n` to change the number of bootstrap samples to take and the number of rows to sample in each run of the bootstrap. The bootstrap estimates will be saved into `data/iptw_np_bootstrap_est.csv`