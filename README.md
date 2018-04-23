# PH252D Causal Inference Final Project: The Average Treatment Effect of Vaccination on Subsequent Admissions to Hospitals

By: Denys Dukhovnov, James Duncan, Eric Kim, David Proudman

In order to download this repository, run
```
git clone https://github.com/erickim/PH252D_final_project.git
```

In order to get the cleaned data set, please create a `data` directory, put the `TAMU_FINAL_SUBSET.csv` file in the `data` directory, and run in the terminal from the main directory:

```
rscript code/clean.R
```

## The g-computation estimator
In order to compute the g-computation estimator for continuous response, please run

```
rscript code/g_computation.R type=\"continuous\" bootstrap=FALSE
```

If `bootstrap=FALSE` not specified, then it will default to `FALSE`. The script can also perform g-computation estimation for binarized response with `type=\"binary\"`. If `type` is not specified, then it will default to continuous. Note that when passing in strings, you must escape the quotation marks.

If you want to compute the g-computation estimator and have a non-parametric bootstrap estimate of the g-computation estimator and its standard error, please run

```
rscript code/g_computation.R bootstrap=TRUE B=500 n=2500
```

Where you can alter `B` and `n` to change the number of bootstrap samples to take and the number of rows to sample in each run of the bootstrap. The bootstrap estimates will be saved into `data/g_comp_np_bootstrap_est.csv`

## The IPTW estimator
In order to compute the iptw estimator for continuous response, please run

```
rscript code/iptw.R type=\"continuous\" bootstrap=FALSE
```

If `bootstrap=FALSE` not specified, then it will default to `FALSE`. The script can also perform iptw estimation for binarized response with `type=\"binary\"`. If `type` is not specified, then it will default to continuous. Note that when passing in strings, you must escape the quotation marks.

If you want to compute the IPTW estimator and have a non-parametric bootstrap estimate of the IPTW estimator and its standard error, please run

```
rscript code/iptw.R bootstrap=TRUE B=500 n=2500
```

Where you can alter `B` and `n` to change the number of bootstrap samples to take and the number of rows to sample in each run of the bootstrap. The bootstrap estimates will be saved into `data/iptw_np_bootstrap_est.csv`

## The TMLE estimate
In order to compute the tmle estimator, please run

```
rscript code/tmle.R
```

For the TMLE, we have a confidence interval based on the Influence Curve hence we do not need to do bootstrap hence it is not implemented.

## Running G-Computation and IPTW Bootstrap
If you run

```
bash code/bootstrap_gcomp_iptw.sh
```

The the bootstrap for the g-comp and iptw esitmator will automatically run for both binary and continuous response with B=500 and n=2500. To change this, you will need to edit the file.
