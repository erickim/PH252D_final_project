###############################################################################
#                                                                             #
#                              implement g-comp                               #
#                                                                             #
###############################################################################

#"""""""""#
# loading #
#"""""""""#

library(tidyverse)
library(magrittr)

# `Rscript code/g_computation.R bootstrap=TRUE B=1000 n=20000`
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  for (arg in args) {
    eval(parse(text = arg))
  }
}

clean <- read.csv("data/cleaned_tamu.csv")[,-1]
clean %<>% mutate(Vaccination_A = as.factor((Vaccination_A >= 1)*1),
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

# if `type` not specified, just do continuous `Y`
if (!("type") %in% ls()) {
  type <- "continuous"
  family <- "gaussian"
}
if (!(type %in% c("continuous", "binary"))) {
  type <- "continuous"
  family <- "gaussian"
}

if (type == "binary") {
  clean %<>% mutate(Hospitalization_Y = as.factor((Hospitalization_Y >= 1)*1))
  family <- "binomial"
}

cat(paste0("G-computation for ", type, " response will be performed.\n"))

#"""""""""""""""""""""""""#
# g-computation estimator #
#"""""""""""""""""""""""""#

tamu_gcomp <- glm(Hospitalization_Y ~ .,
                  data = clean,
                  family = family)

treatment <- clean %>% mutate(Vaccination_A = as.factor(1))
control <- clean %>% mutate(Vaccination_A = as.factor(0))

treatment_predict <- predict(tamu_gcomp, newdata = treatment, type = "response")
control_predict <- predict(tamu_gcomp, newdata = control, type = "response")

Psi <- mean(treatment_predict - control_predict)
cat(paste0("The g-computation estimator is ", Psi, ".\n"))


#""""""""""""""""""""""""""#
# non-parametric bootstrap #
#""""""""""""""""""""""""""#

set.seed(252)

Psi_est <- function(data, n) {
  boot_samp <- sample_n(data, n, replace = TRUE)
  
  tamu_glm <- glm(Hospitalization_Y ~ .,
                  data = boot_samp,
                  family = family)
  
  treatment <- boot_samp %>% mutate(Vaccination_A = as.factor(1))
  control <- boot_samp %>% mutate(Vaccination_A = as.factor(0))
  
  treatment_pred <- predict(tamu_glm, newdata = treatment, type = "response")
  control_pred <- predict(tamu_glm, newdata = control, type = "response")
  
  return(mean(treatment_pred - control_pred))
}

# if bootstrap not passed in command line, dont do the bootstrap
if (!("bootstrap") %in% ls()) bootstrap <- FALSE
if (bootstrap) {
  # set default if B and n not passed in command line
  if (!("B" %in% ls())) B <- 500
  if (!("n" %in% ls())) n <- 2500
  
  estimates <- replicate(B, Psi_est(clean, n))
  write.csv(estimates, "data/g_comp_np_bootstrap_est.csv", row.names = FALSE)
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "g-computation estimator is ",
             mean(estimates),
             ".\n"))
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "standard deviation of the g-computation estimator is ",
             sd(estimates),
             ".\n"))
}


