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

clean <- read.csv("data/cleaned_tamu.csv")[,-1]
clean %<>% mutate(Hospitalization_Y = as.factor((Hospitalization_Y >= 1)*1),
                  Vaccination_A = as.factor((Vaccination_A >= 1)*1),
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

# `Rscript code/g_computation.R bootstrap=TRUE B=1000 n=20000`
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  for (arg in args) {
    eval(parse(text = arg))
  }
}


#"""""""""""""""""""""""""#
# g-computation estimator #
#"""""""""""""""""""""""""#

tamu_gcomp <- glm(Hospitalization_Y ~ .,
                  data = clean,
                  family = "binomial")

treatment <- clean %>% mutate(Vaccination_A = as.factor(1))
control <- clean %>% mutate(Vaccination_A = as.factor(0))

treatment_predict <- predict(tamu_gcomp, newdata = treatment, type = "response")
control_predict <- predict(tamu_gcomp, newdata = control, type = "response")

Psi <- mean(treatment_predict - control_predict)
print(paste("The g-computation estimator is", Psi))


#""""""""""""""""""""""""""#
# non-parametric bootstrap #
#""""""""""""""""""""""""""#

set.seed(252)

Psi_est <- function(data, n) {
  boot_samp <- sample_n(data, n, replace = TRUE)
  
  tamu_glm <- glm(Hospitalization_Y ~ ., data = boot_samp, family = "binomial")
  
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
  if (!("B" %in% ls())) B <- 1000
  if (!("n" %in% ls())) n <- 20000
  
  estimates <- replicate(B, Psi_est(clean, n))
  write.csv(estimates, "data/g_comp_np_bootstrap_est.csv")
  print(paste("The non-parametric bootstrap estimate of the",
              "g-computation estimator is",
              mean(estimates)))
  print(paste("The non-parametric bootstrap estimate of the",
              "standard deviation of the g-computation estimator is",
              sd(estimates)))
}
