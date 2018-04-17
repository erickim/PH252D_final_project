###############################################################################
#                                                                             #
#                               implement IPTW                                #
#                                                                             #
###############################################################################

library(tidyverse)
library(magrittr)

clean <- read.csv("data/cleaned_tamu.csv")[,-1]
clean %<>% mutate(Hospitalization_Y = as.factor((Hospitalization_Y >= 1)*1),
                  Vaccination_A = as.factor((Vaccination_A >= 1)*1),
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

# `Rscript code/iptw.R bootstrap=TRUE B=1000 n=20000`
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  for (arg in args) {
    eval(parse(text = arg))
  }
}


#""""""""""""""""#
# IPTW estimator #
#""""""""""""""""#

gAW_glm <- glm(Vaccination_A ~ Net_Worth_W1 + Above_Pvt_Line_W1 +
                 Education_W2 + College_W2 + Age_W3 + Medical_Risk_W4 +
                 Sex_W5 + Index_Health_ins_engage,
               data = clean,
               family = "binomial")

pred_g1W <- predict(gAW_glm, type = "response")
pred_g0W <- 1 - pred_g1W

gAW <- vector(length = nrow(clean))
gAW[clean$Vaccination_A == 1] <- pred_g1W[clean$Vaccination_A == 1]
gAW[clean$Vaccination_A == 0] <- pred_g0W[clean$Vaccination_A == 0]

wt <- 1/gAW

# HT estimator
IPTW <- mean(wt*(clean$Vaccination_A == 1)*as.numeric(clean$Hospitalization_Y)) -
  mean(wt*(clean$Vaccination_A == 0)*as.numeric(clean$Hospitalization_Y))

print(paste("The IPTW estimator is", round(IPTW, 4)))

# Hajek estimator
stab_IPTW <- mean(wt*(clean$Vaccination_A == 1)*as.numeric(clean$Hospitalization_Y))/mean(wt*(clean$Vaccination_A == 1)) -
  mean(wt*(clean$Vaccination_A == 0)*as.numeric(clean$Hospitalization_Y))/mean(wt*(clean$Vaccination_A == 0))

print(paste("The stabilized IPTW estimator is", stab_IPTW))

#""""""""""""""""""""""""""#
# non-parametric bootstrap #
#""""""""""""""""""""""""""#

set.seed(252)

IPTW_est <- function(data, n) {
  boot_samp <- sample_n(data, n, replace = TRUE)
  
  gAW_glm <- glm(Vaccination_A ~ Income_W1 + Above_Pvt_Line_W1 +
                   Below_Pvt_Line_W1 + Education_W2 + College_W2 +
                   Age_W3 + Medical_Risk_W4 + Sex_W5,
                 data = boot_samp,
                 family = "binomial")
  
  pred_g1W <- predict(gAW_glm, type = "response")
  pred_g0W <- 1 - pred_g1W
  
  gAW <- vector(length = nrow(boot_samp))
  gAW[boot_samp$Vaccination_A == 1] <- pred_g1W[boot_samp$Vaccination_A == 1]
  gAW[boot_samp$Vaccination_A == 0] <- pred_g0W[boot_samp$Vaccination_A == 0]
  
  wt <- 1/gAW
  
  # HT estimator
  IPTW <- mean(wt*(boot_samp$Vaccination_A == 1)*as.numeric(boot_samp$Hospitalization_Y)) -
    mean(wt*(boot_samp$Vaccination_A == 0)*as.numeric(boot_samp$Hospitalization_Y))
  
  # Hajek estimator
  stab_IPTW <- mean(wt*(boot_samp$Vaccination_A == 1)*as.numeric(boot_samp$Hospitalization_Y))/mean(wt*(boot_samp$Vaccination_A == 1)) -
    mean(wt*(boot_samp$Vaccination_A == 0)*as.numeric(boot_samp$Hospitalization_Y))/mean(wt*(boot_samp$Vaccination_A == 0))
  
  return(c(IPTW, stab_IPTW))
}

# if bootstrap not passed in command line, dont do the bootstrap
if (!("bootstrap") %in% ls()) bootstrap <- FALSE
if (bootstrap) {
  # set default if B and n not passed in command line
  if (!("B" %in% ls())) B <- 1000
  if (!("n" %in% ls())) n <- 20000
  
  estimates <- t(replicate(B, IPTW_est(clean, n)))
  write.csv(estimates, "data/iptw_np_bootstrap_est.csv", row.names = FALSE)
  print(paste("The non-parametric bootstrap estimate of the",
              "IPTW estimator is",
              mean(estimates[,1])))
  print(paste("The non-parametric bootstrap estimate of the",
              "standard deviation of the IPTW estimator is",
              sd(estimates[,1])))
  print(paste("The non-parametric bootstrap estimate of the",
              "stabilized IPTW estimator is",
              mean(estimates[,2])))
  print(paste("The non-parametric bootstrap estimate of the",
              "standard deviation of the stabilized IPTW estimator is",
              sd(estimates[,2])))
}