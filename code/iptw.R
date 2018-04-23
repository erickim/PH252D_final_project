###############################################################################
#                                                                             #
#                               implement IPTW                                #
#                                                                             #
###############################################################################

#"""""""""#
# loading #
#"""""""""#

suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(readr))

# `Rscript code/iptw.R bootstrap=TRUE B=1000 n=20000`
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  for (arg in args) {
    eval(parse(text = arg))
  }
}

clean <- read_csv("data/cleaned_tamu.csv")[,-1]
clean %<>% mutate(Vaccination_A = as.factor((Vaccination_A >= 1)*1),
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

# if `type` not specified, just do continuous `Y`
if (!("type") %in% ls()) type <- "continuous"
if (!(type %in% c("continuous", "binary"))) type <- "continuous"

if (type == "binary") {
  clean %<>% mutate(Hospitalization_Y = as.factor((Hospitalization_Y >= 1)*1))
}

cat(paste0("IPTW for ", type, " response will be performed.\n"))

#""""""""""""""""#
# IPTW estimator #
#""""""""""""""""#

gAW_glm <- glm(Vaccination_A ~ . - Hospitalization_Y,
               data = clean, family = "binomial")

pred_g1W <- predict(gAW_glm, type = "response")
pred_g0W <- 1 - pred_g1W

gAW <- vector(length = nrow(clean))
gAW[clean$Vaccination_A == 1] <- pred_g1W[clean$Vaccination_A == 1]
gAW[clean$Vaccination_A == 0] <- pred_g0W[clean$Vaccination_A == 0]

wt <- 1/gAW

# HT estimator
IPTW <- mean(wt*(clean$Vaccination_A == 1)*as.numeric(clean$Hospitalization_Y)) -
  mean(wt*(clean$Vaccination_A == 0)*as.numeric(clean$Hospitalization_Y))

cat(paste0("The IPTW estimator is ", IPTW, ".\n"))

# Hajek estimator
stab_IPTW <- mean(wt*(clean$Vaccination_A == 1)*as.numeric(clean$Hospitalization_Y))/mean(wt*(clean$Vaccination_A == 1)) -
  mean(wt*(clean$Vaccination_A == 0)*as.numeric(clean$Hospitalization_Y))/mean(wt*(clean$Vaccination_A == 0))

cat(paste0("The stabilized IPTW estimator is ", stab_IPTW, ".\n"))

#""""""""""""""""""""""""""""""#
# assess positivity assumption #
#""""""""""""""""""""""""""""""#



#""""""""""""""""""""""""""#
# non-parametric bootstrap #
#""""""""""""""""""""""""""#

set.seed(252)

IPTW_est <- function(data, n) {
  boot_samp <- sample_n(data, n, replace = TRUE)
  
  gAW_glm <- glm(Vaccination_A ~ . - Hospitalization_Y,
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
  if (!("B" %in% ls())) B <- 500
  if (!("n" %in% ls())) n <- 2500
  
  estimates <- t(replicate(B, IPTW_est(clean, n)))
  write.csv(estimates,
            paste0("data/iptw_np_bootstrap_est_",
                   type,
                   ".csv"), row.names = FALSE)
  cat(paste0("The non-parametric bootstrap estimate of the ",
            "IPTW estimator is ",
            mean(estimates[,1]),
            ".\n"))
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "standard deviation of the IPTW estimator is ",
             sd(estimates[,1]),
             ".\n"))
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "stabilized IPTW estimator is ",
             mean(estimates[,2]),
             ".\n"))
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "standard deviation of the stabilized IPTW estimator is ",
             sd(estimates[,2]),
             ".\n"))
}


