#######################
## run full analysis ##
#######################

library(ltmle)
library(tables)
library(tidyverse)
library(magrittr)
library(readr)

B <- 500
n <- 2500

clean <- read_csv("data/cleaned_tamu.csv")[,-1]
clean %<>% mutate(Vaccination_A = as.factor((Vaccination_A >= 1)*1),
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

clean_binary <- clean %>%
    mutate(Hospitalization_Y = as.factor((Hospitalization_Y >= 1)*1))

##"""""""""""""""""""""""""#
## g-computation estimator #
##"""""""""""""""""""""""""#

Psi_est <- function(data, n, family = "gaussian") {
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

######################
## continuous gcomp ##
######################

set.seed(252)

estimates <- replicate(B, Psi_est(clean, n))

gcomp_est <- mean(estimates)
gcomp_sd <- sd(estimates)

##################
## binary gcomp ##
##################

set.seed(252)

estimates <- replicate(B, Psi_est(clean_binary, n, "binomial"))

gcomp_est_binary <- mean(estimates)
gcomp_sd_binary <- sd(estimates)

##""""""""""""""""#
## IPTW estimator #
##""""""""""""""""#

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

#####################
## continuous IPTW ##
#####################

set.seed(252)

estimates <- t(replicate(B, IPTW_est(clean, n)))

IPTW_estim <- mean(estimates[,1])
IPTW_sd <-  sd(estimates[,1])

stab_IPTW_est <- mean(estimates[,2])
stab_IPTW_sd <-  sd(estimates[,2])

#################
## binary IPTW ##
#################

set.seed(252)

estimates <- t(replicate(B, IPTW_est(clean_binary, n)))

IPTW_est_binary <- mean(estimates[,1])
IPTW_sd_binary <-  sd(estimates[,1])

stab_IPTW_est_binary <- mean(estimates[,2])
stab_IPTW_sd_binary <-  sd(estimates[,2])

##""""""""""""""""#
## TMLE estimator #
##""""""""""""""""#

SL.library <- c("SL.glm", "SL.glm.interaction",
                "SL.step", "SL.gam",
                "SL.rpartPrune", "SL.mean")

W <- clean %>% select(-c(Hospitalization_Y, Vaccination_A))
A <- as.numeric(clean$Vaccination_A) - 1

#####################
## continuous TMLE ##
#####################

set.seed(252)

Y_continuous <- clean$Hospitalization_Y
df_continuous <- data.frame(W, A, Y = Y_continuous)

tmle_continuous <- ltmle(data = df_continuous, Anodes = 'A', Ynodes = 'Y',
                         Yrange = c(0, 12), abar = list(1, 0),
                         SL.library = SL.library)

tmle_est <- summary(tmle_continuous)$effect.measures$ATE$estimate
tmle_sd <- summary(tmle_continuous)$effect.measures$ATE$std.dev
tmle_pval <- summary(tmle_continuous)$effect.measures$ATE$pvalue

#################
## binary TMLE ##
#################

set.seed(252)

Y_binary <- as.numeric(clean_binary$Hospitalization_Y) - 1
df_binary <- data.frame(W, A, Y = Y_binary)

tmle_binary <- ltmle(data = df_binary, Anodes = 'A', Ynodes = 'Y',
                     abar = list(1, 0), SL.library = SL.library)

tmle_est_binary <- summary(tmle_binary)$effect.measures$ATE$estimate
tmle_sd_binary <- summary(tmle_binary)$effect.measures$ATE$std.dev
tmle_pval_binary <- summary(tmle_binary)$effect.measures$ATE$pvalue

##""""""""""""""""""""""""""""""""""""""#
## put everything together in one table #
##""""""""""""""""""""""""""""""""""""""#

## continuous first
ests <- c(gcomp_est, IPTW_estim, stab_IPTW_est, tmle_est,
          gcomp_est_binary, IPTW_est_binary, stab_IPTW_est_binary, tmle_est_binary)

sds <- c(gcomp_sd, IPTW_sd, stab_IPTW_sd, tmle_sd,
         gcomp_sd_binary, IPTW_sd_binary, stab_IPTW_sd_binary, tmle_sd_binary)

types <- rep
(c("Continuous", "Binary"), each = 4)

estimands <- rep(c("G-Computation", "IPTW", "Stabilized IPTW", "TMLE"), times = 2)

results <- data.frame("Estimate" = ests, "SD" = sds, types, estimands)

booktabs()
latex(
    tabular( Format(digits = 10)*1 + Percent() +
             (Est_Net_Worth_W1 + Age_W3 +
              (Medical_Risk = exp(Medical_Risk_W4)-.1) +
              Hospitalization_Y)*(mean + sd) ~
                 (Vaccinated = factor(Vaccination_A)) + (Sex_W5) + College_W2 + 1,
            data = clean), booktabs = TRUE
)

latex(
    tabular( Percent() + Format(digits = 10)*1 +
             (Est_Net_Worth_W1 + Age_W3 +
              (Medical_Risk = exp(Medical_Risk_W4)-.1) +
              Hospitalization_Y)*(mean + sd) ~
                 (Vaccinated = factor(Vaccination_A))*
                 factor(Sex_W5)*College_W2,
            data = clean), booktabs = TRUE
)
