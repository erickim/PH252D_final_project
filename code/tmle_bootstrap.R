###############################################################################
#                                                                             #
#                               implement TMLE                                #
#                                                                             #
###############################################################################

suppressMessages(library(tidyverse))
suppressMessages(library(magrittr))
suppressMessages(library(ltmle))
suppressMessages(library(SuperLearner))
suppressMessages(library(readr))

# `Rscript code/g_computation.R bootstrap=TRUE B=1000 n=20000`
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  for (arg in args) {
    eval(parse(text = arg))
  }
}

clean <- read_csv("data/cleaned_tamu.csv")[,-1]

clean %<>% mutate(Vaccination_A = (Vaccination_A >= 1)*1,
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

# if `type` not specified, just do continuous `Y`
if (!("type") %in% ls()) type <- "continuous"
if (!(type %in% c("continuous", "binary"))) type <- "continuous"

if (type == "binary") {
  clean %<>% mutate(Hospitalization_Y = (Hospitalization_Y >= 1)*1)
}

cat(paste0("TMLE for ", type, " response will be performed.\n"))

#""""""""""""""""#
# TMLE estimator #
#""""""""""""""""#

SL.library <- c("SL.glm", "SL.glm.interaction",
                "SL.step", "SL.gam",
                "SL.rpartPrune", "SL.mean")

W <- clean %>% select(-c(Hospitalization_Y, Vaccination_A))
A <- clean$Vaccination_A
Y <- clean$Hospitalization_Y

df <- data.frame(W, A, Y)

## tmle, treating Y as a binary RV
if (type == "binary") {
  tmle_out <- ltmle(data = df,
                    Anodes = 'A',
                    Ynodes = 'Y',
                    abar = list(1, 0),
                    SL.library = SL.library)
} else if (type == "continuous") {
  tmle_out <- ltmle(data = df,
                    Anodes = 'A',
                    Ynodes = 'Y',
                    Yrange = c(0, 12),
                    abar = list(1, 0),
                    SL.library = SL.library)
}

tmle_out_summary <- summary(tmle_out)
cat(paste0("The TMLE estimate is ",
           tmle_out_summary$effect.measures$ATE$estimate,
           ".\n"))
cat(paste0("The TMLE estimate has standard error ",
           tmle_out_summary$effect.measures$ATE$std.dev,
           ".\n"))

#""""""""""""""""""""""""""#
# non-parametric bootstrap #
#""""""""""""""""""""""""""#

set.seed(252)

tmle_est <- function(data, n) {
  boot_samp <- sample_n(data, n, replace = TRUE)
  
  SL.library <- c("SL.glm", "SL.glm.interaction",
                  "SL.step", "SL.gam",
                  "SL.rpartPrune", "SL.mean")
  
  W <- boot_samp %>% select(-c(Hospitalization_Y, Vaccination_A))
  A <- boot_samp$Vaccination_A
  Y <- boot_samp$Hospitalization_Y
  
  df <- data.frame(W, A, Y)
  
  ## tmle, treating Y as a binary RV
  if (type == "binary") {
    tmle_out <- ltmle(data = df,
                      Anodes = 'A',
                      Ynodes = 'Y',
                      abar = list(1, 0),
                      SL.library = SL.library)
  } else if (type == "continuous") {
    tmle_out <- ltmle(data = df,
                      Anodes = 'A',
                      Ynodes = 'Y',
                      Yrange = c(0, 12),
                      abar = list(1, 0),
                      SL.library = SL.library)
  }
  
  tmle_out_summary <- summary(tmle_out)
  
  return(tmle_out_summary$effect.measures$ATE$estimate)
}

if (!("bootstrap") %in% ls()) bootstrap <- FALSE
if (bootstrap) {
  # set default if B and n not passed in command line
  if (!("B" %in% ls())) B <- 500
  if (!("n" %in% ls())) n <- 2500
  
  estimates <- t(replicate(B, tmle_est(clean, n)))
  write.csv(estimates,
            paste0("data/tmle_np_bootstrap_est_",
                   type,
                   ".csv"), row.names = FALSE)
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "TMLE estimator is ",
             mean(estimates),
             ".\n"))
  cat(paste0("The non-parametric bootstrap estimate of the ",
             "standard deviation of the TMLE estimator is ",
             sd(estimates),
             ".\n"))
}
