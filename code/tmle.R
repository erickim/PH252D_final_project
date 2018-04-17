###############################################################################
#                                                                             #
#                               implement TMLE                                #
#                                                                             #
###############################################################################

library(tidyverse)
library(magrittr)
library(ltmle)

clean <- read.csv("data/cleaned_tamu.csv")[,-1]

Y_continuous <- clean$Hospitalization_Y

clean %<>% mutate(Hospitalization_Y = (Hospitalization_Y >= 1)*1,
                  Vaccination_A = (Vaccination_A >= 1)*1,
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

#""""""""""""""""#
# TMLE estimator #
#""""""""""""""""#

SL.library <- c("SL.glm", "SL.step", "SL.gam",
                "SL.rpartPrune", "SL.mean")

W <- clean %>% select(-c(Hospitalization_Y, Vaccination_A))
A <- clean$Vaccination_A
Y_binary <- clean$Hospitalization_Y

df_binary <- data.frame(W, A, Y = Y_binary)

## tmle, treating Y as a binary RV
tmle_out_binary <- ltmle(data = df_binary, Anodes = 'A', Ynodes = 'Y',
                         abar = list(1, 0), SL.library = SL.library)

df_continuous <- data.frame(W, A, Y = Y_continuous)

## treat Y as continuous
tmle_out_continuous <- ltmle(data = df_continuous, Anodes = 'A', Ynodes = 'Y',
                             Yrange = c(0, 12), abar = list(1, 0),
                             SL.library = SL.library)
