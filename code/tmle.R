###############################################################################
#                                                                             #
#                               implement TMLE                                #
#                                                                             #
###############################################################################

library(tidyverse)
library(magrittr)
library(ltmle)

clean <- read.csv("data/cleaned_tamu.csv")[,-1]
clean %<>% mutate(Hospitalization_Y = (Hospitalization_Y >= 1)*1,
                  Vaccination_A = (Vaccination_A >= 1)*1,
                  Sex_W5 = as.factor(Sex_W5),
                  College_W2 = as.factor(College_W2))

#""""""""""""""""#
# TMLE estimator #
#""""""""""""""""#

SL.library <- c("SL.glm", "SL.step", "SL.gam")

W <- clean %>% select(-c(Hospitalization_Y, Vaccination_A))
A <- clean$Vaccination_A
Y <- clean$Hospitalization_Y

df <- data.frame(W, A, Y)

tmle_out <- ltmle(data = df, Anodes = 'A', Ynodes = 'Y',
                  abar = list(1, 0), SL.library = SL.library)
