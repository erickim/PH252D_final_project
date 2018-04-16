#"""""""""""""""""""""""""""#
# load in data and packages #
#"""""""""""""""""""""""""""#

data <- read.csv("data/TAMU_FINAL_SUBSET.csv")

library(tidyverse)

#""""""""""#
# cleaning #
#""""""""""#

# A = Vaccination in 2015
#     [RX_THER_17_YR2015]
# Y = Hospital admission in 2016 
#     [ADMISSIONS]
# W1 = Income, above/below federal poverty line
#      [EST_INCOME, PCT_ABOVE_POVERTY_LINE, PCT_BELOW_POVERTY_LINE]
# W2 = Education, college educated or above/less than college
#      [EDUCATION_LEVEL, COLLEGE]
# W3 = Age
#      [AGE]
# W4 = Medical risk score
#      [RECON_MA_RISK_SCORE_NBR]
# W5 = Sex
#      [SEX_CD]

clean <- data %>%
  select(Vaccination_A = RX_THER_17_YR2015,
         Hospitalization_Y = ADMISSIONS,
         Income_W1 = Est_income,
         Above_Pvt_Line_W1 = Pct_above_poverty_line,
         Below_Pvt_Line_W1 = Pct_below_poverty_line,
         Education_W2 = Education_level,
         College_W2 = College,
         Age_W3 = AGE,
         Medical_Risk_W4 = RECON_MA_RISK_SCORE_NBR,
         Sex_W5 = SEX_CD)

write.csv(clean, file = "cleaned_tamu.csv")