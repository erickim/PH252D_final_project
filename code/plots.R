###############################################################################
#                                                                             #
#                               bootstrap plots                               #
#                                                                             #
###############################################################################

suppressMessages(library(tidyverse))
suppressMessages(library(reshape2))
suppressMessages(library(gridExtra))

system("mkdir plots")

gcomp_binary <- read.csv("data/g_comp_np_bootstrap_est_binary.csv")
iptw_binary <- read.csv("data/iptw_np_bootstrap_est_binary.csv")

names(gcomp_binary) <- "G_Comp"
names(iptw_binary) <- c("IPTW", "Stabilized_IPTW")

boot_dens_binary <- data.frame(gcomp_binary,
                               iptw_binary) %>%
  melt() %>%
  rename(Value = value,
         Method = variable) %>%
  ggplot(aes(x = Value, fill = Method, y = ..scaled..)) +
  geom_density(alpha = .5) +
  ylab("Density") +
  ggtitle("Bootstrap Distribution of G-Computation, IPTW, and Stabilized IPTW for Binary Response") +
  theme_classic()


ggsave(filename = "plots/boot_dens_binary.png",
       plot = boot_dens_binary,
       device = "png",
       width = 12,
       heigh = 6)



gcomp_continuous <- read.csv("data/g_comp_np_bootstrap_est_continuous.csv")
iptw_continuous <- read.csv("data/iptw_np_bootstrap_est_continuous.csv")

names(gcomp_continuous) <- "G_Comp"
names(iptw_continuous) <- c("IPTW", "Stabilized_IPTW")

boot_dens_continuous <- data.frame(gcomp_continuous,
                                   iptw_continuous) %>%
  melt() %>%
  rename(Value = value,
         Method = variable) %>%
  ggplot(aes(x = Value, fill = Method, y = ..scaled..)) +
  geom_density(alpha = .5) +
  ylab("Density") +
  ggtitle("Bootstrap Distribution of G-Computation, IPTW, and Stabilized IPTW for Continuous Response") +
  theme_classic()


ggsave(filename = "plots/boot_dens_continuous.png",
       plot = boot_dens_continuous,
       device = "png",
       width = 12,
       height = 6)

######################################
## assess the positivity assumption ##
######################################

propensity_scores <- read.csv("data/iptw_propensity_scores.csv")

propensity_dens <- propensity_scores %>%
    ggplot(aes(x = x, y = ..scaled..)) + 
    geom_density(alpha = .8, fill = "lightblue") +
    xlab("Value") +        
    ylab("Density") +
    ggtitle("Distribution of Propensity Scores") +
    theme_classic()

gAW <- read.csv("data/iptw_gAW.csv")

gAW_dens <- gAW %>%
    ggplot(aes(x = 1/x, y = ..scaled..)) + 
    geom_density(alpha = .8, fill = "lightblue") +
    xlab("Value") +    
    ylab("Density") +
    ggtitle("Distribution of IPTW Weights") +
    theme_classic()

stab_wt <- read.csv("data/stab_iptw_wt.csv")

stab_wt_dens <- stab_wt %>%
    ggplot(aes(x = x, y = ..scaled..)) + 
    geom_density(alpha = .8, fill = "lightblue") +
    xlab("Value") +    
    ylab("Density") +
    ggtitle("Distribution of Stabilized IPTW Weights") +
    theme_classic()

positivity_grid <- grid.arrange(propensity_dens, gAW_dens, stab_wt_dens, nrow = 3)

ggsave(filename = "plots/positivity.png",
       plot = positivity_grid,
       device = "png",
       width = 7,
       height = 9)

positivity_summary <- data.frame(propensity_scores, 1/gAW, stab_wt)
names(positivity_summary) <- c("Propensity_Scores", "IPTW_Weights",
                               "Stabilized_IPTW")

booktabs()
latex(
    tabular( Propensity_Scores + IPTW_Weights + Stabilized_IPTW ~
                 (min + mean + median + max),
            data = positivity_summary), booktabs = TRUE
)

latex(
    tabular( (Propensity_Scores + IPTW_Weights + Stabilized_IPTW)*
             (min + mean + median + max) ~ 1,
            data = positivity_summary), booktabs = TRUE
)
