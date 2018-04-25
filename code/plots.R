###############################################################################
#                                                                             #
#                               bootstrap plots                               #
#                                                                             #
###############################################################################

suppressMessages(library(tidyverse))
suppressMessages(library(reshape2))

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
  ggplot(aes(x = Value, y = ..scaled.., fill = Method)) +
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
  ggplot(aes(x = Value, y = ..scaled.., fill = Method)) +
  geom_density(alpha = .5) +
  ylab("Density") +
  ggtitle("Bootstrap Distribution of G-Computation, IPTW, and Stabilized IPTW for Continuous Response") +
  theme_classic()


ggsave(filename = "plots/boot_dens_continuous.png",
       plot = boot_dens_continuous,
       device = "png",
       width = 12,
       height = 6)


