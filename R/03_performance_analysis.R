#' @Author: Luísa Viegas, Leonardo Brait
#' @description
#' @requirements R 4.3.1 Ubuntu 20.04


################################# Environment ##################################
library("tidyverse") #2.0.0

################################## Load Data ###################################
performances <- read_csv("data/predictions/scores.csv")
summary_performances <- performances %>%
  group_by(algorithm) %>%
  summarise(
    tss_mean = mean(tss_spec_sens),
    tss_sd = sd(tss_spec_sens),
    auc_mean = mean(auc),
    auc_sd = sd(auc))
write_csv(
    summary_performances,
   "data/predictions/performances_summary.csv")

# auc plot ---------------------------
ggplot(data = performances, aes(x = algorithm, y = auc)) +
theme_bw() +
theme(
  legend.position = "none",
  plot.title = element_text(face = "bold.italic", size = 20),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 15),
  axis.title = element_text(size = 17)
) +
labs(x = "Algorithms") +
ylim(c(-.01, 1.05)) +
geom_boxplot(size = .5, fill = "gray90", color = "black") +
geom_jitter(width = 0.2, size = 4, alpha = .5) +
geom_hline(yintercept =.8, color = "red")

ggsave(
  filename = "results/auc_performance.png",
  width = 10,
  height = 10,
  dpi = 300
)

# tss plot ---------------------------
ggplot(data = performances, aes(x = algorithm, y = tss_spec_sens)) +
theme_bw() +
theme(
  legend.position = "none",
  plot.title = element_text(face = "bold.italic", size = 20),
  axis.text.x = element_text(size = 12),
  axis.text.y = element_text(size = 15),
  axis.title = element_text(size = 17)
) +
labs(x = "Algorithms") +
ylim(c(-.01, 1.05)) +
geom_boxplot(size = .5, fill = "gray90", color = "black") +
geom_jitter(width = 0.2, size = 4, alpha = .5) +
geom_hline(yintercept =.5, color = "red")

ggsave(
  filename = "results/tss_performance.png",
  width = 10,
  height = 10,
  dpi = 300
)
