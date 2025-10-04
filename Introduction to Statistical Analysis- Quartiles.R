# Install packages if needed
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("gridExtra")

# -------------------------------------------------------------------------------------------------
# Remove all stored objects & unload non-base packages
rm(list = ls())

detachAllPackages <- function() {
  base_pkgs <- c("stats", "graphics", "grDevices", "utils", "datasets", "methods", "base")
  loaded_pkgs <- search()[grep("package:", search())]
  pkgs_to_detach <- setdiff(loaded_pkgs, paste("package:", base_pkgs, sep = ""))
  
  if (length(pkgs_to_detach) > 0) {
    lapply(pkgs_to_detach, function(pkg) detach(pkg, character.only = TRUE))
  }
}

detachAllPackages()

# -------------------------------------------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(gridExtra)

# -------------------------------------------------------------------------------------------------
# Automatically set working directory to script location (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("Current working directory:", getwd(), "\n")

# -------------------------------------------------------------------------------------------------
## create data
set.seed(123)
# Generate main data (mostly within 1-1000)
data <- round(rnorm(990, mean = 500, sd = 75))  
data <- pmin(pmax(data, 100), 900)  # truncate to 1–1000

# Add some outliers
outliers <- c(1000, 1100, 50, 0)  # some high and low extremes
data <- c(data, outliers)

# Combine into a dataframe
df <- data.frame(values = data)

# -------------------------------------------------------------------------------------------------
# Compute quartiles
quartiles <- quantile(df$values, probs = c(0.25, 0.5, 0.75))
Q1 <- quartiles[1]
Q2 <- quartiles[2]  # median
Q3 <- quartiles[3]

# Histogram with density curve and Q1, Q2, Q3 lines
hist_plot <- ggplot(df, aes(x = values)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 10, 
                 fill = "#a3effb", color = "black", alpha = 0.7, boundary = 0) +
  geom_density(fill="#7f7f7f", alpha = .1) +
  geom_vline(aes(xintercept = Q1, color = "Q1", linetype = "Q1"), size = 1) +
  geom_vline(aes(xintercept = Q2, color = "Median (Q2)", linetype = "Median (Q2)"), size = 1) +
  geom_vline(aes(xintercept = Q3, color = "Q3", linetype = "Q3"), size = 1) +
  geom_rug(sides = "b", alpha = 0.3) + 
  labs(
    title = "Histogram with quartiles",
    x = "Plant height (cm)", 
    y = "Density",
    color = "Statistic", 
    linetype = "Statistic"
  ) +
  scale_color_manual(
    values = c("Q1" = "blue", "Median (Q2)" = "red", "Q3" = "blue")
  ) +
  scale_linetype_manual(
    values = c("Q1" = "dashed", "Median (Q2)" = "solid", "Q3" = "dashed")
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )
hist_plot

# Horizontal boxplot
box_plot <- ggplot(df, aes(x = values)) +
  geom_boxplot(fill = "#a3effb", color = "black", width = 0.3) +
  labs(title = "Boxplot of distribution", x = "Plant height (cm)")
box_plot

# Merge: Arrange histogram on top and horizontal boxplot below
p = grid.arrange(hist_plot, box_plot, ncol = 1, heights = c(2, 1))
p

ggsave("Quartiles_in_statistics_narrow_histogram.jpeg", height = 15, width = 20, units = 'cm', plot = p)


###################################################################################################
## create data
set.seed(123)
# Generate main data (mostly within 1-1000)
data <- round(rnorm(990, mean = 500, sd = 200))  
data <- pmin(pmax(data, 100), 900)  # truncate to 1–1000

# Add some outliers
outliers <- c(1000, 1100, 50, 0)  # some high and low extremes
data <- c(data, outliers)

# Combine into a dataframe
df <- data.frame(values = data)

# -------------------------------------------------------------------------------------------------
# Compute quartiles
quartiles <- quantile(df$values, probs = c(0.25, 0.5, 0.75))
Q1 <- quartiles[1]
Q2 <- quartiles[2]  # median
Q3 <- quartiles[3]

# Histogram with density curve and Q1, Q2, Q3 lines
hist_plot <- ggplot(df, aes(x = values)) +
  geom_histogram(aes(y = ..density..), 
                 binwidth = 10, 
                 fill = "#a3effb", color = "black", alpha = 0.7, boundary = 0) +
  geom_density(fill="#7f7f7f", alpha = .1) +
  geom_vline(aes(xintercept = Q1, color = "Q1", linetype = "Q1"), size = 1) +
  geom_vline(aes(xintercept = Q2, color = "Median (Q2)", linetype = "Median (Q2)"), size = 1) +
  geom_vline(aes(xintercept = Q3, color = "Q3", linetype = "Q3"), size = 1) +
  geom_rug(sides = "b", alpha = 0.3) + 
  labs(
    title = "Histogram with quartiles",
    x = "Plant height (cm)", 
    y = "Density",
    color = "Statistic", 
    linetype = "Statistic"
  ) +
  scale_color_manual(
    values = c("Q1" = "blue", "Median (Q2)" = "red", "Q3" = "blue")
  ) +
  scale_linetype_manual(
    values = c("Q1" = "dashed", "Median (Q2)" = "solid", "Q3" = "dashed")
  ) +
  theme(
    legend.position = "bottom",
    legend.box = "horizontal"
  )
hist_plot

# Horizontal boxplot
box_plot <- ggplot(df, aes(x = values)) +
  geom_boxplot(fill = "#a3effb", color = "black", width = 0.3) +
  labs(title = "Boxplot of distribution", x = "Plant height (cm)")
box_plot

# Merge: Arrange histogram on top and horizontal boxplot below
p = grid.arrange(hist_plot, box_plot, ncol = 1, heights = c(2, 1))
p

ggsave("Quartiles_in_statistics_wide_histogram.jpeg", height = 15, width = 20, units = 'cm', plot = p)
