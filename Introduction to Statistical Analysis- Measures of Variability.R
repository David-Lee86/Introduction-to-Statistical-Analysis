# Install packages if needed
#install.packages("ggplot2")
#install.packages("dplyr")

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
# Load necessary libraries
library(ggplot2)
library(dplyr)

# -------------------------------------------------------------------------------------------------
# Automatically set working directory to script location (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("Current working directory:", getwd(), "\n")

# -------------------------------------------------------------------------------------------------
##R Lesson: Range, Variance, and Standard Deviation
##Audience: Beginners learning basic descriptive statistics and R.

#Learning objectives- by the end of this lesson students will be able to:
#Define range, variance, and standard deviation.
#Compute these measures in R.
#Interpret the results and explain when each measure is useful.

#Quick definitions
#Range: the difference between the maximum and minimum values in a dataset. Gives a simple sense of spread but is sensitive to outliers.
#Variance: the average squared deviation from the mean. Useful in theory and inference but expressed in squared units.
#Standard deviation (SD): the square root of the variance, expressed in the same units as the original data, easier to interpret.

# -------------------------------------------------------------------------------------------------
# Example dataset
Height <- c(10, 12, 13, 15, 15, 16, 17, 17, 17, 18, 20, 20, 22, 24)

# Range
range_values <- range(Height) # returns min and max
range_values

range_diff <- diff(range_values) # max - min
range_diff

# Variance
var_sample <- var(Height) # sample variance (divides by n-1)
var_sample

# Standard deviation
sd_sample <- sd(Height) # sample standard deviation (divides by n-1)
sd_sample


# -------------------------plot a histogram ------------------------------------------------------------------------
df <- data.frame(Height)

# Calculate mean and SD
mean_h <- mean(df$Height)
sd_h   <- sd(df$Height)

# Plot histogram with mean and SD lines
p = ggplot(df, aes(x = Height)) +
  geom_histogram(binwidth = 3, fill = "#a3effb", color = "black", alpha = 0.5, boundary = 0) +
  geom_vline(aes(xintercept = mean_h, color = "Mean", linetype = "Mean"), size = 1) +
  geom_vline(aes(xintercept = mean_h - sd_h, color = "Mean - SD", linetype = "Mean - SD"), size = 1) +
  geom_vline(aes(xintercept = mean_h + sd_h, color = "Mean + SD", linetype = "Mean + SD"), size = 1) +
  labs(
    title = "Plant height distribution",
    x = "Height (cm)", y = "Frequency",
    color = "Statistic", linetype = "Statistic"
  ) +
  scale_color_manual(
    values = c("Mean" = "black", "Mean - SD" = "#7cd147", "Mean + SD" = "#7cd147")
  ) +
  scale_linetype_manual(
    values = c("Mean" = "dotted", "Mean - SD" = "solid", "Mean + SD" = "solid")
  ) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"),
    legend.position = "bottom",     # put legend below
    legend.box = "horizontal"       # arrange horizontally
  )
p
ggsave("Height_distribution_with_SD.jpeg", height = 12, width = 15, units = 'cm', plot = p)


