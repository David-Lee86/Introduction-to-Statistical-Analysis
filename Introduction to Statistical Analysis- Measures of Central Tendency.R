# Install packages if needed
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("DescTools")

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
library(DescTools)

# -------------------------------------------------------------------------------------------------
# Automatically set working directory to script location (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("Current working directory:", getwd(), "\n")

# -------------------------------------------------------------------------------------------------
# Create a vector of plant heights (in cm)
Height <- c(10, 12, 13, 15, 15, 16, 17, 17, 17, 18, 20, 20, 22, 24)
# The vector 'heights' now holds 14 numerical values

# -------------------------------------------------------------------------------------------------
# Mean
mean(Height) # Calculates the average (mean) of the heights

# Median
median(Height) # Finds the middle value (median) when the heights are sorted

# Mode
Mode(Height, na.rm = TRUE) # Finds the most frequently occurring value(s) (mode)

# Count all frequencies
table(Height)

# -------------------------------------------------------------------------------------------------
# Put into a data frame for plotting
df <- data.frame(Height)

#plot
p = ggplot(df, aes(x = Height)) +
  geom_histogram(binwidth = 3, fill = "lightblue", color = "black", boundary = 0) +
  labs(title = "Plant height distribution", x = "Height (cm)", y = "Frequency") +
  scale_x_continuous(breaks = seq(10, 25, by = 3)) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 9, face = "bold")
  )
p

#save
ggsave("Height_distribution_1.jpeg", height = 6, width = 8, units = 'cm', plot = p)


#########################################################################################
# Import data
df = read.csv("plant_measurments_v3.csv")

# -------------------------------------------------------------------------------------------------
# Mean
mean(df$Height)

# Median
median(df$Height)

# Mode
Mode(df$Height, na.rm = TRUE)

# -------------------------------------------------------------------------------------------------
# Plot histogram with mean, median, and mode lines
p = ggplot(df, aes(x = Height)) +
  geom_histogram(binwidth = 2, fill = "#a3effb", color = "black", alpha = 0.5, boundary = 0) +
  labs(title = "Plant height distribution", x = "Height (cm)", y = "Frequency") +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"))
p
ggsave("Height_distribution_2.jpeg", height = 12, width = 15, units = 'cm', plot = p)

# -------------------------------------------------------------------------------------------------
# plot a histogram with Measures of central tendency labels
p = ggplot(df, aes(x = Height)) +
  geom_histogram(binwidth = 2, fill = "#a3effb", color = "black", alpha = 0.5, boundary = 0) +
  labs(title = "Plant height distribution", x = "Height (cm)", y = "Frequency") +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"))
p

# Calculate statistics (rounded to 2 decimals)
mean_df   <- round(mean(df$Height, na.rm = TRUE), 2)
median_df <- round(median(df$Height, na.rm = TRUE), 2)
mode_df   <- round(Mode(df$Height, na.rm = TRUE), 2)  # from DescTools

# Add measures of central tendency labels
p = p +
  geom_vline(aes(xintercept = mean_df), color = "black", linetype = "dotted", size = 1) +
  geom_vline(aes(xintercept = median_df), color = "red", linetype = "solid", size = 1) +
  geom_vline(aes(xintercept = mode_df), color = "#aa00ff", linetype = "dashed", size = 1) +
  annotate("text", x = 30, y = 10, label = paste("Mean =", mean_df), color = "black", angle = 0, vjust = 0, hjust = 0, size = 3) +
  annotate("text", x = 30, y = 9, label = paste("Median =", median_df), color = "red", angle = 0, vjust = 0, hjust = 0, size = 3) +
  annotate("text", x = 30, y = 8, label = paste("Mode =", mode_df), color = "#aa00ff", angle = 0, vjust = 0, hjust = 0, size = 3)
p
ggsave("Height_distribution_3.jpeg", height = 12, width = 15, units = 'cm', plot = p)

# -------------------------------------------------------------------------------------------------
# plot a histogram with Measures of central tendency labels by Treatment
# If Height needs conversion
df$Height <- as.numeric(as.character(df$Height))

stats <- df %>%
  group_by(Treatment) %>%
  reframe(
    mean = mean(Height, na.rm = TRUE),
    median = median(Height, na.rm = TRUE),
    mode = Mode(round(Height), na.rm = TRUE)  # can return multiple modes
  )

# Merge stats back to original dataset for plotting
df <- df %>%
  left_join(stats, by = c("Treatment"))

p = ggplot(df, aes(x = Height)) +
  geom_histogram(binwidth = 2, fill = "lightblue", color = "black", boundary = 0) +
  geom_vline(aes(xintercept = mean, color = "Mean", linetype = "Mean"), size = 1) +
  geom_vline(aes(xintercept = median, color = "Median", linetype = "Median"), size = 1) + 
  geom_vline(aes(xintercept = mode, color = "Mode", linetype = "Mode"), size = 1) +
  facet_wrap(~ Treatment, ncol = 1) +
  labs(title = "Plant height distribution by Treatment", x = "Height (cm)", y = "Frequency",
       color = "Statistic", linetype = "Statistic") +
  scale_color_manual(values = c("Mean" = "black", "Median" = "red", "Mode" = "#aa00ff")) +
  scale_linetype_manual(values = c("Mean" = "dotted", "Median" = "solid", "Mode" = "dashed")) +
  theme(axis.text.y = element_text(size = 9, color = "black"),
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
        axis.title.y = element_text(size = 9, face = "bold"),
        axis.title.x = element_text(size = 9, face = "bold"),
        plot.title = element_text(size = 9, face = "bold"),
                                  legend.position = "bottom",
                                  legend.box = "vertical"
                                  )
p
ggsave("Height_distribution_by_Treatment.jpeg", height = 15, width = 12, units = 'cm', plot = p)


##########################################################################################################