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
# Example calibration data
concentration <- c(0, 10, 20, 30, 40, 50)   # Known standards
response <- c(0.02, 0.11, 0.21, 0.30, 0.41, 0.52)  # Measured response (absorbance)
df <- data.frame(concentration, response)

# -------------------------------------------------------------------------------------------------
# Plot
ggplot(df, aes(x = concentration, y = response)) +
  geom_point(size = 3, color = "black") +       # Data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear fit
  labs(title = "Calibration Curve",
       x = "Concentration (µM)",
       y = "Response (Absorbance)")

# -------------------------------------------------------------------------------------------------
# Fit Linear Regression
fit <- lm(response ~ concentration, data = df)
summary(fit)   # Shows slope, intercept, R²

# -------------------------------------------------------------------------------------------------
#The regression equation: response = intercept + slope * concentration
intercept <- round(coef(fit)[1], 3)
slope <- round(coef(fit)[2], 3)
equation <- paste0("y = ", slope, "x", " + ", intercept)
equation

# -------------------------------------------------------------------------------------------------
# Plot with regression line and equation
p = ggplot(df, aes(x = concentration, y = response)) +
  geom_point(size = 3, color = "black") +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  annotate("text", x = 35, y = 0.05, label = equation, color = "red", size = 5) +
  labs(title = "Calibration curve",
       x = "Concentration (µM)",
       y = "Response (absorbance)") +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"))
p
ggsave("Calibration_curve.jpeg", height = 7, width = 12, units = 'cm', plot = p)

# -------------------------------------------------------------------------------------------------
#Use regression equation to calculate unknown concentrations from measured responses:
unknown_response <- 0.25
estimated_concentration <- (unknown_response - coef(fit)[1]) / coef(fit)[2]
estimated_concentration



