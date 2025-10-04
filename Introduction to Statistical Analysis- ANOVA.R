# Install packages if needed
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("car")
#install.packages("multcompView")

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
library(car)

# -------------------------------------------------------------------------------------------------
# Automatically set working directory to script location (RStudio only)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
cat("Current working directory:", getwd(), "\n")

# -------------------------------------------------------------------------------------------------
# Import data
df = read.csv("plant_measurments_v3.csv")

# -------------------------------------------------------------------------------------------------
# Check if the data is normally distributed using Shapiro-Wilk test
# By group
by(df$Height, df$Treatment, shapiro.test)
#Null hypothesis: Data is normally distributed.
#p > 0.05 → do not reject H₀ → data is approximately normal.

# -------------------------------------------------------------------------------------------------
# Check homogeneity of variances
leveneTest(Height ~ Treatment, data = df)
#If p > 0.05, variances are equal → you can proceed with ANOVA
#If p < 0.05, assumption is violated → consider Welch’s ANOVA instead

# -------------------------------------------------------------------------------------------------
# Classic one-way ANOVA
anova_model <- aov(Height ~ Treatment, data = df)
summary(anova_model)
#If p < 0.05, reject H₀ → at least one group mean differ

# -------------------------------------------------------------------------------------------------
# Tukey’s HSD post-hoc test
tukey <- TukeyHSD(anova_model)
print(tukey)

# -------------------------------------------------------------------------------------------------
library(multcompView)
# Extract Tukey results
tukey_results <- TukeyHSD(anova_model)$Treatment
letters <- multcompLetters(tukey_results[,"p adj"])$Letters

# Add letters to a boxplot
p = ggplot(df, aes(x = Treatment, y = Height, fill = Treatment)) +
  geom_boxplot(alpha = 0.6, outlier.shape = TRUE) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 0.7, shape = 16, color = "black") +
  geom_text(aes(label = letters[Treatment]), color="red",
            stat = "summary", fun = mean, vjust = 0) +
  labs(title = "ANOVA with Tukey HSD results",
       y = "Plant Height (cm)") +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 9, face = "bold")
  )
p
ggsave("Plant_height_ANOVA.jpeg", height = 7, width = 12, units = 'cm', plot = p)

