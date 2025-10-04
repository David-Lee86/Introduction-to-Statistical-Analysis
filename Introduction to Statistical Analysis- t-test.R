# Install packages if needed
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("ggpubr")

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
# Import data
df = read.csv("plant_measurments_v3.csv")

# Subset data for Control and Fertilizer_B only
df_sub <- subset(df, Treatment %in% c("Control", "Fertilizer_B"))

# Quick summary
tapply(df_sub$Height, df_sub$Treatment, mean)
tapply(df_sub$Height, df_sub$Treatment, sd)

# Independent two-sample t-test
t.test(Height ~ Treatment, data = df_sub, var.equal = TRUE)  # classic Student's t-test
# OR
t.test(Height ~ Treatment, data = df_sub)  # Welch's t-test (default in R)

##plot
# Boxplot with jittered points
p = ggplot(df_sub, aes(x = Treatment, y = Height, fill = Treatment)) +
  geom_boxplot(alpha = 0.6, outlier.shape = TRUE) +
  geom_jitter(width = 0.2, alpha = 0.2, size = 1, shape = 16) +
  labs(
    title = "Plant heights: Control vs Fertilizer_B",
    x = "Treatment",
    y = "Height (cm)"
  ) +
  theme(
    axis.text.y = element_text(size = 9, color = "black"),
    axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, size = 9, color = "black"),
    axis.title.y = element_text(size = 9, face = "bold"),
    axis.title.x = element_text(size = 9, face = "bold"),
    plot.title = element_text(size = 9, face = "bold")
    )
p
ggsave("Plant_height_t-test.jpeg", height = 7, width = 10, units = 'cm', plot = p)


##plot with significance indicators
library(ggpubr)
p = p + stat_compare_means(
  method = "t.test", 
  label = "p.signif",  
  comparisons = list(c("Control", "Fertilizer_B")),
  y.position = max(df_sub$Height) + 2   # position label just above the tallest point
)
p
p = p + coord_cartesian(clip = "off") +
  theme(plot.margin = margin(t = 1, r = 0, b = 0, l = 0))  # extra top margin
ggsave("Plant_height_t-test.jpeg", height = 7, width = 10, units = 'cm', plot = p)

