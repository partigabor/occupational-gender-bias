# Load required packages
library(ordinal)   # for clmm
library(emmeans)   # for post-hoc comparisons

# Load dataset
dat <- read.csv("ratings.csv")

# Ensure Rating is an ordered factor (ordinal response required by clmm)
# If your ratings are numeric Likert like 1-5, set explicit levels to preserve ordering.
if (!is.factor(dat$Rating)) {
  vals <- sort(unique(na.omit(dat$Rating)))
  dat$Rating <- ordered(dat$Rating, levels = vals)
}

# Make sure categorical predictors are factors
dat$Language    <- factor(dat$Language)
dat$Gender      <- factor(dat$Gender)
dat$Participant <- factor(dat$Participant)
dat$Item        <- factor(dat$Item)

# Model: Rating ~ Language * Gender + (1|Participant) + (1|Item)
model <- clmm(Rating ~ Language * Gender + (1|Participant) + (1|Item),
               data = dat, Hess = TRUE)

# Inspect results
summary(model)

# ANOVA table for fixed effects
Anova(model, type = "III")

# Post-hoc pairwise comparisons (Tukey-adjusted)
emm <- emmeans(model, pairwise ~ Language * Gender,
               adjust = "tukey", mode = "linear.predictor")

summary(emm)

# Save the model and emmeans results in a txt file
sink("clmm_results.txt")
cat("Cumulative Link Mixed Model Results:\n")
print(summary(model))
# cat("\nANOVA Table:\n")
# print(Anova(model, type = "III"))
cat("\nPost-hoc Pairwise Comparisons:\n")
print(summary(emm))
sink()