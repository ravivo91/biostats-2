library(tidyverse)

#### Generating data that are just wrong ####

set.seed(416) # set the seed to the Toronto Area code

# paste0 -- what's that?
subjects <- paste0("Subject_", 1:30)
conditions <- c("Patient", "Control")

# expand.grid is that new? What is that? 
data <- expand.grid(subject = subjects,
                    condition = conditions,
                    orientation = c("vertical", "horizontal", "oblique"),
                    spatial_frequency = seq(1, 60, by = 1))

# We've seen rnorm and rt before, what's this runif? What does our choice of simulated distribution say about our prior belief about our simulated data?
data$CSF <- runif(nrow(data), 0.1, 2) + rnorm(nrow(data))

# We're going to simulate some patients. Let's consider the scenario where patients have CSFs that differ from controls at the cardinal orientations, but not the obliques.
# Note that I'm using the tidyverse data passing short hand and the assingnment operator.
#
# The mutate() function is being used here -- note it is one of those functions in both plyr and dplyr functions -- this code will call the dplyr version.  
data <- data %>% mutate(CSF = ifelse(condition == "Patient" & orientation %in% c("vertical", "horizontal"), CSF * 1.2, CSF)) # Patients have higher thresholds at cardinal orientations

# Viewing the first few rows of the dataset is a nice check... are these data in wide or long format?
head(data)


#### Fitting a multiple regression linear model ####

# Fit a linear model?
#
# What about the repeated measures? It's out of scope as we'd need a whole new model to partial out the correlation within subject
lm_model <- lm(CSF ~ condition * orientation * spatial_frequency, data = data)

# Summarize the model
summary(lm_model)

# Visualize the model fit
ggplot(data, aes(x = spatial_frequency, y = CSF, color = orientation)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x, color = 'black') +
  facet_wrap(~orientation) +
  theme_minimal() +
  labs(title = "CSF by Spatial Frequency, Condition, and Orientation",
       x = "Spatial Frequency",
       y = "CSF")

#### Mixed-effect models (simplified) ####

# Load necessary package
library(lme4)
library(dplyr)

# Fit a linear mixed-effects model
# CSF is the outcome, condition, orientation, and spatial_frequency are fixed effects
# Random intercepts for subjects
lmm_model <- lmer(CSF ~ condition + orientation + spatial_frequency + (1|subject), data = data)

# Summary of the model
summary(lmm_model)

# Optional: Visualize the model - does this work? Nope.
library(ggplot2)
data$predicted_CSF <- predict(lmm_model)
ggplot(data, aes(x = spatial_frequency, y = CSF, color = orientation)) +
  geom_point(alpha = 0.5) +  # Original data points
  geom_line(aes(y = predicted_CSF), size = 1) +  # Fitted values
  facet_wrap(~orientation + subject, scales = "free") +
  theme_minimal() +
  labs(title = "Fitted LMM for CSF across Conditions, Orientations, and Spatial Frequencies",
       x = "Spatial Frequency", y = "CSF")


#### Psychometric function fitting ####

library(ggplot2)
library(dplyr)

# Simulate some data
set.seed(2024) # For reproducibility
n <- 7 # Number of observations
contrast_threshold <- runif(n, 0.01, 1) # Simulating contrast thresholds as predictors
odds_of_correct <- exp(1 + 1.06 * contrast_threshold) # Odds of getting a response correct
prob_correct <- odds_of_correct / (1 + odds_of_correct) # Converting odds to probabilities
#percent_correct <- rbinom(n, size = 1, prob = prob_correct) # Simulating binary outcomes

# Create a data frame
data <- data.frame(contrast_threshold, prob_correct)

# Fit a logistic regression model
logit_model <- glm(prob_correct ~ contrast_threshold, family = binomial(link = "logit"), data = data)

# Predict and create a data frame for plotting
new_data <- data.frame(contrast_threshold = seq(min(contrast_threshold), max(contrast_threshold), length.out = 100))
new_data$predicted_prob <- predict(logit_model, newdata = new_data, type = "response")

# Plot the data and the fitted psychometric curve
ggplot(data, aes(x = contrast_threshold, y = prob_correct)) +
  geom_point() +
  geom_line(data = new_data, aes(x = contrast_threshold, y = predicted_prob), color = "blue") +
  labs(title = "Psychometric Function: Percent Correct by Contrast Threshold",
       x = "Contrast Threshold",
       y = "Percent Correct (Predicted Probability)") +
  theme_minimal()

#### Another psychometric function example ####

set.seed(617) # Ensure reproducibility
contrast_levels <- seq(0.01, 0.1, length.out = 7)
odds_of_correct <- exp(1 + 25 * contrast_levels)# Assuming a relationship for simulation
prob_correct <- odds_of_correct / (1 + odds_of_correct)

responses <- (prob_correct - .15)
data <- data.frame(contrast_levels, responses)

# Fit a logistic regression model
fit <- glm(responses ~ contrast_levels, family = gaussian, data = data)

new_data$predicted_prob <- predict(fit, newdata = new_data)

# Assuming a similar approach to generate new responses based on fitted probabilities
predicted_data <- data.frame(contrast_levels = new_data$contrast_levels, predicted_prob = new_data$predicted_prob)

# Plot the predicted dataset
ggplot(predicted_data, aes(x = contrast_levels, y = predicted_prob)) +
  geom_point() +
  #geom_line(data = new_data, aes(x = contrast_levels, y = predicted_prob), color = "red") +
  labs(title = "Predicted Psychometric Function with Original Fitted Curve", x = "Contrast Levels", y = "Predicted Probability of Correct Response")