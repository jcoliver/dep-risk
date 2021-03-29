# Do jackknifing to get coefficient estimates
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-29

# Script settings
# Location of file with data
data_file <- "data/mock_data.csv"
# Number of data resamples to run
nreps <- 100

# Load required libraries
library(lavaan)    # mediation analysis
library(tidyverse) # data wrangling and visualization

# Read data in from file as appropriate
full_data <- readr::read_csv(file = data_file, 
                             col_types = cols()) # Suppresses messages

# Define a simple model, where the only mediator is Education
simple_model <- '
# Full model, with mediators (b), direct effect (c), and covariate
  W4_Dep ~ b1 * H4ED2 + c * DepPRSIN + CESDW1
# Regress mediators on X
  H4ED2 ~ a1 * DepPRSIN
# Define additional parameters of interest to print
# indirect effects (a*b)
  ab1 := a1 * b1
# total effect
  total := c + (a1 * b1)
'

# Iterate over repeated samplings of the data; store parameter estimates for 
# each iteration
estimates <- NULL

for (i in 1:nreps) {
  # Extract one individual per family
  subset_data <- full_data %>%
    group_by(FamilyID) %>%
    sample_n(size = 1)

  # Fit the model on this subset of data
  simple_fit <- lavaan::sem(simple_model,
                            data = subset_data)
  # Extract coefficient estimates
  iter_coeff <- lavaan::coef(simple_fit, type = "user")
  
  # Convert to a data frame for easier wrangling
  iter_coeff <- data.frame(coeff = names(iter_coeff),
                           est = iter_coeff)
  rownames(iter_coeff) <- NULL

  # Pull out those values from the parameter estimates that have coefficient 
  # estimates
  iter_coeff <- iter_coeff %>%
    filter(coeff %in% c("a1", "b1", "ab1", "c", "total")) %>%
    mutate(iter = i)
  
  # Add those estimates to our data frame
  if (is.null(estimates)) { # First time through, instantiate the data frame
    estimates <- iter_coeff
  } else { # Not the first time through, add to existing estimates
    estimates <- bind_rows(estimates, iter_coeff)
  }
  
  # Reporter message to let us know about progress
  if (i %% 10 == 0) {
    message(paste0("Completed ", i, " of ", nreps, " iterations."))
  }
}

# Reorder levels of coefficient so they plot in order we want
estimates$coeff <- factor(x = estimates$coeff,
                          levels = c("ab1", "c", "total", "a1", "b1"))

# Plot histograms for each of the coefficients
coeff_hist <- ggplot(data = estimates, mapping = aes(x = est)) +
  geom_histogram(position = "identity", bins = 30) +
  # geom_vline(xintercept = 0, lty = 2) + # Include for vertical line at 0
  facet_wrap(. ~ coeff, scales = "free_x") +
  labs(y = "Count", x = "Coefficient Estimate") +
  theme_bw()
coeff_hist

# Calculate mean and standard error of coefficient estimates
coeff_stats <- estimates %>%
  group_by(coeff) %>%
  summarize(coeff_mean = mean(est),
            coeff_se = sd(est)/sqrt(n()))
coeff_stats
