# Apply lavaan to attempt mediation analysis
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-24

# Script settings
# Location of file with data
data_file <- "data/mock_data.csv"

# Load required libraries
library(readr)    # read in files
library(dplyr)    # data wrangling, specifically subsampling data
library(lavaan)   # mediation analysis

# Read data in from file as appropriate
full_data <- readr::read_csv(file = data_file, 
                             col_types = cols()) # Suppresses messages

# Testing a single analysis
subset_data <- full_data %>%
  group_by(FamilyID) %>%
  sample_n(size = 1)

mediation_model <- '
# Full model, with mediators (b), direct effect (c), and covariate
  W4_Dep ~ b1 * H4ED2+ b2 * H4HS3 + b3 * H4EC7 + b4 * H4EC8 + c * DepPRSIN + CESDW1
# Regress mediators on X
  H4ED2 ~ a1 * DepPRSIN
  H4HS3 ~ a2 * DepPRSIN
  H4EC7 ~ a3 * DepPRSIN
  H4EC8 ~ a4 * DepPRSIN
# Define additional parameters of interest to print
# indirect effects (a*b)
  ab1 := a1 * b1
  ab2 := a2 * b2
  ab3 := a3 * b3
  ab4 := a4 * b4
# total effect
  total := c + (a1 * b1) + (a2 * b2) + (a3 * b3) + (a4 * b4)
'

# This model fails to find a solution
# Attempted fixes
# parameterization = "theta"
# estimator "ML" and variety of flavors
# bounds = TRUE
# optim.method = "BFGS"
model_fit <- lavaan::sem(mediation_model, 
                         data = subset_data)
summary(model_fit)

# A simpler model works
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
simple_fit <- lavaan::sem(simple_model,
                            data = subset_data)
estimates <- lavaan::parameterEstimates(simple_fit)

