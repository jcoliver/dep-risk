# Apply lavaan to attempt mediation analysis
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-24

# Script settings
# Location of file with data
data_file <- "data/mock_data.csv"
# Number of data resamples to run
nreps <- 10

# Load required libraries
library(readr)    # read in files
library(dplyr)    # data wrangling, specifically subsampling data
library(lavaan)   # mediation analysis

# Read data in from file as appropriate
full_data <- readr::read_csv(file = data_file, 
                             col_types = cols()) # Suppresses messages

# Cycle through resamples, storing results in results list object
results <- list(nreps)

for (i in 1:nreps) {
  subset_data <- full_data %>%
    group_by(FamilyID) %>%
    sample_n(size = 1)
  
  mediation_model <- '
  # Full model, with mediators, direct effect, and covariate
  '
  double_model <- '
# Define the full model, which includes direct effect (X) and covariate (P1)
  Y ~ b1 * M1 + b2 * M2 + c * X + P1
# Regress mediators on X, including P as covariate
  M1 ~ a1 * X
  M2 ~ a2 * X
# Define additional parameters of interest to print
# indirect effects (a*b)
  ab1 := a1 * b1
  ab2 := a2 * b2
# direct effect (not defining, as it comes out in Regressions section)
#  direct := c
# total effect
  total := c + (a1 * b1) + (a2 * b2)
# If desired and warranted, can add covariance between variables
# M1 ~~ M2
'
  
}