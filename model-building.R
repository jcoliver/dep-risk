# Model building
# Jeff Oliver
# jcoliver@arizona.edu
# 2012-03-29

# Start by running four simple models, each including only one mediator
# Location of file with data
data_file <- "data/mock_data.csv"
# Number of data resamples to run
nreps <- 10

# Load required libraries
library(lavaan)    # mediation analysis
library(tidyverse) # data wrangling and visualization

# Read data in from file as appropriate
full_data <- readr::read_csv(file = data_file, 
                             col_types = cols()) # Suppresses messages

# We define four different models; each model is named for the single mediator 
# included in the model. All models have:
# independent variable DepPRSIN
# dependent variable W4_Dep
# covariate CESDW1
model_list <- list()
model_list['education'] <- '
# Full model, with mediators (b), direct effect (c), and covariate
  W4_Dep ~ b1 * H4ED2 + c * DepPRSIN + CESDW1
# Regress mediators on X
  H4ED2 ~ a1 * DepPRSIN
# Define additional parameters of interest to report
# indirect effects (a*b)
  ab1 := a1 * b1
# total effect
  total := c + (a1 * b1)
'
model_list['health_ins'] <- '
# Full model, with mediators (b), direct effect (c), and covariate
  W4_Dep ~ b1 * H4HS3 + c * DepPRSIN + CESDW1
# Regress mediators on X
  H4HS3 ~ a1 * DepPRSIN
# Define additional parameters of interest to report
# indirect effects (a*b)
  ab1 := a1 * b1
# total effect
  total := c + (a1 * b1)
'
model_list['assets'] <- '
# Full model, with mediators (b), direct effect (c), and covariate
  W4_Dep ~ b1 * H4EC7 + c * DepPRSIN + CESDW1
# Regress mediators on X
  H4EC7 ~ a1 * DepPRSIN
# Define additional parameters of interest to report
# indirect effects (a*b)
  ab1 := a1 * b1
# total effect
  total := c + (a1 * b1)
'
model_list['debt'] <- '
# Full model, with mediators (b), direct effect (c), and covariate
  W4_Dep ~ b1 * H4EC8 + c * DepPRSIN + CESDW1
# Regress mediators on X
  H4EC8 ~ a1 * DepPRSIN
# Define additional parameters of interest to report
# indirect effects (a*b)
  ab1 := a1 * b1
# total effect
  total := c + (a1 * b1)
'

# Now that models are defined, we can do resampling, running each model on the 
# subsampled data set. For each model (for now), we'll just record the AIC
# and use that as our starting model
aic_scores <- NULL
for (i in 1:nreps) {
  # Extract one individual per family
  subset_data <- full_data %>%
    group_by(FamilyID) %>%
    sample_n(size = 1)

  # Loop over all models in the list, fit them, and extract AIC score
  for (m in names(model_list)) {
    fitted_model <- lavaan::sem(model_list[[m]],
                                data = subset_data)
    
    model_score <- data.frame(aic_score = AIC(fitted_model),
                              model_name = m)
    # extract the AIC & store in our data frame
    if (is.null(aic_scores)) {
      aic_scores <- model_score
    } else {
      aic_scores <- bind_rows(aic_scores, model_score)
    }
  }
}

# Want to find model with minimum AIC score, do boxplot
aic_boxplot <- ggplot(data = aic_scores, mapping = aes(x = model_name,
                                                       y = aic_score)) +
  geom_boxplot()
aic_boxplot
