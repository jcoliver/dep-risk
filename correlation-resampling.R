# Jackknifing for pairwise correlation estimates
# Jeff Oliver
# jcoliver@arizona.edu
# 2022-04-01

# Script settings
# Location of file with data
data_file <- "data/mock_data.csv"
# Number of data resamples to run
# TODO: Increase to 1000 when testing is complete
nreps <- 100

# Load required libraries
library(tidyverse) # data wrangling and visualization

# Read data in from file as appropriate
full_data <- readr::read_csv(file = data_file, 
                             col_types = cols()) # Suppresses messages

# A largely empty data frame, with columns for each of the risk variables; 
# these columns will be filled in with Pearson's correlation coefficient on 
# each iteration
# TODO: Update with names of columns in real data set
corr_results <- data.frame(iteration = 1:nreps,
                           CESDW1 = NA,
                           H4ED2 = NA,
                           H4HS3 = NA,
                           H4EC7 = NA,
                           H4EC8 = NA,
                           W4_Dep = NA)
# Iterate
for (i in 1:nreps) {
  SIB_DIF3 <- full_data %>%
    group_by(FamilyID) %>% # TODO: Update with name of column in real data set
    sample_n(size = 1)

  # Do each correlation test
  # TODO: Update with names of columns in real data set
  CESDW1_corr <- cor.test(SIB_DIF3$DepPRSIN, SIB_DIF3$CESDW1)
  H4ED2_corr <- cor.test(SIB_DIF3$DepPRSIN, SIB_DIF3$H4ED2)
  H4HS3_corr <- cor.test(SIB_DIF3$DepPRSIN, SIB_DIF3$H4HS3)
  H4EC7_corr <- cor.test(SIB_DIF3$DepPRSIN, SIB_DIF3$H4EC7)
  H4EC8_corr <- cor.test(SIB_DIF3$DepPRSIN, SIB_DIF3$H4EC8)
  W4_Dep_corr <- cor.test(SIB_DIF3$DepPRSIN, SIB_DIF3$W4_Dep)
  
  # Store each correlation coefficient in the appropriate row/column of results
  # data frame
  # TODO: Update with names of columns in real data set
  corr_results$CESDW1[i] <- CESDW1_corr$estimate
  corr_results$H4ED2[i] <- H4ED2_corr$estimate
  corr_results$H4HS3[i] <-H4HS3_corr$estimate
  corr_results$H4EC7[i] <- H4EC7_corr$estimate
  corr_results$H4EC8[i] <- H4EC8_corr$estimate
  corr_results$W4_Dep[i] <- W4_Dep_corr$estimate
 
  # Reporter message to let us know about progress
  if (i %% 10 == 0) {
    message(paste0("Completed ", i, " of ", nreps, " iterations."))
  }
}

# Convert to long-format for easier summarizing and plotting
results_long <- corr_results %>%
  tidyr::pivot_longer(cols = -iteration,
                      names_to = "predictor",
                      values_to = "correlation")

# Calculate means and 95% C.I. for each correlation
results_summary <- results_long %>%
  group_by(predictor) %>%
  summarize(mean_corr = mean(correlation),
            lower_CI = quantile(x = correlation, probs = 0.05, names = FALSE),
            upper_CI = quantile(x = correlation, probs = 0.95, names = FALSE))
# Print results
results_summary

# Create histograms for each predictor
corr_hist <- ggplot(data = results_long,
                    mapping = aes(x = correlation)) +
  geom_histogram(bins = 30) +
  # geom_vline(xintercept = 0, color = "red") +
  facet_wrap(~ predictor, scales = "free") +
  theme_bw()
# Print the plot
corr_hist
