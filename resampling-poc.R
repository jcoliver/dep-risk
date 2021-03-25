# Resampling one individual from each family
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-24

rm(list = ls())

library(dplyr)
set.seed(20210324)

data_file <- "data/mock_data.csv"
dep_data <- read.csv(file = data_file)
# Goal create a data set of unrelated individuals. Incoming data have 
#   + singletons  no known close relatives in data set
#   + dyads       two from same family
#   + triads      three from same family
#   + quads       four from same family
# Want all the singletons, but only one from a family from other three 
# categories: 1 from each dyad, 1 from each triad, 1 from each quad.
# Family affiliation is indicated by FamilyID value

subset <- dep_data %>%
  group_by(FamilyID) %>%
  sample_n(size = 1)

if (length(unique(dep_data$FamilyID)) != nrow(subset)) {
  message("Warning: discrepancy between unique family ids and size of subset")
  message(paste0("\tUnique families: ", length(unique(dep_data$FamilyID))))
  message(paste0("\tSubset data size: ", nrow(subset)))
}

# Well that was easier than anticipated...