# Mock data creation
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-15

################################################################################
# TODO: For ordinal variables, could modify prob parameter to match obs. means
sample_size <- 10
set.seed(seed = 20210315)

#' Random generation for the normal distribution with lower and upper boundaries
#' 
#' @param n number of observations
#' @param mean mean of distribution from which to sample
#' @param sd standard deviation of distribution from which to sample
#' @param lower lower bound (exclusive) for returned values
#' @param upper upper bound (exclusive) for returned values
#' 
#' @examples 
#' /dontrun{
#' # Sample from normal distribution, replacing any values less than zero 
#' # or above 10
#' bounded_norm(n = 10, mean = 5, sd = 1, lower = 0, upper = 10)
#' }
bounded_norm <- function(n, mean, sd, lower = -Inf, upper = Inf) {
  x <- as.numeric(rep(x = NA, times = n))
  while(any(is.na(x)) | any(x < lower) | any(x > upper)) {
    invalid <- which(is.na(x) | x < lower | x > upper)
    x[invalid] <- rnorm(n = length(invalid),
                        mean = mean,
                        sd = sd)
  }
  return(x)
}

# Polygenic risk score
PRS <- bounded_norm(n = sample_size,
                    mean = -0.019,
                    sd = 0.0817,
                    lower = -0.3,
                    upper = 0.3)
  
# Early depression
W1_dep <- bounded_norm(n = sample_size,
                       mean = 11.38,
                       sd = 7.61,
                       lower = 0,
                       upper = 56)

# Education, ordinal, 13 levels
Education <- sample(x = ordered(x = 1:13), 
                    size = sample_size, 
                    replace = TRUE)

# Health Insurance
Health_ins <- bounded_norm(n = sample_size,
                           mean = 9.25,
                           sd = 4.599,
                           lower = 0,
                           upper = 12)

# Total assets
Assets <- sample(x = ordered(x = 1:9),
                 size = sample_size,
                 replace = TRUE)
# Debt
Debt <- sample(x = ordered(x = 1:3),
               size = sample_size,
               replace = TRUE)

# Family information
# 4533 are singletons (i.e. no known relatives in data set)
# 536 dyads (running total N = 4533 + 1072 = 5605)
# 27 triads (running total = 5605 + 81 = 5686)
# 1 set quads (running total 5686 + 4 = 5690)
# These are unique integers
singletons <- 1:4533
dyads <- max(singletons) + rep(x = 1:536, times = 2)
triads <- max(dyads) + rep(x = 1:27, times = 3)
quads <- max(triads) + rep(x = 1:1, times = 4)
FamilyID <- c(singletons, dyads, triads, quads)

mock_data <- data.frame(PRS = PRS,
                        W1_dep = W1_dep,
                        Education = Education,
                        Health_ins = Health_ins,
                        Assets = Assets,
                        Debt = Debt,
                        FamilyID = sample(x = FamilyID, size = sample_size))

write.csv(x = mock_data, file = "mock_data.csv", row.names = FALSE)