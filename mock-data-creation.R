# Mock data creation
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-15

################################################################################
sample_size <- 100
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

#' Sample from Poisson distribution, dependent on value x, resampling out of 
#' bounds values
#' 
#' @param n number of observations
#' @param i vector of independent variable dictating lambda
#' @param i_range vector of upper and lower bounds of distribution
#' from which i is drawn
#' @param lower lower bound (exclusive) for returned values
#' @param upper upper bound (exclusive) for returned values
#' @param positive logical indicating if there is a positive relationship 
#' between independent variable x and lambda (FALSE indicates negative 
#' relationship)
bounded_dep_pois <- function(n, i, i_range = c(-Inf, Inf), 
                             lower = 0, upper = Inf, positive = TRUE) {
  # Find min & max for rescaling purposes
  min_o <- min(i_range)
  max_o <- max(i_range)

  # How to modify lambda? If a positive relationship, want to use positive 
  # modifier; if negative, use negative modifier
  rel <- 1
  if (!positive) {
    rel <- -1
  }

  # Create vector of lambdas, which are simply i rescaled to the output range
  # defined by upper and lower
  lambda <- ( (upper - lower) / (max_o - min_o) ) * (rel * i - max_o) + upper

  # Return vector
  x <- as.numeric(rep(x = NA, times = n))
  while(any(is.na(x)) | any(x < lower) | any(x > upper)) {
    invalid <- which(is.na(x) | x < lower | x > upper)
    x[invalid] <- rpois(n = length(invalid),
                        lambda = lambda[invalid])
  }
  return(x)
}

# Polygenic risk score (Independent)
PRS <- bounded_norm(n = sample_size,
                    mean = -0.019,
                    sd = 0.0817,
                    lower = -0.3,
                    upper = 0.3)
  
# Early depression (Covariate)
W1_dep <- bounded_norm(n = sample_size,
                       mean = 11.38,
                       sd = 7.61,
                       lower = 0,
                       upper = 56)

# Education (Mediator)
Education <- bounded_dep_pois(n = sample_size,
                              i = PRS, i_range = c(-0.3, 0.3),
                              lower = 1, upper = 13, positive = FALSE)

# Health Insurance (Mediator) Cheating and treating as ordinal
Health_ins <- bounded_dep_pois(n = sample_size,
                               i = PRS, i_range = c(-0.3, 0.3),
                               lower = 0, upper = 12, positive = FALSE)

# Total assets (Mediator)
Assets <- bounded_dep_pois(n = sample_size,
                           i = PRS, i_range = c(-0.3, 0.3),
                           lower = 1, upper = 9, positive = FALSE)

# Debt (Mediator)
Debt <- bounded_dep_pois(n = sample_size,
                         i = PRS, i_range = c(-0.3, 0.3), 
                         lower = 1, upper = 3)

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

# Need to simulate dependent variable; generalizing with assumed positive 
# and negative relationships
W4_dep_raw <- PRS + W1_dep - Education - Health_ins - Assets + Debt
# Finally, rescale W4_dep to a range of 0-15
W4_dep <- ( (15 - 0) / (max(W4_dep_raw) - min(W4_dep_raw)) ) * 
  (W4_dep_raw - max(W4_dep_raw)) + 15

# Make a single data frame to write to file
mock_data <- data.frame(DepPRSIN = PRS,
                        CESDW1 = W1_dep,
                        H4ED2 = Education,
                        H4HS3 = Health_ins,
                        H4EC7 = Assets,
                        H4EC8 = Debt,
                        W4_Dep = W4_dep,
                        FamilyID = sample(x = FamilyID, size = sample_size))

write.csv(x = mock_data, file = "mock_data.csv", row.names = FALSE)