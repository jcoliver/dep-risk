# Mock data creation
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-15

################################################################################
# TODO: For continuous variables, need checks for out of bounds values; update 
#       code so these variables use the bounded_norm function (defined below)
# TODO: For ordinal variables, could modify prob parameter to match obs. means
sample_size <- 10
set.seed(seed = 20210315)

# Just using uniform distribution for PRS for now
PRS <- runif(n = sample_size, 
             min = -0.3, 
             max = 0.3)

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

# Early depression
W1_dep <- as.numeric(rep(x = NA, times = sample_size))
W1_dep <- bounded_norm(n = sample_size,
                       mean = 11.38,
                       sd = 7.61,
                       lower = 0,
                       upper = 56)

# Need to deal with out-of-bounds values (x < 0 or x > 56)
while(any(is.na(W1_dep)) | any(W1_dep < 0) | any(W1_dep > 56)) {
  out_of_bounds <- which(is.na(W1_dep) | W1_dep < 0 | W1_dep > 56)
  # message(paste0("Total out of bounds: ", length(out_of_bounds), "\n"))
  W1_dep[out_of_bounds] <- rnorm(n = length(out_of_bounds), 
                                    mean = 11.38,
                                    sd = 7.61) 
}

# Education, ordinal, 13 levels
Education <- sample(x = ordered(x = 1:13), 
                    size = sample_size, 
                    replace = TRUE)
# Health Insurance
# Need to deal with out-of-bounds values (x < 0 or x > 12)
Health_ins <- rnorm(n = sample_size,
                    mean = 9.25,
                    sd = 4.599)
# Total assets
Assets <- sample(x = ordered(1:9),
                 size = sample_size,
                 replace = TRUE)
# Debt
Debt <- sample(x = ordered(1:3),
               size = sample_size,
               replace = TRUE)
