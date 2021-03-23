# Mock data creation
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-15

################################################################################
# TODO: For ordinal variables, could modify prob parameter to match obs. means
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
bounded_dep_pois <- function(n, i, old_range = c(-Inf, Inf), 
                             lower = 0, upper = Inf, positive = TRUE) {
  # Find min & max for rescaling purposes
  min_o <- min(old_range)
  max_o <- max(old_range)

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

# Look at education: slight negative relationship between Education and PRS
# Higher education score, lower PRS, so want to modify probabilities of 
# Education based on PRS score. PRS ranges -0.3 to 0.3, can make a vector of 
# probabilities (prob argument of sample need not sum to one) of appropriate 
# length (in Education case, length = 13)
# Could use a Poisson distribution, modifying mean based on PRS score. If 
# rpois is passed a vector of means (lambda), will only actually return n 
# number of samples, where n is first argument passed to rpois. So will want to 
# pass n the length of the lambda vector. e.g. rpois(n = length(l), lambda = l)
# Mean of Education is 5.67, so if we treat this as independent of PRS, we can 
# do
Education <- rpois(n = sample_size,
                   lambda = 5.67)

# But if we want to nudge Education by PRS, we know PRS is between -0.3 and 0.3
# AND that higher values of PRS result in lower values of Education. So we can 
# use a transformation of PRS and addition to the value passed to lambda
# lambda has to be > 0 (it can be zero, but it returns all 0)
# Ed.     1 ----- 5.67 ------ 13
# PRS    0.3 ----- x ------ -0.3 # Note reversal of scale

min_n <- 1
max_n <- 13

min_o <- -0.3
max_o <- 0.3

# Rescale PRS to the variable of interest range
# Use -1 * PRS because it is a negative relationship
lambda <- ( (max_n - min_n) / (max_o - min_o) ) * (((-1) * PRS) - max_o) + max_n
Ed <- rpois(n = length(lambda), lambda = lambda)
plot(x = PRS, y = Ed)
plot(x = PRS, y = lambda)

Education <- bounded_dep_pois(n = sample_size,
                              i = PRS, old_range = c(-0.3, 0.3),
                              lower = 1, upper = 13, positive = FALSE)
plot(x = PRS, y = Education)

# Education (Mediator)
Education <- sample(x = ordered(x = 1:13), 
                    size = sample_size, 
                    replace = TRUE)

# Health Insurance (Mediator)
Health_ins <- bounded_norm(n = sample_size,
                           mean = 9.25,
                           sd = 4.599,
                           lower = 0,
                           upper = 12)

# Total assets (Mediator)
Assets <- sample(x = ordered(x = 1:9),
                 size = sample_size,
                 replace = TRUE)
# Debt (Mediator)
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

# Need to simulate dependent variable
# Some correlation estimates between PRS and mediators:
# Education: -0.115
# Health_ins: -0.060
# Assets: -0.084
# Debt: 0.082
# W4_dep ~ PRS + W1_dep + Education + Health_ins + Assets + Debt


# W4_dep	W4_Dep
# 2.64	2.57	dependent	Continuous	0-15

mock_data <- data.frame(DepPRSIN = PRS,
                        CESDW1 = W1_dep,
                        H4ED2 = Education,
                        H4HS3 = Health_ins,
                        H4EC7 = Assets,
                        H4EC8 = Debt,
                        FamilyID = sample(x = FamilyID, size = sample_size))

write.csv(x = mock_data, file = "mock_data.csv", row.names = FALSE)