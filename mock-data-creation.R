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
# Look at education: slight negative relationship between Education and PRS
# Higher education score, lower PRS, so want to modify probabilities of 
# Education based on PRS score. PRS ranges -0.3 to 0.3, can make a vector of 
# probabilities (prob argument of sample need not sum to one) of appropriate 
# length (in Education case, length = 13)


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