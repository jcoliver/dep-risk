# Testing SEM via lavaan package for multiple mediators
# Jeff Oliver
# jcoliver@arizona.edu
# 2021-03-19

library(lavaan)

# Single mediation model from https://lavaan.ugent.be/tutorial/mediation.html
set.seed(1234)
X <- rnorm(100)
M <- 0.5*X + rnorm(100)
Y <- 0.7*M + rnorm(100)
Data <- data.frame(X = X, Y = Y, M = M)

single_model <- '
# direct effect
  Y ~ c*X
# mediator
  M ~ a*X
  Y ~ b*M
# indirect effect (a*b)
  ab := a*b
# total effect
  total := c + (a*b)
'

fit <- sem(single_model, data = Data)
summary(fit)
# See https://paolotoffanin.wordpress.com/2017/05/06/multiple-mediator-analysis-with-lavaan/comment-page-1/
# A good example of multiple mediation at 
# https://groups.google.com/g/lavaan/c/b7mtQjJAJts/m/Amj3mBgmAwAJ

# Try adding another mediator for a two-mediator, parallel model

M1 <- 0.5 * X + rnorm(100)
M2 <- 0.15 * X + rnorm(100)
# P1 is a covariate
P1 <- rnorm(100)
Y2 <- 0.7 * M1 + 0.4 * M2 + + 0.3 * P1 + rnorm(100)

Data2 <- data.frame(X = X, 
                    P1 = P1,
                    Y = Y2, 
                    M1 = M1,
                    M2 = M2)

double_model <- '
# The full model?
  Y ~ b1 * M1 + b2 * M2 + c * X + P1
# direct effect (implied by full model?)
#  Y ~ c * X + P1
# mediator
  # regress mediators on X, including P as covariate
  M1 ~ a1 * X + P1
  M2 ~ a2 * X + P1
  # regress Y on mediators
  # Y ~ b1 * M1
  # Y ~ b2 * M2
# indirect effect (a*b)
  ab1 := a1 * b1
  ab2 := a2 * b2
# total effect
  total := c + (a1 * b1) + (a2 * b2)
# direct effect
  direct := c
# covariance between mediators (skipping)
#  M1 ~~ M2
'

double_fit <- sem(double_model, data = Data2)
summary(double_fit)
