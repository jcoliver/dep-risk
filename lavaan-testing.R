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

double_fit <- sem(double_model, data = Data2)
summary(double_fit)

# Annotated output ($$) from summary:

# lavaan 0.6-8 ended normally after 16 iterations
# 
# Estimator                                         ML
# Optimization method                           NLMINB
# Number of model parameters                        11
# 
# Number of observations                           100
# 
# Model Test User Model:
#   
#   Test statistic                                 0.677
# Degrees of freedom                                 1
# P-value (Chi-square)                           0.410
# 
# Parameter Estimates:
#   
#   Standard errors                             Standard
# Information                                 Expected
# Information saturated (h1) model          Structured
# 
# $$ Regressions section includes coefficient estimate for individual paths
# $$ for example, the indirect effects via mediator 1, M1, are the a1 and b1 
# $$ paths. Each estimate is shown in the corresponding regression model 
# $$ section.
#
# $$ Regression on Y (first section) shows 
# $$    M1: effect (b path) of mediator 1 on Y, 0.760
# $$    M2: effect (b path) of mediator 2 on Y, 0.376
# $$    X: direct effect (c path) of independent variable on Y, -0.016
# $$    P1: effect of covariate on Y, 0.224
#
# $$ Mediator (second section) shows the a paths of each mediator
# $$    M1 ~ X: effect on mediator 1 (a path), 0.648
# $$    M2 ~ X: effect on mediator 2 (a path), 0.071
#
# $$ Defined parameters includes the indirect effects (ab paths), direct 
# $$ effect (c' path), and total effects (sum of c' and all ab paths)
# $$    ab1: indirect effect of X via mediator 1, 0.493
# $$         This is the product of a1 and b1 from above, 0.648 * 0.760 = 0.493
# $$    ab2: indirect effect of X via mediator 2, 0.027
# $$         This is the product of a2 and b2 from above, 0.071 * 0.376 = 0.027
# $$    total: total effect on Y of independent variable and mediators, 0.504; 
# $$         -0.016 + (0.648 * 0.760) + (0.071 * 0.376) = -0.016 + 0.493 + 0.027
# $$          note it does not include effect of covariate P1 on Y
#
# Regressions:
#                  Estimate  Std.Err  z-value  P(>|z|)
# Y ~                                                 
#   M1        (b1)    0.760    0.088    8.632    0.000
#   M2        (b2)    0.376    0.083    4.508    0.000
#   X          (c)   -0.016    0.108   -0.146    0.884
#   P1                0.224    0.101    2.209    0.027
# M1 ~                                                
#   X         (a1)    0.648    0.104    6.261    0.000
# M2 ~                                                
#   X         (a2)    0.071    0.111    0.645    0.519
# 
# Variances:
#                  Estimate  Std.Err  z-value  P(>|z|)
#  .Y                 0.828    0.117    7.071    0.000
#  .M1                1.070    0.151    7.071    0.000
#  .M2                1.228    0.169    7.071    0.000
# 
# Defined Parameters:
#                  Estimate  Std.Err  z-value  P(>|z|)
#  ab1               0.486    0.097    4.990    0.000
#  ab2               0.038    0.042    0.890    0.373
#  total             0.508    0.128    3.952    0.000
#  direct           -0.016    0.108   -0.146    0.884

