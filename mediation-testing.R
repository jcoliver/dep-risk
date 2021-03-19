# Testing out mediation package
# Jeff Oliver
# joliver@arizona.edu
# 2021-03-10

# From mediate package vignette
# https://cran.r-project.org/web/packages/mediation/vignettes/mediation.pdf

# For interpreting results of summary, see
# https://towardsdatascience.com/doing-and-reporting-your-first-mediation-analysis-in-r-2fe423b92171

# But see Reinhart et al. 2019 (https://doi.org/10.1186/s12874-018-0654-z) 
# about using mediation with binary outcome variables...?
library(mediation)

data("framing", package = "mediation")

# Asking about factors influencing a person's likelihood of writing a letter to 
# their congressperson

# First, regress the mediator on the independent variable
med_fit <- lm(emo ~ treat, data = framing)

# Second, regress the dependent variable on the independent variable and 
# mediator; we'll call this the total model
# First, though, see if the probit model is better than the logit model
# A logit (logistic regression) model
total_log <- glm(cong_mesg ~ emo + treat, 
               data = framing, 
               family = binomial(link = "logit"))
# A probit model
total_pro <- glm(cong_mesg ~ emo + treat, 
                 data = framing, 
                 family = binomial(link = "probit"))

anova(total_log, total_pro) # use logistic
total_fit <- total_log

# Third, regress the dependent on the independent variable, using the same 
# family of model as in the total model (logistic, in this case)
ind_fit <- glm(cong_mesg ~ treat,
               data = framing,
               family = binomial(link = "logit"))

# Use mediate function to assess indirect effects
med_out <- mediate(med_fit,
                   total_fit,
                   treat = "treat",
                   mediator = "emo",
                   robustSE = TRUE,
                   sims = 100)
summary(ind_fit)
summary(med_fit)
summary(total_fit)
summary(med_out)

# ACME should be product of: 
#    effect of independent variable on the mediator 1.4796  (from med_fit)
#    effect of mediator variable on the dependent variable 0.30473 (from total_fit)
#  => 0.4509
# But med_out separates ACME into control and treated and both are considerably
# different (0.0868 and 0.0880). Most likely due to differences in models?

# ADE should be the direct effect of the independent variable when controlling 
# for the mediator. That is, the coefficient estimate from our total model
#    => 0.06648 (from total_fit)
# Also quite different in med_out for both control & treated (0.0254 and 0.0266)

# Interpretation of results from mediation package seem challenging. See
# https://stats.stackexchange.com/questions/452659/relation-between-causal-mediation-model-and-regular-glm-in-r
# But then see
# https://stats.stackexchange.com/questions/104692/comprehending-output-from-mediation-analysis-in-r

# Total effect is sum of the (average) ACME and ADE: 0.0874 + 0.0260 = 0.1134
# The total effect of 0.1134 is an increase in the probability of sending a
# letter by 0.1134 (or 11.34%).
# Note the proportion of the causal effect mediated by the mediator (emo) rather
# than the direct effect of the independent variable (treat) would normally be 
# calculated by dividing the ACME by the total effect
# 0.0874 / 0.1134 = 0.7707
# But the reported proportion mediated is a bit different (0.7000); likely 
# reflecting different underlying calculation. See second stack exchange result
# above
# In summary, we see a significant indirect effect (ACHE), but the direct 
# effect of treatment on probability of sending a letter is not significant


# The medflex package is an alternative, but looks to be doing something 
# different

# The mma package will accommodate more than two mediators (mediation package 
# appears to top out at 2), and may accommodate random effects (see p. 22 of 
# package manual https://cran.r-project.org/web/packages/mma/mma.pdf)
# https://cran.r-project.org/web/packages/mma/vignettes/MMAvignette.html
