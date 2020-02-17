# 3_model_acronyms.R
# model acronyms over time
# August 2019
library(dplyr)
library(tidyr)
library(lme4) # for random intercepts
library(splines)

## Section 0: data ##
## get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R

## prepare the data
date.centre = as.numeric(as.Date('2000-01-01')) # centre date at this date 
titles = mutate(titles, datec = (as.numeric(date) - date.centre)/(365.25*10)) # scaled to per 10 years
nchar.min = 3 # minimum acronym size for this analysis
source('99_make_analysis_data.R') # creates `for.model` depending on the acronym size 
# add non-linear spline for trend
df = 4 # degrees of freedom
spline = ns(for.model$datec, df=df)
spline.df = as.data.frame(spline); names(spline.df) = paste('s', 1:df, sep='')
for.model = bind_cols(for.model, spline.df)
# store for predictions
boundary.knots = attr(spline, 'Boundary.knots')
knots = attr(spline, 'knots')

## Section 1: run the model ##

# stratify divide by journal? So each journal appears in each 100 at least once

# a) simple linear trend model ignoring journals but using all the data - takes a while
model.0 = glm(n.acronyms ~ datec, offset=log(n.words), data=for.model, family=poisson())
summary(model.0)

# b) add random intercept for journal
# use divide and recombine
n.strata = 200
divide = mutate(for.model, divide = sample(1:n.strata, size=nrow(for.model), replace=TRUE))
  
# turn on verbose options?
cov = list()
parms = AIC = times = NULL
now = Sys.time()
for (k in 1:n.strata){
  glmm = glmer(n.acronyms ~ s1 + s2 + s3 + s4 + type + (1|jabbrv), offset=log(n.words), data=filter(divide, divide==k), family=poisson())
  parms = bind_rows(parms, fixef(glmm))
  cov[[k]] = vcov(glmm)
  AIC = c(AIC, AIC(glmm))
  # times
  time.diff = Sys.time() - now
  times = c(times, time.diff)
  now = Sys.time() # update now
}
save(parms, cov, AIC, times, df, knots, boundary.knots, file = 'data/divide.glmm.results.RData')

# examine odd results
ints = r$jabbrv ; ints$journal = row.names(ints)
filter(ints, `(Intercept)`> 3)

# to do: run linear model per journal and then look for odd slopes and intercepts


