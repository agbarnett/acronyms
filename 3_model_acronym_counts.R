# 3_model_acronym_counts.R
# model the number of acronyms per paper
# April 2020
library(broom)
library(dplyr)
library(lme4) # for random effects
library(merTools) # for prediction intervals
library(stringr)
source('99_make_analysis_data.R') # creates `for.model` depending on the acronym size 

## Section 0: data ##
## get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R

## prepare the data
date.centre = as.numeric(as.Date('2000-01-01')) # centre date at this date 
titles = mutate(titles, datec = (as.numeric(date) - date.centre)/(365.25*10)) # scaled date to per 10 years
abstracts = mutate(abstracts, datec = (as.numeric(date) - date.centre)/(365.25*10)) %>% # scaled date to per 10 years
  filter(n.words >= 50 & n.words <= 300) # exclude small frequences based on total word count for model below (see abstract.words.jpg)
nchar.min = 2 # minimum acronym size for this analysis
for.model = make.data(abstracts.data = abstracts, nchar.selected = 99) # 

## Section 1: run the model for titles ##

# to do
# use fractional polynomial in place of spline
# group n.words
# try abstracts and word count cut-offs - what are common lengths for abstracts
# use linear increase with additional 'bumps' at round numbers 150, 200, 250

# a) simple linear trend model ignoring journals but using all the data - takes a while
# log-transformed authors because variable has massive positive skew
split = filter(for.model, source=='Title') %>%
  sample_n(size=100000) %>%
  mutate(group.words = case_when(
    n.words <= 5 ~ 1,
    n.words > 5 & n.words < 10 ~ 2,
    n.words == 10 ~ 3,
    n.words > 10 & n.words < 20 ~ 4,
    n.words == 20 ~ 5,
    n.words > 20 & n.words <= 30 ~ 6,
    n.words > 30 ~ 7),
    group.words = factor(group.words))
#, levels=1:5, labels=c('[2,5]','(5,10]','(10,20]','(20,30]','30+')))
model.0 = glmer(n.acronyms ~ datec + log2(n.authors+1) + (1|jabbrv), offset=log(n.words), data=split, family=poisson())
summary(model.0)

ests = broom::tidy(model.0) %>%
  filter(str_detect(term, 'n.words'))
plot(ests$estimate)


## Section 2: run the model for abstracts ##
# random effect for journals; use split and recombine
# might have to stratify on the number of words to avoid a large number of estimates

abs = filter(for.model, source=='Abstract') # just abstracts 
n.splits = 100
s.size.words = 10 # number of words lengths to run per model
s.size = 10000 # sample size per model

# need to make sure all number estimates are covered
all_preds = NULL
for (k in 1:n.splits){
  this.random = sample(50:300, size=s.size.words, replace=FALSE) # randomly select word counts to simplify the estimates
  split = filter(abs, n.words %in% this.random) %>%
    sample_n(size=s.size, replace=FALSE)
  model = glmer(n.acronyms ~ factor(n.words) + (1|jabbrv), offset=log(n.words), data=split, family=poisson())
  # get predictions with confidence intervals, see here https://cran.r-project.org/web/packages/merTools/vignettes/Using_predictInterval.html
  new.data = data.frame(split=k, n.words = unique(split$n.words), jabbrv='')
  PI <- predictInterval(merMod = model, newdata = new.data,
                        which='fixed',
                        level = 0.95, n.sims = 1000,
                        stat = "mean", type="linear.prediction",
                        include.resid.var = TRUE)
  new.data = bind_cols(new.data, PI)
  # concatenate
  all_preds =bind_rows(all_preds, new.data)
}

## quick plot of predictions
pplot = ggplot(all_preds, aes(x=n.words, y=exp(fit)))+
               #, ymin=exp(lwr), ymax=exp(upr)))+
  xlab('Number of words in abstract')+
  geom_point()+
#  geom_errorbar(width=0)+
  geom_vline(lty=2, col='green', xintercept = c(150,200,250))+
  geom_smooth()
pplot

# To do, meta-analysis of estimates
# smooth may need very few degrees of freedom in order to show blips at word counts

