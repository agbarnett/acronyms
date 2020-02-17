# 3_time_to_reuse.R
# examine time between first and second use of acronym
# Oct 2019
library(dplyr)
library(survminer)
library(survival)
library(broom)
library(viridis)
library(tidyr)

# get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R
# prepare the data
nchar.min = 2 # minimum acronym size for this analysis
acronyms.in.titles = filter(acronyms.in.titles, nchar >= nchar.min)
# Merge titles (dates) with acronyms
for.model = left_join(titles, acronyms.in.titles, by='pmid') %>%
  filter(!is.na(acronyms), !is.na(jabbrv)) %>% # only where there's an acronym and exclude missing journal name
  distinct(pmid, acronyms, .keep_all=TRUE) %>% # remove duplicated acronyms in the same paper
  arrange(jabbrv, acronyms, date) %>%
  group_by(jabbrv, acronyms) %>%
  slice(1:2) %>% # top two acronyms per journal
  mutate(row=1:n()) %>% # for tranforming to wide
  ungroup()
# wide to long
wide = select(for.model, jabbrv, acronyms, row, date) %>% 
      spread(row, date)
# last publication date at journal for censoring
last.date = group_by(titles, jabbrv) %>%
  filter(!is.na(jabbrv)) %>% # remove missing journals
  summarise(censor = max(date))
# add last date
wide = left_join(wide, last.date, by='jabbrv') %>%
  dplyr::filter(`1` < censor) %>% # must be beyond censor date (excludes special issues, conferences)
  mutate(diff = `2` - `1`,
         reused = ifelse(is.na(`2`), 0, 1), # binary re-used
         diff = ifelse(is.na(`2`), censor - `1`, diff),  # time if censored
         years = diff/365.25, # scale to years
         nchar = nchar(acronyms),
         nchar = ifelse(nchar>5, 5, nchar)) # limit to 5

# kaplan-meier
fit <- survfit(Surv(years, event = reused==1) ~ nchar, data = wide)
g = ggsurvplot(fit, 
               fun = 'event', # reverse curve to plot events
           palette =viridis(4),# custom color palettes
           conf.int = FALSE,          # Add confidence interval
           xlab = "Time in years",
           ylab = 'Cumulative re-use',
           ylim = c(0, 0.42), 
           legend.labs = c('2','3','4','5+'), # change legend labels
           xlim = c(0, 10), # limit to 10 years
           break.time.by =2, 
           censor.size = 0, # supress censoring ticks
           ncensor.plot = FALSE, 
           pval = FALSE,              # Add p-value
           risk.table = FALSE,        # Add risk table
           data = wide) 
jpeg('figures/survival.nchar.jpg', width=6, height=4, units='in', res=500, quality=100)
print(g)
dev.off()

## examine re-use by first letter
# data management
wide = mutate(wide, 
              years = years + 0.001, # temporary, for parametric model
              letter = stringr::str_sub(acronyms, start=1, end=1),
              letter = ifelse(stringr::str_detect(pattern='[A-Z]', string=letter), letter, 'Other'), # convert anything bar upper case letter
              letter = factor(letter),
              letter = relevel(letter, ref = 'A'))
# Weibull survival models
fit.weib.noletter <- survreg(Surv(years, event = reused==1) ~ factor(nchar), data = wide, dist='weibull')
fit.weib <- survreg(Surv(years, event = reused==1) ~ factor(nchar) + letter, data = wide, dist='weibull')
parms = broom::tidy(fit.weib) %>%
  filter(stringr::str_detect(term, pattern='letter')) %>% # just letters
  arrange(estimate) %>%
  mutate(letter = stringr::str_remove(term, 'letter'),
         estimate = exp(estimate), # turn to HRs
         conf.low = exp(conf.low), # turn to HRs
         conf.high = exp(conf.high), # turn to HRs
         x = 1:n())
pplot = ggplot(parms, aes(x=x, y=estimate, ymin=conf.low, ymax=conf.high))+
  geom_point()+
  geom_errorbar(width=0)+
  theme_bw()+
  scale_x_continuous(breaks=1:nrow(parms), labels=parms$letter, limits=c(0,NA))+
  theme(panel.grid.minor = element_blank())+
  coord_flip()+
  xlab('')+
  ylab('Hazard ratio')+
  geom_hline(col='dark red', yintercept=1)+
  geom_text(x=0, y=0.99, label='Slower re-use', hjust=1)+ 
  geom_text(x=0, y=1.01, label='Faster re-use', hjust=0)
pplot
jpeg('figures/survival.letter.jpg', width=6, height=4, units='in', res=500, quality=100)
print(pplot)
dev.off()

# check with boxplot
ggplot(data=wide, aes(x=letter, y=years, col=factor(reused)))+
  geom_boxplot()+
  scale_y_log10()+
  theme_bw()
