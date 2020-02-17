# 3_feature_selection_journals.R
# look at characteristics of results for individuals journals
# October 2019
library(dplyr)
library(tidyr)
library(ggplot2)
library(feasts)
library(tsibble)
library(viridis)
source('99_make_analysis_data.R') # function that creates `for.model` depending on the acronym size

# Section 0
## get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R

## prepare the data
nchar.min = 3 # minimum acronym size for this analysis
for.model = make.data(indata=acronyms.in.titles, nchar.min=nchar.min)
# two necessary edits
for.model = mutate(for.model, p = n.acronyms / n.words) %>% # calculate proportion of acronyms
  filter(!is.na(jabbrv)) # remove small number with missing journal

## Section 1 - simple features per journal
features = group_by(for.model, jabbrv) %>%
  mutate(daten = as.numeric(date)) %>%
  summarise(n=n(), min=min(p), max=max(p), mean=mean(p), sd=sd(p), median=median(p), iqr=IQR(p), # stats on p
            date = median(daten), earliest = min(daten), latest=max(daten)) %>% # date stats
  mutate(sd = ifelse(is.na(sd), 0, sd), # replace missing sd with zero 
         range = latest - earliest) %>%  # date range
  ungroup() %>%
  mutate( # transform back to date
    date = as.Date(date, origin='1970-01-01'),
    earliest = as.Date(earliest, origin='1970-01-01'),
    latest = as.Date(latest, origin='1970-01-01')
  ) %>%
  filter(n >= 50) # just journals with at least 50 entries, lose around 20,000

## plots
# a) mean against sd
splot1 = ggplot(data=features, aes(x=mean, y=sd))+
  geom_point(pch=1)+
  theme_bw()+
  xlab('Mean')+
  ylab('Standard deviation')
splot1
filter(features, sd<0.05, mean>0.05) # high mean but low SD
filter(features, sd>0.05, sd<0.1, mean>0.2) # looking at specific group
# b) median against IQR
splot2 = ggplot(data=features, aes(x=median, y=iqr))+
  geom_point(pch=1)+
  theme_bw()+
  xlab('Median')+
  ylab('Inter-quartile range')
splot2
filter(features, median>0.6) # very high means these are errors
# c) mean against date
splot3 = ggplot(data=filter(features, earliest> as.Date('1950-01-01')), 
                aes(x=earliest, y=mean))+
  geom_point(pch=1)+
  geom_smooth()+
  theme_bw()+
  xlab('First publication date')+
  ylab('Mean')
splot3
jpeg('figures/mean.vs.earliest.jpg', width=5.5, height=4, units='in', res=500, quality=100)
print(splot3)
dev.off()

# Not a data error any more
jpeg('figures/highlight.data.error2.jpg', width=5, height=4, units='in', res=500, quality=100)
print(splot2)
dev.off()
postscript('figures/highlight.data.error2.eps', width=5, height=4)
print(splot2)
dev.off()

## journals with no acronyms
none = filter(features, max==0, n>400) %>%
  arrange(earliest) %>%
  select(-date) %>%
  mutate(i=1:n()) %>%
  gather(`earliest`,`latest`, key='what', value='date')
none.plot = ggplot(data=none, aes(x=i, y=date, group=factor(i))) +
  geom_line(size=1.2)+
  coord_flip()+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(breaks= 1:nrow(none), labels=none$jabbrv)+
  ylab('Time')+
  xlab('')
jpeg('figures/no.acronyms.jpg', width=5.5, height=4, units='in', res=500, quality=100)
print(none.plot)
dev.off()


## Section 2 - use feasts by Rob Hyndman
# make data into a tstibble
time.series = mutate(for.model, 
         year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m')),
         yrqr = year + (floor((month-1)/3)/4)) %>% # make year and quarter (divide by 4 to make fraction of year)
  group_by(jabbrv, yrqr) %>%
  summarise(n = n(), mean=mean(p)) %>% # mean acronym proportion per quarter
  filter(n >= 5) %>% # at least 5 papers per quarter
  group_by(jabbrv) %>%
  mutate(N = n(), # number of months per journal
         delta = c(0, diff(yrqr)) ) %>% # difference in neighbouring times
  #filter(delta <= 0.25) %>% # must be equal time gaps of one quarter
  filter(N >= 12) %>% # at least a year of results per journal
  ungroup() %>%
  select(-delta)
cat('There are ', length(unique(time.series$jabbrv)), ' journals.\n', sep='')
# convert to tstibble
tib = as_tsibble(time.series, key = jabbrv, index=yrqr, regular = TRUE, validate = TRUE)
# get seasonal features
tib_features <- tib %>%
  features(mean, feature_set(tags = "stl"))
tib_features2 <- tib %>%
  features(mean, feature_set(tags = "spectral")) # not working
# plot
fplot =  ggplot(tib_features, aes(x = trend_strength, y = linearity)) +
  geom_hline(yintercept=0, col='dark red')+
  geom_point(pch=1)+
  theme_bw()
jpeg('figures/linearity.vs.strength.jpg', width=5.5, height=4, units='in', res=500, quality=100)
print(fplot)
dev.off()


# what is spikiness?
filter(tib_features, spikiness > 0.000001)


# Look for journals with strong negative/positive linearity and plot time series
journal = filter(tib_features, linearity < -0.7)$jabbrv # negative
journal = filter(tib_features, linearity > 0.5)$jabbrv # positive
to.plot = filter(tib, jabbrv %in%journal)
out.plot = ggplot(data=to.plot, aes(x=yrqr, y=mean, col=factor(jabbrv)))+
  geom_line(size=1.05)+
  theme_bw()+
  scale_color_manual('Journal', values=viridis(4))+
  ylab('Mean proportion')+
  xlab('Time')+
  theme(legend.position = 'top', legend.text = element_text(size=8))
out.plot
jpeg('figures/negative.linear.change.jpg', width=6, height=4, units='in', res=500, quality=100)
print(out.plot)
dev.off()
# look acronyms used at odd journal
ann.surg = make.data(nchar.min=3, include='Ann Surg')
armed.forces = make.data(nchar.min=3, include='Med J Armed Forces India')

# also look at spectral features

# run time series summaries per journal
feature.ts = group_by(time.series, jabbrv) %>%
  mutate(deltap = c(0, diff(mean))) %>%
  summarise(n=n(), meanp = mean(mean), meand = mean(delta), max = max(deltap))
# a) difference against mean
splot4 = ggplot(data=feature.ts, aes(x=meanp, y=max))+
  geom_point(pch=1)+
  theme_bw()+
  xlab('Mean proportion')+
  ylab('Max difference in neighbouring quarters')
splot4

large.changes = filter(feature.ts, max>0.6)$jabbrv
# plot example with large change
selected = sample(large.changes, 1)
to.plot = filter(time.series, jabbrv == selected)
example = ggplot(data=to.plot, aes(x=yrqr, y=mean)) +
  geom_point()+
  geom_line()+
  theme_bw()+
  ggtitle(selected)
example
# frequent peak in 1996.75
check = filter(for.model, jabbrv=='Appl Microbiol') %>%
  mutate(year = as.numeric(format(date, '%Y')),
         month = as.numeric(format(date, '%m'))) %>%
  filter(year == 1996)
