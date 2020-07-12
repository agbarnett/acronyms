# 3_plot_trend_word_counts.R
# plot the monthly/annual trend over time in the number of words of titles and abstracts
# May 2020
library(ggplot2)
library(gridExtra) # for grid.arrange
library(dplyr)
library(tidyr)
colours = c("#c86b3d","#6882d0","#a49c3e","#b25fbc", "#58a968","#ca5677")

## get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R
# add year/month to abstracts and titles
abstracts = mutate(abstracts, 
                   year = as.numeric(format(date, '%Y')), # add months and years
                   month = as.numeric(format(date, '%m')),
                   yrmon = year + ((month-1)/12))
titles = mutate(titles, 
                   year = as.numeric(format(date, '%Y')), # add months and years
                   month = as.numeric(format(date, '%m')),
                   yrmon = year + ((month-1)/12))

#### Part 1 , yearly trends ####
## summary of mean abstract/title length for all papers (by year)
abstract.length.yr = group_by(abstracts, year) %>%
  summarise(n = n(), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(year > 1956) %>% # exclude early years with very few journals
  #filter(njournals > 5) %>% # exclude early years with very few journals
  ungroup()
title.length.yr = group_by(titles, year) %>%
  summarise(n = n(), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(year > 1950) %>% # exclude early years with very few journals
#  filter(njournals > 20) %>% # exclude early years with very few journals
  ungroup()
## Bootstrap estimates of the mean to get confidence interval
N.boot = 100 # number of bootstrap samples
boots = NULL
for (k in 1:N.boot){ # takes a while
  boott = group_by(titles, year) %>%
    sample_frac(size=1, replace=TRUE) %>% # sample with replacement within years
    summarise(meann = mean(n.words)) %>%
    ungroup() %>%
    mutate(source = 'Title')
  boota = group_by(abstracts, year) %>%
    sample_frac(size=1, replace=TRUE) %>% # sample with replacement within years
    summarise(meann = mean(n.words)) %>%
    ungroup() %>%
    mutate(source = 'Abstract')
  boots = bind_rows(boots, boott, boota)
}
# calculate bootstrap confidence intervals
intervals = group_by(boots, source, year) %>%
  summarise(lowern = quantile(meann, 0.025), uppern = quantile(meann, 0.975)) %>%
  ungroup()
# combine estimates
for.plot.yr = bind_rows(title.length.yr, abstract.length.yr, .id='source') %>%
  mutate(source = factor(source, levels=1:2, labels=c('Title','Abstract'))) 
#%>%   left_join(intervals, by=c('source','year'))
# plot mean title/abstract length, no smooth
nplot.yr = ggplot(data=for.plot.yr, aes(x=year, y=meann)) + #, ymin=lowern, ymax=uppern))+
#  geom_ribbon(alpha=0.5)+ # bootstrap confidence interval - too narrow, don't bother
  geom_line(size=1.05, col='dark blue')+
  xlab('Date')+
  ylab('Mean number of words')+
  theme_bw()+
  theme(panel.grid.minor = element_blank(),
        text = element_text(size=7))+ # smaller for PNAS
  facet_wrap(~source, scales='free_y')
# eps for PNAS, 9 x 6 cm ()
postscript('figures/trend_words.eps', width=3.54, height=2.36, horizontal = FALSE)
print(nplot.yr)
dev.off()
#
outfile = paste('figures/trend.title.abstract.length.yearly.jpg', sep='')
jpeg(outfile, width=5.5, height=3.75, units='in', res=400, quality=100)
print(nplot.yr)
dev.off()

#### Part 2 , monthly trends ####
## summary of mean abstract/title length for all papers (by month)
abstract.length = group_by(abstracts, year, month, yrmon) %>%
  summarise(n = n(), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(yrmon < 2019+(11/12), # remove late 2019 data
         n > 500) %>% # exclude months with very little data (pre 1950s)
  ungroup()
title.length = group_by(titles, year, month, yrmon) %>%
  summarise(n = n(), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(yrmon < 2019+(11/12), # remove late 2019 data
         n > 500) %>% # exclude months with very little data (pre 1950s)
  ungroup()
for.plot = bind_rows(title.length, abstract.length, .id='source') %>%
  mutate(source = factor(source, levels=1:2, labels=c('Title','Abstract')))

# plot mean title/abstract length
nplot = ggplot(data=for.plot, aes(x=yrmon, y=meann))+
  xlab('Date')+
  ylab('Mean number of words')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(limits=c(1950, NA))+
  geom_point(pch=1, size=0.9, col=grey(0.6))+
  geom_smooth(col='dark red', span=0.25, size=0.9, method='loess', se=FALSE) +
  facet_wrap(~source, scales='free_y')
nplot 
outfile = paste('figures/trend.title.abstract.length.monthly.jpg', sep='')
jpeg(outfile, width=5.5, height=3.75, units='in', res=400, quality=100)
print(nplot)
dev.off()

# simple statistics at start and end of data
group_by(for.plot.yr, source) %>%
  arrange(source, year) %>%
  slice(1, n()) # first and last

## now by article type
## summary of mean abstract length for all papers - takes a while
abstract.length = mutate(abstracts, 
                         year = as.numeric(format(date, '%Y')), # add months and years
                         month = as.numeric(format(date, '%m')),
                         yrmon = year + ((month-1)/12)) %>%
  group_by(year, type, month, yrmon) %>%
  summarise(n = n(), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(yrmon < 2019+(11/12), # remove late 2019 data
         n > 500) %>% # exclude months with very little data (pre 1950s)
  ungroup()
title.length = mutate(titles, 
                      year = as.numeric(format(date, '%Y')), # add months and years
                      month = as.numeric(format(date, '%m')),
                      yrmon = year + ((month-1)/12)) %>%
  group_by(year, type, month, yrmon) %>%
  summarise(n = n(), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(yrmon < 2019+(11/12), # remove late 2019 data
         n > 500) %>% # exclude months with very little data (pre 1950s)
  ungroup()
for.plot.type = bind_rows(title.length, abstract.length, .id='source') %>%
  mutate(source = factor(source, levels=1:2, labels=c('Title','Abstract')))
# plot mean title/abstract length
nplot.type = ggplot(data=for.plot.type, aes(x=yrmon, y=meann, col=type))+
  xlab('Date')+
  ylab('Mean number of words')+
  theme_bw()+
  scale_color_manual('Type', values = colours, guide = guide_legend(override.aes = list(shape=19)))+ # different shape in legend
  theme(panel.grid.minor = element_blank())+
  scale_x_continuous(limits=c(1950, NA))+
  geom_point(pch=1)+
#  geom_smooth(col='dark red', span=0.25, size=0.9, method='loess', se=FALSE) +
  facet_wrap(~source, scales='free_y')
nplot.type
outfile = paste('figures/trend.title.abstract.length.type.jpg', sep='')
jpeg(outfile, width=5.5, height=3.75, units='in', res=400, quality=100)
print(nplot.type)
dev.off()

### other basic plots #
# plot number of papers over time with title and abstract
for.plot = mutate(for.plot,
                  January = factor(as.numeric(month==1), levels=0:1, labels=c('No','Yes')))
paper.plot = ggplot(data=for.plot, aes(x=yrmon, y=n, col=January))+
  xlab('Time')+
  ylab('Mean number of papers')+
  scale_x_continuous(limits=c(1950, NA))+
  theme_bw()+
  geom_point()+
  facet_wrap(~source)
jpeg('figures/trend.paper.numbers.jpg', width=6, height=4, units='in', res=400, quality=100)
print(paper.plot)
dev.off()

# plot number of journals over time with included titles and abstracts
journal.plot = ggplot(data=for.plot, aes(x=yrmon, y=njournals, col=January))+
  xlab('Time')+
  ylab('Number of journals')+
  scale_x_continuous(limits=c(1950, NA))+
  theme_bw()+
  geom_point( )+
  facet_wrap(~source)
jpeg('figures/trend.journal.numbers.jpg', width=6, height=4, units='in', res=400, quality=100)
print(journal.plot)
dev.off()
