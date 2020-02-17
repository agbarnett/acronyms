# 3_plot_trend.R
# plot the trend over time in acronym probability 
# Oct 2019
library(ggplot2)
library(gridExtra) # for grid.arrange
library(dplyr)
library(tidyr)
library(viridis) # for colours
source('99_make_analysis_data.R') # function that creates `for.model` depending on the acronym size

# get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R
# prepare the data
nchar.min = 3 # minimum acronym size for this analysis
for.model = make.data(indata = acronyms.in.titles, nchar.min=nchar.min, only.letters=TRUE)

# make summary statistics by year/month - takes a while to process data
to.plot = mutate(for.model, p = n.acronyms/n.words,
                 year = as.numeric(format(date, '%Y')), # add months and years
                 month = as.numeric(format(date, '%m')),
                 yrmon = year + ((month-1)/12)) %>%
  group_by(year, month, yrmon) %>%
  summarise(n = n(), meanp = mean(p), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(yrmon < 2019, # remove 2019 data
         n>500) %>% # exclude months with very little data (pre 1950s)
  ungroup()

# plot probability of acronym (1945 to 2018)
wplot = ggplot(data=to.plot, aes(x=yrmon, y=meanp))+
  geom_point()+
  xlab('Time')+
  ylab('Proportion')+
  theme_bw()
wplot
outfile = paste('figures/trend.titles.char.', nchar.min, '.letters.jpg', sep='')
jpeg(outfile, width=6, height=4, units='in', res=400, quality=100)
print(wplot)
dev.off()

# plot mean title length
nplot = ggplot(data=to.plot, aes(x=yrmon, y=meann))+
  xlab('Time')+
  ylab('Mean number of words in title')+
  theme_bw()+
  geom_point()
nplot # interesting result, add linear regression!
outfile = paste('figures/trend.title.length.char.', nchar.min, '.jpg', sep='')
jpeg(outfile, width=6, height=4, units='in', res=400, quality=100)
print(nplot)
dev.off()
# make summary statistics by year/month and article type - takes a while to process data
to.plot.type = mutate(for.model, p = n.acronyms/n.words,
                 year = as.numeric(format(date, '%Y')), # add months and years
                 month = as.numeric(format(date, '%m')),
                 yrmon = year + ((month-1)/12)) %>%
  group_by(year, month, yrmon, type) %>%
  summarise(n = n(), meanp = mean(p), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  filter(yrmon < 2019, yrmon>=1950) %>% # remove 2019 data
  ungroup()
# title length by journal
nplot.journal = ggplot(data=to.plot.type, aes(x=yrmon, y=meann, color=type))+
  xlab('Time')+
  ylab('Mean number of words in title')+
  theme_bw()+
  scale_y_continuous(limits=c(NA, 20))+
  scale_color_manual('Type', values = c("#c86b3d","#6882d0","#a49c3e","#b25fbc", "#58a968","#ca5677"), guide = guide_legend(override.aes = list(shape=19)))+ # different shape in legend
  geom_point(pch=1)
outfile = paste('figures/trend.title.length.by.journal.char.', nchar.min, '.jpg', sep='')
jpeg(outfile, width=6, height=4, units='in', res=400, quality=100)
print(nplot.journal)
dev.off()

# plot number of papers over time
paper.plot = ggplot(data=to.plot, aes(x=yrmon, y=n))+
  xlab('Time')+
  ylab('Mean number of papers')+
  theme_bw()+
  geom_point()
jpeg('figures/trend.paper.numbers.jpg', width=6, height=4, units='in', res=400, quality=100)
print(paper.plot)
dev.off()

# plot number of journals over time
journal.plot = ggplot(data=to.plot, aes(x=yrmon, y=njournals))+
  xlab('Time')+
  ylab('Number of journals')+
  theme_bw()+
  geom_point(col='dark blue')
jpeg('figures/trend.journal.numbers.jpg', width=6, height=4, units='in', res=400, quality=100)
print(journal.plot)
dev.off()

# grid of papers and journals over time
jpeg('figures/trend.journal.paper.numbers.jpg', width=7.5, height=4, units='in', res=400, quality=100)
grid.arrange(paper.plot, journal.plot, ncol=2)
dev.off()

### Section 2 ###
## plot of trend in acronyms leaving out most common acronyms
to.table = filter(acronyms.in.titles, nchar(acronyms)>3) # 
tab = table(to.table$acronyms)
top20 = names(head(tab[order(-tab)], 20))
# loop through top 20 (takes a while)
to.plot.all = NULL
for (k in 0:20){ # start at zero for comparison
  if(k==0){for.model = make.data(indata = acronyms.in.titles, nchar.min=nchar.min)}
  if(k>0){for.model = make.data(indata = acronyms.in.titles, nchar.min=nchar.min, exclude=top20[1:k])} # get data with excluded acronyms}
  # make summary statistics by year/month - takes a while to process data
  to.plot = mutate(for.model, 
                   p = n.acronyms/n.words,
                   year = as.numeric(format(date, '%Y')), # add months and years
                   month = as.numeric(format(date, '%m')),
                   yrmon = year + ((month-1)/12)) %>%
    filter(yrmon >= 1970, # exclude 'bumpy' period for this analysis
           yrmon < 2019) %>% # remove 2019 data
    group_by(year, month, yrmon) %>%
    summarise(n = n(), meanp = mean(p), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
    filter(n>500) %>% # exclude months with very little data (pre 1950s)
    ungroup() %>%
    mutate(k=k)
  to.plot.all = bind_rows(to.plot.all, to.plot)
}
# plot probability of acronym (1945 to 2018) for increasing exclusions; just plot smooth mean
wplot = ggplot(data=to.plot.all, aes(x=yrmon, y=meanp, col=factor(k)))+
  geom_smooth(se = FALSE)+
  scale_color_viridis(discrete=TRUE)+
  xlab('Time')+
  ylab('Proportion')+
  theme_bw()+
  theme(legend.position = 'none')
wplot
jpeg('figures/trend.remove.top20.jpg', width=5.5, height=4, units='in', res=400, quality=100)
print(wplot)
dev.off()
