# 3_plot_ngrams.R
# plot selected acronyms over time
# also looks at frequencies of acronyms
# Oct 2019
library(ggplot2)
library(gridExtra) # for grid.arrange
library(dplyr)
library(tidyr)
library(pander) # for table
source('99_make_analysis_data.R') # function that creates `for.model` depending on the acronym size

# get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R
# prepare the data
nchar.min = 3 # minimum acronym size for this analysis

## table of top acronyms
tab = table(acronyms.in.titles$acronyms)
to.table = data.frame(head(tab[order(-tab)], 10))
pander(to.table, style='simple', big.mark=',')

# frequency table, how many are used just once?
to.plot = data.frame(tab, stringsAsFactors = FALSE); names(to.plot) = c('Acronym','Count') 
to.plot = mutate(to.plot, bar = case_when(
  Count == 1 ~ 1,
  Count > 1 & Count<=10 ~ 2,
  Count > 10 & Count<=100 ~ 3,
  Count > 100 & Count<=1000 ~ 4,
  Count > 1000 & Count<=10000 ~ 5,
  Count > 10000 ~ 6))
table.results = data.frame(table(to.plot$bar), stringsAsFactors = FALSE); names(table.results)[1] = 'bar'; 
table.results = mutate(table.results,
                       bar = as.numeric(bar),
                       label = format(Freq, big.mark = ','))
bar.plot = ggplot(data=to.plot, aes(x=bar)) +
  geom_bar(fill='skyblue')+
  geom_label(data=table.results, aes(x=bar, y=Freq, label=label), col=grey(0.2), vjust=0)+
  scale_x_continuous(breaks=1:6, labels=c('1','(1,10]','(10,100]','(100,1000]','(1000,10,000]','10,000+'))+
#  scale_y_continuous(limits=c(0,114000))+
  xlab('Frequency of use')+
  ylab('Count')+
  theme_bw()
jpeg('figures/usage.bar.plot.jpg', width=5.5, height=4, units='in', res=500, quality=100)
print(bar.plot)
dev.off()

## ngrams ##
# plot counts for four selected acronyms
selected = filter(acronyms.in.titles, acronyms %in% c('DNA','HIV','MRI','PCR'))
to.plot = left_join(selected, titles, by='pmid') %>% # takes a while
  mutate(year = as.numeric(format(date, '%Y')), # add months and years
         month = as.numeric(format(date, '%m')),
         yrmon = year + ((month-1)/12)) %>%
  filter(year >= 1945, year < 2019) %>% # exclude data that is too thin
  group_by(yrmon, acronyms) %>%
  summarise(count = n()) %>%
  ungroup()

# plot acronym count (1945 to 2018)
ngram.plot = ggplot(data=to.plot, aes(x=yrmon, y=count))+
  geom_point(pch=1, size=0.9)+ # make slightly smaller than normal
  xlab('Time')+
  ylab('Monthly counts')+
  theme_bw()+
  facet_wrap(~acronyms)
jpeg('figures/trend.four.selected.jpg', width=6, height=4, units='in', res=500, quality=100)
print(ngram.plot)
dev.off()
