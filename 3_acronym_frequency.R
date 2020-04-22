# 3_acronym_frequency.R
# examines frequencies of acronyms
# March 2020
library(ggplot2)
library(scales) # for comma in y-axis
library(gridExtra) # for grid.arrange
library(dplyr)
library(tidyr)
library(pander) # for table
source('99_make_analysis_data.R') # function that creates `for.model` depending on the acronym size

# get the data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R

## table of top acronyms
tab = table(acronyms$acronyms)
to.table = data.frame(head(tab[order(-tab)], 10))
pander(to.table, style='simple', big.mark=',')

# frequency table, how many are used just once?
counts = group_by(acronyms, source, acronyms) %>%
  summarise(count = n()) %>%
  arrange(source, -count) %>%
  slice(1:10) %>% # top-10
  ungroup() 
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
  scale_x_continuous(breaks=1:6, labels=c('1','(1,10]','(10,100]','(100,1000]','(1000,10,000]','10,001+'))+
  scale_y_continuous(labels = comma, limits = c(0, 460000))+ # add small buffer to top
  xlab('Frequency of use')+
  ylab('Count')+
  theme_bw()+
  theme(panel.grid.minor = element_blank())
jpeg('figures/usage.bar.plot.jpg', width=5.5, height=4, units='in', res=500, quality=100)
print(bar.plot)
dev.off()

