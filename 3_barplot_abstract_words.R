# 3_barplot_abstract_words.R
# plot the number of words per abstract to show the spikes
# April 2020
load('data/for.analysis.RData')
library(dplyr)
library(ggplot2)
library(scales) # for comma

# get the counts
count = group_by(abstracts, n.words) %>%
  summarise(count = n())

# plot the counts
cplot = ggplot(count, aes(x=n.words, y=count))+
  geom_bar(col='skyblue', stat='identity')+
  geom_vline(xintercept=c(150,200,250), lty=2)+ # reference lines at word counts
  xlab('Number of words in the abstract')+
  ylab('Count')+
  scale_y_continuous(labels=comma)+
  coord_cartesian(xlim=c(0,500))+
  theme_bw()+
  theme(panel.grid.minor = element_blank()) # for twitter size
jpeg('figures/abstract.words.jpg', width=5, height=3, units='in', res=300)
#jpeg('figures/abstract.words.jpg', width=1024*2, height=512*2, units='px', res=600) # for twitter
print(cplot)
dev.off()
