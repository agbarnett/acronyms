# 3_plot_trend_year.R
# plot the trend over time in acronym probability 
# plus version with top 20 to 100
# plus trend by article type
# version using year only, see also 3_plot_trend_yrmon.R for year/month
# gets the summary data from lyra
# June 2020
library(ggplot2)
library(gridExtra) # for grid.arrange
library(dplyr)
library(tidyr)
library(stringr)
# colours from `i want hue`
colours = c("#ca568b",
  "#72a659",
  "#8377cb",
  "#c6763f")

## get the summary data from lyra, dependent on lots of variants
# a) single file
nchar.selected = 99 # number of characters for acronym
exclude = 'None' # acronyms to exlcude
only.letters = TRUE # acronyms are letters only
j.include = 'Nature'
#j.include = 'PLoS ONE'
year.start = 1950 # year to start plot from (1956 is first year of abstracts)
source('99_make_filename.R')
data = readRDS(paste('Z:/acronym/data/', filename, sep='')) # from lyra/2_make_many_summaries.R

# b) get all the data from lyra
rds.files = dir(path='Z:/acronym/data/', pattern='.rds')
not.rds.files = dir(path='Z:/acronym/data/', pattern='.excludenum') # the files exclude the top 100
rds.files = rds.files[!rds.files%in%not.rds.files]
all.trends = NULL
for (f in rds.files){
  data = readRDS(paste('Z:/acronym/data/', f, sep=''))
  # add variables, see # 99_make_filename.R
  data$journals = str_split(f, '\\.')[[1]][6]
  data$exclude = str_split(f, '\\.')[[1]][5]
  data$only.letters = str_split(f, '\\.')[[1]][4]
  data$n.chars = str_split(f, '\\.')[[1]][3]
  # concatenate
  all.trends = bind_rows(all.trends, data)
}

## arrange into panels
to.plot = filter(all.trends, 
                 year >= year.start,
#                 exclude == 'None',
#                 journals == 'Proc Natl Acad Sci USA' 
#                 journals == 'PLoS ONE' 
                 journals == 'Nature' 
#                 journals == 'alljournals'
                 ) %>%
  mutate(facet = ifelse(only.letters == TRUE, 'Letters only', 'Letters and/or numbers'))

# plot probability of acronym (1950 to 2019)
wplot = ggplot(data=to.plot, aes(x=year, y=meanp, col=factor(n.chars)))+
  geom_line(size=1.05)+
  xlab('Year')+
  scale_color_manual('Number of\ncharacters', values=colours, labels=c('2','3','4','2+'))+
  ylab('Proportion of acronyms')+
  scale_y_continuous(limits=c(0,NA))+ # add zero on y-axis
  theme_bw()+
  theme(panel.spacing.x = unit(0.5, "lines"), # increase space between panels because of years on x-axis
        text = element_text(size=7))+ # smaller for PNAS
  facet_grid(source~facet)
wplot
# eps for PNAS, 9 x 6 cm ()
postscript('figures/trend_acronyms.eps', width=3.54, height=2.36, horizontal = FALSE)
print(wplot)
dev.off()
outfile = paste('figures/trend.acronyms.PNAS.jpg', sep='')
jpeg(outfile, width=5.5, height=4, units='in', res=400, quality=100)
print(wplot)
dev.off()

### Split by journal
# plot probability of acronym (1950 to 2019)
wplot2 = ggplot(data=to.plot, aes(x=year, y=meanp, col=factor(n.chars)))+
  geom_line(size=1.05)+
  ggtitle('B: Nature')+
  xlab('Year')+
  scale_color_manual('Number of\ncharacters', values=colours, labels=c('2','3','4','2+'))+
  ylab('Proportion of acronyms')+
  scale_y_continuous(limits=c(0,NA))+ # add zero on y-axis
  theme_bw()+
  theme(panel.spacing.x = unit(0.5, "lines"), # increase space between panels because of years on x-axis
        text = element_text(size=12))+ 
  facet_grid(source~facet)
wplot2

# 
postscript('figures/trend_acronyms_PLOS_Nature_large.eps', width=5, height=7, horizontal = FALSE)
grid.arrange(wplot1, wplot2, nrow=2)
dev.off()


# stats on numbers for text; for all characters, 2+
filter(to.plot, 
       year %in% c(1950,1956,2019),
       journals == 'alljournals',
       exclude == 'None',
       only.letters == FALSE,
       n.chars == 'allchars' ) %>% 
  mutate(meanp = round(meanp*1000)/10)# per 100 words


### Section 2 - exclude top abbreviations ###

# get files for those that exclude top acronyms
rds.files = dir(path='Z:/acronym/data/', pattern='excludenum')
cat('There are ', length(rds.files),' files \n', sep='')
all.trends = NULL
for (f in rds.files){
  data = readRDS(paste('Z:/acronym/data/', f, sep=''))
  # add variables, see # 99_make_filename.R
  data$journals = str_split(f, '\\.')[[1]][6]
  data$exclude = str_split(f, '\\.')[[1]][5]
  data$only.letters = str_split(f, '\\.')[[1]][4]
  data$n.chars = str_split(f, '\\.')[[1]][3]
  data$n.excluded = as.numeric(str_remove_all(f, pattern='[A-Za-z]|\\.'))
  # concatenate
  all.trends = bind_rows(all.trends, data)
}

# plot probability of acronym for increasing exclusions
cols = grey(seq(0.2,0.8, length.out = 100))
wplot = ggplot(data=all.trends, aes(x=year, y=meanp, group=factor(n.excluded), col=factor(n.excluded)))+
  geom_line()+
  scale_color_manual(NULL, values=cols)+
  xlab('Time')+
  ylab('Mean proportion of acronyms')+
#  scale_y_log10()+ # does not help
  theme_bw()+
  theme(legend.position = 'none')+
  facet_wrap(~source, scales='free_y')
wplot
jpeg('figures/trend.remove.top100.jpg', width=5.5, height=4, units='in', res=400, quality=100)
print(wplot)
dev.off()
postscript('figures/trend_remove_top100.eps', width=5.5, height=4, horizontal = FALSE)
print(wplot)
dev.off()


### Section 3 - plot by article type ###

# prepare the data
source('99_make_analysis_data.R')
load('data/for.analysis.RData')
for.model = make.data(indata = acronyms, nchar.selected=99, only.letters=FALSE) # from 99_make_analysis_data.R

# make summary statistics by year and article type - takes a while to process data
to.plot.type = mutate(for.model, 
                      p = n.acronyms/n.words,
                      year = as.numeric(format(date, '%Y'))) %>%
  group_by(year, source, type) %>%
  summarise(n = n(), meanp = mean(p), meann=mean(n.words), njournals = length(unique(jabbrv))) %>%
  ungroup() %>%
  filter(n > 100) # remove small samples
# a) title length by article type
nplot.type.words = ggplot(data=to.plot.type, aes(x=year, y=meann, color=type))+
  geom_line(size=1.05)+
  xlab('Year')+
  ylab('Mean number of words')+
  theme_bw()+
  scale_color_manual('Type', values = c("#c86b3d","#6882d0","#a49c3e","#b25fbc", "#58a968","#ca5677"))+
  facet_wrap(~source, scales='free_y')
jpeg('figures/trend.title.length.by.type.jpg', width=6, height=4, units='in', res=400, quality=100)
print(nplot.type.words)
dev.off()
postscript('figures/trend_title_length_by_type.eps', width=6, height=4, horizontal=FALSE)
print(nplot.type.words)
dev.off()
# b) proportion by article type
nplot.type = ggplot(data=to.plot.type, aes(x=year, y=meanp, color=type))+
  geom_line(size=1.05)+
  xlab('Year')+
  ylab('Mean proportion of acronyms')+
  theme_bw()+
  scale_color_manual('Type', values = c("#c86b3d","#6882d0","#a49c3e","#b25fbc", "#58a968","#ca5677"))+
  facet_wrap(~source, scales='free_y')
jpeg('figures/trend.title.proportion.by.type.jpg', width=6, height=4, units='in', res=400, quality=100)
print(nplot.type)
dev.off()
postscript('figures/trend_title_proportion_by_type.eps', width=6, height=4, horizontal=FALSE)
print(nplot.type)
dev.off()
# c) truncated panel for titles
# for elife with legend and text (27 June 2020)
caption = 'Figure 3 - supplement 1.\nThis shows the trend in titles using a truncated y-axis to reduce the influence\nof these few extreme years for the “Other” type, and so more clearly show the\nupward trend in acronyms for titles.'
nplot.type.trunc = ggplot(data=filter(to.plot.type, source=='Title'), aes(x=year, y=meanp, color=type))+
  geom_line(size=1.05)+
  xlab('Year')+
  ylab('Mean proportion of acronyms')+
  coord_cartesian(ylim=c(0,0.04))+
  theme_bw()+
  scale_color_manual('Type', values = c("#c86b3d","#6882d0","#a49c3e","#b25fbc", "#58a968","#ca5677"))+
  theme(plot.caption = element_text(hjust = 0))+ # left-aligned caption
  labs(caption = caption) 
jpeg('figures/trend.title.proportion.by.type.truncated.jpg', width=6, height=4, units='in', res=400, quality=100)
print(nplot.type.trunc)
dev.off()
postscript('figures/trend_title_proportion_by_type_truncated.eps', width=6, height=4, horizontal=FALSE)
print(nplot.type.trunc)
dev.off()
