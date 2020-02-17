# 2_concatenate_processed_data.R
# concatenate the processed data back together
# January 2020
source('99_combine_article_types.R') # for combining article types
library(dplyr)
library(forcats)

to.combine = dir('processed')
cat('There are ', length(to.combine), ' files.\n', sep='')
titles = acronyms = ex = NULL
for (file in to.combine){
  load(paste('processed/', file, sep=''))
  if(class(title.data$date) == 'numeric'){ title.data = mutate(title.data, date=as.Date(date, origin='1970-01-01'))} # was not always in date format
  # exclude missing dates (mostly e-pubs ahead of print)
  if(is.null(title.data)==FALSE) {
    ex.date = filter(title.data, is.na(date) == TRUE)
    if(nrow(ex.date) > 0){
      date.excluded = select(ex.date, pmid, date, type) %>%
        mutate(reason = 'Missing date')
      ex = bind_rows(ex, date.excluded)
      title.data = filter(title.data, !is.na(date))
    }
    title.data = filter(title.data, date <= as.Date('2019-12-31')) # only from the end of 2019, not added to exclusions as it's an inclusion
  }
  excluded = filter(excluded, date <= as.Date('2019-12-31')) # only from the end of 2019
  if(is.null(acronyms.in.titles)==FALSE) {acronyms.in.titles = filter(acronyms.in.titles, pmid %in% title.data$pmid)} # only use if not excluded; make sure there's some data first
  # concatenate
  ex = bind_rows(ex, excluded)
  if(is.null(title.data)==FALSE) {titles = bind_rows(titles, title.data)}
  if(is.null(acronyms.in.titles)==FALSE) {acronyms = bind_rows(acronyms, acronyms.in.titles)}
}
excluded = ex # rename

## remove duplicates, e.g., 20029614
# ideally would take first instance by date, but need to remove all duplicates because of the two data sources (titles and acronyms)
dups = duplicated(titles$pmid)
n.duplicates = sum(dups)
to.remove = unique(titles$pmid[dups])
# add to exclusions
ex.duplicates = filter(titles, pmid %in% to.remove) %>%
  group_by(pmid) %>%
  slice(1) %>%
  select(pmid, date, type) %>%
  mutate(reason = 'Duplicate') %>%
  ungroup()
excluded = bind_rows(excluded, ex.duplicates)
# remove
titles = filter(titles, !pmid %in% to.remove)
acronyms = filter(acronyms, !pmid %in% to.remove)

## combine article type
titles = combine.types(titles)
excluded = combine.types(excluded)

# save
acronyms.in.titles = acronyms
save(titles, acronyms.in.titles, excluded, file='data/for.analysis.RData')
