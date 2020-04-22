# 2_concatenate_processed_data.R
# concatenate the processed data into one large file
# March 2020
source('99_combine_article_types.R') # for combining article types
library(dplyr)
library(forcats)

# check for missing files
nums = as.numeric(gsub('[a-z|A-Z]|.RData|\\.', '', dir('processed')))
which(1:1015 %in% nums == FALSE)
nums = as.numeric(gsub('[a-z|A-Z]|.RData|\\.', '', dir('raw')))
which(1:1015 %in% nums == FALSE)

# loop to combine
to.combine = dir('processed') # find the files to combine
cat('There are ', length(to.combine), ' files.\n', sep='')
titles = abstracts = acronyms = ex.titles = ex.abstracts = NULL # start with empty data
for (file in to.combine){
  load(paste('processed/', file, sep='')) # from 1_process_pubmed.R
  # date was not always in date format:
  if(class(title.data$date) == 'numeric'){ title.data = mutate(title.data, date=as.Date(date, origin='1970-01-01'))} 
  if(class(abstract.data$date) == 'numeric'){ abstract.data = mutate(abstract.data, date=as.Date(date, origin='1970-01-01'))} 
  # exclude missing dates (mostly e-pubs ahead of print)
  ex = NULL
  if(is.null(title.data)==FALSE) {
    ex.date = dplyr::filter(title.data, is.na(date) == TRUE)
    if(nrow(ex.date) > 0){
      date.excluded = select(ex.date, pmid, date, type) %>%
        mutate(reason = 'Missing date')
      ex = bind_rows(ex, date.excluded)
      # remove rows with missing dates from abstracts and titles:
      title.data = filter(title.data, !is.na(date))
      abstract.data = filter(abstract.data, !is.na(date))
    }
    # only from the end of 2019, not added to exclusions as it's an inclusion:
    title.data = dplyr::filter(title.data, date <= as.Date('2019-12-31')) 
    if(is.null(abstract.data)==FALSE){abstract.data = dplyr::filter(abstract.data, date <= as.Date('2019-12-31'))} 
  }
  # only from the end of 2019 (also for exclusions):
  excluded.abstracts = dplyr::filter(excluded.abstracts, date <= as.Date('2019-12-31')) 
  excluded.titles = dplyr::filter(excluded.titles, date <= as.Date('2019-12-31')) 
  # only use acronyms if not excluded; make sure there's some data first
  if(is.null(acronyms.in.titles)==FALSE) {
    acronyms.in.titles = dplyr::filter(acronyms.in.titles, pmid %in% title.data$pmid) %>% # making sure PMIDs are in included data
      mutate(source = 'Title')
  } 
  if(is.null(acronyms.in.abstracts)==FALSE) {
    acronyms.in.abstracts = dplyr::filter(acronyms.in.abstracts, pmid %in% abstract.data$pmid) %>%
      mutate(source = 'Abstract')
  } 
  ## concatenate ##
  if(is.null(ex)==FALSE){ # add records with missing dates to both excluded datasets
    excluded.titles = bind_rows(excluded.titles, ex)
    excluded.abstracts = bind_rows(excluded.abstracts, ex)
  }
  if(is.null(title.data)==FALSE) {titles = bind_rows(titles, title.data)}
  if(is.null(abstract.data)==FALSE) {abstracts = bind_rows(abstracts, abstract.data)}
  if(is.null(acronyms.in.titles)==FALSE) {acronyms = bind_rows(acronyms, acronyms.in.titles)} # combine acronyms into one data set
  if(is.null(acronyms.in.abstracts)==FALSE) {acronyms = bind_rows(acronyms, acronyms.in.abstracts)}
  if(is.null(excluded.titles)==FALSE) {ex.titles = bind_rows(ex.titles, excluded.titles)}
  if(is.null(excluded.abstracts)==FALSE) {ex.abstracts = bind_rows(ex.abstracts, excluded.abstracts)}
  # tidy up to avoid results being carried forward into empty results
  remove(title.data, acronyms.in.titles, excluded.titles, abstract.data, excluded.abstracts, acronyms.in.abstracts)
}
excluded.abstracts = ex.abstracts # rename
excluded.titles = ex.titles # rename

## remove duplicates, e.g., 20029614
# ideally would take first instance by date, but need to remove all duplicates because of the two data sources (titles and acronyms)
# a) titles
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
excluded.titles = bind_rows(excluded.titles, ex.duplicates)
titles = filter(titles, !pmid %in% to.remove) # remove from data
acronyms = filter(acronyms, !pmid %in% to.remove) # remove from data
# b) abstracts
dups = duplicated(abstracts$pmid)
n.duplicates = sum(dups)
to.remove = unique(abstracts$pmid[dups])
# add to exclusions
ex.duplicates = filter(abstracts, pmid %in% to.remove) %>%
  group_by(pmid) %>%
  slice(1) %>%
  select(pmid, date, type) %>%
  mutate(reason = 'Duplicate') %>%
  ungroup()
excluded.abstracts = bind_rows(excluded.abstracts, ex.duplicates)
abstracts = filter(abstracts, !pmid %in% to.remove) # remove from data
acronyms = filter(acronyms, !pmid %in% to.remove) # remove from data

## remove empty document types
no.type = filter(titles, is.na(type) == TRUE) %>%
  select(pmid, date, type) %>%
  mutate(reason = 'No article type')
excluded.titles = bind_rows(excluded.titles, no.type)
# now remove from data
to.remove = no.type$pmid
titles = filter(titles, !pmid %in% to.remove) 
abstracts = filter(abstracts, !pmid %in% to.remove) 
acronyms = filter(acronyms, !pmid %in% to.remove) 

## combine article types
titles = combine.types(titles)
abstracts = combine.types(abstracts)
excluded.titles = combine.types(excluded.titles)
excluded.abstracts = combine.types(excluded.abstracts)

## exclude abstracts of 16 or more characters (just gene strings)
acronyms = filter(acronyms, nchar < 16)

# save
save(titles, abstracts, acronyms, excluded.titles, excluded.abstracts, file='data/for.analysis.RData')
