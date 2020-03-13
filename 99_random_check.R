# 99_random_check.R
# random check of titles and abstracts
# March 2020
library(rentrez)
library(dplyr)
n.sample = 300 # number to randomly sample

# get the processed data
load('data/for.analysis.RData') # from 2_concatenate_processed_data.R
# list of PMIDs with acronyms ...
with.titles = unique(dplyr::filter(acronyms, source=='Title')$pmid) # ... for titles
with.abstracts = unique(dplyr::filter(acronyms, source=='Abstract')$pmid) # ... for abstracts

# function to get original titles from pubmed
get.title = function(pmids, n.sample, add.acronyms=FALSE, type='title'){
  s = as.character(sample(pmids, size=n.sample, replace=FALSE)) # random sample of IDs
  recs = entrez_fetch(db="pubmed", id=s, rettype="xml") # get from pubmed
  p = parse_pubmed_xml(recs)
  to.check = NULL # start with blank
  
  # make smaller data set of selected papers (takes time now, but saves time below)
  selected = dplyr::filter(acronyms, pmid %in% s)
  
  # loop
  for (j in 1:n.sample){
    if(type=='title'){f = data.frame(pmid=s[j], title = p[[j]]$title, stringsAsFactors = FALSE)}
    if(type=='abstract'){f = data.frame(pmid=s[j], abstract = paste(p[[j]]$abstract, collapse=' '), stringsAsFactors = FALSE)} # collapse paragraphs in abstracts
    # add acronyms found
    if(add.acronyms == TRUE){
      if(type=='title'){
        acronyms.add = paste(dplyr::filter(selected, source=='Title', pmid ==s[j])$acronyms, collapse=', ')
      }
      if(type=='abstract'){
        acronyms.add = paste(dplyr::filter(selected, source=='Abstract', pmid ==s[j])$acronyms, collapse=', ')
      }
      f$acronyms = acronyms.add
    } 
    # concatenate
    to.check = bind_rows(to.check, f)
    if(nrow(f)>1){cat('Check ', s[j], '\n')}
    
  }
  
  return(to.check)
}

### get _titles_ with acronyms ###
to.check = get.title(with.titles, add.acronyms=TRUE, n.sample = n.sample, type='title')
write.table(to.check, 'checks/check.titles.with.acronym.txt', sep='\t', quote=FALSE, row.names = FALSE)

### get _titles_ without acronyms ###
without = titles$pmid[titles$pmid %in% with.titles == FALSE] # PMIDs with no acronyms
to.check = get.title(without, n.sample = n.sample, type='title')
write.table(to.check, 'checks/check.titles.no.acronym.txt', sep='\t', quote=FALSE, row.names = FALSE)

### get _abstracts_ with acronyms ###
to.check = get.title(with.abstracts, add.acronyms=TRUE, n.sample = n.sample, type='abstract')
write.table(to.check, 'checks/check.abstracts.with.acronym.txt', sep='\t', quote=FALSE, row.names = FALSE)

### get _abstracts_ without acronyms ###
abstracts = filter(titles, !is.na(n.words.abstract)) # remove missing abstracts
without = abstracts$pmid[abstracts$pmid %in% with.abstracts == FALSE] # PMIDs with no acronyms
to.check = get.title(without, n.sample = n.sample, type='abstract')
write.table(to.check, 'checks/check.abstracts.no.acronym.txt', sep='\t', quote=FALSE, row.names = FALSE)
