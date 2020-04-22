# 1_process_pubmed.R
# process the pubmed data to extract acronyms from the title and abstract
# moved this to lyra
# March 2020
library(Unicode) # for math and other symbols
library(stringr)
library(dplyr)
source('99_remove_dots.R') # to remove full-stops within acronyms
source('99_main_function_title.R') # main function for titles
source('99_main_function_abstract.R') # main function for abstracts

# load data for further processing.
#numbers.to.loop = round(seq(1, 1014, length.out = 20)) # selected points in time, useful for an initial run
#set.seed(12345); numbers.to.loop = sample(1:1014, replace=FALSE, size=10) # random points in time
numbers.to.loop = 1:1015 # all of them!
for (number in numbers.to.loop){ 

  if(number%%10 == 0){cat(paste('Up to file',number,'\r'))} # progress bar
  
infile = paste('raw/unprocessed.pubmed.', number, '.RData', sep='') # from 0_read_pubmed_xml.R
load(infile)
#raw_pubmed = filter(raw_pubmed, pmid !=7983341) # very odd title, had to exclude here as it caused crash - no longer needed, excluded via programming
title.data = acronyms.in.titles = excluded.titles = abstract.data = excluded.abstracts = acronyms.in.abstracts = NULL # start with empty data sets

## process the papers in a large loop
for (k in 1:nrow(raw_pubmed)){
  
  # a) titles
  if(is.na(raw_pubmed$title[k]) == TRUE | raw_pubmed$title[k]==''){ # don't even start if title is empty. Not many missing titles, e.g., 10710667[pmid],  11028052 
    this.exclude = data.frame(pmid=raw_pubmed$pmid[k], date=raw_pubmed$date[k], type=raw_pubmed$type[k], reason='Empty title', stringsAsFactors = FALSE)
    excluded.titles = bind_rows(excluded.titles, this.exclude)
  }
  results = title_acronyms(indata=raw_pubmed, k=k) # using the main function 99_main_function_title.R
  # concatenate the three data sets
  if(results$exclude == TRUE) {excluded.titles = bind_rows(excluded.titles, results$this.exclude)}
  if(results$exclude == FALSE) {
    title.data = bind_rows(title.data, results$tframe) 
    if(is.null(results$aframe) == FALSE) {acronyms.in.titles = bind_rows(acronyms.in.titles, results$aframe)}
  }
  remove(results) # tidy up

  # b) abstracts
  abstract.empty = FALSE
  # don't even start if abstract is empty or is very short
  if(is.na(raw_pubmed$abstract[k]) == TRUE | raw_pubmed$abstract[k]=='' | raw_pubmed$abstract[k]==' ' | 
     tolower(raw_pubmed$abstract[k])=='n/a' | tolower(raw_pubmed$abstract[k])=='n/a.'|
     tolower(raw_pubmed$abstract[k])=='no abstract available' | tolower(raw_pubmed$abstract[k])=='no abstract available.'){abstract.empty = TRUE}
  if(abstract.empty==FALSE){
    n.words = str_count(raw_pubmed$abstract[k], ' ') # rough word count
    if(is.na(n.words) == TRUE){n.words = 0}
    if(n.words <= 10 & raw_pubmed$type[k] == 'Published Erratum'){abstract.empty = TRUE} # short abstracts with errata are usually just a citation/note
    if(n.words <= 10 & str_detect(string=raw_pubmed$abstract[k], pattern='This corrects the article')) {abstract.empty = TRUE} # alternative search for errata (in case they are listed as type 'journal article')
  }
  if(abstract.empty == TRUE){
    this.exclude = data.frame(pmid=raw_pubmed$pmid[k], date=raw_pubmed$date[k], type=raw_pubmed$type[k], reason='No abstract', stringsAsFactors = FALSE)
    excluded.abstracts = bind_rows(excluded.abstracts, this.exclude) 
  }
  if(abstract.empty == FALSE){
    aresults = abstract_acronyms(indata=raw_pubmed, k=k) # using the main function 99_main_function_abstract.R
    # concatenate the three data sets
    if(aresults$exclude == TRUE) {excluded.abstracts = bind_rows(excluded.abstracts, aresults$this.exclude)}
    if(aresults$exclude == FALSE) {
      abstract.data = bind_rows(abstract.data, aresults$tframe) 
      if(is.null(aresults$aframe) == FALSE) {acronyms.in.abstracts = bind_rows(acronyms.in.abstracts, aresults$aframe)}
    }
    remove(aresults) # tidy up
  }

  #
  if(k%%500 == 0){cat(paste('Up to paper', k, '\r'))} # progress bar
}

# save
outfile = paste('processed/pubmed.', number, '.RData', sep='') # 
save(title.data, acronyms.in.titles, excluded.titles, abstract.data, excluded.abstracts, acronyms.in.abstracts, file=outfile)

}
