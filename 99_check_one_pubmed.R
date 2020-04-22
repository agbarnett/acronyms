# 99_check_one_pubmed.R
# check one result, used to verify how the main function is working
# March 2020
library(stringr)
library(dplyr)
library(Unicode)
source('99_remove_dots.R') # to remove full-stops within acronyms
source('99_main_function_title.R') # main function for titles
source('99_main_function_abstract.R') # main function for abstracts
load('data/pubmed_list.RData') # id.list from 1_make_pubmed_list.R

# pubmed id to check
to.check = 29753800 

# find this pubmed ID in big list of files
which.number = filter(id.list, pmid==to.check) 

# get the data
infile = paste('raw/unprocessed.pubmed.', which.number$file, '.RData', sep='') # from 0_read_pubmed_xml.R
load(infile)
k = which(raw_pubmed$pmid == to.check) # find file to check within the large data set

# run the main function on the title
results = title_acronyms(indata=raw_pubmed, k=k) # 
results

# run the main function on the abstract
if(is.na(raw_pubmed$abstract[k]) ==FALSE){
  aresults = abstract_acronyms(indata=raw_pubmed, k=k) # 
}
aresults

