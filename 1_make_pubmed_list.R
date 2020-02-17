# 1_make_pubmed_list.R
# make a list of the file numbers and PMIDs (used by 99_check_one_pubmed.R)
# January 2020
library(dplyr)

# big loop through files
id.list = NULL
for (number in 1:1015){
  infile = paste('raw/unprocessed.pubmed.', number, '.RData', sep='') # from 0_read_pubmed_xml.R
  load(infile)
  frame = data.frame(file=number, pmid = raw_pubmed$pmid)
  id.list = bind_rows(id.list, frame)
  remove(frame) # tidy up
  
  if(number%%100 == 0){cat(paste('Up to file', number, '\r'))} # progress bar
}

# save
save(id.list, file='data/pubmed_list.RData')
