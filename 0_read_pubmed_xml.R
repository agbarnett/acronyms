# 0_read_pubmed_xml.R
# get all pubmed data from web; takes a while
# pubmed XML files are here ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline
# for testing can use small file = 'testXml/test.xml' (others in same folder)
# April 2020
library(XML)
library(dplyr)
library(stringr)
library(R.utils) # for gunzip
library(textclean) # for replace non-ASCII
library(easyPubMed) # for new_PM_df which imports XML into R
source('99_table_articles_byAuth_adapted.R') # use my adapted versions of this code
source('99_article_to_df_adapted.R') # faster without author data
# see https://cran.r-project.org/web/packages/easyPubMed/vignettes/getting_started_with_easyPubMed.html

# download NLM file from web
# numbers from 1 to 1015
#numbers.to.loop = round(seq(1, 1014, length.out = 20)) # selected points in time, useful for an initial run
#set.seed(12345); numbers.to.loop = sample(1:1014, replace=FALSE, size=10) # random points in time
numbers.to.loop = 979:1015 # all of them!
#numbers.to.loop = 1010:1015 # update most recent files
for (number in numbers.to.loop){ 
file = paste("pubmed20n", sprintf("%04d", number), ".xml", sep='') # number with leading zeros
filez = paste(file, ".gz", sep='') # zipped version of filename
url = paste('ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/', filez, sep='')
zipped.file = download.file(url = url, destfile=filez) # read pubmed files from FTP to local file
gunzip(filez) # unzip
# import the XML file into R - takes a while
very_raw_pubmed <- table_articles_byAuth_adapted(pubmed_data = file, # from 99_table_articles_byAuth_adapted.R
  encoding = "UTF-8") # recommended encoding
start.N = very_raw_pubmed$N # store starting number of papers
very_raw_pubmed = very_raw_pubmed$papers.authors.df # reuse old data name
# remove pubmed file from directory to save space
if(str_detect(string=file, pattern='pubmed20') == TRUE){file.remove(file)} # do not remove testing files

## preliminary processing
# strings to remove from abstract:
string.to.remove = c('This article is protected by copyright\\. All rights reserved\\.',
                     'Published [0-9][0-9][0-9][0-9]\\.',
                     'This article is a US Government work and is in the public domain in the USA\\.')
string.to.remove = paste(string.to.remove, collapse='|')
raw_pubmed = mutate(very_raw_pubmed, 
  pmid = as.numeric(pmid),
  abstract = str_remove(string=abstract, pattern=string.to.remove),
  title = replace_non_ascii(title),
  title = str_remove(string=title, pattern="[:punct:]authors' transl[:punct:]|[:punct:]author's transl[:punct:]|[:punct:]authors transl[:punct:]|[:punct:]authors transl"), # remove this note
  title = str_remove(string=title, pattern='[:punct:]$'), # remove punctuation at end of title
  title = str_remove(string=title, pattern='\\] $|^\\[|\\]$') # remove square brackets around title
) %>% 
  dplyr::select(pmid, language, n.authors, first.author, jabbrv, type, date, title, abstract) # reduce the number of variables

## Paper numbers throughout the data management process
# lost in processing - none, so don't need
# numbers = data.frame(start = start.N, post.processing = nrow(raw_pubmed))
numbers = data.frame(start = start.N)
# remove non-English
raw_pubmed = filter(raw_pubmed, language=='eng') %>%
  dplyr::select(-language)
numbers$post.non.english = nrow(raw_pubmed)

# save data for further processing.
outfile = paste('raw/unprocessed.pubmed.', number, '.RData', sep='')
save(raw_pubmed, numbers, file=outfile)

} # end of numbers loop
