# 0_read_pubmed_daily_xml.R
# get all pubmed data from web; takes a while
# version using daily data at ftp://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/
# for testing can use small file = 'testXml/test.xml' (others in same folder)
# April 2020, with update post-COVID in February 2021
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
#numbers.to.loop = round(seq(1, 1014, length.out = 20)) # selected points in time, useful for an initial run
#set.seed(12345); numbers.to.loop = sample(1:1014, replace=FALSE, size=10) # random points in time
numbers.to.loop = 1102:1154 # post-COVID (full range is 1064 to 1154)
number = 1101
while(number <= 1154){
  file_exists = length(dir('raw', paste('unprocessed.pubmed.daily.', number, '.RData', sep='')))>0
  if(file_exists ==TRUE){ # if file exists move to next number
    number = number + 1
    next
  }
  if(file_exists ==FALSE){ # if file does not exist then run
    try(source('0_run_read.R')) # run and ignore errors
  }
}
