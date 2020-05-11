# Analysing acronyms in _PubMed_ data

R code to read and analyse data to examine the use of acronyms in published papers over time. The analysis examines titles and abstracts published on [_PubMed_](https://pubmed.ncbi.nlm.nih.gov/) up until 2019. Our definition of acronym includes initialisms and abbreviations.

The folder `animation` contains animations of the top 20 acronyms per year over time in titles and abstracts.

The folder `data` contains the following data on acronyms and the meta-data on papers:
* `titles.rds` meta-data on the 24,873,372 included titles in RDS format
* `titles.csv` a random sample of 1,000 included titles from `titles.rds` in CSV format
* `abstracts.rds` meta-data on the 18,249,091 included titles in RDS format
* `abstracts.csv` a random sample of 1,000 included abstracts from `abstracts.RDS` in CSV format
* `acronyms.csv` a random sample of acronyms from 1,000 papers in CSV format 
I have used random samples because the complete data are too large to share here.

The data were sourced directly from _PubMed_ in XML format ([available here](ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/)) hosted by the National Library of Medicine. The data here do not reflect the most current/accurate data available from the National Library of Medicine. The data were downloaded between 14 to 22 April 2020.

The variables in `title.rds`, `titles.csv`, `abstracts.rds` and `abstracts.csv` are:
* `pmid` _PubMed_ ID number
* `date` date published on _PubMed_
* `type` article type, e.g., "Journal Article" or "Editorial"
* `jabbrv` journal abbreviation, e.g., "Biochem Med"
* `n.authors` number of authors
* `n.words` number of words in title or abstract

The `acronyms.csv` and `acronyms.rds` variables are:
* `pmid` _PubMed_ ID number
* `acronyms` the acronym (e.g., "HIV")
* `nchar` the number of characters in the acronym
* `source` 'Title' or 'Abstract'

The acronyms used above are:
* CSV = comma separated value
* RDS = R data source?
* XML = Extensible markup language
