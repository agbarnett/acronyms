# Analysing acronyms in _PubMed_ data

R code to read and analyse data to examine the use of acronyms in published papers over time. The code looks at titles and abstracts published on _PubMed_ up until 2019.

The folder `animation` contains an animation of the top 20 acronyms per year over time.

The folder `data` contains the following data on acronyms and the meta-data on papers:
* `titles.csv` a random sample of 1000 papers in CSV format (the complete data is too large to share here)
* `x.csv` a random sample of the acronyms in CSV format (the complete data is too large to share here)

The data were sourced directly from _PubMed_ in XML format [here](ftp://ftp.ncbi.nlm.nih.gov/pubmed/baseline/) hosted by the National Library of Medicine. The data here do not reflect the most current/accurate data available from the National Library of Medicine. The data were downloaded between 14 to 22 April 2020.

The `title.csv` and `titles.RDS` variables are:
* `pmid` _PubMed_ ID number
* `date` date published on _PubMed_
* `type` article type, e.g., "Journal Article" or "Editorial"
* `jabbrv` journal abbreviation, e.g., "Biochem Med"
* `n.authors` number of authors
* `n.words` number of words in title

The `acronyms.csv` and `acronyms.RDS` variables are:
* `pmid` _PubMed_ ID number
* `acronym` the acronym
