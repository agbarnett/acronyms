# 99_check_roman_numerals.R
# check if there are Roman Numerals that are also acronyms by their volume
# January 2020
library(easyPubMed)
library(dplyr)

roman.numerals = as.character(as.roman(1:1000))
roman.numerals = roman.numerals[nchar(roman.numerals)>1] # do not bother with single digits

counts = NULL
for (k in 1:length(roman.numerals)){
  query = paste(roman.numerals[k], '[TI]', sep='') # look for this roman numeral in the title
  number_count <- as.numeric(get_pubmed_ids(query)$Count)
  frame = data.frame(roman = roman.numerals[k], count=number_count, stringsAsFactors = FALSE)
  counts = bind_rows(counts, frame)
}

counts = arrange(counts, -count)
head(counts, 20)

# "IV" is still predominantly a number, e.g., 26606335[pmid] and 26610912[pmid] 
# "VI" is predominantly a number, e.g. "chromium(VI)"
