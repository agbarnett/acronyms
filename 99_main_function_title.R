# 99_main_function_title.R
# main function to extract the acronyms from titles
# used by 1_process_pubmed.R
# Feb 2020

## Section 1: key characters ##

## used to remove roman numerals
roman.start = as.character(as.roman(1:30)) # just up to 30 based on trial and error
roman.start = roman.start[nchar(roman.start)>1] # do not bother with single digits
roman.start = roman.start[order(-nchar(roman.start))] # longest to shortest
# first version
roman.numerals = paste('^', roman.start, "$", sep='') # for whole words only
roman.numerals = paste(roman.numerals, collapse='|')
# repeat with 'th' at the end
roman.numerals.th = paste('^', roman.start, "th$", sep='') # for whole words only
roman.numerals.th = paste(roman.numerals.th, collapse='|')
# roman numbers plus lower case letters
roman.start = as.character(as.roman(2:9)) # just to 9
roman.start = roman.start[order(-nchar(roman.start))] # longest to shortest
roman.numerals.letters = NULL
for (r in roman.start){
  roman.numerals.letters = c(roman.numerals.letters, paste(r, letters[1:5], sep=''))
}
roman.numerals.letters = paste('^', roman.numerals.letters, "$", sep='') # for whole words only
roman.numerals.letters = paste(roman.numerals.letters, collapse='|')

# used to remove chromosomes
chromosomes = c('XX','XY','XO','ZO','XXYY','ZW','ZWW','XXX','XXXX','XXXXX','YYYYY')
chromosomes = paste('^', chromosomes, "$", sep='') # for whole words only
chromosomes = paste(chromosomes, collapse='|')

## things to replace in titles and abstracts before counting acronyms
# remove subscript and superscript
special.words = c('sub','sup','super','i','exp','fraction')
open = paste('\\<', special.words, '\\>', sep='') 
close = paste('\\<\\/', special.words, '\\>', sep='')
to.replace = c(open, close)
to.replace = to.replace[order(-nchar(to.replace))] # longest to shortest
to.replace = paste(to.replace, collapse='|')

# remove these words as acronyms from the abstract
bogus.acronyms.abstract = c('WITHDRAWN','CORRIGENDUM','BACKGROUND','INTRODUCTION','OBJECTIVE','METHODS','RESULTS','DISCUSSION','CONCLUSION','UNASSIGNED','PRACTICAL APPLICATION')
bogus.acronyms.abstract = paste(bogus.acronyms.abstract, collapse='|')
# remove these words/phrases as acronyms from the title (first word)
bogus.acronyms.title = c('WITHDRAWN','CORRIGENDUM','EDITORIAL','MEDICAL','TRANSACTIONS','CASE RECORDS','SYMPOSIUM','MASSACHUSETTS GENERAL HOSPITAL')
bogus.acronyms.title = paste(bogus.acronyms.title, collapse='|')
# remove punctuation, can't use [:punct:] because it includes & and we want to keep that, e.g. for "AT&T"
# decided to remove + because of things like 28517515[pmid] and 28516485[pmid] 
punctuation = c('!',"'",'"','#','%','(',')','*',',','-','.',"\\",'/',':',';','<','=','>','?','@','[','/',']','^','_','{','|','}','~',"`",'+')
punctuation = paste(paste('\\', punctuation, sep=''), collapse='|')

## Section 2: function ##
title_acronyms = function(indata, k){

to.return = list()
to.return$exclude = FALSE
  
# extract title
title = indata$title[k]
title = str_remove_all(title, pattern=to.replace) 
title = str_replace_all(title, pattern=bogus.acronyms.title, replacement='dummy') # Replaced with 'dummy' so that word count is not effected
title = str_remove_dots(title)  # remove full-stops in acronyms
title = str_replace_all(string=title, pattern="-[0-9]* |-[0-9]*$", replacement=' ') # remove numbers after a hyphen (e.g., 21745015[pmid]); keep numbers without a hyphen because these are often real words
title = str_replace_all(string=title, pattern=" [0-9]*-|^[0-9]*-", replacement=' ') # remove numbers before a hyphen (e.g., 21744858[pmid])
title = str_replace_all(title, pattern=punctuation, replacement = ' ') # replace all punctuation as it just gets in the way - need to do after contracting dots
title = str_replace_all(title, pattern='\\.\\.\\.', replacement = '~') # replace '...' with '~' so it does not get counted as an acronym, e.g. 15299935

## break the title into words
words = str_split(string=title, pattern=' ')[[1]]
words = words[words!=''] # remove blank words
# are all or most words in capitals? if they are then ignore this title because it's a journal style, e.g., 29669138[pmid] 
capital.words = str_remove_all(string=words, pattern='[^A-Z]')
all.caps = nchar(words) == nchar(capital.words)
number.all.capital = sum(all.caps)
proportion.capital = number.all.capital / length(words)
# if proportion in capitals is on or over 0.5 then exclude
this.exclude = NULL
if(length(words)==0){
  this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Empty title', stringsAsFactors = FALSE)
  to.return$exclude = TRUE
  to.return$this.exclude = this.exclude
  return(to.return) # break early
} # 
if(proportion.capital >= 0.5){
  if(length(words)>2 | proportion.capital==1){ # skip if there's two words and one of them is in capitals, e.g., 6551089[pmid]
    this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Title in capitals', stringsAsFactors = FALSE)
    to.return$exclude = TRUE
    to.return$this.exclude = this.exclude
    return(to.return) # break early
  }
} # 
# if there's a cluster of capitals at the start or end (subtitle, e.g, 29689030[pmid])
n.words=length(words)
if(n.words > 4){ # only for longer titles
  if( all(all.caps[1:4]) & any(nchar(words[1:4]) >= 5) & sum(all.caps)==4) { # first 4 words must all be in capitals and one word must be 5 chars or longer
    this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Title in capitals (subtitle, start)', stringsAsFactors = FALSE)
    to.return$exclude = TRUE
    to.return$this.exclude = this.exclude
    return(to.return) # break early
  }
  if( all(all.caps[(n.words-3):n.words]) & any(nchar(words[(n.words-3):n.words]) >= 5) & sum(all.caps)==4) { # same for last 4 words
    this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Title in capitals (subtitle, end)', stringsAsFactors = FALSE)
    to.return$exclude = TRUE
    to.return$this.exclude = this.exclude
    return(to.return) # break early
  }
  
}
# if first word is in capitals and is long then change to dummy; this is a formating style for some journals, e.g., 17836690[pmid] and 12987773[pmid]
if(all.caps[1] == TRUE & nchar(words)[1] > 6 ){words[1]='dummy'}

## further processing
# words = str_remove_all(string=words, pattern="\\b[0-9]+\\b") # remove words that are all numbers 
# words = str_remove_all(string=words, pattern="\\w*[0-9]+\\w*\\s*") # remove words that are all numbers or a mix of words and numbers
words = str_replace_all(string=words, pattern=roman.numerals, replacement='dummy') # replace Roman numerals (see above); Replaced with 'dummy' so that word count is not effected
words = str_replace_all(string=words, pattern=roman.numerals.th, replacement='dummy') # replace Roman numerals with 'th' (see above); Replaced with 'dummy' so that word count is not effected
words = str_replace_all(string=words, pattern=roman.numerals.letters, replacement='dummy') # replace Roman numerals with letters (see above); Replaced with 'dummy' so that word count is not effected
words = str_replace_all(string=words, pattern=chromosomes, replacement='dummy') # replace chromosomes (see above); Replaced with 'dummy' so that word count is not effected
words = words[words!=''] # remove blank words
# replace gene sequences; they come in bunches of three's 
gcount = (str_count(words, pattern='A|T|C|G') == nchar(words)) & (nchar(words) %in% seq(3,21,3))
if(any(gcount)==TRUE){words[gcount] = 'dummy'} # Replaced with 'dummy' so that word count is not effected
n.words = length(words) # word count (after replacements of Roman numerals, etc)
if(n.words <= 1){ # skip to next if title is just one word
  this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Short title', stringsAsFactors = FALSE)
  to.return$exclude = TRUE
  to.return$this.exclude = this.exclude
  return(to.return) # break early
} 
# find the acronyms; count the number of upper case letters per word
words.length = nchar(words) # length of each word
nwords = nchar(str_remove_all(string=words, pattern='[^0-9]')) # number of numbers
lwords = nchar(str_remove_all(string=words, pattern='[^a-z]')) # number of lower case letters
uwords = nchar(str_remove_all(string=words, pattern='[^A-Z]')) # number of upper case letters
# acronym if more upper case than lower and numbers combined
acronym.match = (uwords > (lwords+nwords)) & uwords>=2 # more upper case words than lower case

# previous
#acronym.match.2 = (uwords==2) & (words.length == 2) # two letters, both upper case
#acronym.match.3 = (words.length <= (uwords+1)) & (words.length >= 3) # allow one non upper case letter; just acronyms of 3+ letters 
#acronym.match = acronym.match.2| acronym.match.3

# store acronyms in separate data set
aframe = NULL
if (any(acronym.match)){
  aframe = data.frame(pmid=raw_pubmed$pmid[k], acronyms = words[acronym.match], stringsAsFactors = FALSE) %>%
    mutate(acronyms = str_remove(string=acronyms, pattern='s$'), # convert any plurals to single
           nchar = nchar(acronyms)) # number of characters
}

# return the results
tframe = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], jabbrv=indata$jabbrv[k], n.authors=indata$n.authors[k],
                    n.words=n.words, stringsAsFactors = FALSE)
to.return$tframe = tframe
to.return$aframe = aframe
to.return$this.exclude = this.exclude
return(to.return)

}
