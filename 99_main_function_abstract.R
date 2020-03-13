# 99_main_function_abstract.R
# main function to extract the acronyms from abstracts
# used by 1_process_pubmed.R
# March 2020

## Section 1: key characters (also uses those from 99_main_function_titles.R) ##

# remove these upper case words as acronyms from the abstract (also removed `Graphical Abtract`)
# also removed sub-headings in 0_read_pubmed_xml.R, but some remain
# some short words added with colons as they may also be acronyms
# some found in random checking, e.g., 18688099[pmid] 
bogus.acronyms.abstract = c('ABSTRACT','KEYWORDS','KEY WORDS','PRéCIS',
                            "PHYSICIAN'S QUALIFICATIONS","FACILITY",'PREOPERATIVE COUNSELING AND INFORMED CONSENT','ANESTHESIA','POSTOPERATIVE CARE', # from 18688102[pmid]
                            'WITHDRAWN','CORRIGENDUM','BACKGROUND','INTRODUCTION','OBJECTIVE',
                            'METHODS','METHOD','MATERIAL AND METHODS','MATERIALS AND METHODS','MATERIALS \\& METHODS',
                            'PRACTITIONER POINTS','RANDOMISATION','AIM:','AIMS:','DESIGN:',
                            "HYPOTHETICAL PATIENTS",'PHYSICIAN SUBJECTS','DATA SOURCES',
                            'MEASUREMENTS','EXPERIMENTAL PREPARATIONS','NATURALSIZEFLAG',
                            'SETTING:','SUBJECTS:','PRIMARY OUTCOME','SECONDARY OUTCOMES',
                            "PHYSICIANS' QUALIFICATIONS", 'INDICATIONS FOR BOTULINUM TOXIN', # 18688099[pmid] 
                            'STATISTICAL POWER','DEVELOPMENT WORK','INTERVENTION',
                            'PURPOSE','HYPOTHESIS','PARTICIPANTS','SUMMARY','DATA AVAILABILITY',
                            'SEARCH METHODS','SELECTION CRITERIA','DATA COLLECTION AND ANALYSIS','MAIN RESULTS',"AUTHORS' CONCLUSIONS",
                            'EXPERIMENTAL DESIGH','EXPERIMENTAL DESIGN', # with typo for 12114404
                            'RESULTS','DISCUSSION','CONCLUSION','CONCLUSIONs','IMPORTANCE','UNASSIGNED',
                            'DATA:','IMPLICATIONS FOR HEALTH CARE PROVISION AND USE','AIMS OF THE STUDY',
                            'IMPACT STATEMENT','SIGNIFICANCE','SIGNIFICANCE STATEMENT',
                            'SIGNIFICANCE OF RESULTS','SAMPLE, DESIGN AND MEASUREMENTS',
                            'RECOMMENDATIONS','PRACTICAL APPLICATION','Graphical Abstract',
                            'FORTHCOMING DEVELOPMENTS','MATRIX DEVELOPMENT',
                            'NEW \\& NOTEWORTHY','NEW &amp; NOTEWORTHY','RECOMMENDATION',
                            'PERSPECTIVE','FUNDING:','LAY ABSTRACT','LIMITATIONS',
                            'IMPLICATIONS FOR REHABILITATION','REHABILITATION',
                            'LEVEL OF EVIDENCE','Wiley Periodicals',
                            'This corrects the article DOI', 'This corrects the article',
                            'TRIAL REGISTRATION','ClinicalTrials.gov','CLINICALTRIALS','CLINICALTRIALSGOV',
                            'ABSTRACT TRUNCATED AT \\d+ WORDS','CopyrightInformation','Copyright Information')
if(length(bogus.acronyms.abstract) != length(unique(bogus.acronyms.abstract))){cat('Warning, duplicated sub-heading\n')}
bogus.acronyms.abstract = bogus.acronyms.abstract[order(-nchar(bogus.acronyms.abstract))] # longest to shortest
bogus.acronyms.abstract = paste(bogus.acronyms.abstract, collapse='|')

# remove / from common units so that it only gets counted as one word
units = c('mmol/l','mmol/L','MMOL/L','kg/m2','KG/M2','pg/ml','pg/mL','PG/ML','ng/ml','ng/mL','NG/ML','mg/dl','mg/dL','MG/DL','g/ml','g/mL','G/ML','g2/m','G2/M') # longest to shortest character; all potential cases
units = paste(units, collapse='|')

# replace numbers because they can get linked to words and become wrongly classed as acronyms
numbers.to.replace = c("-\\d+\\.\\d+"," \\d+\\.\\d+", " \\d+", "-\\d+") # replace, e.g., 1.11, 22, -22
numbers.to.replace = paste(numbers.to.replace, collapse='|')

## Section 2: function ##
abstract_acronyms = function(indata, k){

to.return = list()
to.return$exclude = FALSE
  
# process the abstract
abstract = indata$abstract[k]
abstract = str_replace_all(abstract, pattern=to.replace, replacement = ' ') # remove fractions, superscripts, etc; use space so that sub/super-script and word are separated
abstract = str_replace_all(abstract, pattern=bogus.acronyms.abstract, replacement='dummy') # Replaced with 'dummy' so that word count is not effected
abstract = str_replace_all(abstract, pattern=numbers.to.replace, replacement=' number ') # replace numbers that are not part of other words (could replace with 'rebmun' - number backwards - so that denominator can be done with and without numbers)
abstract = str_remove_dots(abstract)  # remove full-stops in acronyms
abstract = str_replace_all(abstract, pattern=units, replacement = ' units ') # replace units
abstract = str_replace_all(string=abstract, pattern="-[0-9]* |-[0-9]*$", replacement=' ') # remove numbers after a hyphen (e.g., 21745015[pmid]); keep numbers without a hyphen because these are often real words
abstract = str_replace_all(string=abstract, pattern=" [0-9]*-|^[0-9]*-", replacement=' ') # remove numbers before a hyphen (e.g., 21744858[pmid])
abstract = str_replace_all(abstract, pattern=punctuation, replacement = ' ') # replace all punctuation as it just gets in the way - need to do after contracting dots
abstract = str_replace_all(abstract, pattern='\\.\\.\\.', replacement = '~') # replace '...' with '~' so it does not get counted as an acronym, e.g. 15299935
# remove symbols, do not add to word count
abstract = str_replace_all(abstract, pattern=symbols, replacement = ' ')
abstract = str_replace_all(abstract, pattern=other.math.symbols, replacement = ' ')

## break the abstract into words
words = str_split(string=abstract, pattern=' ')[[1]]
words = words[words!=''] # remove blank words

## Exclusion 1: are all or most words in capitals? if they are then ignore this abstract because it is the journal's style to use capitals
capital.words = str_remove_all(string=words, pattern='[^A-Z]')
all.caps = nchar(words) == nchar(capital.words) & nchar(words)>1 # exclude single letter words
number.all.capital = sum(all.caps)
proportion.capital = number.all.capital / sum(nchar(words)>1) # denominator of number of words longer than 1
# if proportion in capitals is on or over 0.6 then exclude
this.exclude = NULL
if(length(words)==0 | is.na(proportion.capital)==TRUE){
  this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Empty abstract', stringsAsFactors = FALSE)
  to.return$exclude = TRUE
  to.return$this.exclude = this.exclude
  return(to.return) # break early
} # 
if(proportion.capital >= 0.6){
  if(length(words)>2 | proportion.capital==1){ # skip if there's two words and one of them is in capitals, e.g., 6551089[pmid]
    this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Abstract largely in capitals', stringsAsFactors = FALSE)
    to.return$exclude = TRUE
    to.return$this.exclude = this.exclude
    return(to.return) # break early
  }
} # 

## Exclusion 2: are there groups of four or more capital words? if they are then ignore this abstract because it is a style issue
string.to.test = paste(as.numeric(diff(which(all.caps)) <=1), collapse = '') # gaps between words that are all capital
line.of.four = str_detect(string = string.to.test, pattern='111') # four or more capital words in a line
if(line.of.four == TRUE){
  this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Too many capital words in abstract', stringsAsFactors = FALSE)
  to.return$exclude = TRUE
  to.return$this.exclude = this.exclude
  return(to.return) # break early
} # 

# if first word is in capitals and is long then change to dummy; this is a formating style for some journals
if(all.caps[1] == TRUE & nchar(words)[1] > 6 ){words[1]='dummy'}

## further processing
words = str_replace_all(string=words, pattern=roman.numerals, replacement='dummy') # replace Roman numerals (see above); Replaced with 'dummy' so that word count is not effected
words = str_replace_all(string=words, pattern=roman.numerals.th, replacement='dummy') # replace Roman numerals with 'th' (see above); Replaced with 'dummy' so that word count is not effected
words = str_replace_all(string=words, pattern=roman.numerals.letters, replacement='dummy') # replace Roman numerals with letters (see above); Replaced with 'dummy' so that word count is not effected
words = str_replace_all(string=words, pattern=chromosomes, replacement='dummy') # replace chromosomes (see above); Replaced with 'dummy' so that word count is not effected
words = words[words!=''] # remove blank words
# replace gene sequences, see https://en.wikipedia.org/wiki/Nucleic_acid_notation
gcount = (str_count(words, pattern='A|T|C|G|U|p') == nchar(words)) & (nchar(words) >= 6) # 
if(any(gcount)==TRUE){words[gcount] = 'dummy'} # Replaced with 'dummy' so that word count is not effected
n.words = length(words) # word count (after replacements of Roman numerals, etc)
if(n.words <= 1){ # skip to next if abstract is just one word
  this.exclude = data.frame(pmid=indata$pmid[k], date=indata$date[k], type=indata$type[k], reason = 'Short abstract', stringsAsFactors = FALSE)
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
acronym.match = ((uwords > (lwords+nwords)) & uwords>=2)  | words=='H1N1' # added H1N1 as specific and common rule break

# store acronyms in separate data set
aframe = NULL
if (any(acronym.match)){
  # add to data
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
