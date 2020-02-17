# 99_article_to_df_adapted.R
# process a single article
# adapted code from github, https://github.com/cran/easyPubMed
# called by 99_table_articles_byAuth_adapted.R
# documentation for pubmed elements is here https://dtd.nlm.nih.gov/ncbi/pubmed/doc/out/180101/index.html
# January 2020

article_to_df_adapted <-
  function(pubmedArticle, 
           max_chars = 500) 
  {
    #
    options(warn = -1)
    
    # Global Check!
    if (class(pubmedArticle) != "character" |
        regexpr("(<PubmedArticle)(.+)(\\/PubmedArticle>)", pubmedArticle) < 0 )
    {
      message("An error occurred")
      return(NULL)
    }
    
    # max_chars Check
    if (!is.numeric(max_chars)) {
      max_chars <- 500
    } else if (max_chars < 0) {
      max_chars <- -1  
    }
    
    # Get started
    tryCatch({
      
      tmp.article <- custom_grep(xml_data = pubmedArticle, tag = "PubmedArticle", format = "char")
      if (is.null(tmp.article)) 
      {
        message("An error occurred")
        return(NULL)
      }
      
      # Fetch ID string
      tmp.paperID  <- custom_grep(xml_data = tmp.article, tag = "ArticleIdList", format = "char")
      if (is.null(tmp.paperID)) 
      {
        message("An error occurred")
        return(NULL)
      } else {
        tmp.paperID <- gsub("[[:space:]]", "", tmp.paperID[1])
      }
      
      # Get PMID
      tmp.PMID <- gsub("^(.*ArticleIdIdType=\\\"pubmed\\\")([[:space:]]|[[:alnum:]]){0,20}>", "", tmp.paperID)
      tmp.PMID <- gsub("<.*$", "", tmp.PMID)

      # number of authors
      n.authors = str_count(string=tmp.article, pattern="\\<\\/Author\\>")
      
      # Title
      tmp.title <- custom_grep(xml_data = tmp.article, tag = "ArticleTitle", format = "char")
      if (length(tmp.title) > 1){
        tmp.title <- paste(tmp.title, collapse = " ", sep = " ")
      } else if (length(tmp.title) < 1) {
        # use my own search because of danger of "Title" appearing elsewhere
        title.loc = str_locate(pattern='\\<Title\\>', tmp.article)
        title.loc.end = str_locate(pattern='\\</Title\\>', tmp.article)
        tmp.title = str_sub(string=tmp.article, start = title.loc[2]+1, end=title.loc.end[1]-1)
        if (length(tmp.title) > 1){
          tmp.title <- NA # if title and alternative title are missing
        }
      }
      
      # Language, only want English, will exclude later
      # had to use my own search because "Language" was being picked up in abstract ...
      # ... specific code not working: tmp.language <- custom_grep(xml_data = tmp.article, tag = "Language", format = "char")
      language.loc = str_locate_all(pattern='\\<Language\\>', tmp.article)
      tmp.language <- str_sub(string = tmp.article, start=language.loc[[1]][2]+1, end=language.loc[[1]][2]+3) # extract three letter language
      tmp.language = tolower(tmp.language) # make sure it is lower case
      if(nchar(tmp.language)!=3){tmp.language = 'Missing'}

      ## Abstract & Copyright
      # first remove <OtherAbstract>, which can be abstract in another language
      abstract.other.location.start <- str_locate_all(tmp.article, pattern='\\<OtherAbstract')[[1]]
      if(length(abstract.other.location.start)>0){
          n.other = nrow(abstract.other.location.start)
          abstract.other.location.end <- str_locate_all(tmp.article, pattern='\\<\\/OtherAbstract')[[1]]
          first.string = str_sub(tmp.article, start = 1, end = abstract.other.location.start[1,1]-1)
          last.string = str_sub(tmp.article, start = abstract.other.location.end[n.other,2], end = nchar(tmp.article))
          tmp.article = paste(first.string, last.string) # create new article
      }  
      # now extract abstract
      tmp.abstract <- custom_grep(xml_data = tmp.article, tag = "AbstractText", format = "char")
      if (length(tmp.abstract) > 1){
        tmp.abstract <- paste(tmp.abstract, collapse = " ", sep = " ")
        # remove Copyright, sometimes ends up in abstract
        tmp.copyright <- custom_grep(xml_data = tmp.article, tag = "CopyrightInformation", format = "char")
        if(is.null(tmp.copyright)==FALSE){tmp.abstract = str_remove(fixed(tmp.abstract), pattern=fixed(tmp.copyright))}
        if(max_chars >= 0) {
          tmp.abstract <- gsub("</{0,1}i>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}b>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}sub>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}exp>", "", tmp.abstract, ignore.case = T)
          
          tmp.abstract <- substr(tmp.abstract, 0, max_chars)
        }
      } else if (length(tmp.abstract) < 1) {
        tmp.abstract <- NA
      } else {
        if(max_chars >= 0) {
          tmp.abstract <- substr(tmp.abstract, 0, max_chars)
          tmp.abstract <- gsub("</{0,1}i>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}b>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}sub>", "", tmp.abstract, ignore.case = T)
          tmp.abstract <- gsub("</{0,1}exp>", "", tmp.abstract, ignore.case = T)
          
        }
      }
      
      ## Dates, use pubmed date
      dstart = str_locate(string=tmp.article, pattern='PubMedPubDate PubStatus="pubmed"')
      tmp.date = str_sub(string=tmp.article, start = dstart[2]+2, end=nchar(tmp.article)) # start from pubmed date so that next end of pubmed date work
      dend = str_locate(string=tmp.date, pattern='\\/PubMedPubDate')
      tmp.date = str_sub(string=tmp.date, start = 1, end=dend[1]-2) # now go to next end
      ystart = str_locate(string=tmp.date, pattern='\\<Year\\>') # year
      year = as.numeric(str_sub(tmp.date, ystart[2]+1, ystart[2]+4))
      mstart = str_locate(string=tmp.date, pattern='\\<Month\\>') # month
      mend = str_locate(string=tmp.date, pattern='\\<\\/Month\\>')
      month = as.numeric(str_sub(tmp.date, mstart[2]+1, mend[1]-1))
      dstart = str_locate(string=tmp.date, pattern='\\<Day\\>') # day
      dend = str_locate(string=tmp.date, pattern='\\<\\/Day\\>')
      day = as.numeric(str_sub(tmp.date, dstart[2]+1, dend[1]-1))
      if(is.na(month) == TRUE){month=6} # middle of year if month missing
      if(is.na(day) == TRUE){day = 15} # middle of month if day missing
      date = as.Date(ISOdate(year=year, 
                             month=month, 
                             day=day)) # convert day/month/year to date

      # Get Journal Abbrv
      tmp.jabbrv  <- custom_grep(xml_data = tmp.article, tag = "ISOAbbreviation", format = "char")
      tmp.jabbrv <- ifelse(is.null(tmp.jabbrv), NA, tmp.jabbrv)
      tmp.jabbrv = str_remove_all(string=tmp.jabbrv, pattern='\\.') # tidy journal names to make them a bit shorter
      
      # Get article type, see list here https://www.nlm.nih.gov/mesh/pubtypes.html
      tmp.type  <- custom_grep(xml_data = tmp.article, tag = "PublicationType", format = "char")
      tmp.type <- ifelse(is.null(tmp.type), NA, tmp.type)
      
      # frame to output
      final.mat <- data.frame(pmid = tmp.PMID, 
                      language = tmp.language,
                      n.authors = n.authors,
                      title = tmp.title,
                      abstract = tmp.abstract,
                      date = date,
                      type = tmp.type,
                      jabbrv = tmp.jabbrv, stringsAsFactors = FALSE)
      
      # Final check and return only if all elements are present
      if (ncol(final.mat) != 8) { # number of variables
        # export lost PMIDs
        ofile = file('U:/Research/Projects/ihbi/aushsi/aushsi_barnetta/meta.research/text.mining/acronyms/LostPMIDs.txt', 'a') # append
        cat(tmp.PMID, '\n', file=ofile)
        close(ofile)
        #
        final.mat <- NULL
      }
    }, error = function(e) {NULL}, 
    finally = {
      options(warn = 0)
      return(final.mat)
    })
  }
