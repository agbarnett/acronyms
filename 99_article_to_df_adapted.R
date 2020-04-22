# 99_article_to_df_adapted.R
# process a single article
# adapted code from github, https://github.com/cran/easyPubMed
# called by 99_table_articles_byAuth_adapted.R
# documentation for pubmed elements is here https://dtd.nlm.nih.gov/ncbi/pubmed/doc/out/180101/index.html
# April 2020

article_to_df_adapted <-
  function(pubmedArticle) 
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
    
    # Get started
    tryCatch({
      
      tmp.article <- custom_grep(xml_data = pubmedArticle, tag = "PubmedArticle", format = "char") # extract the article
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
      
      # first author's surname
      first.author = ''
      if(n.authors > 0){
        tmp.author <- custom_grep(xml_data = tmp.article, tag = "Author", format = "char")
        tmp.last <- custom_grep(xml_data = tmp.author, tag = "LastName", format = "char")
        first.author = ifelse(is.null(tmp.last)==TRUE, '', tmp.last)
      }
      
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
      
      # Language, only want English (is excluded later so we can see numbers)
      # had to use my own search because "Language" was being picked up in abstract ...
      # ... specific code not working: tmp.language <- custom_grep(xml_data = tmp.article, tag = "Language", format = "char")
      language.loc = str_locate_all(pattern='\\<Language\\>', tmp.article)
      tmp.language <- str_sub(string = tmp.article, start=language.loc[[1]][2]+1, end=language.loc[[1]][2]+3) # extract three letter language
      tmp.language = tolower(tmp.language) # make sure it is lower case
      if(nchar(tmp.language)!=3){tmp.language = 'Missing'}

      ## Abstract & Copyright
      # remove <OtherAbstract>, which can be abstract in another language - had to do whilst still in XML format
      abstract.other.location.start <- str_locate_all(tmp.article, pattern='\\<OtherAbstract')[[1]]
      if(length(abstract.other.location.start)>0 & is.na(abstract.other.location.start)[1] == FALSE){
        n.other = nrow(abstract.other.location.start)
        abstract.other.location.end <- str_locate_all(tmp.article, pattern='\\<\\/OtherAbstract')[[1]]
        first.string = str_sub(tmp.article, start = 1, end = abstract.other.location.start[1,1]-1)
        last.string = str_sub(tmp.article, start = abstract.other.location.end[n.other,2], end = nchar(tmp.article))
        tmp.article = paste(first.string, last.string) # create new article
      }  

      # get "raw" abstract from rest of entry
      abstract.raw = ''
      abstract.start <- str_locate(tmp.article, pattern='\\<Abstract') # first mention
      if(length(abstract.start)>0 & is.na(abstract.start)[1] == FALSE){
        abstract.end = str_locate_all(tmp.article, pattern='\\<\\/Abstract')[[1]]
        abstract.end = abstract.end[nrow(abstract.end),] # last ending
        abstract.raw = str_sub(tmp.article, start=abstract.start[1], end=abstract.end[2]+1)
      }  
      
      # remove systematic review registrations, e.g., 28320459[PMID] - test17.xml
      abstract.registration.location.start <- str_locate(abstract.raw, pattern='SYSTEMATIC REVIEW REGISTRATION')[1]
      if(!is.na(abstract.registration.location.start)){
        abstract.registration.location.end = str_locate_all(abstract.raw, pattern='\\<\\/AbstractText\\>')[[1]]
        end = min(abstract.registration.location.end[abstract.registration.location.end[,2] > abstract.registration.location.start, 2]) # find the next "/AbstractText" after the systematic review
        first.string = str_sub(abstract.raw, start = 1, end = abstract.registration.location.start-22) # minus 22 to also remove <AbstractText>
        last.string = str_sub(abstract.raw, start = end+1, end = nchar(abstract.raw))
        abstract.raw = paste(first.string, last.string) # create new article
      }  
      
      # remove abstract sub-headings
      sub.headings.start <- str_locate(abstract.raw, pattern='\\<AbstractText') # find first
      if(is.na(sub.headings.start)[1] == FALSE){ # first remove all </AbstractText> 
        abstract.raw = str_remove_all(string=abstract.raw, pattern='\\<\\/AbstractText\\>')
      }
      while(is.na(sub.headings.start)[1] == FALSE){
        first = str_sub(abstract.raw, start = 1, end = sub.headings.start[1,1]-1)
        second = str_sub(abstract.raw, start = sub.headings.start[1,2]+1, end = nchar(abstract.raw))
        next.ending = str_locate(second, '>') # now find the next ending (varying because of label length)
        second = str_sub(second, next.ending[1]+1, nchar(second))
        abstract.raw = paste(first, second)
        sub.headings.start <- str_locate(abstract.raw, pattern='\\<AbstractText') # find next, if there is one
      }  
      
      # now extract abstract and leave behind <CopyrightInformation>
      tmp.abstract <- custom_grep(xml_data = abstract.raw, tag = "Abstract", format = "char")
      if (is.null(tmp.abstract) == FALSE){
        tmp.abstract <- paste(tmp.abstract, collapse = " ", sep = " ") # if it is a list
        # remove Copyright, sometimes ends up in abstract
        tmp.copyright <- custom_grep(xml_data = tmp.article, tag = "CopyrightInformation", format = "char")
        if(is.null(tmp.copyright)==FALSE){tmp.abstract = str_remove(fixed(tmp.abstract), pattern=fixed(tmp.copyright))}
      } else if (is.null(tmp.abstract) == TRUE) {tmp.abstract <- NA}

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
                      first.author = first.author,
                      title = tmp.title,
                      abstract = tmp.abstract,
                      date = date,
                      type = tmp.type,
                      jabbrv = tmp.jabbrv, stringsAsFactors = FALSE)
      
      return(final.mat)
    })

  }
