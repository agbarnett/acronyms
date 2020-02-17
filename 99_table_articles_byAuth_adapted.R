# 99_table_articles_byAuth_adapted.R
# adapted code from github, https://github.com/cran/easyPubMed
# January 2020

table_articles_byAuth_adapted <-
  function (pubmed_data, 
            max_chars = 500, 
            encoding = "UTF-8") 
  {
    message("Processing PubMed data ", appendLF = FALSE)
    
    paper.data <- articles_to_list(pubmed_data = pubmed_data, encoding = encoding) # convert to list
    N = length(paper.data) # number of papers to start with
    
    expFields <- c("pmid", "title", "language", "n.authors", "abstract", "year", "month", "day", "jabbrv")
    papers.authors.list <- lapply(1:length(paper.data), (function(i) {
      if (length(paper.data) > 50) {
        rep.dot <- as.integer(seq(1, length(paper.data), 
                                  length.out = 50))
        if (i %in% rep.dot) 
          message(".", appendLF = FALSE)
      } else {
        message(".", appendLF = FALSE)
      }
      art <- paper.data[[i]]
      out <- tryCatch({article_to_df_adapted(pubmedArticle = art,  # call function from 99_article_to_df_adapted
                                     max_chars = max_chars)}, 
                      error = function(e) { NULL })
      
      if (is.null(out) == TRUE) { # nothing returned
        out <- data.frame(pmid = NA, language=NA, title = NA, 
                          abstract = NA, date = NA, 
                          jabbrv = NA, type=NA)
      }
      out <- out[1, ] # just first row (first author)

    }))
    message(" done!")
    
    papers.authors.df <- do.call(rbind, papers.authors.list)
    keep.rw <- apply(papers.authors.df, 1, (function(rw) {
      sum(is.na(rw)) < length(rw)
    }))
    papers.authors.df <- papers.authors.df[keep.rw, ]
    
    # return data and original sample size
    to.return = list()
    to.return$N = N
    to.return$papers.authors.df = papers.authors.df
    return(to.return)
  }
