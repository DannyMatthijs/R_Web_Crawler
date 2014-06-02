IMDBScraper <- function() {
    # load require packages
    require(RCurl)
    require(XML)
    
    # I added a wrapper around xpathSApply to deal with cases are NULL and are lost during the list to vector conversion process.
    xpathLVApply <- function(doc, xpath.base, xpath.ext, FUN = I, FUN2 = NULL) {
        # get xpaths to each child node of interest
        nodes.len <- length(xpathSApply(doc, xpath.base))
        paths <- sapply(1:nodes.len, function(i) paste(xpath.base, "[", i, "]", xpath.ext, sep = ""))
        
        # extract child nodes
        xx <- lapply(paths, function(xpath) xpathSApply(doc, xpath, FUN))
        
        # perform extra processing if required
        if(!is.null(FUN2)) xx <- FUN2(xx)
        
        # convert NULL to NA in list
        xx[sapply(xx, length)<1] <- NA
        
        # return node values as a vector
        return(as.vector(unlist(xx)))
    }

    get_imdb_df <- function(f) {
        # parse document into html tree structure
        html <- paste(readLines(f, warn=FALSE), collapse = "\n", sep = "")
        doc <- htmlParse(html)
        
        # xpath to node set of interest       
        xpath.base <-  "//table[@class='results']//tr[contains(concat(' ', normalize-space(@class), ' '), 'detailed')]"        
        # parse Top 50
        title <- xpathLVApply(doc, xpath.base, "/td[@class='title']/a/text()", xmlValue)
        year <- xpathLVApply(doc, xpath.base, "/td[@class='title']/span[@class='year_type']/text()", xmlValue)
	#format year
	for(i in 1:(length(year))){    
	    x <- sub("\\).*", "", year[i])
	    year[i] <- sub(".*\\(", "", x)
	}
        rating <- xpathLVApply(doc, xpath.base, "/td[@class='title']/div[@class='user_rating']//span[@class='rating-rating']/span[@class='value']/text()", xmlValue)
	expr <- paste(xpath.base, "//td[@class='title']/div[@class='user_rating']/div[contains(concat(' ', normalize-space(@class), ' '), 'rating rating-list')]", sep="")
	counts <- xpathSApply(doc, expr, xmlGetAttr, 'title')
	#Get the number
	for(i in 1:(length(counts))){    
	    x <- sub(" votes.*", "", counts[i])
	    counts[i] <- sub(".*\\(", "", x)
	}
	counts <- gsub(",", "", counts, fixed = TRUE)
        summary <- xpathLVApply(doc, xpath.base, "/td[@class='title']/span[@class='outline']/text()", xmlValue)
	actors <- c()
	for(i in 1:(length(title))){
         	expr <- paste("(", xpath.base, "/td[@class='title']/span[@class='credit'])[", i, "]/a[position()>1]/text()", sep='')
         	a <- xpathSApply(doc, expr, xmlValue)    
         	actors[i] <- paste(a, collapse=', ')       
     	}

	director <- xpathLVApply(doc, xpath.base, "/td[@class='title']/span[@class='credit']/a[1]/text()", xmlValue)
	runtimeMinutes = c()     
        for(i in 1:(length(title))){
    		expr <- paste("substring-before((", xpath.base, "/td[@class='title']/span[@class='runtime']/text())[", i, "],' ')", sep='')
    		e <- xpathSApply(doc, expr, xmlValue)    
    		runtimeMinutes[i] <- e       
	}
	
	genres <- c()
	for(i in 1:(length(title))){
         	expr <- paste("(", xpath.base, "/td[@class='title']/span[@class='genre'])[", i, "]/a/text()", sep='')
         	g <- xpathSApply(doc, expr, xmlValue)    
         	genres[i] <- paste(g, collapse=', ')       
     	}
	incomedollarM <- xpathLVApply(doc, xpath.base, "/td[@class='sort_col']/text()", xmlValue)
	#Format income ($)M
	for(i in 1:(length(year))){    
	    x <- sub("M.*", "", incomedollarM[i])
	    incomedollarM[i] <- sub(".*\\$", "", x)
	}
        # pull vectors into a single data.frame
        df <- data.frame(title, year = as.numeric(year), rating = as.numeric(rating), counts = as.numeric(counts), summary, actors, director, runtimeMinutes = as.numeric(runtimeMinutes), genres, incomedollarM = as.numeric(incomedollarM), stringsAsFactors=FALSE)
        
        # free doc from memory (very important as otherwise doc will continue to exit outside of this local function)
        free(doc)
        
        # return df
        return( df )
    }
    
    
    ### main function code ###
    # step 1: read in imdb html file stored
    f <- file.choose()
    
    # step 2: parse imdb webpage
    df <- get_imdb_df(f)

    return(df)
}