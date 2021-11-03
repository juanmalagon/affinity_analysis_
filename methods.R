## This model is created with the help of "recommenderlab" package
## The input needed for the algorithm is a matrix with the items bought by the customers
## Three models are created:
## IBCF - item based collaborative filtering
## UBCF - user based collaborative filtering
## POPULAR - the most popular (bought) items 
## The ratings based on these three methods are calculated and the result is 
## the top 3 recommended items with the associated ratings

obtainRecommendations <- function(df_input_segment, del_cols, nrRecom){
  m = as.integer(nrRecom)
  createRecommendations <- function(df_input_segment){
    ## Divide the Products over multiple columns
    spreadit <- spread(df_input_segment, IDI_PROPOSITION, Totalscore)
    spreadit.m <- as.data.frame(spreadit)
    
    ## Make rownames equal to 1st column (customer number)
    rownames(spreadit.m) <- as.character(spreadit.m[,1])
    
    ## Delete the columns which are not the products bought
    spreadit.m <- spreadit.m[,!(names(spreadit.m) %in% del_cols)]
    spreadit.m <- as.matrix(spreadit.m)
    
    # With coercion, the matrix can be easily converted into a realRatingMatrix object which stores
    # the data in sparse format (only non-NA values are stored explicitly; NA values are represented by a dot).
    spreadit.m <- sapply(data.frame(spreadit.m),as.numeric)
    spreadit.m <- as(spreadit.m, "matrix")
    r <- as(spreadit.m, "realRatingMatrix")
    
    # Check the rating matrix "r" created above:
    #head(getRatingMatrix(r))
    
    #############################################
    ## Method 1: Item based
    ibcfParamsPearson <- list(method="pearson", normalize='center', k=16, alpha=0.5)
    
    ## Make recommender-model
    recIB <- Recommender(r, method = "IBCF", parameter=ibcfParamsPearson)
    
    ## Make predictions
    recomIB <- predict(recIB, r, type="topNList", n=3)
    recomRatingsIB <- predict(recIB, r, type="ratingMatrix", n=3)
    
    ## Make a list of it
    recomibcf <- as(recomIB, "list")
    ratingsibcf <- as(recomRatingsIB, "list")
    
    # Fill out all customers with "n=3" values (if no real value, fill NA)
    recomibcf5 <- lapply(recomibcf, `length<-`, max(lengths(recomibcf)))
    ratingsibcf <- lapply(ratingsibcf, `length<-`, max(lengths(ratingsibcf)))
    
    #############################################
    ## Method 2: User based
    
    ubcfParamsCosine = list(method="Cosine", normalize='center', nn=50)
    
    recUB <- Recommender(r, method="UBCF", parameter=ubcfParamsCosine)
    
    recomUB <- predict(recUB, r, type="topNList", n=3)
    recomRatingsUB <- predict(recUB, r, type="ratingMatrix", n=3)
    
    recomubcf <- as(recomUB, "list")
    ratingsubcf <- as(recomRatingsUB, "list")
    
    recomubcf5 <- lapply(recomubcf, `length<-`, max(lengths(recomubcf)))
    ratingsubcf5 <- lapply(ratingsubcf, `length<-`, max(lengths(ratingsubcf)))
    
    #############################################
    ## Method 3: Popular
    
    recP <- Recommender(r, method = "POPULAR")
    recomP <- predict(recP, r, n=3)
    recompopu5 <- as(recomP, "list")
    recompopu5 <- lapply(recompopu5, `length<-`, max(lengths(recompopu5)))
    
    
    #############################################
    ## Calculate rating: combinations of the 3 previous methods
    
    recomsamen <- recomibcf5
    
    ## We have calculated the average revenue for different orders from IB and UB:
    ## Avg. expected turnover per person is highest at IB [1] -> UB [1] -> IB [2] etc
    # IB first: 366.085
    # UB first: 356.281
    # IB 1,2,3: 339.629
    # UB 1,2,3: 304.26
    
    ## This function combines IB, UB and popular
    
    ## IB -> UB
    for(i in 1:length(recomsamen)) {
      
      top9 <- c(recomibcf5[[i]][1], recomubcf5[[i]][1],
                recomibcf5[[i]][2], recomubcf5[[i]][2],
                recomibcf5[[i]][3], recomubcf5[[i]][3],
                recompopu5[[i]][1], recompopu5[[i]][2], recompopu5[[i]][3]
      )
      
      
      top3 <-  c(unique(top9[!is.na(top9)]), NA, NA, NA)[1:m]
      
      # Get the corresponding ratings
      ratingsIB <- sort(ratingsibcf[[i]], decreasing = T)[1:m]
      names(ratingsIB) <- NULL
      ratingsIB[ratingsIB==0] <- NA
      
      ratingsUB <- sort(ratingsubcf[[i]], decreasing = T)[1:m]
      names(ratingsUB) <- NULL
      ratingsUB[ratingsUB==0] <- NA
      
      rat9 <- c(ratingsIB[1], ratingsUB[1],
                ratingsIB[2], ratingsUB[2],
                ratingsIB[3], ratingsUB[3]
      )
      
      rat3 <-  c(rat9[!is.na(top9)], NA, NA, NA)[1:m]
      
      for(j in 1:m) {
        recomsamen[[i]][j] <- top3[j]
      }
      for(j in 1:m) {
        recomsamen[[i]][j+m] <- rat3[j]
      }
      
    }
    
    recom <- list(spreadit = spreadit,
                  recomsamen = recomsamen)
    return(recom)
  }
  
  recom <- createRecommendations(df_input_segment)
  
  #############################################
  ## Create the output and addapt the structure to be used in a dashboard
  createOutput <- function(recom){
    
    df.output <- t(as.data.frame(recom$recomsamen))
    df.output <- as.data.frame(df.output)
    #rownames(df.output) <- gsub("X", "", rownames(df.output))
    
    # SETTING NEW ITEM COLUMN NAMES
    new_item_col_names <- c()
    for (i in 1:m){
      new_item_col_names <- c(new_item_col_names, paste0("ITEM_", as.character(i)))
    }
    
    # SETTING NEW RATINGS COLUMN NAMES
    new_rating_col_names <- c()
    for (i in 1:m){
      new_rating_col_names <- c(new_rating_col_names, paste0("RATING_", as.character(i)))
    }
    
    # SETTING NEWLY CREATED CLUMN NAMES TO DATAFRAME
    colnames(df.output) <- c(new_item_col_names, new_rating_col_names)
    
    df.output$IDENTIFIER <- rownames(df.output)
    df.output$IDENTIFIER <- gsub("X", "", df.output$IDENTIFIER)
    rownames(df.output) <- c()
    return(df.output)
    
  }
  
  df.output <- createOutput(recom)
  
  return(df.output)

}