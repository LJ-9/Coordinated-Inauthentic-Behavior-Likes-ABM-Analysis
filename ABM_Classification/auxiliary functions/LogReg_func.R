LR <- function(dataset, classification,cl,pub){
  
  if (cl[1,2] == 1){cluster1 = (dataset[,classification==cl[1,1]]) } else { 
  cluster1 = apply(dataset[,classification==cl[1,1]],1,mean)} # mean of cluster 1 agents votes
  
  if (cl[2,2] == 1){cluster2 = (dataset[,classification==cl[2,1]]) } else { 
    cluster2 = apply(dataset[,classification==cl[2,1]],1,mean)} # mean of cluster 2 agents votes 


  
  if (nrow(cl)==2){ #if K = 2
    
    #if ((3 %in% classification & 4 %in% classification)== FALSE){ #if K = 2
    logr_lasso <- 0
    logr <- 0
    tryCatch( # if glm returns warning, use lasso glm instead
      logr <-glm(pub ~ cluster1 + cluster2, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
    #logr_bayes <-bayesglm(pub ~ cluster1 + cluster2 + cluster3 + cluster4, family = binomial) # bayesglm also solves the perfect separation problem, opted for glmnet though, (bayesglm:arm package)
  }
  
  # if (K == 3){
  # if (((4 %in% classification == FALSE) & (3 %in% classification == TRUE)) == TRUE){
  if (nrow(cl)==3){ #if K = 3
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
    
  }
  # if (K == 4){
  # if ((4 %in% classification & 3 %in% classification) == TRUE){
  if (nrow(cl)==4){ #if K = 4
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==5){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==6){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
   
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==7){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
  
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==8){ #if K = 5
    
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean)} # mean of cluster 8 agents votes
      
      
      
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  
  
  if (nrow(cl)==9){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean)} # mean of cluster 8 agents votes
      
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
        cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
      
      
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  
  if (nrow(cl)==10){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
      
      if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
        cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
      
      if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
        cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
      
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==11){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
      
      if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
        cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
      
      if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
        cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
      
      if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
        cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
      
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  
  if (nrow(cl)==12){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
      
      if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
        cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
      
      if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
        cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
      
      if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
        cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
      
      if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
        cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
      
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  if (nrow(cl)==13){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
      
      if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
        cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
      
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==14){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
      
      if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
        cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
      
  
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==15){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
    
    if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
      cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
      
      if (cl[15,2] == 1){cluster15 = (dataset[,classification==cl[15,1]]) } else { 
        cluster15 = apply(dataset[,classification==cl[15,1]],1,mean) }
      
     
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14 + cluster15, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14,cluster15), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  
  if (nrow(cl)==16){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
    
    if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
      cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
    
    if (cl[15,2] == 1){cluster15 = (dataset[,classification==cl[15,1]]) } else { 
      cluster15 = apply(dataset[,classification==cl[15,1]],1,mean) }
      
      if (cl[16,2] == 1){cluster16 = (dataset[,classification==cl[16,1]]) } else { 
        cluster16 = apply(dataset[,classification==cl[16,1]],1,mean) }
      
      
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14 + cluster15 + cluster16, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14,cluster15,cluster16), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==17){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
    
    if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
      cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
    
    if (cl[15,2] == 1){cluster15 = (dataset[,classification==cl[15,1]]) } else { 
      cluster15 = apply(dataset[,classification==cl[15,1]],1,mean) }
    
    if (cl[16,2] == 1){cluster16 = (dataset[,classification==cl[16,1]]) } else { 
      cluster16 = apply(dataset[,classification==cl[16,1]],1,mean) }
      
      if (cl[17,2] == 1){cluster17 = (dataset[,classification==cl[17,1]]) } else { 
        cluster17 = apply(dataset[,classification==cl[17,1]],1,mean) }
      
     
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14 + cluster15 + cluster16 + cluster17, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14,cluster15,cluster16,cluster17), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  
  if (nrow(cl)==18){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
    
    if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
      cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
    
    if (cl[15,2] == 1){cluster15 = (dataset[,classification==cl[15,1]]) } else { 
      cluster15 = apply(dataset[,classification==cl[15,1]],1,mean) }
    
    if (cl[16,2] == 1){cluster16 = (dataset[,classification==cl[16,1]]) } else { 
      cluster16 = apply(dataset[,classification==cl[16,1]],1,mean) }
    
    if (cl[17,2] == 1){cluster17 = (dataset[,classification==cl[17,1]]) } else { 
      cluster17 = apply(dataset[,classification==cl[17,1]],1,mean) }
      
    if (cl[18,2] == 1){cluster18 = (dataset[,classification==cl[18,1]]) } else { 
        cluster18 = apply(dataset[,classification==cl[18,1]],1,mean) }
      
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14 + cluster15 + cluster16 + cluster17 + cluster18, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14,cluster15,cluster16,cluster17,cluster18), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  
  if (nrow(cl)==19){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
    
    if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
      cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
    
    if (cl[15,2] == 1){cluster15 = (dataset[,classification==cl[15,1]]) } else { 
      cluster15 = apply(dataset[,classification==cl[15,1]],1,mean) }
    
    if (cl[16,2] == 1){cluster16 = (dataset[,classification==cl[16,1]]) } else { 
      cluster16 = apply(dataset[,classification==cl[16,1]],1,mean) }
    
    if (cl[17,2] == 1){cluster17 = (dataset[,classification==cl[17,1]]) } else { 
      cluster17 = apply(dataset[,classification==cl[17,1]],1,mean) }
    
    if (cl[18,2] == 1){cluster18 = (dataset[,classification==cl[18,1]]) } else { 
      cluster18 = apply(dataset[,classification==cl[18,1]],1,mean) }
      
    if (cl[19,2] == 1){cluster19 = (dataset[,classification==cl[19,1]]) } else { 
        cluster19 = apply(dataset[,classification==cl[19,1]],1,mean) }
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14 + cluster15 + cluster16 + cluster17 + cluster18 + cluster19, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14,cluster15,cluster16,cluster17,cluster18,cluster19), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  if (nrow(cl)==20){ #if K = 5
    
    if (cl[3,2] == 1){cluster3 = (dataset[,classification==cl[3,1]]) } else { 
      cluster3 = apply(dataset[,classification==cl[3,1]],1,mean)} # mean of cluster 3 agents votes 
    
    if (cl[4,2] == 1){cluster4 = (dataset[,classification==cl[4,1]]) } else { 
      cluster4 = apply(dataset[,classification==cl[4,1]],1,mean)} # mean of cluster 4 agents votes
    
    if (cl[5,2] == 1){cluster5 = (dataset[,classification==cl[5,1]]) } else { 
      cluster5 = apply(dataset[,classification==cl[5,1]],1,mean)} # mean of cluster 5 agents votes
    
    if (cl[6,2] == 1){cluster6 = (dataset[,classification==cl[6,1]]) } else { 
      cluster6 = apply(dataset[,classification==cl[6,1]],1,mean)} # mean of cluster 6 agents votes
    
    if (cl[7,2] == 1){cluster7 = (dataset[,classification==cl[7,1]]) } else { 
      cluster7 = apply(dataset[,classification==cl[7,1]],1,mean)} # mean of cluster 7 agents votes
    
    if (cl[8,2] == 1){cluster8 = (dataset[,classification==cl[8,1]]) } else { 
      cluster8 = apply(dataset[,classification==cl[8,1]],1,mean) }# mean of cluster 8 agents votes
    
    if (cl[9,2] == 1){cluster9 = (dataset[,classification==cl[9,1]]) } else { 
      cluster9 = apply(dataset[,classification==cl[9,1]],1,mean) }
    
    if (cl[10,2] == 1){cluster10 = (dataset[,classification==cl[10,1]]) } else { 
      cluster10 = apply(dataset[,classification==cl[10,1]],1,mean) }
    
    if (cl[11,2] == 1){cluster11 = (dataset[,classification==cl[11,1]]) } else { 
      cluster11 = apply(dataset[,classification==cl[11,1]],1,mean) }
    
    if (cl[12,2] == 1){cluster12 = (dataset[,classification==cl[12,1]]) } else { 
      cluster12 = apply(dataset[,classification==cl[12,1]],1,mean) }
    
    if (cl[13,2] == 1){cluster13 = (dataset[,classification==cl[13,1]]) } else { 
      cluster13 = apply(dataset[,classification==cl[13,1]],1,mean) }
    
    if (cl[14,2] == 1){cluster14 = (dataset[,classification==cl[14,1]]) } else { 
      cluster14 = apply(dataset[,classification==cl[14,1]],1,mean) }
    
    if (cl[15,2] == 1){cluster15 = (dataset[,classification==cl[15,1]]) } else { 
      cluster15 = apply(dataset[,classification==cl[15,1]],1,mean) }
    
    if (cl[16,2] == 1){cluster16 = (dataset[,classification==cl[16,1]]) } else { 
      cluster16 = apply(dataset[,classification==cl[16,1]],1,mean) }
    
    if (cl[17,2] == 1){cluster17 = (dataset[,classification==cl[17,1]]) } else { 
      cluster17 = apply(dataset[,classification==cl[17,1]],1,mean) }
    
    if (cl[18,2] == 1){cluster18 = (dataset[,classification==cl[18,1]]) } else { 
      cluster18 = apply(dataset[,classification==cl[18,1]],1,mean) }
    
    if (cl[19,2] == 1){cluster19 = (dataset[,classification==cl[19,1]]) } else { 
      cluster19 = apply(dataset[,classification==cl[19,1]],1,mean) }
     
    if (cl[20,2] == 1){cluster20 = (dataset[,classification==cl[20,1]]) } else { 
    cluster20 = apply(dataset[,classification==cl[20,1]],1,mean) }
    
    logr_lasso <- 0
    logr <- 0
    tryCatch(
      logr <-glm(pub ~ cluster1 + cluster2 + cluster3 + cluster4 + cluster5 + cluster6 + cluster7 + cluster8 + cluster9 + cluster10 + cluster11 + cluster12 + cluster13 + cluster14 + cluster15 + cluster16 + cluster17 + cluster18 + cluster19 + cluster20, family = binomial),
      warning = function(w) { logr_lasso <<- cv.glmnet(cbind(cluster1, cluster2, cluster3, cluster4, cluster5, cluster6, cluster7, cluster8, cluster9, cluster10, cluster11, cluster12, cluster13, cluster14,cluster15,cluster16,cluster17,cluster18,cluster19,cluster20), pub, family = "binomial", alpha = 1, type.measure = "class") }
    )
  }
  
  lasso = list()
  lasso <- 0
  
  if ((length(logr)> 1) == TRUE) {  
    p_val <-coef(summary(logr))[,4]
    p_val = p_val[-1] # discard intercept p-value
    classI = which(p_val <= 0.05)
    
    for (i in classI){
      classification[classification == i] <- 'I'
    }
    
    classification[classification != 'I'] <- 2
    classification[classification == 'I'] <- 1
    classification = as.numeric(classification)
  } else {
    coef_lasso = coef(logr_lasso, s = logr_lasso$lambda.1se)
    coef_lasso = coef_lasso[-1,]
    classB = which(coef_lasso == 0)
    #length(classB)
    for (i in classB){
      classification[classification == i] <- 'B'
    }
    classification[classification != 'B'] <- 1
    classification[classification == 'B'] <- 2
    classification = as.numeric(classification)
    lasso <- 1 # report in output whether glm net with lasso was used as glm logisitic regression did not convert due to perfect separation
  }
  returnc <- list(classification = classification, lasso = lasso)
  return(returnc)
}
