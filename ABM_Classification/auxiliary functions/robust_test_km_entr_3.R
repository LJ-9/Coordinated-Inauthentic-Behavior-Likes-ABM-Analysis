# entropy 3# KMeans clustering with SVD and Entropy--------------------------------------------------------------------------------

km_entropy_3 <- function(dataset,dim1,dim2,n_agents,size){
  
  # dim(dataset)
  # Prelims
  #edat <- dataset
  # dim(edat)
  # relevant nefarious agents
  nef <- c(1:(size+300))
  clusterdata <- cbind(dim1[nef],dim2[nef]) # dim1 and dim 2,  calculates  eigenvector of dimension 1 (ie column 1 of all eigenvectors from svd(u), times (matrixmultiplication (diag(eigenvalue), ie multiple with I*iegenvalue) eigenvalue of dim 1 (there is one for each dim, )
  # dim(clusterdata)
  rownames(clusterdata) <- nef
  
  
  # clusts = kmeans(clusterdata,2, trace=FALSE) # cluster vector
  
  
  
  kmeans_more <- function(x,centers,iter.max){kmeans(x,centers, iter.max=60)} # specify more iterations to help convergence
  gap_fail_entr_ <-0
  tgap<-clusGap((scale(clusterdata, center = TRUE, scale=TRUE)), kmeans_more, 20, B = 50,verbose = FALSE, d.power = 2)
  nk<-maxSE(f = tgap$Tab[, "gap"], SE.f = tgap$Tab[, "SE.sim"],method="Tibs2001SEmax")
  if (nk == 1){ 
    
    # make sure we don't by accident/by seed end up with 1 cluster chosen by gap stat. This would cause an error (logr not found) in the log regression function. 
    # Make sure we get stable results that at least identify 2 clusters as ideal with 500 bootstraps, which is when: The main result <res>$Tab[,"gap"] of course is from bootstrapping aka Monte Carlo simulation 
    # and hence random, or equivalently, depending on the initial random seed (see set.seed()). On the other hand, in our experience, using B = 500 gives quite precise results such that the gap plot
    # is basically unchanged after an another run. https://rdrr.io/cran/cluster/man/clusGap.html 
    # 500 bootstraps cost runtime
    
    tgap<-clusGap((scale(clusterdata, center = TRUE, scale=TRUE)), kmeans_more, 20, B = 500,verbose = FALSE, d.power = 2)
    nk<-maxSE(f = tgap$Tab[, "gap"], SE.f = tgap$Tab[, "SE.sim"],method="Tibs2001SEmax")
  }
  
  # https://stats.stackexchange.com/questions/140711/why-does-gap-statistic-for-k-means-suggest-one-cluster-even-though-there-are-ob 
  
  if (nk == 1){ # if still 1 cluster picked, last resort: force kmeans to pick 2 clusters to make log regr work then
    clusts <- kmeans((scale(clusterdata, center = TRUE, scale=TRUE)),2,iter.max = 60)
    gap_fail_entr_ <- 1 } else {
      clusts<-kmeans((scale(clusterdata, center = TRUE, scale=TRUE)),nk,iter.max = 60)}
  
  
  
  # Get clustering
  classification = clusts$cluster
  
  #check which labels are present in cluster (sometimes some dont make it for classificaetion dueto low probability, then e run into troubles later)
  
  cl<-as.data.frame(table(classification))
  
  chaos <- list()
  
  overall <- data.frame(matrix(ncol = 2, nrow = 0))
  nams <- c("agentid", "entr")
  colnames(overall) <- nams
  
  
  # get entropy measures for satisfactorily small clustered clusters
  source(here("ABM_Classification/auxiliary functions/entropy_new.R"))
  
  chaos <- En(cl, dataset,classification)
  # get names when 
  
  # create newlybefore entering loop
  single <- data.frame(matrix(ncol = 2, nrow = 0))
  nams <- c("agentid", "entr")
  colnames(single) <- nams
  names(overall) <- names(single)
  
  for (i in 1:nrow(cl)){
    
    eachcl <- data.frame()
    agentid <- as.numeric(names(classification)[classification == cl[i,1]])
    entr <- as.numeric(rep(chaos[i],length(agentid))  )
    
    eachcl <- data.frame(agentid,entr)
    nams <- c("agentid", "entr")
    colnames(eachcl) <- nams
    
    names(single) <- names(eachcl)
    single <- rbind(single,eachcl)
    
  } #end loop collect entropies for clusters and agent ids
  # append to overall dataframe
  
  overall <- rbind(overall, single)    #appending
  
  # dim(overall)
  head(overall)
  
  overall_ordered <- overall[order(overall$agentid),]
  #overall_ordered[group_7]
  #class[overall_ordered]
  
  clusts  <- kmeans(cbind(dim1[nef],overall_ordered[,2]),2,iter.max=60) 
  # length(overall_ordered[,2])
  # length(dim1[nef])
  #clusts = kmeans(overall_ordered[,2],2, trace=TRUE) # cluster vector 
  
  classification = clusts$cluster
  
  #check which labels are present in cluster (sometimes some dont make it for classificaetion dueto low probability, then e run into troubles later)
  
  names(classification) <- rownames(clusterdata)
  classification = as.numeric(classification)
  cl<-as.data.frame(table(classification))
  
  
  
  if ((mean(overall_ordered$entr[classification == cl[1,1]])) < (mean(overall_ordered$entr[classification == cl[2,1]]))){
    classification[classification == cl[1,1]] <- 'B' # so subsetting does not get messy
    classification[classification != 'B'] <- 1
    classification[classification == 'B'] <- 2
  } else {
    classification[classification == cl[1,1]] <- 'I' # so subsetting does not get messy
    classification[classification != 'I'] <- 2
    classification[classification == 'I'] <- 1
  }
  
  classification = as.numeric(classification)
  returne <- list(classification = classification, gap_fail_entr_ = gap_fail_entr_)
  return(returne)
  
  #return(classification)
}












