# KMeans clustering with SVD and Entropy--------------------------------------------------------------------------------

gmm_entropy_2 <- function(dataset,dim1,dim2,n_agents,size){
  
  # dim(dataset)
  # Prelims
  #edat <- dataset
  #dim(edat)
  # relevant agents
  
  nef <- c(1:(size+300))
  clusterdata <- cbind(dim1[nef],dim2[nef]) # dim1 and dim 2,  calculates  eigenvector of dimension 1 (ie column 1 of all eigenvectors from svd(u), times (matrixmultiplication (diag(eigenvalue), ie multiple with I*iegenvalue) eigenvalue of dim 1 (there is one for each dim, )
  # dim(clusterdata)
  rownames(clusterdata) <- nef
  chaos <- list()
  
  
  BIC <- NULL
  for(j in 2:20){
    rBIC <- mclustBIC(data=clusterdata, verbose = FALSE)
    BIC <- mclustBICupdate(BIC, rBIC)
  }
  
  # Fit model
  gaus <- Mclust(data=clusterdata, x=BIC) # using the mclust package
  # Get clustering
  classification = gaus$classification
  names(classification) <- nef
  #check which labels are present in cluster (sometimes some dont make it for classificaetion dueto low probability, then e run into troubles later)
  cl<-as.data.frame(table(classification))
  
  chaos <- list()
  
  overall <- data.frame(matrix(ncol = 2, nrow = 0))
  nams <- c("agentid", "entr")
  colnames(overall) <- nams
  
  
  # get entropy measures for  clustered clusters
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
  
  
  overall_ordered <- overall[order(overall$agentid),]
  #overall_ordered[group_7]
  #class[overall_ordered]
  
  
  clusts  <- Mclust(data=cbind(dim1[nef],overall_ordered[,2]), G=2) 
  #length(overall_ordered[,2])
  # length(dim1[nef])
  #clusts = kmeans(overall_ordered[,2],2, trace=TRUE) # cluster vector 
  
  classification = clusts$classification
  
  #check which labels are present in cluster (sometimes some dont make it for classificaetion dueto low probability, then e run into troubles later)
  
  names(classification) <- rownames(clusterdata)
  classification = as.numeric(classification)
  cl<-as.data.frame(table(classification))
  
  # plot 
  #  pchs  = 15+classification
  # 
  #  plot((overall_ordered[,2]), pch=pchs,col=add.alpha(cols_simplified,.5),
  # bty='n', main="KMeans Entropies", xlab = "Agents",ylab = "entropies")
  #  legend("top", agenttypes_simplified ,pch=19, title = "Agent Types",
  #       col=add.alpha(c(col1,col2),.6), bty='n')
  # #
  #  plot(overall_ordered[,2], pch=pchs,col=add.alpha(cols,.5),
  #       bty='n', main="KMeans Entropies")
  #  legend("bottomleft", agenttypes ,pch = c(16,rep(17,9)), title = "Agent Types",col=add.alpha(c(col1,colb),.6), bty='n', y.intersp = 0.8,cex=0.7)
  # 
  # 
  
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
  
  return(classification)}












