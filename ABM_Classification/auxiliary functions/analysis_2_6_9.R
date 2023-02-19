# myTryCatch is a wrapper to report errors and warnings
# does not report warning if glm logistic regression does not converge, since this is dealt with in the code (the lasso/glmnet is used in that case, which is documented, too)

myTryCatch <- function(expr) {
  warn <- err <- NULL
  output <- withCallingHandlers(
    tryCatch(expr, error=function(e) {
      err <<- e
      NULL
    }), warning=function(w) {
      warn <<- w
      invokeRestart("muffleWarning")
    })
  list(out_par=output, warning=warn, error=err)
}


clusters <- function(dataset, vrounds, threshold_u, size, group){
  n_agents=size+100

  dataset = dataset[1:vrounds,]
  dataset = dataset[,c(1:(size+13),group)]
  # dim(dataset)
  pars = dataset[1,1:10]     # store parameter combination
  # add voting rounds to pars
  pars[11] <- list(rounds = vrounds)
  dataset = dataset[,-(1:10)]        # Remove pars from data structure
  
   dim(dataset)
  # run analysis on bootstraps (if voting rounds <= batch size)/random sampling without replacement (if voting rounds > batch size) of size 500 if dataset has more than 500 rows(=votingrounds) 
  # and average over label in an attempt to decrease variance
  # batch size 500: 
  # - Smaller batch sizes, such as 100 or 200 run a higher risk of having agents in them that always vote the same, causing a division by 0 problem when it comes to the covariance matrix. 
  # - When looking at the eigenvalues (from svd on covariance matrix, all rounds), eigenvalues 1-1000 are non-zero, 1001-1900 are zero. When considering thresholds one may think to use the batch size according to when the eigenvalues drop below that relative threshold.
  # the eigenvalues drop below 1 around 500, below 2 around 220, and below 3 around 75. 
  # --> choose batch size 500
  # --> use 5 batches for run time reasons
  
  matlist = list()
  
  
  #   # store all sub matrices as matrix in list matlist
  #   matlist <- lapply(split(dataset, rep(1:(vrounds/split), each = split)),function(a) data.frame(a)) # end if vrounds >= split
  # } else {matlist[[1]] <- dataset}
  
  ## if 1000 voting rounds
  if (vrounds == 1000){
    ## first make use of all data
    matlist[[1]] <- dataset[1:500,] # make sure all data is used
    matlist[[2]] <- dataset[501:1000,]
    matlist[[3]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] # replace not deemed necessary given batch size < available votign rounds
    matlist[[4]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] # replace not deemed necessary given batch size < available votign rounds
    matlist[[5]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] # replace not deemed necessary given batch size < available votign rounds
  }
  
  ## if 750 voting rounds
  if (vrounds == 750){
    ## first make use of all data
    matlist[[1]] <- dataset[1:500,] # make sure all data is used
    matlist[[2]] <- dataset[251:750,]
    matlist[[3]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] # replace not deemed necessary given batch size < available voting rounds
    matlist[[4]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] # replace not deemed necessary given batch size < available voting rounds
    matlist[[5]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] # replace not deemed necessary given batch size < available voting rounds
  }
  
  # if 500 voting rounds
  if (vrounds == 500){
    
    matlist[[1]] <- dataset[1:500,]
    matlist[[2]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[3]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[4]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[5]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
  }
  # if 250 voting rounds
  if (vrounds == 250){
    
    matlist[[1]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[2]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[3]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[4]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
    matlist[[5]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
  }
  
  # dis<-c()
  # for (i in 1:5){
  #   test0 <- matlist[[i]]
  #   test0 <-test0[,-(1:3)]
  #   test0 = as.matrix(test0)
  #   mode(test0) = "numeric" # turn numeric to allow for colSums calculations
  # 
  #   dis[i] <- ((sum(colSums(test0)==500) > 0) || (sum(colSums(test0)==0))>0)}
  # sum(dis==TRUE)
  # # #
  
  
  ## check whether there is data sampled with columns comprised of the same entry, causing an error with corr(x) later. If so: resample
  # but only do up to 30 times, then let error happen. To prevent that one core does not get stuck in an infinite loop
  
  
  if (vrounds == 250 || vrounds == 500){
    for (i in 1:length(matlist)){
      test0 <- matlist[[i]]
      test0 <-test0[,-(1:3)]
      test0 = as.matrix(test0)         
      mode(test0) = "numeric" # turn numeric to allow for colSums calculations
      
      if ( (sum(colSums(test0)==500)>0) || (sum(colSums(test0)==0)>0) ) {
        b <-0
        repeat {
          matlist[[i]] <- dataset[(sample(nrow(dataset), 500, replace=TRUE)),] # replace TRUE given batch size >= available votign rounds
          
          test0 <- matlist[[i]]
          test0 <-test0[,-(1:3)]
          test0 = as.matrix(test0)         
          mode(test0) = "numeric" # turn numeric to allow for colSums calculations
          b <- b+1
          if ( ((sum(colSums(test0)==500)==0)  && (sum(colSums(test0)==0)==0)) || (b==30) ) {break}
        }
      }
    }
  }
  
  if (vrounds == 750 || vrounds == 1000){
    for (i in 1:length(matlist)){
      test0 <- matlist[[i]]
      test0 <-test0[,-(1:3)]
      test0 = as.matrix(test0)         
      mode(test0) = "numeric" # turn numeric to allow for colSums calculations
      
      if ( (sum(colSums(test0)==500)>0) || (sum(colSums(test0)==0)>0) ) {
        b <-0
        repeat {
          matlist[[i]] <- dataset[(sample(nrow(dataset), 500, replace=FALSE)),] 
          
          test0 <- matlist[[i]]
          test0 <-test0[,-(1:3)]
          test0 = as.matrix(test0)         
          mode(test0) = "numeric" # turn numeric to allow for colSums calculations
          b <- b+1
          
          if ( ((sum(colSums(test0)==500)==0)  && (sum(colSums(test0)==0)==0)) || (b==30) ) {break}
        }
      }
    }
  }
  
  
  
  
  # to store each label / classification result per resampling/bootstrap
  labmat_gmm = data.frame(matrix(ncol=(size+100)))
  labmat_km = data.frame(matrix(ncol=(size+100)))
  #labmat_gmm_entr = data.frame(matrix(ncol=(size+100)))
  #labmat_km_entr = data.frame(matrix(ncol=(size+100)))
  gap_fail <- c()
 # gap_fail_entr <- c()
  lasso_gmm_yes <-c()
  lasso_km_yes <- c()
  
  for (h in 1:length(matlist)) {
    
    dataset = matlist[[h]]
    # dataset = matlist[[1]]
    
    #dim(dataset)
    
    
    
    pub = dataset[,1]        # Public signal for all (genuine) agents
    pri_u_d = dataset[,2]        # Private signal for nefarious agents
    pri_y_n = dataset[,3]
    dataset = dataset[,-(1:3)]        # Remove signals from data structure
    # dim(dataset)
    dataset = as.matrix(dataset)         # Turn data into a matrix structure (to enable linear algebra calculations)
    mode(dataset) = "numeric" 
    #dataset <- scale(dataset, center=TRUE, scale=TRUE)  # i guess not necessary, standardize and mean center data
    
    
    #class(dataset)
    
    # Decomposition

    
    M = cor(dataset)          # Calculate correlation matrix
    s = svd(M)                # Singular value decomposition
    
    # (apply(M, 2, function(x) sum(x))==0)
    # class(M)
    # colSums(M)
    #   # test <- dataset[,colSums(dataset != 0) != 0] # if there is a column with all zeros, svd does nto work
    #  # dim(test)
    #  # dataset[dataset == 0] <- 2 # does not help
    #  sum(((colSums(dataset)==200)==TRUE))
    
    # Functions needed
    # Specify functions that are used below
    
    
    # add.alpha <- function(col, alpha=1){
    #   if(missing(col))
    #     stop("Please provide a vector of colours.")
    #   apply(sapply(col, col2rgb)/255, 2,
    #         function(x)
    #           rgb(x[1], x[2], x[3], alpha=alpha))
    # }
    # plots and calculates weights for specific dimensions (either one or more)
    dim_plot <- function(dims, decomp, n_agents, b_pct, weight=TRUE){
      # col1 = 'dodgerblue', col2='orange'){ 
      u = s$u  # eigenvectors: these weigh the individual agents
      l = s$d  # eigenvalues:  these weigh the corresponding dimension
      if(!weight){ # don't weight dimensions, only collapse (sum) over eigenvectors
        l = 0*s$d+1  
      }
      if(length(dims)>1){ # we use more than 1 dimension 
        # => collapse (sum) over dimensions
        tmp_plot = apply(u[,dims]%*%diag(l[dims]),1,sum)  
      } else{
        tmp_plot = u[,dims]*l[dims] # only use 1 dimension, 
        # hence pick 1 eigenvector and multiply by the eigenvalue
      }
      
      # idx1 = 1:floor(tail(group_1, n=1)) # index for genuine agents
      # idx2 = floor(tail(group_1, n=1)+1):tail(group_2, n=1) # index for biased agents group_2
      # idx3 = floor(tail(group_2, n=1)+1):tail(group_3, n=1) # index for biased agents group_3
      # idx4 = floor(tail(group_3, n=1)+1):tail(group_4, n=1) # index for biased agents group_4
      # idx5 = floor(tail(group_4, n=1)+1):tail(group_5, n=1) # index for biased agents group_5
      # idx6 = floor(tail(group_5, n=1)+1):tail(group_6, n=1) # index for biased agents group_6
      # idx7 = floor(tail(group_6, n=1)+1):tail(group_7, n=1) # index for biased agents group_7
      # idx8 = floor(tail(group_7, n=1)+1):tail(group_8, n=1) # index for biased agents group_8
      # idx9 = floor(tail(group_8, n=1)+1):tail(group_9, n=1) # index for biased agents group_9
      # idx10 = floor(tail(group_9, n=1)+1):tail(group_10, n=1) # index for biased agents group_10
      
      # canvas (don't plot anything by itself, only setup a new plot)
      #  plot(0,0, type='n', main=paste0("# dimensions: ",length(dims)),
      #  xlim=c(1,n_agents), ylim=c(-20,20), ylab ='Variable weight in dimension/Eigenvector',
      #  xlab='Variables (agents)',bty='n')
      # # plot biased agents:
      #  points(idx1,tmp_plot[idx1], pch=16, col=add.alpha(col1,.5))
      # # plot unbiased agents:
      #  points(idx2,tmp_plot[idx2], pch=16, col=add.alpha(col2,.5))
      #  points(idx3,tmp_plot[idx2], pch=16, col=add.alpha(col3,.5))
      # 
      #  points(idx3,tmp_plot[idx2], pch=16, col=add.alpha(col4,.5))
      #  points(idx4,tmp_plot[idx2], pch=16, col=add.alpha(col4,.5))
      #  points(idx5,tmp_plot[idx2], pch=16, col=add.alpha(col5,.5))
      #  points(idx6,tmp_plot[idx2], pch=16, col=add.alpha(col6,.5))
      #  points(idx7,tmp_plot[idx2], pch=16, col=add.alpha(col7,.5))
      #  points(idx8,tmp_plot[idx2], pch=16, col=add.alpha(col8,.5))
      #  points(idx9,tmp_plot[idx2], pch=16, col=add.alpha(col9,.5))
      #  points(idx10,tmp_plot[idx2], pch=16, col=add.alpha(col10,.5))
      # 
      #  mtext(paste0("Dimension from ", min(dims)," to ",max(dims)),side = 3,line = 0)
      
      invisible(tmp_plot) # return the collapsed plotted dimensions (but don't print)
    }
    
    
    
    confusion <- function(cls, n_agents= size+100, b_pct=(100/(size+100)), pct=TRUE){ # same as confusion function but allows for up to 4 classified groups
      # Set the true classes of each agent
      tru = c(rep(1,size),rep(2,100))
      
      # Compare true vs classified agent states, 
      # this returns the actual number classified in each group
      confusion = matrix(c( sum(tru==1 & cls==1),
                            sum(tru==1 & cls==2),
                            
                            sum(tru==2 & cls==1),
                            sum(tru==2 & cls==2)
                            #sum(tru==2 & cls==3), #uncertainty class
                            
                            
                            
      ), byrow=TRUE, nr=2, nc=2) #nc is 3 if consider uncertainty class
      # rownames(confusion) = c("True Nefarious","True Genuine")
      # colnames(confusion) = c("Class Nefarius","Class Genuine")#, "Uncertain")
      
      rownames(confusion) = c("True Genuine","True Nefarious")
      colnames(confusion) = c("Class Genuine","Class Nefarious")#, "Uncertain")
      
      if(pct){ # return in percent instead of actual numbers
        confusion = apply(confusion,2,function(x) x/(c((1-b_pct),b_pct)*n_agents))
        # confusion = apply(confusion,2,function(x) x/(c(b_pct,(1-b_pct))*n_agents))
        
      }
      return(confusion)
    }
    
    
 
    
    
    
    # SVD based decompositon of component 1, 2, 3  --------------------------------------------------------------------------------
    
    # Dimensions through SVD
    # plotting disabled atm
    
    # Plot some dimensions
    # par(mfrow=c(3, 1))
    # par(mar=c(5,5,4,4))
    
    dim1 = dim_plot(1,s, ncol(dataset), (100/(size+100)), TRUE) 
    
    # legend("top", agenttypes ,pch=19, title = "Agent Types",
    # col=add.alpha(colb,.6), bty='n')
    
    
    dim2 = dim_plot(2,s, ncol(dataset), (100/(size+100)), TRUE)
    #legend("top", agenttypes ,pch=19, title = "Agent Types",
    #        col=add.alpha(c(col1,colb),.6), bty='n')
    
    #dim3 = dim_plot(3,s, ncol(dataset), .2, TRUE)
    # legend("top", agenttypes ,pch=19, title = "Agent Types",
    #        col=add.alpha(c(col1,colb),.6), bty='n')
    
    
    # plot(dim1,dim2, pch=16, col=add.alpha(c(cols),.6),
    # bty='n', main="SVD")
    # legend("topright", agenttypes ,pch=19, title = "Agent Types",
    #       col=add.alpha(c(col1,colb),.6), bty='n')
    
   #plot(dim1,dim2, col = c(rep("black",100),rep("dodgerblue",100)))
    
    #### GMM  --------------------------------------------------------------------------------
    
    
    ## Gaussian mixture model BIC AIC Information criterion to check optimal # clusters
    
    BIC <- NULL
    for(j in 2:20){
      rBIC <- mclustBIC(data=cbind(dim1,dim2), verbose = FALSE)
      BIC <- mclustBICupdate(BIC, rBIC)
    }
    
    # Fit model
    gaus <- Mclust(data=cbind(dim1,dim2), x=BIC) # using the mclust package
  
    
    # Get clustering
    classification = gaus$classification
    
    #check which labels are present in cluster (sometimes some dont make it for classificaetion dueto low probability, then e run into troubles later)
    
    cl<-as.data.frame(table(classification))
    
    if (length(cl[,1]) == 1) {
    gaus <- Mclust(data=cbind(dim1,dim2), G=2) # using the mclust package
    # Get clustering
    classification = gaus$classification
    #check which labels are present in cluster (sometimes some dont make it for classification due to low probability, then run into troubles later)
    cl<-as.data.frame(table(classification))
    }
    ### here logistic regression
    source(here("ABM_Classification/auxiliary functions/LogReg_func.R"))
    classification_gmm <- LR(dataset, classification,cl,pub)
    
    # store classification in label matrix
    labmat_gmm[h,] <- classification_gmm$classification
    
    # store whether lasso was used
    lasso_gmm_yes[h]<-classification_gmm$lasso
    

    
    
    # KMeans clustering with SVD --------------------------------------------------------------------------------
  
    kmeans_more <- function(x,centers,iter.max){kmeans(x,centers, iter.max=60)} # specify more iterations to help convergence
    gap_fail[h] <-0
    tgap<-clusGap((scale(cbind(dim1,dim2), center = TRUE, scale=TRUE)), kmeans_more, 20, B = 50,verbose = FALSE, d.power = 2)
    nk<-maxSE(f = tgap$Tab[, "gap"], SE.f = tgap$Tab[, "SE.sim"],method="Tibs2001SEmax")
    
    if (nk == 1){ 
      
      # make sure we don't by accident/by seed end up with 1 cluster chosen by gap stat. This would cause an error (logr not found) in the log regression function. 
      # Make sure we get stable results that at least identify 2 clusters as ideal with 500 bootstraps, which is when: The main result <res>$Tab[,"gap"] of course is from bootstrapping aka Monte Carlo simulation 
      # and hence random, or equivalently, depending on the initial random seed (see set.seed()). On the other hand, in our experience, using B = 500 gives quite precise results such that the gap plot
      # is basically unchanged after an another run. https://rdrr.io/cran/cluster/man/clusGap.html 
      # 500 bootstraps cost runtime
      
      tgap<-clusGap((scale(cbind(dim1,dim2), center = TRUE, scale=TRUE)), kmeans_more, 20, B = 500,verbose = FALSE, d.power = 2)
      nk<-maxSE(f = tgap$Tab[, "gap"], SE.f = tgap$Tab[, "SE.sim"],method="Tibs2001SEmax")
    }
    if (nk == 1){ # if still 1 cluster picked, last resort: force kmeans to pick 2 clusters to make log regr work then
      clusts <- kmeans((scale(cbind(dim1,dim2), center = TRUE, scale=TRUE)),2,iter.max = 60)
      gap_fail[h] <- 1 } else {
        clusts<-kmeans((scale(cbind(dim1,dim2), center = TRUE, scale=TRUE)),nk,iter.max = 60)}
    
    classification  = clusts$cluster
    # here logisit regression
    cl<-as.data.frame(table(classification))
    
    classification_km <- LR(dataset, classification,cl,pub)
    
    labmat_km[h,] <- classification_km$classification
    
    lasso_km_yes[h]<-classification_km$lasso
    
    
    
    
    
  } #end for loop for bootstrapped matrices
  
 
  
  #GMM
  freq_gmm <- apply(labmat_gmm, 2, function(x) table(factor(x, levels=1:2)))
  prob_gmm <- freq_gmm/(nrow(labmat_gmm)) # row 1: prob that agent is unbiased. row 2: prob that agent is biased. Column 1-1000 are agents 1-1000
  prob_gmm = prob_gmm[-2,]
  
  prob_gmm[prob_gmm >= threshold_u] <- 'U'
  prob_gmm[prob_gmm != 'U'] <- 'B'
  
  
  prob_gmm[prob_gmm == 'B'] <- 2
  prob_gmm[prob_gmm == 'U'] <- 1 
  
  classification_gmm_ = as.numeric(prob_gmm)
  
  #KM
  freq_km <- apply(labmat_km, 2, function(x) table(factor(x, levels=1:2)))
  prob_km <- freq_km/(nrow(labmat_km)) # row 1: prob that agent is unbiased. row 2: prob that agent is biased. Column 1-1000 are agents 1-1000
  prob_km = prob_km[-2,]
  
  prob_km[prob_km >= threshold_u] <- 'U'
  prob_km[prob_km != 'U'] <- 'B'
  

  prob_km[prob_km == 'B'] <- 2
  prob_km[prob_km == 'U'] <- 1 
  
  classification_km_ = as.numeric(prob_km)
  
  
  
  confusion_matrix_gmm <- confusion(classification_gmm_)
  confusion_matrix_km <- confusion(classification_km_)
  
  
  
  
  outp = list(km_lr = confusion_matrix_km,
              gmm_lr = confusion_matrix_gmm, 
              pars = pars, 
              lasso_km_yes = lasso_km_yes, 
              lasso_gmm_yes = lasso_gmm_yes,
              gap_fail = gap_fail,
              classification_km_lr = classification_km_,
              classification_gmm_lr = classification_gmm_
             
  )
  return(outp)
}

