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


clusters <- function(dataset, vrounds, threshold_u){
  n_agents=1900
  #load('testset.R')
  #head(dataset_test)

  #vrounds = 250
  dataset = dataset[1:vrounds,]
  pars = dataset[1,1:10]     # store parameter combination
  # add voting rounds to pars
  pars[11] <- list(rounds = vrounds)
  dataset = dataset[,-(1:10)]        # Remove pars from data structure
  
  # dim(dataset)
  # run analysis on bootstraps (if voting rounds <= batch size)/random sampling without replacement (if voting rounds > batch size) of size 500 if dataset has more than 500 rows(=votingrounds) 
  # and average over label in an attempt to decrease variance
  # batch size 500: 
  # - Smaller batch sizes, such as 100 or 200 run a higher risk of having agents in them that always vote the same, causing a division by 0 problem when it comes to the covariance matrix. 
  # - When looking at the eigenvalues (from svd on covariance matrix, all rounds), eigenvalues 1-1000 are non-zero, 1001-1900 are zero. When considering thresholds one may think to use the batch size according to when the eigenvalues drop below that relative threshold.
  # the eigenvalues drop below 1 around 500, below 2 around 220, and below 3 around 75. 
  # --> choose batch size 500
  # --> use 5 batches for run time reasons
  
  matlist = list()
  
     

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
  labmat_gmm = data.frame(matrix(ncol=n_agents))
  labmat_km = data.frame(matrix(ncol=n_agents))
  labmat_gmm_entr = data.frame(matrix(ncol=length(c(group_1,group_2,group_3,group_4)))) 
  labmat_km_entr = data.frame(matrix(ncol=length(c(group_1,group_2,group_3,group_4)))) 
  gap_fail <- c()
  gap_fail_entr <- c()
  lasso_gmm_yes <-c()
  lasso_km_yes <- c()
  
    for (h in 1:length(matlist)) {
    
    dataset = matlist[[h]]
    #dim(dataset)

    
    
    pub = dataset[,1]        # Public signal for all (unbiased) agents
    pri_u_d = dataset[,2]        # Private signal for biased agents
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
    
   
    
    confusion <- function(cls, n_agents= 1900, b_pct=(900/1900), pct=TRUE){ # same as confusion function but allows for up to 4 classified groups
      # Set the true classes of each agent
      tru = c(rep(1,1000),rep(2,900))
      
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
    
    
    confusion_entr <- function(cls, n_agents= 1300, b_pct=(300/1300), pct=TRUE){ # same as confusion function but allows for up to 4 classified groups
      # Set the true classes of each agent
      tru = c(rep(1,1000),rep(2,300))
      
      # Compare true vs classified agent states, 
      # this returns the actual number classified in each group
      confusion = matrix(c( sum(tru==1 & cls==1),
                            sum(tru==1 & cls==2),
                            
                            sum(tru==2 & cls==1),
                            sum(tru==2 & cls==2)
                            #sum(tru==1 & cls==3)
                            
      ), byrow=TRUE, nr=2, nc=2) #nc is 3 if consider uncertainty class
      rownames(confusion) = c("True Genuine","True Nefarious")
      colnames(confusion) = c("Class Genuine","Class Nefarious")#, "Uncertain")
      
      if(pct){ # return in percent instead of actual numbers
        confusion = apply(confusion,2,function(x) x/(c((1-b_pct),b_pct)*1300))
      }
      return(confusion)
    }
    plot_cluster <- function(center,radius,col){ # Plot a filled circle with center,
      # radius and color as specified
      t = seq(0,2*pi,length=100)   
      coords <- t(rbind( center[1]+sin(t)*radius, center[2]+cos(t)*radius)) 
      polygon(coords, col=add.alpha(col,.3), border=NA)
    }
    #pal <- colorRampPalette(c("orange", "black"))
    
    #display.brewer.all()
    
    #colb <-brewer.pal(9,"Set1")
    #colb<-rainbow(9)
    #colb<-pal(9)
    # colb<- c('orangered4','orangered2','orange','plum4','plum','plum2','khaki4','khaki3','khaki1')
    # colb<- c('dodgerblue4','cyan3','darkslateblue','brown4','tomato1','firebrick3','forestgreen','olivedrab2','limegreen')
    # 
    # col1 = c('grey50')
    # col2 = colb[1]
    # col3 = colb[2]
    # col4 = colb[3]
    # col5 = colb[4]
    # col6 = colb[5]
    # col7 = colb[6]
    # col8 = colb[7]
    # col9 = colb[8]
    # col10 = colb[9]
    
    # agents<-c(rep('pub',500),rep('pri_2',50),rep('pri_3',50),rep('pri_4',50),rep('pri_5',50),rep('pri_6',50),rep('pri_7',50),rep('pri_8',50),rep('pri_9',50),rep('pri_10',50))
    # cols = c(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10)[as.factor(agents)] # biased agents = orange, indep agents = blue
    # cols_simplified <- c(rep(col1,length(group_1)), 
    #                      rep(col2,n_agents-length(group_1)))
    # 
    # cols <- c(rep(col1,length(group_1)), 
    #           rep(col2,length(group_2)),
    #           rep(col3,length(group_3)),
    #           rep(col4,length(group_4)),
    #           rep(col5,length(group_5)),
    #           rep(col6,length(group_6)),
    #           rep(col7,length(group_7)),
    #           rep(col8,length(group_8)),
    #           rep(col9,length(group_9)),
    #           rep(col10,length(group_10)))
    # 
    # 
    # agenttypes <- c("1 genuine agents",
    #                 "2 nefarious agents | conspiracy U",
    #                 "3 nefarious agents | conspiracy D",
    #                 "4 nefarious agents | conspiracy U + D",
    #                 "5 nefarious agents | AQ High",
    #                 "6 nefarious agents | AQ Low ",
    #                 "7 nefarious agents | AQ High + Low",
    #                 "8 nefarious agents | Lone Wolf High",
    #                 "9 nefarious agents | Lone Wolf Low",
    #                 "10 nefarious agents | Lone Wold High + Low")
    # 
    # agenttypes_simplified <- c("1 genuine agents",
    #                            "2 nefarious agents")
    
    
    
    # SVD based decompositon of component 1, 2, 3  --------------------------------------------------------------------------------
    
    # Dimensions through SVD
    # plotting disabled atm
    
    # Plot some dimensions
    # par(mfrow=c(3, 1))
    # par(mar=c(5,5,4,4))
    
    dim1 = dim_plot(1,s, ncol(dataset), (900/1900), TRUE) 
    
    # legend("top", agenttypes ,pch=19, title = "Agent Types",
    # col=add.alpha(colb,.6), bty='n')
    
    
    dim2 = dim_plot(2,s, ncol(dataset), (900/1900), TRUE)
    #legend("top", agenttypes ,pch=19, title = "Agent Types",
    #        col=add.alpha(c(col1,colb),.6), bty='n')
    
    #dim3 = dim_plot(3,s, ncol(dataset), .2, TRUE)
    # legend("top", agenttypes ,pch=19, title = "Agent Types",
    #        col=add.alpha(c(col1,colb),.6), bty='n')
    
    
    #plot(dim1,dim2, pch=16, col=add.alpha(c(cols),.6), 
    #bty='n', main="SVD")
    #legend("topright", agenttypes ,pch=19, title = "Agent Types",
    #       col=add.alpha(c(col1,colb),.6), bty='n')
    
    
    
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
    ### here logistic regression
    
    source(here("ABM_Classification/auxiliary functions/LogReg_func.R"))
    classification_gmm <- LR(dataset, classification,cl,pub)
    
    # store classification in label matrix
    labmat_gmm[h,] <- classification_gmm$classification
    
    # store whether lasso was used
    lasso_gmm_yes[h]<-classification_gmm$lasso
    
    # get code from plotting file
    
    
    
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
    
    
    
    ### kmeans - Entropy - only genuine vs first type of nefarious agents (xyz)
    
    # 1 kmeans new, then kmeans 2 clusters dim1 vs entropy, lower entropy nefarious) - did not seem feasible.
    # 2 gmm, then gmm 2 clusters dim 1 vs entropy, lower entropy nefarious
    # 3 just kmeans 2 clusters, lower entropy nefarious
    
    # meaning: call 3 scripts
    
   
    # prepare entropy measures:
    # get new svd for subset of data
    # Decomposition
    M = cor(dataset[,1:1300]) # Recalculate correlation matrix for subset of data now
    s = svd(M)                # Singular value decomposition
    dim1 = dim_plot(1,s, ncol(dataset[,1:1300]), (300/1300), TRUE) 
    dim2 = dim_plot(2,s, ncol(dataset[,1:1300]), (300/1300), TRUE) 

    
    
    # 2
    source(here("ABM_Classification/auxiliary functions/gmm_entropy_2.R"))
    
    classification_entr_1 <- gmm_entropy_2(dataset,dim1,dim2,n_agents)
    
    labmat_gmm_entr[h,] <- classification_entr_1
    
    # 3
    source(here("ABM_Classification/auxiliary functions/kmeans_entropy_3.R"))
    
    classification_entr_2 <- km_entropy_3(dataset,dim1,dim2,n_agents)
    
    labmat_km_entr[h,] <- classification_entr_2$classification
    
   
    
    gap_fail_entr[h]<-classification_entr_2$gap_fail_entr_
    # get plotting from plotting file
    
  } #end for loop for split matrices
  

  
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

  
  # GMM entropy
  freq_gmm_entr <- apply(labmat_gmm_entr, 2, function(x) table(factor(x, levels=1:2)))
  prob_gmm_entr <- freq_gmm_entr/(nrow(labmat_gmm_entr)) # row 1: prob that agent is unbiased. row 2: prob that agent is biased. Column 1-1000 are agents 1-1000
  prob_gmm_entr = prob_gmm_entr[-2,]
  
  prob_gmm_entr[prob_gmm_entr >= threshold_u] <- 'U'
  prob_gmm_entr[prob_gmm_entr != 'U'] <- 'B'
  
  
  prob_gmm_entr[prob_gmm_entr == 'B'] <- 2
  prob_gmm_entr[prob_gmm_entr == 'U'] <- 1 
  
  classification_gmm_entr = as.numeric(prob_gmm_entr)
  
  
  # KM entropy
  freq_km_entr <- apply(labmat_km_entr, 2, function(x) table(factor(x, levels=1:2)))
  prob_km_entr <- freq_km_entr/(nrow(labmat_km_entr)) # row 1: prob that agent is unbiased. row 2: prob that agent is biased. Column 1-1000 are agents 1-1000
  prob_km_entr = prob_km_entr[-2,]
  
  prob_km_entr[prob_km_entr >= threshold_u] <- 'U'
  prob_km_entr[prob_km_entr != 'U'] <- 'B'
  
  
  prob_km_entr[prob_km_entr == 'B'] <- 2
  prob_km_entr[prob_km_entr == 'U'] <- 1 
  
  classification_km_entr = as.numeric(prob_km_entr)
  
  
  
  
  confusion_matrix_gmm <- confusion(classification_gmm_)
  confusion_matrix_km <- confusion(classification_km_)
  
  confusion_matrix_entr_1_gmm <- confusion_entr(classification_gmm_entr)
  confusion_matrix_entr_2_km <- confusion_entr(classification_km_entr)
  

  
  
  outp = list(km_lr = confusion_matrix_km,
              gmm_lr = confusion_matrix_gmm, 
              km_entr = confusion_matrix_entr_2_km, 
              gmm_entr = confusion_matrix_entr_1_gmm, 
              pars = pars, 
              lasso_km_yes = lasso_km_yes, 
              lasso_gmm_yes = lasso_gmm_yes,
              gap_fail = gap_fail,
              gap_fail_entr = gap_fail_entr,
              classification_km_lr = classification_km_,
              classification_gmm_lr = classification_gmm_,
              classification_km_entr = classification_km_entr,
              classification_gmm_entr = classification_gmm_entr
              )
  return(outp)
}

