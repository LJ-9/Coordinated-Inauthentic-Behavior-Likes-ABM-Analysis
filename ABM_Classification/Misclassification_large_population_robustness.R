### Misclassification 

# TODO: 
# run PRELIMS (such as here library and group vectors) in ABM_and_Classification.R script

# Robustness checks: 
# this file reads classification results from populations with 
      #### 1000 #### 
    # genuine agents 
# in it (large population) 
# and plots mean misclassifications ans saves results as confusion matrices.


vr = 750 # decide number of votingrounds per run you want to look at


# Plot --------------------------------------------------------------------
############ MEAN/ SD FOR CLASSIFICATION RESULTS in PLOT FORMAT #############

setwd(here("Data/classification_results/P_large_pop_robust/"))            # where classification results with 1000 genuine agents will be stored
misclass<-list()
for (i in 1:nrow(parlist)){ # how many par combinations there are
  
    # get all seed and voting round files with same pars and voting rounds
  
    # specify if you want to look at a particular voting round size (250,500,750,1000) below:
    file_names = list.files(here("Data/classification_results/P_large_pop_robust/"), all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_rounds_",vr))
    #file_names = list.files(here("Data/classification_results/P_large_pop_robust/"), all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_round"))

    

    mis_1_km_lr <- c()
    mis_1_gmm_lr <-c()
    # mis_1_km_entr <- c()
    # mis_1_gmm_entr <- c()
    
    mis_2_km_lr <- c()
    mis_2_gmm_lr <-c()
    # mis_2_km_entr <- c()
    # mis_2_gmm_entr <- c()
    
    mis_3_km_lr <- c()
    mis_3_gmm_lr <-c()
    # mis_3_km_entr <- c()
    # mis_3_gmm_entr <- c()
    
    mis_4_km_lr <- c()
    mis_4_gmm_lr <-c()
    # mis_4_km_entr <- c()
    # mis_4_gmm_entr <- c()
    
    mis_5_km_lr <- c()
    mis_5_gmm_lr <-c()
    # mis_5_km_entr <- c()
    # mis_5_gmm_entr <- c()
    
    mis_6_km_lr <- c()
    mis_6_gmm_lr <-c()
    # mis_6_km_entr <- c()
    # mis_6_gmm_entr <- c()
    
    mis_7_km_lr <- c()
    mis_7_gmm_lr <-c()
    # mis_7_km_entr <- c()
    # mis_7_gmm_entr <- c()
    
    mis_8_km_lr <- c()
    mis_8_gmm_lr <-c()
    # mis_8_km_entr <- c()
    # mis_8_gmm_entr <- c()
    
    mis_9_km_lr <- c()
    mis_9_gmm_lr <-c()
    # mis_9_km_entr <- c()
    # mis_9_gmm_entr <- c()
    
    mis_10_km_lr <- c()
    mis_10_gmm_lr <-c()
    # mis_10_km_entr <- c()
    # mis_10_gmm_entr <- c()
    
    for (file in file_names){
      # for (j in votingrounds){
      load(file)
      if(length(conf[["error"]])>0) next # skip run for analysis if error occurred
     
      mis_1_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_1]!=1)/(length(group_1))
      mis_1_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_1]!=1)/(length(group_1))
      # mis_1_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_1]!=1)/(length(group_1))
      # mis_1_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_1]!=1)/(length(group_1))
      
      mis_2_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_2]!=2)/(length(group_2))
      mis_2_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_2]!=2)/(length(group_2))
      # mis_2_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_2]!=2)/(length(group_2))
      # mis_2_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_2]!=2)/(length(group_2))
      
      mis_3_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_3]!=2)/(length(group_3))
      mis_3_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_3]!=2)/(length(group_3))
      # mis_3_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_3]!=2)/(length(group_3))
      # mis_3_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_3]!=2)/(length(group_3))
      
      mis_4_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_4]!=2)/(length(group_4))
      mis_4_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_4]!=2)/(length(group_4))
      # mis_4_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_4]!=2)/(length(group_4))
      # mis_4_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_4]!=2)/(length(group_4))
      
      mis_5_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_5]!=2)/(length(group_5))
      mis_5_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_5]!=2)/(length(group_5))
      
      mis_6_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_6]!=2)/(length(group_6))
      mis_6_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_6]!=2)/(length(group_6))
      
      mis_7_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_7]!=2)/(length(group_7))
      mis_7_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_7]!=2)/(length(group_7))
      
      mis_8_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_8]!=2)/(length(group_8))
      mis_8_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_8]!=2)/(length(group_8))
      
      mis_9_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_9]!=2)/(length(group_9))
      mis_9_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_9]!=2)/(length(group_9))
      
      mis_10_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_10]!=2)/(length(group_10))
      mis_10_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_10]!=2)/(length(group_10))
      
      }
    
    misclass_km_lr <- matrix(ncol=10)
    misclass_km_lr <- cbind(mis_1_km_lr, mis_2_km_lr,mis_3_km_lr,mis_4_km_lr,mis_5_km_lr,mis_6_km_lr,mis_7_km_lr,mis_8_km_lr,mis_9_km_lr,mis_10_km_lr)

    misclass_gmm_lr <- matrix(ncol=10)
    misclass_gmm_lr <- cbind(mis_1_gmm_lr, mis_2_gmm_lr,mis_3_gmm_lr,mis_4_gmm_lr,mis_5_gmm_lr,mis_6_gmm_lr,mis_7_gmm_lr,mis_8_gmm_lr,mis_9_gmm_lr,mis_10_gmm_lr)
    
    # misclass_km_entr <- matrix(ncol=4)
    # misclass_km_entr <- cbind(mis_1_km_entr, mis_2_km_entr,mis_3_km_entr,mis_4_km_entr)
    
    
    # misclass_gmm_ntr <- matrix(ncol=4)
    # misclass_gmm_entr <- cbind(mis_1_gmm_entr, mis_2_gmm_entr,mis_3_gmm_entr,mis_4_gmm_entr)
    
    
    misclass[[i]] <- list(misclass_km_lr=misclass_km_lr,misclass_gmm_lr=misclass_gmm_lr)# ,misclass_km_entr=misclass_km_entr,misclass_gmm_entr=misclass_gmm_entr)
    
}

# list misclass[[1]] <- misclassifications in parameter combination 1
# list misclass[[2]] <- misclassifications in parameter combination 2 etc.
par(mfrow=c(2,2), xpd = NA, mar=c(5,3,3,1.5), oma=c(0,4,4,0), tck=NA)#, din = dev.size(units = c("in")))

for (i in 1:nrow(parlist)){ 
  
  plot(colMeans(misclass[[i]]$misclass_km_lr),type="b",ylim=c(-0.15,1),xlab="Agent groups", ylab="Mean share misclassified",xaxt="n",pch=15,col="aquamarine3",main = paste0("Parameter combination ",i,", vr = ",vr))
       xtick<-seq(1, 10, by=1)
       axis(side=1, at=xtick, labels = TRUE)
       # legend("topright", c("KM LR","GMM LR","KM Entropy","GMM Entropy"),pch=c(15,16,15,16), title = "Method",
              # col=c("aquamarine3","aquamarine4","chocolate3","chocolate4"))
        legend("topright", c("KM LR","GMM LR"),pch=c(15,16), title = "Method",
                col=c("aquamarine3","aquamarine4"))
       sdev<-apply(misclass[[i]]$misclass_km_lr, 2, sd)
       arrows(1:10, colMeans(misclass[[i]]$misclass_km_lr)-sdev, 1:10, colMeans(misclass[[i]]$misclass_km_lr)+sdev, length=0.05, angle=90, code=3, col="aquamarine3")
       
  lines(colMeans(misclass[[i]]$misclass_gmm_lr),type="b", pch=16, col= "aquamarine4")
  sdev<-apply(misclass[[i]]$misclass_gmm_lr, 2, sd)
  arrows(1:10, colMeans(misclass[[i]]$misclass_gmm_lr)-sdev, 1:10, colMeans(misclass[[i]]$misclass_gmm_lr)+sdev, length=0.05, angle=90, code=3, col="aquamarine4")
  
  
  
  # lines(colMeans(misclass[[i]]$misclass_km_entr),type="b", pch=15, col= "chocolate3")
  # sdev<-apply(misclass[[i]]$misclass_km_entr, 2, sd)
  # arrows(1:4, colMeans(misclass[[i]]$misclass_km_entr)-sdev, 1:4, colMeans(misclass[[i]]$misclass_km_entr)+sdev, length=0.05, angle=90, code=3, col="chocolate3")
  
  
  
  # lines(colMeans(misclass[[i]]$misclass_gmm_entr),type="b", pch=16, col= "chocolate4")
  # sdev<-apply(misclass[[i]]$misclass_gmm_entr, 2, sd)
  # arrows(1:4, colMeans(misclass[[i]]$misclass_gmm_entr)-sdev, 1:4, colMeans(misclass[[i]]$misclass_gmm_entr)+sdev, length=0.05, angle=90, code=3, col="chocolate4")

}
  

# Tables ------------------------------------------------------------------


############ MEAN/ SD FOR CLASSIFICATION RESULTS in TABLE FORMAT#############

## adapt what files to be read in directly in function ## 

# further analyse results: mean/std dev in each parameter combination, i.e. collapse over the seeds
#setwd(here("Data/classification_results/P_large_pop_robust/")) # set wdir to where you results per seed stored


### ### ### the below can be adapted to read different population files into the function --> adapt the file_names below!
### ### ### also adapt voting rounds if wanted
## ### ### make sure to change file name unter which it should be saved (nam5 below)


# for (i in 1:nrow(parlist)){ # how many par combinations there are
for (i in 1:4){ # how many par combinations there are
  
  for (j in vr){ # times the sizes in votingrounds
    # get all seed files with same pars and voting rounds
    file_names = list.files(here("Data/classification_results/P_large_pop_robust/"), all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_rounds_",j,"_run"))

    
    # load the files and safe in new list
    list_res <- list()
    for (file in file_names){
      # for (j in vr){
      load(file)
      if(length(conf[["error"]])>0) next # skip run for analysis if error occurred
      list_res[[file]] <-  conf}
    
    # perform mean and std dev calc
    km <- lapply(lapply(list_res, `[[`, 1), `[[`,1)         # retrieve kmeans confusion matrix for all seeds
    gmm <- lapply(lapply(list_res, `[[`, 1), `[[`,2)        # retrieve gmm confusion matrix for all seeds
    km_entr <- lapply(lapply(list_res, `[[`, 1), `[[`,3)    # retrieve km entropy confusion matrix for all seeds
    gmm_entr <- lapply(lapply(list_res, `[[`, 1), `[[`,4)    # retrieve km entropy confusion matrix for all seeds
    
    km_mean <- round(apply(simplify2array(km), 1:2, mean),5)
    km_sd <- round(apply(simplify2array(km), 1:2, sd),5)
    
    gmm_mean <- round(apply(simplify2array(gmm), 1:2, mean),5)
    gmm_sd <- round(apply(simplify2array(gmm), 1:2, sd),5)
    
    km_entr_mean <- round(apply(simplify2array(km_entr), 1:2, mean),5)
    km_entr_sd <- round(apply(simplify2array(km_entr), 1:2, sd),5)
    
    gmm_entr_mean <- round(apply(simplify2array(gmm_entr), 1:2, mean),5)
    gmm_entr_sd <- round(apply(simplify2array(gmm_entr), 1:2, sd),5)
    
    
    pars <- lapply(lapply(list_res, `[[`, 1), `[[`,5)[1] # save one of the pars documentation for reference
    
    performance <- list(km_mean = km_mean,km_sd = km_sd,gmm_mean = gmm_mean,gmm_sd=gmm_sd, km_entr_mean = km_entr_mean, km_entr_sd = km_entr_sd, gmm_entr_mean = gmm_entr_mean, gmm_entr_sd = gmm_entr_sd, pars = pars)
    
    # save both performance and indivdual results
    
    perform <- list(performance, list_res) # store performance results in list, followed by individual results
    # nam5 = paste0(here("Data/classification_results/"),"mean_stddev_",i,"_",j,"_split.Rda")  # save performance means/std dev labeled after parameter combination and rounds
    nam5 = paste0(here("Data/"),"mean_stddev_",i,"_",j,"all_1000_gen_robustness_res.Rda")  # save performance means/std dev labeled after parameter combination and rounds
    
    save(perform, file = nam5)
    
  }
}


