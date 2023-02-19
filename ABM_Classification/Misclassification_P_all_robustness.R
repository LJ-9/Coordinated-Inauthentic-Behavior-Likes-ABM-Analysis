### Misclassification 
# TODO: 
# run PRELIMS (such as here library and group vectors) in ABM_and_Classification.R script

# plots and saves misclassification, split per each agent type AND summarized in confusion matrix with genuine and nefarious agents split only

# where results per seed are stored
setwd(here("Data/classification_results/P_all/"))
misclass<-list()
size = 100 # number of genuine agents 100, fixed
vr = 250 # number of voting rounds --> robustness checks, adapt here: 250, 500, 750, 1000
# Adapt rounds in file names at bottom accordingly!

group_2 = (size+1):(size+100)
group_3 = (group_2[100]+1):(group_2[100]+100)
group_4 = (group_3[100]+1):(group_3[100]+100)
group_5 = (group_4[100]+1):(group_4[100]+100)
group_6 = (group_5[100]+1):(group_5[100]+100)
group_7 = (group_6[100]+1):(group_6[100]+100)
group_8 = (group_7[100]+1):(group_7[100]+100)
group_9 = (group_8[100]+1):(group_8[100]+100)
group_10 = (group_9[100]+1):(group_9[100]+100)

# for (i in 1:nrow(parlist)){ # how many par combinations there are
  for (i in 1:3){ # how many par combinations there are
    
  # get all seed and voting round files with same pars and voting rounds
  # specify if you want to look at a particular voting round size (250,500,750,1000)and at genuine agents size
   file_names = intersect(list.files(here("Data/classification_results/P_all/"), all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_rounds_",vr)), list.files(here("Data/classification_results/P_all/"), all.files = FALSE, full.names = FALSE, pattern = paste0(size,"_gen_agents_.Rda")))
   #file_names = list.files(wdir4, all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_rounds_",vr))
  
  #file_names = list.files(wdir4, all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_round"))# "\\splitsize_",splitsize,".Rda$"
  
  
  
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
    
    mis_1_km_lr[file] <- sum(conf$out_par$classification_km_lr[1:size]!=1)/(length(1:size))
    mis_1_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[1:size]!=1)/(length(1:size))
    # mis_1_km_entr[file] <- sum(conf$out_par$classification_km_entr[1:size]!=1)/(length(1:size))
    # mis_1_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[1:size]!=1)/(length(1:size))
    # 
    mis_2_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_2]!=2)/(length(group_2))
    mis_2_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_2]!=2)/(length(group_2))
    # mis_2_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_2]!=2)/(length(group_2))
    # mis_2_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_2]!=2)/(length(group_2))
    # 
    mis_3_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_3]!=2)/(length(group_3))
    mis_3_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_3]!=2)/(length(group_3))
    # mis_3_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_3]!=2)/(length(group_3))
    # mis_3_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_3]!=2)/(length(group_3))
    # 
    mis_4_km_lr[file] <- sum(conf$out_par$classification_km_lr[group_4]!=2)/(length(group_4))
    mis_4_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[group_4]!=2)/(length(group_4))
    # mis_4_km_entr[file] <- sum(conf$out_par$classification_km_entr[group_4]!=2)/(length(group_4))
    # mis_4_gmm_entr[file] <- sum(conf$out_par$classification_gmm_entr[group_4]!=2)/(length(group_4))
    # 
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
  # 
  # 
  # misclass_gmm_ntr <- matrix(ncol=4)
  # misclass_gmm_entr <- cbind(mis_1_gmm_entr, mis_2_gmm_entr,mis_3_gmm_entr,mis_4_gmm_entr)
  # 
  
  misclass[[i]] <- list(misclass_km_lr=misclass_km_lr,misclass_gmm_lr=misclass_gmm_lr)#,misclass_km_entr=misclass_km_entr,misclass_gmm_entr=misclass_gmm_entr)
  
}

# list misclass[[1]] <- misclassifications in parameter combination 1
# list misclass[[2]] <- misclassifications in parameter combination 2 etc.


######
# tikz
par(mfrow=c(1,1), xpd = NA, mar=c(4.4,2.4,0,1.5), oma=c(0,2,2,0), tck=NA, xpd=FALSE)#, din = dev.size(units = c("in")))

my_plot <- function(misclass){

 
  ## for tikz
  
  position = data.frame(low = c(0.8,1.8,2.8,3.8,4.8,5.8,6.8,7.8,8.8,9.8), mid = c(1,2,3,4,5,6,7,8,9,10), high = c(1.2,2.2,3.2,4.2,5.2,6.2,7.2,8.2,9.2,10.2))
  
  plot(position$low, colMeans(misclass[[1]]$misclass_km_lr),type="p",ylim=c(-0.15,1.15),xlim=c(1,10), ylab=paste0("misclassified percent"),xaxt="n",yaxt = "n",pch='L',col="darkorange4", lty = 1, cex.lab = 1, cex.main = 0.8,cex=0.6,xlab='Agent types')
  xtick<-seq(1, 10, by=1)
  axis(side=1, at=xtick, labels = c('g','a1','a2','a3','d1','d2','d3','l1','l2','l3'), cex.axis = 1,lwd=0)
  ytick<-seq(0, 1, by=0.1)
  axis(side=2, at=ytick, labels = c(0,10,20,30,40,50,60,70,80,90,100), cex.axis = .9)
  abline(h=c(0,.25,.5,.75,1), col=c("snow4"), lty=3, lwd=0.5)
  
  
  sdev<-apply(misclass[[1]]$misclass_km_lr, 2, sd)
  arrows(position$low, colMeans(misclass[[1]]$misclass_km_lr)-sdev, position$low, colMeans(misclass[[1]]$misclass_km_lr)+sdev, length=0.05, angle=90, code=3, col="darkorange4", lty = 1,cex=0.6,lwd=0.4)
  

  
  points(position$low,colMeans(misclass[[1]]$misclass_gmm_lr),type="p", pch='L', col= "darkblue",lty=1,cex=.6)
  sdev<-apply(misclass[[1]]$misclass_gmm_lr, 2, sd)
  arrows(position$low, colMeans(misclass[[1]]$misclass_gmm_lr)-sdev, position$low, colMeans(misclass[[1]]$misclass_gmm_lr)+sdev, length=0.05, angle=90, code=3, col="darkblue",lty=1,lwd=0.5)
  
  
  # plot par 2
  lines(position$mid,colMeans(misclass[[2]]$misclass_km_lr),type="p", pch='M', col= "darkorange3", lty = 1,cex=.6)
  sdev<-apply(misclass[[2]]$misclass_km_lr, 2, sd)
  arrows(position$mid, colMeans(misclass[[2]]$misclass_km_lr)-sdev, position$mid, colMeans(misclass[[2]]$misclass_km_lr)+sdev, length=0.05, angle=90, code=3, col="darkorange3", lty = 1,lwd=0.5)
  
  
  lines(position$mid,colMeans(misclass[[2]]$misclass_gmm_lr),type="p", pch='M', col= "deepskyblue4",lty=1,cex=.6)
  sdev<-apply(misclass[[2]]$misclass_gmm_lr, 2, sd)
  arrows(position$mid, colMeans(misclass[[2]]$misclass_gmm_lr)-sdev, position$mid, colMeans(misclass[[2]]$misclass_gmm_lr)+sdev, length=0.05, angle=90, code=3, col="deepskyblue4",lty=1,lwd=0.5)
  
  # plot par 3
  lines(position$high,colMeans(misclass[[3]]$misclass_km_lr),type="p", pch='H', col= "darkorange", lty = 1,cex=.6)
  sdev<-apply(misclass[[3]]$misclass_km_lr, 2, sd)
  arrows(position$high, colMeans(misclass[[3]]$misclass_km_lr)-sdev, position$high, colMeans(misclass[[3]]$misclass_km_lr)+sdev, length=0.05, angle=90, code=3, col="darkorange", lty = 1,lwd=0.5)
  
  
  lines(position$high, colMeans(misclass[[3]]$misclass_gmm_lr),type="p", pch='H', col= "dodgerblue",lty=1,cex=.6)
  sdev<-apply(misclass[[3]]$misclass_gmm_lr, 2, sd)
  arrows(position$high, colMeans(misclass[[3]]$misclass_gmm_lr)-sdev, position$high, colMeans(misclass[[3]]$misclass_gmm_lr)+sdev, length=0.05, angle=90, code=3, col="dodgerblue",lty=1,lwd=0.5)
  
  abline(v=c(1.5,2.5,3.5,4.5,5.5,6.5,7.5,8.5,9.5), col=c("grey27"), lty=1, lwd=0.5)
  
 
}
#library(tikzDevice)
#tikz(("..."), 
    #  standAlone = FALSE, 
    #  width=4, 
    #  height=2.6)

my_plot(misclass) ## consolidated visualiation cf figure 4 in paper.
# dev.off()


# individual preliminary visualization  
par(mfrow=c(2,2), xpd = NA, mar=c(5,3,3,1.5), oma=c(0,4,4,0), tck=NA)#, din = dev.size(units = c("in")))

sdev_gmm<-list()
sdev_km<-list()
mean_gmm<-list()
mean_km<-list()

for (i in 1:3){#nrow(parlist)){ 
  
  plot(colMeans(misclass[[i]]$misclass_km_lr),type="b",ylim=c(-0.15,1),xlab="Agent groups", ylab="Mean share misclassified",xaxt="n",pch=15,col="aquamarine3",main = paste0("Par combi ",i,", vr= ",vr,", genuine agents:", size,", all seeds"))
  xtick<-seq(1, 10, by=1)
  axis(side=1, at=xtick, labels = TRUE)
  # legend("topright", c("KM LR","GMM LR","KM Entropy","GMM Entropy"),pch=c(15,16,15,16), title = "Method",
  # col=c("aquamarine3","aquamarine4","chocolate3","chocolate4"))
  legend("topright", c("KM LR","GMM LR"),pch=c(15,16), title = "Method",
         col=c("aquamarine3","aquamarine4"))
  
  sdev<-apply(misclass[[i]]$misclass_km_lr, 2, sd)
  arrows(1:10, colMeans(misclass[[i]]$misclass_km_lr)-sdev, 1:10, colMeans(misclass[[i]]$misclass_km_lr)+sdev, length=0.05, angle=90, code=3, col="aquamarine3")
  
  sdev_km[[i]] <- apply(misclass[[i]]$misclass_km_lr, 2, sd)
  mean_km[[i]] <- colMeans(misclass[[i]]$misclass_km_lr)
  
  lines(colMeans(misclass[[i]]$misclass_gmm_lr),type="b", pch=16, col= "aquamarine4")
  sdev<-apply(misclass[[i]]$misclass_gmm_lr, 2, sd)
  arrows(1:10, colMeans(misclass[[i]]$misclass_gmm_lr)-sdev, 1:10, colMeans(misclass[[i]]$misclass_gmm_lr)+sdev, length=0.05, angle=90, code=3, col="aquamarine4")
  
  sdev_gmm[[i]] <- apply(misclass[[i]]$misclass_gmm_lr, 2, sd)
  mean_gmm[[i]] <- colMeans(misclass[[i]]$misclass_gmm_lr)
  
  
}

#####
# store results, split per agent type
nami = paste0(here("Data/"),"mean_100_gen_agents_gmm_",vr,"_rounds_robustness_res.Rda")
nami2 = paste0(here("Data/"),"mean_100_gen_agents_km_",vr,"_rounds_robustness_res.Rda")
nami3 = paste0(here("Data/"),"sd_100_gen_agents_gmm_",vr,"_rounds_robustness_res.Rda")
nami4 = paste0(here("Data/"),"sd_100_gen_agents_km_",vr,"_rounds_robustness_res.Rda")

save(mean_gmm,file=nami)
save(mean_km,file=nami2)
save(sdev_gmm,file=nami3)
save(sdev_km,file=nami4)

#######
# store results, split into nefarious and genuine classification in form of confusion matrix, not split by nefarious agent type

# for (i in 1:nrow(parlist)){ # how many par combinations there are
for (i in 1:3){ # how many par combinations there are
  
  for (j in vr){ # times the sizes in votingrounds
    # get all seed files with same pars and voting rounds
    # * AUHTOR RESPONSE P_all but with voting rounds 500 and increased number of genuine agents, ie 1000.
    #
    # P_all but with voting rounds 250,750,1000: 
    file_names = intersect(list.files(here("Data/classification_results/P_all/"), all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_rounds_",j)), list.files(here("Data/classification_results/P_all/"), all.files = FALSE, full.names = FALSE, pattern = paste0("100_gen_agents_.Rda")))
    
    
    # load the files and safe in new list
    list_res <- list()
    for (file in file_names){
      # for (j in votingrounds){
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
    nam5 = paste0(here("Data/"),"mean_stddev_",i,"_",j,"rounds_100_gen_robustness_res.Rda")  # save performance means/std dev labeled after parameter combination and rounds
    
    save(perform, file = nam5)
    
  }
}
