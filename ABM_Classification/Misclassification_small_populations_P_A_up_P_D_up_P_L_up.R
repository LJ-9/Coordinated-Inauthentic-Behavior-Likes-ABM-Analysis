### Misclassification 
# TODO: 
# run PRELIMS (such as here library and group vectors) in ABM_and_Classification.R script


setwd(here("Data/classification_results/P_A_D_L_up/")) # where classification results are be stored
size = 100 # specify which robustness check to go for, i.e. which size of genuine agents to check
vr = 500
groups= c(2,6,9)

# re-specify list dimension 
misclass <- list()
misclass[[1]]<-list(1,2,3)
misclass[[2]]<-list(1,2,3)
misclass[[3]]<-list(1,2,3)

for (i in 1:3){ # how many parameter combinations there are
  for (h in groups){  
    # get all seed and voting round files with same pars and voting rounds
    # specify if you want to look at a particular voting round size (250,500,750,1000)and at genuine agents size
    file_names = intersect(list.files(here("Data/classification_results/P_A_D_L_up/"), all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_par_",i,"_rounds_",vr)), list.files(here("Data/classification_results/P_A_D_L_up/"), all.files = FALSE, full.names = FALSE, pattern = paste0(size,"_gen_agents_and_gr_group_",h,".Rda")))
    # file_names = intersect(list.files(wdir4, all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_par_",1,"_rounds_",500)), list.files(wdir4, all.files = FALSE, full.names = FALSE, pattern = paste0(100,"_gen_agents_and_gr_group_",6,".Rda")))
    
    #file_names = list.files(wdir4, all.files = FALSE, full.names = FALSE, pattern = paste0("analysis_parcombi_",i,"_round"))# "\\splitsize_",splitsize,".Rda$"
    
    
    
    mis_1_km_lr <- c()
    # mis_1_km_lr_vs_6 <- c()
    # mis_1_km_lr_vs_9 <- c()
    
    mis_1_gmm_lr <-c()
    # mis_1_gmm_lr_vs_6 <-c()
    # mis_1_gmm_lr_vs_9 <-c()
    
    mis_km_lr <- c()
    mis_gmm_lr <-c()
    
    # mis_6_km_lr <- c()
    # mis_6_gmm_lr <-c()
   
    # mis_9_km_lr <- c()
    # mis_9_gmm_lr <-c()
 
    
    
    for (file in file_names){ # per paramater and per runoff
      # for (j in votingrounds){
      load(file)
      if(length(conf[["error"]])>0) next # skip run for analysis if error occurred
      
      
        mis_1_km_lr[file] <- sum(conf$out_par$classification_km_lr[1:size]!=1)/(length(1:size))
        mis_1_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[1:size]!=1)/(length(1:size))
        
        mis_km_lr[file] <- sum(conf$out_par$classification_km_lr[101:200]!=2)/(100)
        mis_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[101:200]!=2)/(100)
      
      # if (h == 2){
      #   mis_1_km_lr_vs_2[file] <- sum(conf$out_par$classification_km_lr[1:size]!=1)/(length(1:size))
      #   mis_1_gmm_lr_vs_2[file] <- sum(conf$out_par$classification_gmm_lr[1:size]!=1)/(length(1:size))
      #   
      #   mis_2_km_lr[file] <- sum(conf$out_par$classification_km_lr[101:200]!=2)/(100)
      #   mis_2_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[101:200]!=2)/(100)
      # }
      
      
      # if (h == 6){
      #   mis_1_km_lr_vs_6[file] <- sum(conf$out_par$classification_km_lr[1:size]!=1)/(length(1:size))
      #   mis_1_gmm_lr_vs_6[file] <- sum(conf$out_par$classification_gmm_lr[1:size]!=1)/(length(1:size))
      
      #   mis_6_km_lr[file] <- sum(conf$out_par$classification_km_lr[101:200]!=2)/(100)
      #   mis_6_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[101:200]!=2)/(100)
      # }
    
      # if (h == 9){
      #   mis_1_km_lr_vs_9[file] <- sum(conf$out_par$classification_km_lr[1:size]!=1)/(length(1:size))
      #   mis_1_gmm_lr_vs_9[file] <- sum(conf$out_par$classification_gmm_lr[1:size]!=1)/(length(1:size))
      #   
      #   mis_9_km_lr[file] <- sum(conf$out_par$classification_km_lr[101:200]!=2)/(100)
      #   mis_9_gmm_lr[file] <- sum(conf$out_par$classification_gmm_lr[101:200]!=2)/(100)
      # }
      
      
    }
    
    misclass_km_lr <- matrix(ncol=2)
    misclass_km_lr<- cbind(mis_1_km_lr, mis_km_lr)
    
    misclass_gmm_lr <- matrix(ncol=2)
    misclass_gmm_lr <- cbind(mis_1_gmm_lr, mis_gmm_lr)
    
    # misclass_km_lr_vs_2 <- matrix(ncol=2)
    # misclass_km_lr_vs_2 <- cbind(mis_1_km_lr_vs_2, mis_2_km_lr)
    # 
    # misclass_gmm_lr_vs_2 <- matrix(ncol=2)
    # misclass_gmm_lr_vs_2 <- cbind(mis_1_gmm_lr_vs_2, mis_2_gmm_lr)
   
    # misclass_km_lr_vs_6 <- matrix(ncol=2)
    # misclass_km_lr_vs_6 <- cbind(mis_1_km_lr_vs_6, mis_6_km_lr)
    # 
    # misclass_gmm_lr_vs_6 <- matrix(ncol=2)
    # misclass_gmm_lr_vs_6 <- cbind(mis_1_gmm_lr_vs_6, mis_6_gmm_lr)
    
    
    # misclass_km_lr_vs_9 <- matrix(ncol=2)
    # misclass_km_lr_vs_9 <- cbind(mis_1_km_lr_vs_9, mis_9_km_lr)
    # 
    # misclass_gmm_lr_vs_9 <- matrix(ncol=2)
    # misclass_gmm_lr_vs_9 <- cbind(mis_1_gmm_lr_vs_9, mis_9_gmm_lr)
     
    
    
    a = match(h, groups)
    misclass[[i]][[a]] <- list(misclass_km_lr=misclass_km_lr,
                          misclass_gmm_lr=misclass_gmm_lr)#,
                          
                          # misclass_km_lr_vs_6=misclass_km_lr_vs_6,
                          # misclass_gmm_lr_vs_6=misclass_gmm_lr_vs_6,
                          # 
                          # misclass_km_lr_vs_9=misclass_km_lr_vs_9,
                          # misclass_gmm_lr_vs_9=misclass_gmm_lr_vs_9
                         # )
    
  }}
  

  # list misclass[[1]] <- misclassifications in parameter combination 1
  # list misclass[[1]][[2]] <- misclassifications in parameter combination 1, run off vs group 6 ([[1]] would refer to group 2, [[3]] would refer to group 9)
  # list misclass[[2]] <- misclassifications in parameter combination 2 etc.
  par(mfrow=c(3,3), xpd = NA, mar=c(5,3,3,1.5), oma=c(0,4,4,0), tck=NA)#, din = dev.size(units = c("in")))
  
  ######## TODO CHOOSER ######
  b = 2 # specify which group to look at: 1 = group_2, 2 = group_6, 3 = group_9
  if (b ==1){groups = "xyz up"}
  if (b ==2){groups = "AQ low"}
  if (b ==3){groups = "LW low"}
  
  
  sdev_gmm<-list()
  sdev_km<-list()
  mean_gmm<-list()
  mean_km<-list()
 
  
   for (i in 1:3){#nrow(parlist)){ 
    
    plot(colMeans(misclass[[i]][[b]]$misclass_km_lr),type="b",ylim=c(-0.15,1),xlab="Agent groups", ylab="Mean share misclassified",xaxt="n",pch=15,col="aquamarine3",main = paste0("Par ",i,", voting rounds: ",vr,", genuine agents:", size,", 2 = ",groups))
    xtick<-seq(1, 2, by=1)
    axis(side=1, at=xtick, labels = TRUE)
    # legend("topright", c("KM LR","GMM LR","KM Entropy","GMM Entropy"),pch=c(15,16,15,16), title = "Method",
    # col=c("aquamarine3","aquamarine4","chocolate3","chocolate4"))
    
    sdev<-apply(misclass[[i]][[b]]$misclass_km_lr, 2, sd)
    arrows(1:2, colMeans(misclass[[i]][[b]]$misclass_km_lr)-sdev, 1:2, colMeans(misclass[[i]][[b]]$misclass_km_lr)+sdev, length=0.05, angle=90, code=3, col="aquamarine3")
    
    sdev_km[[i]] <- apply(misclass[[i]][[b]]$misclass_km_lr, 2, sd)
    mean_km[[i]] <- colMeans(misclass[[i]][[b]]$misclass_km_lr)
    
    lines(colMeans(misclass[[i]][[b]]$misclass_gmm_lr),type="b", pch=16, col= "aquamarine4")
    sdev<-apply(misclass[[i]][[b]]$misclass_gmm_lr, 2, sd)
    arrows(1:2, colMeans(misclass[[i]][[b]]$misclass_gmm_lr)-sdev, 1:2, colMeans(misclass[[i]][[b]]$misclass_gmm_lr)+sdev, length=0.05, angle=90, code=3, col="aquamarine4")
    
     sdev_gmm[[i]] <- apply(misclass[[i]][[b]]$misclass_gmm_lr, 2, sd)
     mean_gmm[[i]] <- colMeans(misclass[[i]][[b]]$misclass_gmm_lr)
    # 
   
   }
  
  if (b ==1){
    
    nami = paste0(here("Data/"),"group_2_mean_100_gen_agents_gmm.Rda") 
    nami2 = paste0(here("Data/"),"group_2_mean_100_gen_agents_km.Rda") 
    nami3 = paste0(here("Data/"),"group_2_sd_100_gen_agents_gmm.Rda") 
    nami4 = paste0(here("Data/"),"group_2_sd_100_gen_agents_km.Rda") 
    
    save(mean_gmm,file=nami)
    save(mean_km,file=nami2)
    save(sdev_gmm,file=nami3)
    save(sdev_km,file=nami4)
  }
  
  if (b ==2){
    
    nami = paste0(here("Data/"),"group_6_mean_100_gen_agents_gmm.Rda") 
    nami2 = paste0(here("Data/"),"group_6_mean_100_gen_agents_km.Rda") 
    nami3 = paste0(here("Data/"),"group_6_sd_100_gen_agents_gmm.Rda") 
    nami4 = paste0(here("Data/"),"group_6_sd_100_gen_agents_km.Rda") 
    
    save(mean_gmm,file=nami)
    save(mean_km,file=nami2)
    save(sdev_gmm,file=nami3)
    save(sdev_km,file=nami4)
  }
  
  if (b ==3){
  
  nami = paste0(here("Data/"),"group_9_mean_100_gen_agents_gmm.Rda") 
  nami2 = paste0(here("Data/"),"group_9_mean_100_gen_agents_km.Rda") 
  nami3 = paste0(here("Data/"),"group_9_sd_100_gen_agents_gmm.Rda") 
  nami4 = paste0(here("Data/"),"group_9_sd_100_gen_agents_km.Rda") 
  
  save(mean_gmm,file=nami)
  save(mean_km,file=nami2)
  save(sdev_gmm,file=nami3)
  save(sdev_km,file=nami4)
  }
  
  
  
  