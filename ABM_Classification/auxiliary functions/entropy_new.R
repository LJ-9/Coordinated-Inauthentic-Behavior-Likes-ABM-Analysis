# Entropy, package Desctools
# library(DescTools)
# Shannon entropy according to https://rstudio-pubs-static.s3.amazonaws.com/455435_30729e265f7a4d049400d03a18e218db.html
# and according to Entropy function in r (Desctools), if you feed in contingency table

# row wise entropies


# per voting row entropy, summed, for each cluster

# for new kmeans algo:
# define ncol similar to subsetiing dataset

En <- function(cl, dataset,classification){
entrops <- list()
  for (i in 1:nrow(cl)){ #per cluster entropy
  # calculate contingency tables per row
  #dont use 'table', replace "table" function, since things get messy if there is a couple rows that only feature ones, or only zeros, dimensions are screwed up
  da <- dataset[,classification==cl[i,1]]
  da = as.data.frame(da)
  contingencies <- data.frame()
  da$count.0 <- apply(da, 1, function(x) length(which(x==0)))
  da$count.1 <- apply(da, 1, function(x) length(which(x==1)))
  
  #calculate entropy per row, sum over all entropies
  entropies <- round(sum(apply(cbind(da$count.0,da$count.1),1, Entropy))/(nrow(dataset)),3)

clus = paste0("Entropy Cluster_",i)
entrops[[clus]] = entropies
}
return(entrops)
}

