### FILE IS RELATIVE TO A GROUP OF NEFARIOS AGENTS UNDER CONSIDERATION,
### HERE GROUP 9: Lone Wolf low
### TO ALTER FOR OTHER GROUPS, search for "CHANGE HERE"
# Now uses 2*SD for best/worst cases in gmm.list and km.list. Search for "2*SD" to find changed places

# Stuff to load ----
library(here)
source(here("MCS/x Data1 Groups Signals Parameters.R")) # groupings, parameters, etc.
load(here("Data/ABM Data.Rda")) #contains data.frame "all"
source(here("MCS/x Shared Mean-SD.R")) #1 dataset subsetting function and technicality
source(here("MCS/CW1 Mean-SD Tables.R")) # Import majority correctness etc. functions
# Usage reminder: correct.won.mean.sd.table(seeds, rounds, named.juries, param)
# CHANGE HERE
load(here("Data/group_9_mean_100_gen_agents_gmm.Rda"))
load(here("Data/group_9_mean_100_gen_agents_km.Rda"))
load(here("Data/group_9_sd_100_gen_agents_gmm.Rda"))
load(here("Data/group_9_sd_100_gen_agents_km.Rda"))
# Usage reminder: mean_gmm[[3]][1] : the mean MISCLASSIFICATION of genuine agents, given Param. 3
#                 mean_gmm[[3]][2] : the mean misclassification of the NEFARIOUS AGENTS UNDER CONSIDERATION
#                 mean_sdev[[3]][1] : the corresponding SD.

# Trimming functions keep.gen and keep.nef ----

# NEFARIOUS: X in [0,1] is percent MISCLASSIFICATION
# Then we can throw away 1-X : those that were classified correctly as nefarious
# If SD means X > 1: We misclassify 100% so keep all.
# If SD means X < 0: We misclassify 0% so keep none.

# Throw away % of nefarious agents correctly classified as nefarious:
keep.nef <- function(group, percent){
  if(percent <= 0){  # if we misclassify 0% (or less, due to SD), 
    keepers <- c()   # then we keep none of these nefarious agents
  }
  else if(percent >= 1){ # if we misclassify 100% (or more, due to SD)
    keepers <- group       # then we keep all of these nefarious agents
  }
  else{ # we keep the percentage that was misclassified, starting from group[1]
    keepers <- c(group[1]:(group[1]+round(length(group)*(percent))-1))
  }
  return(keepers)
}

# TEST AND EXAMPLE nefarious
#test <- 10:30
# (test[1]):(test[1]+round(length(test)*(0.8))-1)
# (test[1]):(test[1]+round(length(test)*(1-0.8))-1)

#keep.nef(test, -1) # OK, keep NULL
#keep.nef(test, 0) # OK, keep NULL
#keep.nef(test, 0.8) # OK, we throw away few due to high misclassification
#keep.nef(test, 1.2) # OK, keep all


# GENUINE: X in [0,1] is still percent MISCLASSIFICATION
# Then we can keep 1-X : those that were classified correctly as genuine
# If SD means X > 1: We misclassify 100% so keep none.
# If SD means X < 0: We misclassify 0% so keep all.

# Throw away % of nefarious agents correctly classified as nefarious:
keep.gen <- function(group, percent){
  if(percent <= 0){  # if we misclassify 0% (or less, due to SD), 
    keepers <- group   # then we keep all these genuine agents
  }
  else if(percent >= 1){ # if we misclassify 100% (or more, due to SD)
    keepers <- c()       # then we keep none of these genuine agents
  }
  else{ # we keep 1 - (the percentage that was misclassified), starting from group[1]
    keepers <- c(group[1]:(group[1]+round(length(group)*(1-percent))-1))
  }
  return(keepers)
}

# TEST AND EXAMPLE genuine
#test <- 10:30

#keep.gen(test, -1) # OK, we misclassify none, so keep all
#keep.gen(test, 0) # OK, we misclassify none, so keep all
#keep.gen(test, 0.8) # OK, we misclassify a lot, so keep few
#keep.gen(test, 1.2) # OK, we misclassify all, so keep none





# Function gmm.list that gathers original group, mean+SD and best, average, and worst case trimmed groups ----
# outputs a list of 6 lists: 
# Original groups
# GMM Means
# GMM SD
# Best case trimmed groups -- trimmed as in "original group with (misclassified genuine / correctly classified nefarious) removed
# Average trimmed
# Worst case trimmed
gmm.list <- function(parameter){
  #These are the groups we are interested in: " CHANGE HERE
  #groups <- list(group_1[1:100], group_2, group_3, group_4, group_5, group_6, group_7, group_8, group_9, group_10)
  #names(groups) <- c("Genuine", "XYZup", "XYZdown", "XYZalways", "AQhigh", "AQlow", "AQalways", "LWhigh", "LWlow", "LWalways")
  groups <- list(group_1[1:100], group_9)
  names(groups) <- c("Genuine", "LWlow")
  
  # in these cases, where now reads 2 was 10 before :
  ## best case: Low misclassification = mean - SD
  best <- vector(mode = "list", length = 2) 
  best[[1]] <- keep.gen(groups[[1]], (mean_gmm[[parameter]][1] - 2*sdev_gmm[[parameter]][1])) # 2*SD
    for (k in 2:2){
    best[[k]] <- keep.nef(groups[[k]], (mean_gmm[[parameter]][k] - 2*sdev_gmm[[parameter]][k])) # 2*SD
  }
  ## average case: Mean misclassification = mean  
  average <- vector(mode = "list", length = 2)
  average[[1]] <- keep.gen(groups[[1]], mean_gmm[[parameter]][1])
  for (k in 2:2){
    average[[k]] <- keep.nef(groups[[k]], mean_gmm[[parameter]][k])
  }
  ## worst case: High misclassification = mean + SD  
  worst <- vector(mode = "list", length = 2)
  worst[[1]] <- keep.gen(groups[[1]], (mean_gmm[[parameter]][1] + 2*sdev_gmm[[parameter]][1])) # 2*SD
  for (k in 2:2){
    worst[[k]] <- keep.nef(groups[[k]], (mean_gmm[[parameter]][k] + 2*sdev_gmm[[parameter]][k])) # 2*SD
  }  

  ## Gather lists in list, name and return
  gmm.ls <- list(groups, mean_gmm[[parameter]], sdev_gmm[[parameter]], best, average, worst)
  names(gmm.ls) <- c("original", "gmm.mean", "gmm.sd", "best", "average", "worst")
  return(gmm.ls)
}

# Function km.list (K-means version of gmm.list) ----
km.list <- function(parameter){
  #These are the groups we are interested in: CHANGE HERE
  #groups <- list(group_1[1:100], group_2, group_3, group_4, group_5, group_6, group_7, group_8, group_9, group_10)
  #names(groups) <- c("Genuine", "XYZup", "XYZdown", "XYZalways", "AQhigh", "AQlow", "AQalways", "LWhigh", "LWlow", "LWalways")
  groups <- list(group_1[1:100], group_9)
  names(groups) <- c("Genuine", "LWlow")
  
  # in these cases, where now reads 2 was 10 before :
  ## best case: Low misclassification = mean - SD
  best <- vector(mode = "list", length = 2)
  best[[1]] <- keep.gen(groups[[1]], (mean_km[[parameter]][1] - 2*sdev_km[[parameter]][1])) # 2*SD
  for (k in 2:2){
    best[[k]] <- keep.nef(groups[[k]], (mean_km[[parameter]][k] - 2*sdev_km[[parameter]][k])) # 2*SD
  }
  ## average case: Mean misclassification = mean  
  average <- vector(mode = "list", length = 2)
  average[[1]] <- keep.gen(groups[[1]], mean_km[[parameter]][1])
  for (k in 2:2){
    average[[k]] <- keep.nef(groups[[k]], mean_km[[parameter]][k])
  }
  ## worst case: High misclassification = mean + SD  
  worst <- vector(mode = "list", length = 2)
  worst[[1]] <- keep.gen(groups[[1]], (mean_km[[parameter]][1] + 2*sdev_km[[parameter]][1])) # 2*SD
  for (k in 2:2){
    worst[[k]] <- keep.nef(groups[[k]], (mean_km[[parameter]][k] + 2*sdev_km[[parameter]][k])) # 2*SD
  }  
  
  ## Gather lists in list, name and return
  km.ls <- list(groups, mean_km[[parameter]], sdev_km[[parameter]], best, average, worst)
  names(km.ls) <- c("original", "km.mean", "km.sd", "best", "average", "worst")
  return(km.ls)
}
# TEST AND EXAMPLE ----
## Best case test:
# gmm.list(3)$gmm.mean[1] - gmm.list(3)$gmm.sd[1]  
#gmm.list(3)$best[1] : best case for agent group 1 in parameter 3.

# gmm.list(3)$gmm.mean[10] - gmm.list(3)$gmm.sd[10]  
# gmm.list(3)$best[10] : best case for agent group 10 in parameter 3.
# LOOKS CORRECT



# ----
# Function trimmed.jury. Feed with "gmm.list(parameter)$case" or "km.list(parameter)$case" aka "method.param.case" to produce a jury given chosen agent groups ----
 

trimmed.jury <- function(method.param.case, group.vector){
  trimmed <- list()
  for(i in group.vector){
    trimmed <- unlist(c(trimmed, as.vector(method.param.case[i])))
  }
  named.trimmed <- list()
  named.trimmed[[1]] <- trimmed
  names(named.trimmed) <- c("Trimmed Jury")
return(named.trimmed)
}

# TEST AND EXAMPLE ----
# test <- list()  
# test <- c(test, as.vector(gmm.list(3)$best[1]))
# test <- c(test, as.vector(gmm.list(3)$best[3]))
# unlist(test)
# trimmed.jury(gmm.list(3)$best, c(1,3))
# OK!


# my.untrimmed.jury setup for matrices - list of 100 genuine and nefarious under consideration ----
my.untrimmed.jury <- list()
# CHANGE HERE:
#my.untrimmed.jury[[1]] <- c(group_1[1:100], group_2, group_3, group_4, group_5, group_6, group_7, group_8, group_9, group_10)
my.untrimmed.jury[[1]] <- c(group_1[1:100], group_9)
names(my.untrimmed.jury) <- c("Untrimmed Jury")

# Seeds and rounds to run analysis on, and place to save results ----
my.seeds <- 1:100
my.rounds <- 1:500
# CHANGE HERE
#save.file <- here(paste0("MCS/Effect/Effects_All_Groups_", Sys.Date(), ".Rda"))
save.file <- here(paste0("MCS/JSP Effect/Effects_Group_9_", Sys.Date(), ".Rda"))

# GMM: matrix of best, average and worst correctness scores (all rounds) ----
# Usage recall: correct.won.mean.sd.table(seeds, rounds, named.juries, param)

gmm.correct <- matrix(NA, nrow= 3, ncol = 4)
rownames(gmm.correct) = c("Par. 1", "Par. 2", "Par. 3")
colnames(gmm.correct) = c("Untrimmed", "Best", "Average", "Worst")

gmm.sd <- matrix(NA, nrow= 3, ncol = 4)
rownames(gmm.sd) = c("Par. 1", "Par. 2", "Par. 3")
colnames(gmm.sd) = c("Untrimmed", "Best", "Average", "Worst")

# Setting up so matrix contains only correctness scores, not SD -- cf. [1] in end of lines
# PARAM 1
gmm.1 <- matrix(NA, nrow = 2, ncol = 4)
rownames(gmm.1) = c("Mean", "SD")
colnames(gmm.1) = c("Untrimmed", "Best", "Average", "Worst")

gmm.1[,1] <- correct.won.mean.sd.table(my.seeds, my.rounds, my.untrimmed.jury, 1)
gmm.1[,2] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(1)$best, c(1,2)), 1)
gmm.1[,3] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(1)$average, c(1,2)), 1)
gmm.1[,4] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(1)$worst, c(1,2)), 1)

gmm.correct[1,] <- gmm.1[1,]
gmm.sd[1,] <- gmm.1[2,]

# PARAM 2
gmm.2 <- matrix(NA, nrow = 2, ncol = 4)
rownames(gmm.2) = c("Mean", "SD")
colnames(gmm.2) = c("Untrimmed", "Best", "Average", "Worst")

gmm.2[,1] <- correct.won.mean.sd.table(my.seeds, my.rounds, my.untrimmed.jury, 2)
gmm.2[,2] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(2)$best, c(1,2)), 2)
gmm.2[,3] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(2)$average, c(1,2)), 2)
gmm.2[,4] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(2)$worst, c(1,2)), 2)

gmm.correct[2,] <- gmm.2[1,]
gmm.sd[2,] <- gmm.2[2,]

# PARAM 3
gmm.3 <- matrix(NA, nrow = 2, ncol = 4)
rownames(gmm.3) = c("Mean", "SD")
colnames(gmm.3) = c("Untrimmed", "Best", "Average", "Worst")

gmm.3[,1] <- correct.won.mean.sd.table(my.seeds, my.rounds, my.untrimmed.jury, 3)
gmm.3[,2] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(3)$best, c(1,2)), 3)
gmm.3[,3] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(3)$average, c(1,2)), 3)
gmm.3[,4] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(gmm.list(3)$worst, c(1,2)), 3)

gmm.correct[3,] <- gmm.3[1,]
gmm.sd[3,] <- gmm.3[2,]


# KM: matrix of best, average and worst correctness scores (all rounds) ----
# Usage recall: correct.won.mean.sd.table(seeds, rounds, named.juries, param)

km.correct <- matrix(NA, nrow= 3, ncol = 4)
rownames(km.correct) = c("Par. 1", "Par. 2", "Par. 3")
colnames(km.correct) = c("Untrimmed", "Best", "Average", "Worst")

km.sd <- matrix(NA, nrow= 3, ncol = 4)
rownames(km.sd) = c("Par. 1", "Par. 2", "Par. 3")
colnames(km.sd) = c("Untrimmed", "Best", "Average", "Worst")

# Setting up so matrix contains only correctness scores, not SD -- cf. [1] in end of lines
# PARAM 1
km.1 <- matrix(NA, nrow = 2, ncol = 4)
rownames(km.1) = c("Mean", "SD")
colnames(km.1) = c("Untrimmed", "Best", "Average", "Worst")

km.1[,1] <- correct.won.mean.sd.table(my.seeds, my.rounds, my.untrimmed.jury, 1)
km.1[,2] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(1)$best, c(1,2)), 1)
km.1[,3] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(1)$average, c(1,2)), 1)
km.1[,4] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(1)$worst, c(1,2)), 1)

km.correct[1,] <- km.1[1,]
km.sd[1,] <- km.1[2,]

# PARAM 2
km.2 <- matrix(NA, nrow = 2, ncol = 4)
rownames(km.2) = c("Mean", "SD")
colnames(km.2) = c("Untrimmed", "Best", "Average", "Worst")

km.2[,1] <- correct.won.mean.sd.table(my.seeds, my.rounds, my.untrimmed.jury, 2)
km.2[,2] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(2)$best, c(1,2)), 2)
km.2[,3] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(2)$average, c(1,2)), 2)
km.2[,4] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(2)$worst, c(1,2)), 2)

km.correct[2,] <- km.2[1,]
km.sd[2,] <- km.2[2,]

# PARAM 3
km.3 <- matrix(NA, nrow = 2, ncol = 4)
rownames(km.3) = c("Mean", "SD")
colnames(km.3) = c("Untrimmed", "Best", "Average", "Worst")

km.3[,1] <- correct.won.mean.sd.table(my.seeds, my.rounds, my.untrimmed.jury, 3)
km.3[,2] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(3)$best, c(1,2)), 3)
km.3[,3] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(3)$average, c(1,2)), 3)
km.3[,4] <- correct.won.mean.sd.table(my.seeds, my.rounds, trimmed.jury(km.list(3)$worst, c(1,2)), 3)

km.correct[3,] <- km.3[1,]
km.sd[3,] <- km.3[2,]


## RESTRICTION

# GMM: matrix of best, average and worst correctness scores (up and yes) ----
# Usage recall: correct.won.mean.sd.table_given.yes.and.up(seeds, rounds, named.juries, param)

gmm.correct.up.and.yes <- matrix(NA, nrow= 3, ncol = 4)
rownames(gmm.correct.up.and.yes) = c("Par. 1", "Par. 2", "Par. 3")
colnames(gmm.correct.up.and.yes) = c("Untrimmed", "Best", "Average", "Worst")

gmm.sd.up.and.yes <- matrix(NA, nrow= 3, ncol = 4)
rownames(gmm.sd.up.and.yes) = c("Par. 1", "Par. 2", "Par. 3")
colnames(gmm.sd.up.and.yes) = c("Untrimmed", "Best", "Average", "Worst")

# Setting up so matrix contains only correctness scores, not SD -- cf. [1] in end of lines
# PARAM 1
gmm.1 <- matrix(NA, nrow = 2, ncol = 4)
rownames(gmm.1) = c("Mean", "SD")
colnames(gmm.1) = c("Untrimmed", "Best", "Average", "Worst")

gmm.1[,1] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.untrimmed.jury, 1)
gmm.1[,2] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(1)$best, c(1,2)), 1)
gmm.1[,3] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(1)$average, c(1,2)), 1)
gmm.1[,4] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(1)$worst, c(1,2)), 1)

gmm.correct.up.and.yes[1,] <- gmm.1[1,]
gmm.sd.up.and.yes[1,] <- gmm.1[2,]

# PARAM 2
gmm.2 <- matrix(NA, nrow = 2, ncol = 4)
rownames(gmm.2) = c("Mean", "SD")
colnames(gmm.2) = c("Untrimmed", "Best", "Average", "Worst")

gmm.2[,1] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.untrimmed.jury, 2)
gmm.2[,2] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(2)$best, c(1,2)), 2)
gmm.2[,3] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(2)$average, c(1,2)), 2)
gmm.2[,4] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(2)$worst, c(1,2)), 2)

gmm.correct.up.and.yes[2,] <- gmm.2[1,]
gmm.sd.up.and.yes[2,] <- gmm.2[2,]

# PARAM 3
gmm.3 <- matrix(NA, nrow = 2, ncol = 4)
rownames(gmm.3) = c("Mean", "SD")
colnames(gmm.3) = c("Untrimmed", "Best", "Average", "Worst")

gmm.3[,1] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.untrimmed.jury, 3)
gmm.3[,2] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(3)$best, c(1,2)), 3)
gmm.3[,3] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(3)$average, c(1,2)), 3)
gmm.3[,4] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(gmm.list(3)$worst, c(1,2)), 3)

gmm.correct.up.and.yes[3,] <- gmm.3[1,]
gmm.sd.up.and.yes[3,] <- gmm.3[2,]


# KM: matrix of best, average and worst correctness scores (up and yes) ----
# Usage recall: correct.won.mean.sd.table_given.yes.and.up(seeds, rounds, named.juries, param)

km.correct.up.and.yes <- matrix(NA, nrow= 3, ncol = 4)
rownames(km.correct.up.and.yes) = c("Par. 1", "Par. 2", "Par. 3")
colnames(km.correct.up.and.yes) = c("Untrimmed", "Best", "Average", "Worst")

km.sd.up.and.yes <- matrix(NA, nrow= 3, ncol = 4)
rownames(km.sd.up.and.yes) = c("Par. 1", "Par. 2", "Par. 3")
colnames(km.sd.up.and.yes) = c("Untrimmed", "Best", "Average", "Worst")

# Setting up so matrix contains only correctness scores, not SD -- cf. [1] in end of lines
# PARAM 1
km.1 <- matrix(NA, nrow = 2, ncol = 4)
rownames(km.1) = c("Mean", "SD")
colnames(km.1) = c("Untrimmed", "Best", "Average", "Worst")

km.1[,1] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.untrimmed.jury, 1)
km.1[,2] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(1)$best, c(1,2)), 1)
km.1[,3] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(1)$average, c(1,2)), 1)
km.1[,4] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(1)$worst, c(1,2)), 1)

km.correct.up.and.yes[1,] <- km.1[1,]
km.sd.up.and.yes[1,] <- km.1[2,]

# PARAM 2
km.2 <- matrix(NA, nrow = 2, ncol = 4)
rownames(km.2) = c("Mean", "SD")
colnames(km.2) = c("Untrimmed", "Best", "Average", "Worst")

km.2[,1] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.untrimmed.jury, 2)
km.2[,2] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(2)$best, c(1,2)), 2)
km.2[,3] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(2)$average, c(1,2)), 2)
km.2[,4] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(2)$worst, c(1,2)), 2)

km.correct.up.and.yes[2,] <- km.2[1,]
km.sd.up.and.yes[2,] <- km.2[2,]

# PARAM 3
km.3 <- matrix(NA, nrow = 2, ncol = 4)
rownames(km.3) = c("Mean", "SD")
colnames(km.3) = c("Untrimmed", "Best", "Average", "Worst")

km.3[,1] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.untrimmed.jury, 3)
km.3[,2] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(3)$best, c(1,2)), 3)
km.3[,3] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(3)$average, c(1,2)), 3)
km.3[,4] <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, trimmed.jury(km.list(3)$worst, c(1,2)), 3)

km.correct.up.and.yes[3,] <- km.3[1,]
km.sd.up.and.yes[3,] <- km.3[2,]


# Output matrices ----
#gmm.correct#; gmm.sd
#km.correct#; km.sd

#gmm.correct.up.and.yes#; gmm.sd.up.and.yes
#km.correct.up.and.yes#; km.sd.up.and.yes

# Save output ----
save(gmm.correct, gmm.sd, gmm.correct.up.and.yes, gmm.sd.up.and.yes,
     km.correct, km.sd, km.correct.up.and.yes, km.sd.up.and.yes,
     file = save.file)


# Plotting ----

#Gather mean+sd in lists: CHANGE HERE
gmm.jury.correctness <- list("GMM Jury Correctness, Gen+LWlow, 500 Rounds" = gmm.correct, "SD" = round(gmm.sd,2) )
gmm.jury.correctness.up.and.yes <- list("GMM Jury Correctness, Gen+LWlow, Up and Yes" = gmm.correct.up.and.yes, "SD" = round(gmm.sd.up.and.yes,2) )
km.jury.correctness <- list("KM Jury Correctness, Gen+LWlow, 500 Rounds" = km.correct, "SD" = round(km.sd,2) )
km.jury.correctness.up.and.yes <- list("KM Jury Correctness, Gen+LWlow, Up and Yes" = km.correct.up.and.yes, "SD" = round(km.sd.up.and.yes,2) )

library(plot.matrix)
sigbert.plot <- function(mean.sd.list){
  res <- plot(mean.sd.list[[1]],
              main=paste0("Correct Won Percent: ", names(mean.sd.list[1])),xlab="Jury Cases",ylab="% Correct",
              col=heat.colors(7), breaks=c(50,60,70,80,90,95,99.9), cex=0.5, 
              text.cell=list(cex=1, adj = c(0.5,0)), fmt.cell='%.2f', key=NULL)
  for (i in 1:nrow(mean.sd.list[[1]])) {
    for (j in 1:ncol(mean.sd.list[[1]])) {
      args        <- res$cell.text[[i,j]]
      args$labels <- paste0('(', format(mean.sd.list[[2]][i,j], 1), ')')
      args$y      <- args$y-0.25
      args$cex    <- 0.75
      do.call(text, args)
    }
  }
}

par(mfrow=c(2,2))
sigbert.plot(gmm.jury.correctness)
sigbert.plot(gmm.jury.correctness.up.and.yes)
sigbert.plot(km.jury.correctness)
sigbert.plot(km.jury.correctness.up.and.yes)


# Some sanity check:
# worst case KM: Trimming seems to be correct.
# km.list(3)$km.mean[1] + km.list(3)$km.sd[1] #: KM param 3 mean misclass % of Genuine + SD
# km.list(3)$worst[1] #: worst case of genuine agents in parameter 3.
# km.list(3)$km.mean[2] + km.list(3)$km.sd[2] #: KM param 3 mean misclass % of Nefarious + SD
# km.list(3)$worst[2] #: worst case of nefarious agents under consideration in parameter 3.


# started 17:05
# 