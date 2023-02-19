# This script generates the vote data by invoking an agent-based model, subsequently analyses this data (classification). Analysis may take a long time.

########## PRELIMS

rm(list = ls())

# load packages to be used below
library(parallel)
library(iterators)
library(doParallel)
registerDoParallel(4)  # parallel computing, use multicore-style forking (doe not run on Windows machines)
library(RColorBrewer)
library(glmnet)
library(mclust)
library(DescTools)
library(cluster)


# source functions needed in the model pipeline sourced throughout with here library
library(here) # should be [1] "/ ... /Code". Here should direct to folder "Code"

# ...(here("ABM_Classification/auxiliary functions/")) # where auxiliary functions are located
# ...(here("Data/single_DatABM/"))                     # where single vote data files will be stored
# ...(here("Data/"))                                   # where appended vote data will be stored
# ...(here("Data/classification_results/"))            # where classification results will be stored


group_1 <- c(1:1000)     #  1 genuine agents
group_2 <- c(1001:1100)   # 2 nefarious - c1 Amplifier up (A_up) - U - Consp
group_3 <- c(1101:1200)   # 3 nefarious - Amplifier down (A_down) -D - Consp, c2 
group_4 <- c(1201:1300)   # 4 nefarious - Amplifier both (A_updown) - Full Consp -Parallel Universe, c3, A
group_5 <- c(1301:1400)   # 5 nefarious - Distorter downvote - AQ - High - Coordinating, cd1
group_6 <- c(1401:1500)   # 6 nefarious - Distorter upvote - AQ - Low - Coordinating, cd2
group_7 <- c(1501:1600)   # 7 nefarious - Distorter both - AQ - Always - Coordinating, cd3
group_8 <- c(1601:1700)   # 8 nefarious - Lone Wolf downvote - High - Non-Coordinating, id1
group_9 <- c(1701:1800)   # 9 nefarious - Lone Wolf upvote - Low - Non-Coordinating, id2
group_10 <- c(1801:1900)  # 10 nefarious - Lone Wolf both - Always - Non-Coordinating, id3


# signal vectors

signal_1<-c('high','low') # p1 property
signal_2<-c('U','D')      # p2 property
signal_3<-c('Y','N')      # p3/pa propery


parlist_prep = list(h_l = c(.75),                     # sample p1/high with probability 75%, and low with 1-75% = 25%
                    u_d = c(.75,.5,.1),               # sample p2/up with probability 75/50/10%, and down with 1-75/50/10% = 25/50/90%
                    y_n = c(.9,.5,.1),                # sample p3/yes with probability 90/50/10%, and no with 1-90/50/10% = 10/50/90%
                    y_n_p = c(.9,.5,.1),              # sample pa/yes_private with probability 90/50/10%, and no_private with 1-90/50/10% = 10/50/90%
                    n_agents = c(1900),                # number of agents
                    pri_perc_comp_lower = c(75,100),   # drawing perception competence for biased agents uniformely from interval [1,1] or [0.75,0.95]
                    pri_perc_comp_upper = c(95,100),
                    pub_perc_comp_lower = c(65),     # drawing perception competence for p1/public high/low signal uniformely from interval [0.65,0.95]
                    pub_perc_comp_upper = c(95))


# creates dataframe from all combinations of parameter values
parlist_all=expand.grid(parlist_prep, stringsAsFactors = FALSE)

    
# number of agents, fixed
# voting rounds --> specified below in lapply fucntion

# Run MC simulation with following parameter combinations: 

par_low <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.75 & parlist_all$y_n == .9 & parlist_all$y_n_p == .9 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_med <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.5 & parlist_all$y_n == .5 & parlist_all$y_n_p == .5 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_high <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.1 & parlist_all$y_n == .1 & parlist_all$y_n_p == .1 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_high_asterix <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.1 & parlist_all$y_n == .1 & parlist_all$y_n_p == .1 & parlist_all$pri_perc_comp_lower==75 & parlist_all$pri_perc_comp_upper==95 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)

parlist = rbind(par_low,par_med,par_high, par_high_asterix)

######### END PRELIMS 


# 0. SEED 
# set here once! and then leave it like this for data generation
set.seed(1) 

# 1. SIMULATION

# generates data with parallel computing using 4 cores, while sweeping through parameters. 
# Output: Rda files

#contains the function that generates the data(votes)
source(here("ABM_Classification/auxiliary functions/b_voting_function_new.R"))


# 1.b FUNCTION f that calls the votes function to generate the data and stores it

# f <- function(i=NULL, parlist){ # parlist optional here

s = 1:100 # round/seed count

f <- function(i=NULL){ 
    
    res = list()
    for(j in 1:nrow(parlist)){ 
        res[[j]] = votes(parlist[j,],l,group_1, group_2 , group_3 ,group_4 ,group_5 ,group_6 ,group_7 ,group_8 ,group_9 ,group_10 ,signal_1, signal_2,signal_3)
    }
    fn = paste0(here("Data/single_Data_ABM/"),"simresults_run_",l,"_round_",i,".Rda") # i = rounds, all parameters. If i=1, then all agents vote once in a given parameter combination
    
    save(res,file=fn)
}

# TEST FUNCTION f
 # f(1)

# 1.c START SIMULATION

# start simulation, specify number of voting rounds for each parameter combination in mclapply

s = 1:100 # round/seed count
rounds = 1000 # votingrounds
#ptm <- proc.time();
for (l in s){ 
    parallel::mclapply(1:rounds,f, mc.cores=4) # 1000 voting rounds each
}#;proc.time() - ptm 


# 2. LOAD and append data


# 2.1 COMBINE/APPEND all single files stored here (each file contains 1 vote round for each possible parameter combination (up to 54 x vote rounds (2) parameters))

nam = paste0(here("Data/"),"ABM Data.Rda") # name of file for appended results

list_all <- list()
file_names = list.files(here("Data/single_Data_ABM/"), all.files=FALSE,
                        full.names=FALSE)

setwd(here("Data/single_Data_ABM/"))
# store all results from all all files in a list (each file contains [number of par combinations] lists)
for (j in 1:length(file_names)) {
    load(file_names[j])
    list_all[[j]] = res
}

list_all<-unlist(list_all, recursive = FALSE) #unpack the lists 1 level

data_1 <- matrix(ncol=1913,nrow = length(list_all)) # store data in matrix (all votes, parameter combinations, seed, and correpsonding signals)
for (h in 1:length(list_all)){
    
    data_1[h,] <- unlist(list_all[[h]]) # store each results list res as a row in matrix
}
colnames(data_1)<-c('h_l','u_d', 'y_n','y_n_p', 'n_agents','pri_perc_comp_lower', 'pri_perc_comp_upper','pub_perc_comp_lower', 'pub_perc_comp_upper','run','sig_pub','sig_u_d','sig_y_n', 1:1900)
save(data_1, file = nam)

load(here("Data/ABM Data.Rda")) #contains matrix data_1

nam_ = paste0(here("Data/"),"ABM Data.Rda") # name of file for appended results
all = data.frame(data_1) # to allow subsetting, faster to load later
save(all, file = nam_) # one may choose to combine this into 1 step, we added this for efficiency reasons when loading all simulation results



### 3 ANALYSIS
# load data


#load 
load(here("Data/ABM Data.Rda"))
#contains matrix "all"

votingrounds = c(250,500,750,1000)
s=1:100 # run/seed count

threshold_u = 0.8
source(here("ABM_Classification/auxiliary functions/c_analysis.R"))


# Large Population All Data  ----------------------------------------------


g <- function(i=NULL){
    for (j in 1:nrow(parlist)) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
       # for (j in 1:1) {
        for (vrounds in votingrounds) { # slice up in different votingrounds
             #for (vrounds in 1000:1000) { # slice up in different votingrounds
            
            # subset datasets to analyse
            dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[i])
            
            
            conf <- myTryCatch(clusters(dataset,vrounds, threshold_u))
            
            # save output 
            nam10 = paste0(here("Data/classification_results/P_large_pop_robust/"),"analysis_parcombi_",j,"_rounds_",vrounds,"_run_",i,".Rda") 
            
            save(conf,file=nam10)
        }}
}

# TEST FUNCTION g
# g(1)
  
############ Large population analysis: 1000 genuine agents, 900 nefarious agents (robustness runs)

# 4 cores didn't prove stable on this machine. 3 cores seemed more stable and reliable for tad longer time.
set.seed(1) # set seed for analysis here once

# start analysis
# Went through with all 3 cores until 1-44, then one core stopped working
start_time <- Sys.time()
parallel::mclapply(1:100,g, mc.cores=3)
end_time <- Sys.time()
end_time - start_time

# Note: Kept all experiments runs in script for absolute reproducibility
# next attempt: 45-100 
set.seed(2)
# start analysis
start_time <- Sys.time()
parallel::mclapply(45:100,g, mc.cores=3)
end_time <- Sys.time()
end_time - start_time

# P_All Classification ----------------------------------------------------

############ P_All population analysis: 100 genuine agents, 900 nefarious agents 
# 100 genuine agents -> main P_all in paper

rm("myTryCatch")
rm("clusters")
rm("gmm_entropy_2")
rm("km_entropy_3")

source(here("ABM_Classification/auxiliary functions/robust_analysis.R"))


gens <-c(100)
s=1:100
ggr <- function(i=NULL){
  for (j in 1:nrow(parlist)) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
    for (vrounds in votingrounds) { # slice up in different votingrounds
      for (size in gens){
        
        
        # subset datasets to analyse
        dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[i])
        
        
        conf <- myTryCatch(clusters(dataset,vrounds, threshold_u,size)) # for robustness checks: adapt size of genuine agents
        
        # save output 
        nam10 = paste0(here("Data/classification_results/P_all/"),"analysis_parcombi_",j,"_rounds_",vrounds,"_run_",i,"_",size,"_gen_agents_.Rda") 
        
        save(conf,file=nam10)
      }}}
}


# run in two sets: runs 1:25 and 26:100, both initiated with seed(1)
set.seed(1)
parallel::mclapply(c(1:25),ggr, mc.cores=4)
set.seed(1)
parallel::mclapply(c(26:100),ggr, mc.cores=4)


# P_A_up, P_D_up, P_L_up Classification -----------------------------------


############ P_A_up, D_up, L_up populations analysis: ## Populations A_up, D_up, L_up (respectively referred to as 2,6,9)

## zoom in/ robustness check 100 genuine agents vs agent groups 2,6,9 in isolation: individual run-off

rm("myTryCatch")
rm("clusters")

source(here("ABM_Classification/auxiliary functions/analysis_2_6_9.R"))

votingrounds = c(500)
gr = list(group_2=group_2,group_6=group_6,group_9=group_9)
size = 100
threshold_u = 0.8
s=1:100
vrounds = 500
ggrr <- function(i=NULL){
  for (j in 1:3){ # nrow(parlist)) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
    for (f in 1:3){ # for each sub population
      
      
      # subset datasets to analyse
      dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[i])
      
      
      conf <- myTryCatch(clusters(dataset, vrounds, threshold_u, size, gr[[f]])) 
      
      # save output 
      nam10 = paste0(here("Data/classification_results/P_A_D_L_up/"),"analysis_par_",j,"_rounds_",vrounds,"_run_",i,"_",size,"_gen_agents_and_gr_",names(gr[f]),".Rda") 
      
      save(conf,file=nam10)
    }}
}

set.seed(1)
parallel::mclapply(1:100,ggrr, mc.cores=4)




##### re-runs regarding robustness checks 
# Run the erroronous runs 102/1600 = 6.375 % [Error identified: in rare cases, apply (mean) triggers an error during logistic regression, when a cluster is of size 1]

# Large Population Classification re-runs and robustness  ----------------------------------------------------------------------

# Par 4 -1000 # adapt parameter combi in function gg below

set.seed(3)
parallel::mclapply(c(1,6,7,11,15,16,20,22,25,31,32,34,46,47,63,64,66,67,68,72,73,74,75,83,85,88,90,91,94,97,98),gg, mc.cores=4)

set.seed(3)
# Par 4 -750 # adapt parameter combi in function gg below - (adapted in gg function)
parallel::mclapply(c(1,2,4,8,9,11,20,22,25,28,32,34,40,42,44,45,60,64,66,68,69,73,76,79,83,88,95,98),gg, mc.cores=4)

set.seed(3)
# Par 4- 500  (adapted in gg function)
parallel::mclapply(c(1,11,22,25,71,73,88),gg, mc.cores=4)

set.seed(3)
# Par 4- 250 (adapted in gg function)
parallel::mclapply(c(11,36,55),gg, mc.cores=4)

set.seed(3)
# Par 2 -250 (adapted in gg function)
parallel::mclapply(c(7,9,19,33),gg, mc.cores=4)

set.seed(3)
# Par 2 -500  (adapted in gg function)
parallel::mclapply(c(21,46,57,58,66,73,88),gg, mc.cores=4)

set.seed(3)
# Par 2 -750  (adapted in gg function)
parallel::mclapply(c(8,35,37,46,52,53,66,94,96),gg, mc.cores=4)

set.seed(3)
# Par 1 -250  (adapted in gg function)
parallel::mclapply(c(44,46,59,66,82),gg, mc.cores=4)

set.seed(3)
# Par 1 -250 (adapted in gg function)
parallel::mclapply(c(60,67),gg, mc.cores=4) # WARNING: Run 67 in this parameter combination causes error 
                                            # since there is an agents that always votes 1: this cannot be 
                                            # solved with resampling. Hence causing a 
                                            # division by 0 error in he cor(x) and subsequent svd. (Only case 1/1600)

set.seed(3)
# Par 1 - 750 (adapted in gg function)
parallel::mclapply(c(3, 27,37),gg, mc.cores=4)

set.seed(3)
# Par 1 - 1000 (adapted in gg function)
parallel::mclapply(c(6,85,91),gg, mc.cores=4)

set.seed(3)
# Par 3 - 250 (adapted in gg function)
parallel::mclapply(c(66),gg, mc.cores=4)

gg <- function(i=NULL){
  for (j in 1:1) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
    for (vrounds in 250:250) { # slice up in different votingrounds
      #for (vrounds in 1000:1000) { # slice up in different votingrounds
      
      # subset datasets to analyse
      dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[i])
      
      
      conf <- myTryCatch(clusters(dataset,vrounds, threshold_u))
      
      # save output 
      nam10 = paste0(here("Data/classification_results/P_large_pop_robust/"),"analysis_parcombi_",j,"_rounds_",vrounds,"_run_",i,".Rda") 
      
      save(conf,file=nam10)
    }}
}

###  robustness
### smaller number of genuine agents: 50, 100, 200
rm("myTryCatch")
rm("clusters")
rm("gmm_entropy_2")
rm("km_entropy_3")

source(here("ABM_Classification/auxiliary functions/robust_analysis.R"))


gens <-c(50,100,200)
s=1:100
ggr <- function(i=NULL){
  for (j in 1:nrow(parlist)) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
    for (vrounds in votingrounds) { # slice up in different votingrounds
      for (size in gens){
        
        
        # subset datasets to analyse
        dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[i])
        
        
        conf <- myTryCatch(clusters(dataset,vrounds, threshold_u,size)) # for robustness checks: adapt size of genuine agents
        
        # save output 
        nam10 = paste0(here("Data/classification_results/large_pop_robust/"),"analysis_parcombi_",j,"_rounds_",vrounds,"_run_",i,"_",size,"_gen_agents.Rda") 
        
        save(conf,file=nam10)
      }}}
}

set.seed(1)

# check robustness if there is fewer genuine agents, check for 3 seeds/runs and all combis
parallel::mclapply(c(1,50,95),ggr, mc.cores=4)

###### re-runs regarding robustness checks END 


