library(here)
source(here("MCS/x Data1 Groups Signals Parameters.R")) # groupings, parameters, etc.
load(here("Data/ABM Data.Rda")) #loads ABM data, contains data.frame "all"
source(here("MCS/x Data3 Named Groups.R")) # named groups, gathered in     my.groups.named
source(here("MCS/x Shared Mean-SD.R")) #1 dataset subsetting function and technicality
source(here("MCS/CW1 Mean-SD Tables.R")) # Functions for building mean and sd tables for Correct Win

##  This file generates mean and SD tables of
##  for Majorioty Correctness Scores 
##  over the following seeds seeds and runs:

### RUNNING THIS FILE MAY TAKE A LONG TIME, SEE SOME STATS BELOW

my.seeds = 1:100  ## max range is 1:100. For some functions, seed range must start from 1
my.rounds = 1:1000 ## max range is 1:1000


# 1. TABLE BUILDING
                
        # Parameter 1
        start_time <- Sys.time() 
        mean.sd.table.1 <- correct.won.mean.sd.table(my.seeds, my.rounds, my.groups.named, 1)
        end_time <- Sys.time()
        saveRDS(mean.sd.table.1, here("MCS/MCS Tables/CW Mean-SD 1.rds"))
        end_time - start_time
          # 49 sec w. 1 seed
          # 9.75 min with 10 seeds (and youtube)
          # 1.27 hours for 100 seeds

        # Parameter 2
        Sys.time()
        start_time <- Sys.time() 
        mean.sd.table.2 <- correct.won.mean.sd.table(my.seeds, my.rounds, my.groups.named, 2)
        end_time <- Sys.time()
        saveRDS(mean.sd.table.2, here("MCS/MCS Tables/CW Mean-SD 2.rds"))
        end_time - start_time
        Sys.time()
        # 1.26 h

        # Parameter 3
        Sys.time()
        start_time <- Sys.time() 
        mean.sd.table.3 <- correct.won.mean.sd.table(my.seeds, my.rounds, my.groups.named, 3)
        end_time <- Sys.time()
        saveRDS(mean.sd.table.3, here("MCS/MCS Tables/CW Mean-SD 3.rds"))
        end_time - start_time                
        Sys.time()
        # 1.23 h
        
        # Parameter 4
        Sys.time()
        start_time <- Sys.time() 
        mean.sd.table.4 <- correct.won.mean.sd.table(my.seeds, my.rounds, my.groups.named, 4)
        end_time <- Sys.time()
        saveRDS(mean.sd.table.4, here("MCS/MCS Tables/CW Mean-SD 4.rds"))
        end_time - start_time                
        Sys.time()
        
        ### NOW WITH RESTRICTIONS
        
        # PARAM 1
        CW.given.up.1 <- correct.won.mean.sd.table_given.up(my.seeds, my.rounds, my.groups.named, 1)
        saveRDS(CW.given.up.1, here("MCS/MCS Tables/CW+Up Mean-SD 1.rds"))
        
        CW.given.yes.1 <- correct.won.mean.sd.table_given.yes(my.seeds, my.rounds, my.groups.named, 1)
        saveRDS(CW.given.yes.1, here("MCS/MCS Tables/CW+Yes Mean-SD 1.rds"))
        
        CW.given.up.and.yes.1 <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.groups.named, 1)
        saveRDS(CW.given.up.and.yes.1, here("MCS/MCS Tables/CW+Up+Yes Mean-SD 1.rds"))
        
        # PARAM 2
        CW.given.up.2 <- correct.won.mean.sd.table_given.up(my.seeds, my.rounds, my.groups.named, 2)
        saveRDS(CW.given.up.2, here("MCS/MCS Tables/CW+Up Mean-SD 2.rds"))
        
        CW.given.yes.2 <- correct.won.mean.sd.table_given.yes(my.seeds, my.rounds, my.groups.named, 2)
        saveRDS(CW.given.yes.2, here("MCS/MCS Tables/CW+Yes Mean-SD 2.rds"))
        
        CW.given.up.and.yes.2 <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.groups.named, 2)
        saveRDS(CW.given.up.and.yes.2, here("MCS/MCS Tables/CW+Up+Yes Mean-SD 2.rds"))
        
        # PARAM 3
        CW.given.up.3 <- correct.won.mean.sd.table_given.up(my.seeds, my.rounds, my.groups.named, 3)
        saveRDS(CW.given.up.3, here("MCS/MCS Tables/CW+Up Mean-SD 3.rds"))
        
        CW.given.yes.3 <- correct.won.mean.sd.table_given.yes(my.seeds, my.rounds, my.groups.named, 3)
        saveRDS(CW.given.yes.3, here("MCS/MCS Tables/CW+Yes Mean-SD 3.rds"))
        
        CW.given.up.and.yes.3 <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.groups.named, 3)
        saveRDS(CW.given.up.and.yes.3, here("MCS/MCS Tables/CW+Up+Yes Mean-SD 3.rds"))
        
        # PARAM 4
        CW.given.up.4 <- correct.won.mean.sd.table_given.up(my.seeds, my.rounds, my.groups.named, 4)
        saveRDS(CW.given.up.4, here("MCS/MCS Tables/CW+Up Mean-SD 4.rds"))
        
        CW.given.yes.4 <- correct.won.mean.sd.table_given.yes(my.seeds, my.rounds, my.groups.named, 4)
        saveRDS(CW.given.yes.4, here("MCS/MCS Tables/CW+Yes Mean-SD 4.rds"))
        
        CW.given.up.and.yes.4 <- correct.won.mean.sd.table_given.yes.and.up(my.seeds, my.rounds, my.groups.named, 4)
        saveRDS(CW.given.up.and.yes.4, here("MCS/MCS Tables/CW+Up+Yes Mean-SD 4.rds"))
        
        