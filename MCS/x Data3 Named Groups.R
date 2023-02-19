### my.groups.named is defined in bottom of this file

my.GEN <- list(G1=c(1:1), G3=c(1:3), G5=c(1:5), G10=c(1:10),
               G25=c(1:25), G50=c(1:50), G75=c(1:75),
               G100=c(1:100), G125=c(1:125), G150=c(1:150), G175=c(1:175), 
               G200=c(1:200), G225=c(1:225), G250=c(1:250), G275=c(1:275), 
               G300=c(1:300), G325=c(1:325), G350=c(1:350), G375=c(1:375),
               G400=c(1:400), G425=c(1:425), G450=c(1:450), G475=c(1:475),
               G500=c(1:500), G525=c(1:525), G550=c(1:550), G575=c(1:575),
               G600=c(1:600),
               G900=c(1:900)
)
#my.groups.named2 <- c(my.GEN)

my.GEN_XYZup <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup[[i]] <- c(unlist(my.GEN[i]),group_2)
  names(my.GEN_XYZup)[i] <- paste(names(my.GEN)[i],"XYZup")
}
#my.groups.named2 <- c(my.groups.named2, my.GEN_XYZup)

my.GEN_XYZdown <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZdown[[i]] <- c(unlist(my.GEN[i]),group_3)
  names(my.GEN_XYZdown)[i] <- paste(names(my.GEN)[i],"XYZdown")
}

my.GEN_XYZalways <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZalways[[i]] <- c(unlist(my.GEN[i]),group_4)
  names(my.GEN_XYZalways)[i] <- paste(names(my.GEN)[i],"XYZalways")
}

my.GEN_AQhigh <- list()
for (i in 1:length(my.GEN)){
  my.GEN_AQhigh[[i]] <- c(unlist(my.GEN[i]),group_5)
  names(my.GEN_AQhigh)[i] <- paste(names(my.GEN)[i],"AQhigh")
}

my.GEN_AQlow <- list()
for (i in 1:length(my.GEN)){
  my.GEN_AQlow[[i]] <- c(unlist(my.GEN[i]),group_6)
  names(my.GEN_AQlow)[i] <- paste(names(my.GEN)[i],"AQlow")
}

my.GEN_AQalways <- list()
for (i in 1:length(my.GEN)){
  my.GEN_AQalways[[i]] <- c(unlist(my.GEN[i]),group_7)
  names(my.GEN_AQalways)[i] <- paste(names(my.GEN)[i],"AQalways")
}

my.GEN_LWhigh <- list()
for (i in 1:length(my.GEN)){
  my.GEN_LWhigh[[i]] <- c(unlist(my.GEN[i]),group_8)
  names(my.GEN_LWhigh)[i] <- paste(names(my.GEN)[i],"LWhigh")
}

my.GEN_LWlow <- list()
for (i in 1:length(my.GEN)){
  my.GEN_LWlow[[i]] <- c(unlist(my.GEN[i]),group_9)
  names(my.GEN_LWlow)[i] <- paste(names(my.GEN)[i],"LWlow")
}

my.GEN_LWalways <- list()
for (i in 1:length(my.GEN)){
  my.GEN_LWalways[[i]] <- c(unlist(my.GEN[i]),group_10)
  names(my.GEN_LWalways)[i] <- paste(names(my.GEN)[i],"LWalways")
}


### DOUBLE NEFARIOUS 
# XYZ + AQ
my.GEN_XYZup_AQhigh <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_AQhigh[[i]] <- c(unlist(my.GEN[i]),group_2,group_5)
  names(my.GEN_XYZup_AQhigh)[i] <- paste(names(my.GEN)[i],"XYZup_AQhigh")
}

my.GEN_XYZup_AQlow <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_AQlow[[i]] <- c(unlist(my.GEN[i]),group_2,group_6)
  names(my.GEN_XYZup_AQlow)[i] <- paste(names(my.GEN)[i],"XYZup_AQlow")
}

my.GEN_XYZup_AQal <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_AQal[[i]] <- c(unlist(my.GEN[i]),group_2,group_7)
  names(my.GEN_XYZup_AQal)[i] <- paste(names(my.GEN)[i],"XYZup_AQal")
}

# XYZ + LW

my.GEN_XYZup_LWal <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_LWal[[i]] <- c(unlist(my.GEN[i]),group_10,group_2)
  names(my.GEN_XYZup_LWal)[i] <- paste(names(my.GEN)[i],"XYZup_LWal")
}

my.GEN_XYZup_LWlow <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_LWlow[[i]] <- c(unlist(my.GEN[i]),group_9,group_2)
  names(my.GEN_XYZup_LWlow)[i] <- paste(names(my.GEN)[i],"XYZup_LWlow")
}

my.GEN_XYZup_LWhigh <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_LWhigh[[i]] <- c(unlist(my.GEN[i]),group_8,group_2)
  names(my.GEN_XYZup_LWhigh)[i] <- paste(names(my.GEN)[i],"XYZup_LWhigh")
}

### TRIPLE NEFARIOUS

my.GEN_XYZup_AQhigh_LWhigh <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_AQhigh_LWhigh[[i]] <- c(unlist(my.GEN[i]),group_2,group_6,group_9)
  names(my.GEN_XYZup_AQhigh_LWhigh)[i] <- paste(names(my.GEN)[i],"XYZup_AQhigh_LWhigh")
}

my.GEN_XYZup_AQlow_LWlow <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_AQlow_LWlow[[i]] <- c(unlist(my.GEN[i]),group_2,group_6,group_9)
  names(my.GEN_XYZup_AQlow_LWlow)[i] <- paste(names(my.GEN)[i],"XYZup_AQlow_LWlow")
}

my.GEN_XYZup_AQal_LWal <- list()
for (i in 1:length(my.GEN)){
  my.GEN_XYZup_AQal_LWal[[i]] <- c(unlist(my.GEN[i]),group_2,group_7,group_10)
  names(my.GEN_XYZup_AQal_LWal)[i] <- paste(names(my.GEN)[i],"XYZup_AQal_LWal")
}


### ALL NEFARIOUS

my.GEN_ALLnef <- list()
for (i in 1:length(my.GEN)){
  my.GEN_ALLnef[[i]] <- c(unlist(my.GEN[i]),
                                group_2, group_3, group_4,
                                group_5, group_6, group_7,
                                group_8, group_9, group_10
                                )
  names(my.GEN_ALLnef)[i] <- paste(names(my.GEN)[i],"ALLnef")
}

### COLLECT ALL GROUPS NAMED

my.groups.named <- c(my.GEN,
                     my.GEN_XYZup,
                     my.GEN_XYZdown,
                     my.GEN_XYZalways,
                     my.GEN_AQhigh,
                     my.GEN_AQlow,
                     my.GEN_AQalways,
                     my.GEN_LWhigh,
                     my.GEN_LWlow,
                     my.GEN_LWalways,
                     my.GEN_XYZup_AQhigh,
                     my.GEN_XYZup_AQlow,
                     my.GEN_XYZup_AQal,
                     my.GEN_XYZup_LWhigh,
                     my.GEN_XYZup_LWlow,
                     my.GEN_XYZup_LWal,
                     my.GEN_XYZup_AQhigh_LWhigh,
                     my.GEN_XYZup_AQlow_LWlow,
                     my.GEN_XYZup_AQal_LWal,
                     my.GEN_ALLnef
)



