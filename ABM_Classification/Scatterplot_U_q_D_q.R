## Exemplary Scatterplot in each parameter combination


group_1 <- c(1:1000)     # 1 genuine agents
group_2 <- c(1001:1100)   # 2 nefarious - c1 Amplifier up (A_up) - U - Consp
group_3 <- c(1101:1200)   # 3 nefarious - Amplifier down (A_down) -D - Consp, c2 
group_4 <- c(1201:1300)   # 4 nefarious - Amplifier both (A_updown) - Full Consp -Parallel Universe, c3, A
group_5 <- c(1301:1400)   # 5 nefarious - Distorter downvote - AQ - High - Coordinating, cd1
group_6 <- c(1401:1500)   # 6 nefarious - Distorter upvote - AQ - Low - Coordinating, cd2
group_7 <- c(1501:1600)   # 7 nefarious - Distorter both - AQ - Always - Coordinating, cd3
group_8 <- c(1601:1700)   # 8 nefarious - Lone Wolf downvote - High - Non-Coordinating, id1
group_9 <- c(1701:1800)   # 9 nefarious - Lone Wolf upvote - Low - Non-Coordinating, id2
group_10 <- c(1801:1900)  # 10 nefarious - Lone Wolf both - Always - Non-Coordinating, id3



parlist_prep = list(h_l = c(.75),                     # sample high with probability 75%, and low with 1-75% = 25%
                    u_d = c(.75,.5,.1),               # sample up with probability 75/50/10%, and down with 1-75/50/10% = 25/50/90%
                    y_n = c(.9,.5,.1),                # sample yes with probability 90/50/10%, and no with 1-90/50/10% = 10/50/90%
                    y_n_p = c(.9,.5,.1),              # sample yes_private with probability 90/50/10%, and no_private with 1-90/50/10% = 10/50/90%
                    n_agents = c(1900),                # number of agents
                    pri_perc_comp_lower = c(75,100),   # drawing perception competence for biased agents uniformely from interval [1,1] or [0.75,0.95]
                    pri_perc_comp_upper = c(95,100),
                    pub_perc_comp_lower = c(65),     # drawing perception competence for public high/low signal uniformely from interval [0.65,0.95]
                    pub_perc_comp_upper = c(95))

# creates dataframe from all combinations of parameter values
parlist_all=expand.grid(parlist_prep, stringsAsFactors = FALSE)

par_low <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.75 & parlist_all$y_n == .9 & parlist_all$y_n_p == .9 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_med <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.5 & parlist_all$y_n == .5 & parlist_all$y_n_p == .5 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_high <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.1 & parlist_all$y_n == .1 & parlist_all$y_n_p == .1 & parlist_all$pri_perc_comp_lower==100 & parlist_all$pri_perc_comp_upper==100 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)
par_high_asterix <- subset(parlist_all, parlist_all$h_l == .75 & parlist_all$u_d ==.1 & parlist_all$y_n == .1 & parlist_all$y_n_p == .1 & parlist_all$pri_perc_comp_lower==75 & parlist_all$pri_perc_comp_upper==95 & parlist_all$pub_perc_comp_lower==65 & parlist_all$pub_perc_comp_upper==95)

parlist = rbind(par_low,par_med,par_high, par_high_asterix)

# load dataset
load(here("Data/ABM Data.Rda"))
 #contains matrix "all"

# load functions
    
    add.alpha <- function(col, alpha=1){
      if(missing(col))
        stop("Please provide a vector of colours.")
      apply(sapply(col, col2rgb)/255, 2,
            function(x)
              rgb(x[1], x[2], x[3], alpha=alpha))
    }
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
      
      
      invisible(tmp_plot) # return the collapsed plotted dimensions (but don't print)
    }
    
    
  # color vectors for plotting
    
    colb<- c('dodgerblue4','cyan3','darkslateblue','brown4','tomato1','firebrick3','forestgreen','olivedrab2','limegreen')

    col1 = c('black')#c('grey50')
    col2 = colb[1]
    col3 = colb[2]
    col4 = colb[3]
    col5 = colb[4]
    col6 = colb[5]
    col7 = colb[6]
    col8 = colb[7]
    col9 = colb[8]
    col10 = colb[9]
    

    cols <- c(rep(col1,100),#length(group_1)),
              rep(col2,length(group_2)),
              rep(col3,length(group_3)),
              rep(col4,length(group_4)),
              rep(col5,length(group_5)),
              rep(col6,length(group_6)),
              rep(col7,length(group_7)),
              rep(col8,length(group_8)),
              rep(col9,length(group_9)),
              rep(col10,length(group_10)))


    agenttypes <- c('g',
                    expression('A1'),
                    expression('A2'),
                    expression('A3'),
                    expression('D1'),
                    expression('D2'),
                    expression('D3'),
                    expression('L1'),
                    expression('L2'),
                    expression('L3'))

   # latex test
    # agenttypes <- c( "{$\mathtt{G}$}",
    #                 " {$\mathtt{A}_\uparrow$}",
    #                  "{$\mathtt{A}_\downarrow$}",
    #                  "{$\mathtt{A}_\updownarrow$}",
    #                  "{$\mathtt{D}_\downarrow$}",
    #                  "{$\mathtt{D}_\uparrow$}",
    #                  "{$\mathtt{D}_\updownarrow$}",
    #                  "{$\mathtt{L}_\downarrow$}",
    #                 " {$\mathtt{L}_\uparrow$}",
    #                  "{$\mathtt{L}_\updownarrow$}")
   
    
# pchs for plotting
    
pchs = c(rep(21,100),rep(24,100),rep(25,100),rep(23,100),rep(25,100),rep(24,100),rep(23,100),rep(25,100),rep(24,100),rep(23,100))
pchsl = c(21,24,25,23,25,24,23,25,24,23) # for the legend

cex= 1.7
cex.axis = 1.7
cex.lab = 1.7
xlim=c(-13,3.6)
ylim=c(-8,9)

  
  
    #library(tikzDevice)
    par(mfrow=c(1,3),xpd = NA,pty="m")
   # tikz(("....tex"),
   #      standAlone = FALSE, 
   #      width=2.8, 
   #      height=3.6)

    ############# 1
    s=1:100
    for (j in 1:1) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
      for (k in 3:3) { # per seed
        for (vrounds in 500:500) { # slice up in different votingrounds
          
          dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[k])
          
          return(dataset)
        }}}
    dataset = dataset[1:vrounds,]
    pars = dataset[1,1:10]     # store parameter combination
    pars[11] <- list(rounds = vrounds)
    dataset = dataset[,-(1:10)]        # Remove pars from data structure
    
    pub = dataset[,1]        # Public signal for all (unbiased) agents
    pri_u_d = dataset[,2]        # Private signal for biased agents
    pri_y_n = dataset[,3]
    dataset = dataset[,-(1:3)]        # Remove signals from data structure
    
  
    dataset =dataset[,-(101:1000)] 
    
    dataset = as.matrix(dataset)         # Turn data into a matrix structure (to enable linear algebra calculations)
    mode(dataset) = "numeric" 
    
    M = cor(dataset)          # Calculate correlation matrix
    s = svd(M)   
    
    dim1 = dim_plot(1,s, ncol(dataset), .2, TRUE) 
    dim2 = dim_plot(2,s, ncol(dataset), .2, TRUE)
    
    par(mar=c(3,0,0.2,0))
    #par(oma=c(0,1.5,0,0))
    
    plot(dim1,dim2, 
         pch=pchs, 
         col=add.alpha(c(cols),.5),
         bty='n', 
         xlim=xlim, 
         ylim=ylim, 
         #cex.main = 0.8,
         cex = cex, 
         yaxt="n",
         bg = add.alpha(c(cols),.4),
         ylab= "",
         xlab = substitute(paste(bold("U")[1],bold('D')[1])),
         cex.lab=cex.lab,
         cex.axis = cex.axis)
    box(which = "plot", lty = "solid")
    
    #mtext(substitute(paste(bold("U")[1],bold('D')[1])),side=1,line=0,outer=TRUE,cex=.8)
    mtext(substitute(paste(bold("U")[2],bold('D')[2])),side=2,line=0,outer=TRUE,las=0,cex=cex)
    text(-12.5,c(-5,0,5), c(-5,0,5), cex = cex)
    axis(2, at=c(-5,0,5), NA, cex.axis=cex.axis, tcl = 0.5)

   ############# 2
    s=1:100
    # ######## get test dataset #################################
    for (j in 2:2) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
      for (k in 3:3) { # per seed
        for (vrounds in 500:500) { # slice up in different votingrounds
          
          dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[k])
          
          return(dataset)
        }}}
     dataset = dataset[1:vrounds,]
    pars = dataset[1,1:10]     # store parameter combination
    pars[11] <- list(rounds = vrounds)
    dataset = dataset[,-(1:10)]        # Remove pars from data structure
    
    pub = dataset[,1]        # Public signal for all (unbiased) agents
    pri_u_d = dataset[,2]        # Private signal for biased agents
    pri_y_n = dataset[,3]
    dataset = dataset[,-(1:3)]        # Remove signals from data structure
    
    dim(dataset)
    dataset =dataset[,-(101:1000)] 
    
    dataset = as.matrix(dataset)         # Turn data into a matrix structure (to enable linear algebra calculations)
    mode(dataset) = "numeric" 
    
    M = cor(dataset)          # Calculate correlation matrix
    s = svd(M)   
    
    dim1 = dim_plot(1,s, ncol(dataset), .2, TRUE) 
    dim2 = dim_plot(2,s, ncol(dataset), .2, TRUE)
    
 
    par(mar=c(3,0,0.2,0))
    # par(oma=c(0,1.5,0,0))
    
    plot(dim1,dim2, 
         pch=pchs, 
         col=add.alpha(c(cols),.5),
         bty='n', 
         xlim=xlim, 
         ylim=ylim, 
         #cex.main = 0.8,
         cex = cex, 
         bg = add.alpha(c(cols),.4),
         ylab= "",
         xlab = substitute(paste(bold("U")[1],bold('D')[1])),
    cex.lab=cex.lab,
    cex.axis = cex.axis,
         yaxt='n')
    box(which = "plot", lty = "solid")
    
    ############# 3
    s=1:100
    # ######## get test dataset #################################
    for (j in 3:3) { #parlist contains all combinations of parameters, nrow(parlist) = number of combinations
      for (k in 3:3) { # per seed
        for (vrounds in 500:500) { # slice up in different votingrounds
          
          dataset <- subset(all, h_l==parlist$h_l[j] & u_d==parlist$u_d[j] & y_n==parlist$y_n[j] & y_n_p==parlist$y_n_p[j] & pri_perc_comp_lower == parlist$pri_perc_comp_lower[j] & pri_perc_comp_upper == parlist$pri_perc_comp_upper[j] & pub_perc_comp_lower == parlist$pub_perc_comp_lower[j] & pub_perc_comp_upper == parlist$pub_perc_comp_upper[j] &  run == s[k])
          
          return(dataset)
        }}}
    
    dataset = dataset[1:vrounds,]
    pars = dataset[1,1:10]     # store parameter combination
    pars[11] <- list(rounds = vrounds)
    dataset = dataset[,-(1:10)]        # Remove pars from data structure
    
    pub = dataset[,1]        # Public signal for all (unbiased) agents
    pri_u_d = dataset[,2]        # Private signal for biased agents
    pri_y_n = dataset[,3]
    dataset = dataset[,-(1:3)]        # Remove signals from data structure
    
    dim(dataset)
    dataset =dataset[,-(101:1000)] 
    
    dataset = as.matrix(dataset)         # Turn data into a matrix structure (to enable linear algebra calculations)
    mode(dataset) = "numeric" 
    
    M = cor(dataset)          # Calculate correlation matrix
    s = svd(M)   
    
    dim1 = dim_plot(1,s, ncol(dataset), .2, TRUE) 
    dim2 = dim_plot(2,s, ncol(dataset), .2, TRUE)
    
    par(mar=c(3,0,0.2,0))
    
    plot(dim1,dim2, 
         pch=pchs, 
         col=add.alpha(c(cols),.5),
         bty='n', 
         xlim=xlim, 
         ylim=ylim, 
         #cex.main = 0.8,
         cex = cex, 
         bg = add.alpha(c(cols),.4),
         ylab= "",
         xlab = substitute(paste(bold("U")[1],bold('D')[1])),
         cex.lab=cex.lab,
         cex.axis = cex.axis,
         yaxt='n')
    legend("topright", agenttypes ,ncol = 1,pch=pchsl,
            col=add.alpha(c(col1,colb),.8), bty='n', cex = cex, pt.bg = add.alpha(c(col1,colb),.7))
    box(which = "plot", lty = "solid")
    


  #  dev.off() # for tikz
    
    