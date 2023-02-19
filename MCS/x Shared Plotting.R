### CORRECT WIN:
# CORRECT WIN PLOT FUNCTION 
# I think this is not used for plotting anymore.
CW.plot <- function(list){
  plot(gen.sizes(my.GEN), unname(list[[1]][1,names(my.GEN)]) , col=col1, pch = 20, 
       xaxp  = c(0, 900, 900/25),
       xlim=c(0,900),
       ylim=c(0,100), main=paste0("Correct Won Percent: ", names(list[1])),xlab="No. of Genuine Agents",ylab="% Correct"
  )  
  abline(v = seq(0,900,25),  lty = 2, col = "grey") # Grid lines
  abline(v = c(100, 200, 300),  lty = 2, col = "red") # Grid lines
  abline(h = list[2], col = col1) # Grid line: % of Correct being Correct (100%). "The Truth".
}

#. SHARED PLOTTING FUNCTIONS

# Technicality for plotting x-axis labels 
gen.sizes <- function(groups.list){ #groups list contains genuine agents groups of growing size
  sizes <- c()
  for (i in 1:length(groups.list)){
    sizes[i] <- length(unlist(groups.list[i]))
  }
  return(sizes)
}

# For drawing both points and lines
draw <- function(group, list, color, bag, point_style, draw_style = "b", lty = 1, lwd = 1, cex = 1){
  lines(gen.sizes(my.GEN),
        unname(list[[1]][1,names(group)]),
        type = draw_style, col = color, bg = bag, pch = point_style, lty = lty, lwd = lwd, cex = cex)
}


#### COLORS FOR PLOTTING:

## Colors per agent group
  
  add.alpha <- function(col, alpha=1){
    if(missing(col))
      stop("Please provide a vector of colours.")
    apply(sapply(col, col2rgb)/255, 2,
          function(x)
            rgb(x[1], x[2], x[3], alpha=alpha))
  }

  
  colb<- c('dodgerblue4','cyan3','darkslateblue','brown4','tomato1','firebrick3','forestgreen','olivedrab2','limegreen')
  
  col1 = c('black')
  col2 = colb[1]
  col3 = colb[2]
  col4 = colb[3]
  col5 = colb[4]
  col6 = colb[5]
  col7 = colb[6]
  col8 = colb[7]
  col9 = colb[8]
  col10 = colb[9]
  
  # agents<-c(rep('pub',500),rep('pri_2',50),rep('pri_3',50),rep('pri_4',50),rep('pri_5',50),rep('pri_6',50),rep('pri_7',50),rep('pri_8',50),rep('pri_9',50),rep('pri_10',50))
  # cols = c(col1,col2,col3,col4,col5,col6,col7,col8,col9,col10)[as.factor(agents)] # biased agents = orange, indep agents = blue
  
  # cols_simplified <- c(rep(col1,length(group_1)),
  #                     rep(col2,n_agents-length(group_1)))
  
  cols <- c(rep(col1,length(group_1)),
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
  
# pchs for plotting
# pchs = c(rep(21,100),rep(24,100),rep(25,100),rep(23,100),rep(25,100),rep(24,100),rep(23,100),rep(25,100),rep(24,100),rep(23,100))
   pchsl = c(21,24,25,23,25,24,23,25,24,23) # for the legend