library(here)
library(tikzDevice)
source(here("MCS/x Data1 Groups Signals Parameters.R")) # groupings, parameters, etc.
source(here("MCS/x Data3 Named Groups.R")) # named groups, gathered in     my.groups.named
source(here("MCS/x Shared Plotting.R")) # Provides gen.sizes function for plotting plus colors
source(here("MCS/CW3 Table Names.R")) # load tables CW1, CW2, CW3, CW4

### PLOTTING
pcol = add.alpha(c(col1),.8)
bgcol = add.alpha(c(col1),.8)

CW.plotBa <- function(list){
  plot(gen.sizes(my.GEN), unname(list[[1]][1,names(my.GEN)]) , col=pcol, bg=bgcol, pch = 21, cex=1,
       #xaxp  = c(0, 900, 900/25),
       yaxp  = c(0, 100, 100/5),
       xlim=c(0,525), #525
       ylim=c(0,100), 
       #main=paste0("Correct Won Percent: ", names(list[1])),
       xlab="", ylab="", xaxt='n',#, axes=FALSE,
      title(ylab="MCS", xlab="", line=1.5, cex.lab=1.1)
       #title(ylab="Majority Correctness Score", xlab="", line=0, cex.lab=1.5)
       
  )  
 # abline(v = seq(0,900,25),  lty = 2, col = "grey") # Grid lines
  abline(v = c(0,100, 200, 300),  lty = 2, col = "snow4",lwd=0.5) # Grid lines
  abline(h = c(0,100), lty = 2, col = "snow4",lwd=0.5) # Grid line: % of Correct being Correct (100%). "The Truth".
  axis(1, seq(0, 525, 25), labels=rep("", 22), tck=-0.02)
  axis(1, seq(0, 500, 100), tck=-0.05, labels = (seq(0, 500, 100)),cex.axis=1.2)
  box(which = "plot", lty = "solid")}

CW.plotBb <- function(list){
  plot(gen.sizes(my.GEN), unname(list[[1]][1,names(my.GEN)]) ,  col=pcol, bg=bgcol, pch = 21, cex=1,
    #   xaxp  = c(0, 900, 900/25),
      # yaxp  = c(0, 100, 100/5),
       xlim=c(0,525),
       ylim=c(0,100), 
       #main=paste0("Correct Won Percent: ", names(list[1])),
       xlab="", ylab="", yaxt='n', axes=FALSE,
       title(xlab="Genuine Agents in Population", line=2, cex.lab=1.2)
  )  
  # abline(v = seq(0,900,25),  lty = 2, col = "grey") # Grid lines
  abline(v = c(0,100, 200, 300),  lty = 2, col = "snow4",lwd=0.5) # Grid lines
  abline(h = c(0,100), lty = 2, col = "snow4",lwd=0.5) # Grid line: % of Correct being Correct (100%). "The Truth".
  axis(1, seq(0, 525, 25), labels=rep("", 22), tck=-0.02)
  axis(1, seq(0, 500, 100), tck=-0.05, labels = (seq(0, 500, 100)),cex.axis=1.2)
  box(which = "plot", lty = "solid")
  }

CW.plotBc <- function(list){
  plot(gen.sizes(my.GEN), unname(list[[1]][1,names(my.GEN)]) ,   col=pcol, bg=bgcol, pch = 21,cex=1,
       #xaxp  = c(0, 900, 900/25),
       #yaxp  = c(0, 100, 100/5),
       xlim=c(0,525),
       ylim=c(0,100), 
       #main=paste0("Correct Won Percent: ", names(list[1])),
        yaxt='n', xaxt='n', axes=FALSE#,cex.axis = 1.2
      # title(xlab="", line=2, cex.lab=1.2)
  )  
  # abline(v = seq(0,900,25),  lty = 2, col = "grey") # Grid lines
  abline(v = c(0,100, 200, 300),  lty = 2, col = "snow4",lwd=0.5) # Grid lines
  abline(h = c(0,100), lty = 2, col = "snow4",lwd=0.5) # Grid line: % of Correct being Correct (100%). "The Truth".
  axis(1, seq(0, 525, 25), labels=rep("", 22), tck=-0.02)
  axis(1, seq(0, 500, 100), tck=-0.05, labels = (seq(0, 500, 100)),cex.axis=1.2)
  box(which = "plot", lty = "solid")
  
  
}

### Produce TikZ Plot B

tikz(here("MCS/Figures/PlotB_L.tex"), 
     standAlone = FALSE, 
     width=4, 
     height=2.2)
#### PLOT B ----
par(mfrow=c(1,3),mar=c(3,1.5,0.2,0))
#PAR 1

now <- CW.1; CW.plotBa(now) 
draw(my.GEN_XYZup, now,  add.alpha(c(col2),1), add.alpha(c(col2),.8), 24,cex=1.6)
draw(my.GEN_XYZdown, now,  add.alpha(c(col3),1), add.alpha(c(col3),.8), 25,cex=1.2)
draw(my.GEN_XYZalways, now,  add.alpha(c(col4),1), add.alpha(c(col4),.8), 23,cex=1.2)

draw(my.GEN_AQhigh, now,  add.alpha(c(col5),1), add.alpha(c(col5),.8), 25,cex=1.2)
draw(my.GEN_AQlow, now,  add.alpha(c(col6),1), add.alpha(c(col6),.8), 24,cex=1.2)
draw(my.GEN_AQalways, now,  add.alpha(c(col7),1), add.alpha(c(col7),.8), 23,cex=1.2)

draw(my.GEN_LWhigh, now,  add.alpha(c(col8),1), add.alpha(c(col8),.8), 25,cex=1.2)
draw(my.GEN_LWlow, now,  add.alpha(c(col9),1), add.alpha(c(col9),.8), 24,cex=1.2)
draw(my.GEN_LWalways, now,  add.alpha(c(col10),1), add.alpha(c(col10),.8), 23,cex=1.2)
draw(my.GEN_ALLnef, now,  add.alpha(c(col1),.8), 23, "L",cex=0.9)

#text(500,50,'MCS', cex = 1,srt=90)

 #text(505,c(seq(0, 100, by=10)), c(seq(0, 100, by=10)), cex = 1.2)
# axis(4, at=c(seq(0, 100, by=10)), NA, cex.axis=0.9, tcl = 0.5,tck=0.02)

par(mar=c(3,0.3,0.2,0))
now <- CW.2; CW.plotBb(now)
draw(my.GEN_XYZup, now,  add.alpha(c(col2),1), add.alpha(c(col2),.8), 24,cex=1.6)
draw(my.GEN_XYZdown, now,  add.alpha(c(col3),1), add.alpha(c(col3),.8), 25,cex=1.6)
draw(my.GEN_XYZalways, now,  add.alpha(c(col4),1), add.alpha(c(col4),.8), 23,cex=1.2)

draw(my.GEN_AQhigh, now,  add.alpha(c(col5),1), add.alpha(c(col5),.8), 25,cex=1.2)
draw(my.GEN_AQlow, now,  add.alpha(c(col6),1), add.alpha(c(col6),.8), 24,cex=1.2)
draw(my.GEN_AQalways, now,  add.alpha(c(col7),1), add.alpha(c(col7),.8), 23,cex=1.2)

draw(my.GEN_LWhigh, now,  add.alpha(c(col8),1), add.alpha(c(col8),.8), 25,cex=1.2)
draw(my.GEN_LWlow, now,  add.alpha(c(col9),1), add.alpha(c(col9),.8), 24,cex=1.2)
draw(my.GEN_LWalways, now,  add.alpha(c(col10),1), add.alpha(c(col10),.8), 23,cex=1.2)

draw(my.GEN_ALLnef, now,  add.alpha(c(col1),.8), 23, "M",cex=0.9)

# text(505,c(seq(0, 100, by=10)), c(seq(0, 100, by=10)), cex = 1.1)
# axis(4, at=c(seq(0, 100, by=10)), NA, cex.axis=0.9, tcl = 0.5,tck=0.02)

par(mar=c(3,0.3,0.2,0.4))
now <- CW.3; CW.plotBc(now)
draw(my.GEN_XYZup, now,  add.alpha(c(col2),1), add.alpha(c(col2),.8), 24,cex=1.6)
draw(my.GEN_XYZdown, now,  add.alpha(c(col3),1), add.alpha(c(col3),.8), 25,cex=1.2)
draw(my.GEN_XYZalways, now,  add.alpha(c(col4),1), add.alpha(c(col4),.8), 23,cex=1.2)

draw(my.GEN_AQhigh, now,  add.alpha(c(col5),1), add.alpha(c(col5),.8), 25,cex=1.2)
draw(my.GEN_AQlow, now,  add.alpha(c(col6),1), add.alpha(c(col6),.8), 24,cex=1.2)
draw(my.GEN_AQalways, now,  add.alpha(c(col7),1), add.alpha(c(col7),.8), 23,cex=1.2)

draw(my.GEN_LWhigh, now,  add.alpha(c(col9),1), add.alpha(c(col9),.8), 25,cex=1.2)
draw(my.GEN_LWlow, now,  add.alpha(c(col9),1), add.alpha(c(col9),.8), 24,cex=1.6)
draw(my.GEN_LWalways, now,  add.alpha(c(col10),1), add.alpha(c(col10),.8), 23,cex=1.2)
draw(my.GEN_ALLnef, now, add.alpha(c(col1),.8), 23, "H",cex=0.9)

legend("right", agenttypes ,ncol = 1,pch=pchsl,
       col=add.alpha(c(col1,colb),.8), bty='n', pt.bg = add.alpha(c(col1,colb),.7),cex=1.1)

dev.off()

