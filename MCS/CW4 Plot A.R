## produces Plot A and exports to tex via tikz. This "tikz update" script has changes in pch, colors, sizes to align with remainder of figures

library(here)
source(here("MCS/x Data1 Groups Signals Parameters.R")) # groupings, parameters, etc.
source(here("MCS/x Data3 Named Groups.R")) # named groups, gathered in     my.groups.named
source(here("MCS/x Shared Plotting.R")) # Provides gen.sizes function for plotting plus colors
source(here("MCS/CW3 Table Names.R")) # load tables CW1, CW2, CW3, CW4

library(tikzDevice)

pcol = add.alpha(c(col1),.8)
bgcol = add.alpha(c(col1),.8)

CW.plotA <- function(list){
  plot(gen.sizes(my.GEN), 
       unname(list[[1]][1,names(my.GEN)]), 
       bg = pcol, 
       col = bgcol, 
       pch = 21, 
       cex=1,
       #lwd = 3,
      # xaxp  = c(0, 900, 900/25),
       #yaxp  = c(0, 100, 100/5),
       xlim=c(0,550),
       ylim=c(75,100), 
       #main=paste0("Correct Won Percent: ", names(list[1])),
       xlab="", ylab="", 
       title(ylab="Majority Correctness Score", 
             xlab="Genuine Agents in Population", 
             line=2, 
             cex.lab=1)
       
      )  
  axis(1, seq(0, 525, 25), labels=rep("", 22), tck=-0.02)
  axis(1, seq(0, 500, 100), tck=-0.04, labels = (seq(0, 500, 100)),cex.axis=1)
  # abline(v = seq(0,900,25),  lty = 2, col = "grey") # Grid lines
  abline(v = c(0,100, 200, 300),  lty = 2, col = "snow4") # Grid lines
  abline(h = c(75,100), lty = 2, col = "snow4") # Grid line: % of Correct being Correct (100%). "The Truth".
}

### Produce TikZ Plot A

tikz(here("MCS/Figures/PlotA_L.tex"), 
     standAlone = FALSE, 
     width=5, 
     height=3)
### PLOT A: ----
par(mfrow=c(1,1))
now <- CWupyes.1
names(now) = c("Up and Yes Rounds (aka PLOT A)", "100%")
CW.plotA(now)
draw(my.GEN_XYZup, now,  add.alpha(c(col2),.5), add.alpha(c(col2),.8), 24, lty = 1, cex = 1.6)
draw(my.GEN_LWlow, now,  add.alpha(c(col9),.5), add.alpha(c(col9),.8), 24, cex = 1.1)
draw(my.GEN_AQlow, now,  add.alpha(c(col6),.5), add.alpha(c(col6),.8), 24, cex = 0.7)

draw(my.GEN_ALLnef, now, pcol,bgcol, "L",cex=0.8)
now <- CWupyes.2; draw(my.GEN_ALLnef, now, pcol, bgcol, "M", cex=0.8)
now <- CWupyes.3; draw(my.GEN_ALLnef, now, pcol, bgcol, "H",cex=0.8)

legend('right', c(agenttypes[1], agenttypes[2],agenttypes[6], agenttypes[9]) ,ncol = 1,pch= c(21,24,24,24), # for the legend,
       col=add.alpha(c(col1,col2,col6,col9),.8), bty='n', cex = 0.8, pt.bg = add.alpha(c(col1,col2,col6,col9),.8))

box(which = "plot", lty = "solid")

#text(518  ,83.5, 'Noise L/M/H:', cex = 1)
#text(512,82.5, 'All Nef Agents', cex = 1)


dev.off()

