probeInfo.plot <- function(color=TRUE,lbls=TRUE) {

#
# probeInfo.plot() - Plots NIRx probeinfo geometry
#

    if (!require("plotrix", quietly=TRUE)) {
        install.packages("plotrix", quiet=TRUE)
    }
    
    if(color) {
        posCol <- "grey"
        posChn <- 19
        srcCol <- "red"
        srcChn <- 19
        detCol <- "green"
        detChn <- 19
        chnCol <- "navy"
        chnChn <- 19
        chnLCol <- "blue"
    } else {
        posCol <- "gray"
        posChn <- 8
        srcCol <- "black"
        srcChn <- 16
        detCol <- "black"
        detChn <- 17
        chnCol <- "black"
        chnChn <- 15
        chnLCol <- "black"
    }
    
    plot(posCoord2d, type="n", axes=FALSE, xlab="", ylab="", main=probeinfoName, asp=1)
    
    # Plot references
    draw.circle(0, 0, posCoord2d[ 1,2], border="gray")
    draw.circle(0, 0, posCoord2d[ 3,2], border="gray")
    draw.circle(0, 0, posCoord2d[ 9,2], border="gray")
    draw.circle(0, 0, posCoord2d[21,2], border="gray")
    draw.circle(0, 0, posCoord2d[42,2], border="gray")
    segments(posCoord2d[ 1,1], posCoord2d[ 1,2], posCoord2d[136,1], posCoord2d[136,2], col="grey")
    segments(posCoord2d[58,1], posCoord2d[58,2], posCoord2d[ 68,1], posCoord2d[ 68,2], col="grey")
    text(posCoord2d[  1,1], posCoord2d[  1,2],"Nasion", col="grey", pos=3)
    text(posCoord2d[136,1], posCoord2d[136,2],"Inion",  col="grey", pos=1)
    text(posCoord2d[ 58,1], posCoord2d[ 58,2],"Left",   col="grey", pos=2)
    text(posCoord2d[ 68,1], posCoord2d[ 68,2],"Right",  col="grey", pos=4)
    
    # Plot 10-10 system
    points(posCoord2d, type="p", col=posCol, pch=posChn)
    
    # Plot channels
    segments(srcCoord2d[chnPairs[,1],1], srcCoord2d[chnPairs[,1],2], detCoord2d[chnPairs[,2],1], detCoord2d[chnPairs[,2],2], col=chnLCol)
    points(chnCoord2d, type="p", col=chnCol, pch=chnChn)
    
    # Plot sources
    points(srcCoord2d, type="p", col=srcCol, pch=srcChn)
    
    # Plot detectors
    points(detCoord2d, type="p", col=detCol, pch=detChn)
    
    if(lbls) {
        srcLblNumb <- seq(1:nSrcs)
        detLblNumb <- seq(1:nDets)
        chnLblNumb <- seq(1:nChan)
        text(posCoord2d[,1], posCoord2d[,2], posLabel, col=posCol, pos=1, cex=0.5)
        text(posCoord2d[srcsIndex,1], posCoord2d[srcsIndex,2], paste("S",as.character(srcLblNumb),sep=""), col=srcCol,pos=4, offset=0.3,cex=0.5)
        text(posCoord2d[detsIndex,1], posCoord2d[detsIndex,2], paste("D",as.character(detLblNumb),sep=""), col=detCol, pos=4, offset=0.3,cex=0.5)
        text(chnCoord2d[,1], chnCoord2d[,2], paste("C",as.character(chnLblNumb),sep=""), col=chnCol, pos=4, offset=0.3, cex=0.5)
    }    
    
    legend(posCoord2d[68,1], posCoord2d[87,2], pch=c(posChn,srcChn,detChn,chnChn), col=c("black",srcCol,detCol,chnCol), c("10-10 system","Sources","Detectors","Channels"), title="Legend")
    
    detach("package:plotrix")
}