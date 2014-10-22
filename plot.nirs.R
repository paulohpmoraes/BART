###### TEST ######
#sample input
channels <- list(ch1="PO9-PO7", ch2="PO7-O1", ch3="PO7-PO3", ch4="I1-O1", ch5="PO3-O1",
                 ch6="O1-Oz", ch7="PO3-PO2", ch8="POz-Oz", ch9="Oz-Iz", ch10="POz-PO4",
                 ch11="Oz-O2", ch12="PO4-O2", ch13="O2-I2", ch14="PO4-PO8", ch15="O2-PO8",
                 ch16="PO8-PO10")
betas <- c(0.9, 0.6, 0.75, 0.5, 0.70, 0.78, 0.85, 0.3, 0.22, 0.2, 0.3, 0.15, 0.27, 0.22, 0.32, 0.15)
#sample usage
plot.nirs.img(channels,betas)
plot.nirs.coords(channels,betas)
#################

#libraries
require(png)
require(raster)
require(EBImage)

### Function to plot results on a background image with 64 eeg positions ###
plot.nirs.img <- function(channels, betas) {
  
  #load table with eeg coordinates
  #coordinates should be between 0 and 1 with origin on the bottom left corner
  res <- 1000 #resolution of image in pixels
  ec <- as.data.frame(read.table("eegcoords64.txt",header=T))
  ec[,c(2,3)] <- round(ec[,c(2,3)]*res) #multiply x,y values in ec by res
  
  #load background image with eeg coordinates
  bg.img <- readPNG("eegmap64.png",native=F)
  bg.matrix <- bg.img[,,1] #only get one channel
  bg.raster <- raster(bg.matrix,xmn=1,xmx=res,ymn=1,ymx=res) #create a raster
  bwpalette <- colorRampPalette(c("black", "white"), bias=1)
  bg.colors <- bwpalette(255) #greyscale color ramp
  
  #create activation matrix, define color palette
  m<-matrix(0,res,res) #activation matrix
  colpalette <- colorRampPalette(c("green","yellow","red"), bias=1) #create ramp palette
  m.colors <- colpalette(255) #sample colors from palette
  m.colors <- paste(m.colors,substring(rgb(0,0,70,maxColorValue=100),6,7),sep="") #define alpha as HEX (ex: #FF00FF50)
  
  #draw a circle in a matrix m centered on coordinates x,y and with radius r and value v
  drawCircle <- function(m,x,y,r,v) {
    for(i in -r:r) { j <- round(sqrt(r^2-abs(i)^2)); m[c((x-j):(x+j)),(y+i)] <- v }
    return(m)
  }
  
  #add values to activation matrix
  for(k in 1:length(channels)) {
    #calculate middle point px,py between channels
    ch <- strsplit(channels[[k]],"-")[[1]]
    x <- c(ec[ec$name==ch[1],]$x, ec[ec$name==ch[2],]$x)
    y <- c(ec[ec$name==ch[1],]$y, ec[ec$name==ch[2],]$y)
    if(x[1]>x[2]) { dx <- x[1]-x[2]; px <- x[1]-round(dx/2) } else { dx <- x[2]-x[1]; px <- x[2]-round(dx/2) }
    if(y[1]>y[2]) { dy <- y[1]-y[2]; py <- y[1]-round(dy/2) } else { dy <- y[2]-y[1]; py <- y[2]-round(dy/2) }
    d <- sqrt(dx^2+dy^2) #distance d between channels
    #plot point in matrix m, radius of point correlates to distance between channels
    m <- drawCircle(m,px,py,d/2.5,betas[k])
  }
  
  #gaussian blur activation matrix and make raster of it
  m<-t(m[,ncol(m):1]) #flip activation matrix
  m<-gblur(m, sigma=12) #gaussian blur
  m<-ifelse(m<0.0001,NaN,m) #make small values NaN so they are transparent
  mr<-raster(m,xmn=1,xmx=res,ymn=1,ymx=res) #make raster of matrix
  
  #plot everything in R
  par(mfrow=c(1,1),mar=c(1,5,1,1),oma=c(0,0,0,0)) #define graphical parameters
  plot(bg.raster,col=bg.colors,axes=F,legend=F,box=F) #plot background image
  plot(mr,col=m.colors,add=T,axes=F,legend=F,box=F) #plot activation matrix
  
  #print result in PNG file
  png("nirsmap.png",height=res,width=res) #define output device as PNG
  par(mfrow=c(1,1),mar=c(1,5,1,1),oma=c(0,0,0,0)) #define graphical parameters
  plot(bg.raster,col=bg.colors,axes=F,legend=F,box=F) #plot background image
  plot(mr,col=m.colors,add=T,axes=F,legend=F,box=F) #plot activation matrix
  dev.off() #turn device off (saves image)
  
  if(1==0) {
    #plot background image in X11 window. use locator(n) to get mouse coordinates of clicks inside
    #the image (where n=number of coordinates). use to map coordinates of eeg locations.
    x11(display="",5,5)
    par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(1,1,1,1))
    plot.new(); rasterImage(bg.img,0,0,1,1)
    #locator(1)
  }

} #end of plot.nirs.img

### Function to plot results on a cartesian coordinate system with 128 eeg positions ###

## TO DO ##
## 1. Fix overflow of results in the activation matrix (modify blur? add margin to matrix?)

plot.nirs.coords <- function(channels, betas) {
  
  #load table with eeg coordinates
  #coordinates should be between 0 and 1 with origin at the bottom left corner
  resol <- 1000 #resolution (higher for better smoothing)
  ec <- as.data.frame(read.table("eegcoords128_2d.txt",header=T))
  ec[,c(2,3)] <- round(ec[,c(2,3)]*resol) #multiply x,y values in ec by res to enable gaussian smoothing
  
  #create activation matrix, define color palette, create list with channels
  usedchannels <- matrix(NA,length(channels)*2,3) #list of channels used
  colnames(usedchannels) <- c("names","x","y") #xy coordinates of used channels
  m <- matrix(0,resol,resol) #activation matrix
  colpalette <- colorRampPalette(c("green","yellow","red"), bias=1) #create ramp palette
  m.colors <- colpalette(255) #sample colors from palette
  m.colors <- paste(m.colors,substring(rgb(0,0,70,maxColorValue=100),6,7),sep="") #define alpha as HEX (ex: #FF00FF50)
  
  #draw a circle in a matrix m centered on coordinates x,y and with radius r and value v
  drawCircle <- function(m,x,y,r,v) {
    for(i in -r:r) { j <- round(sqrt(r^2-abs(i)^2)); m[c((x-j):(x+j)),(y+i)] <- v }
    return(m)
  }
  
  #add values to activation matrix
  for(k in 1:length(channels)) {
    ch <- strsplit(channels[[k]],"-")[[1]]
    #add channel to used channels list
    usedchannels[k*2-1,] <- c(ch[1],ec[ec$name==ch[1],]$x,ec[ec$name==ch[1],]$y)
    usedchannels[k*2,] <- c(ch[2],ec[ec$name==ch[2],]$x,ec[ec$name==ch[2],]$y)
    #calculate middle point px,py between channels
    x <- c(ec[ec$name==ch[1],]$x, ec[ec$name==ch[2],]$x)
    y <- c(ec[ec$name==ch[1],]$y, ec[ec$name==ch[2],]$y)
    if(x[1]>x[2]) { dx <- x[1]-x[2]; px <- x[1]-round(dx/2) } else { dx <- x[2]-x[1]; px <- x[2]-round(dx/2) }
    if(y[1]>y[2]) { dy <- y[1]-y[2]; py <- y[1]-round(dy/2) } else { dy <- y[2]-y[1]; py <- y[2]-round(dy/2) }
    d <- sqrt(dx^2+dy^2) #distance d between channels
    #plot point in matrix m, radius of point correlates to distance between channels
    m <- drawCircle(m,px,py,d/2.5,betas[k])
  }
  
  #gaussian blur activation matrix and make raster of it
  m<-t(m[,ncol(m):1]) #flip matrix
  m<-gblur(m, sigma=12) #gaussian blur
  m<-ifelse(m<0.0001,NaN,m) #make small values NaN so they are transparent
  mr<-raster(m,xmn=1,xmx=resol,ymn=1,ymx=resol) #make raster of matrix
  
  #plot everything in R
  par(mfrow=c(1,1),mar=c(1,5,1,1),oma=c(0,0,0,0)) #define graphical parameters
  plot(mr,col=m.colors,axes=F,legend=F,box=F) #plot activation matrix
  text(unique(usedchannels)[,2:3],unique(usedchannels)[,1],cex=0.5) #add names of used channels
  abline(v=resol/2,h=resol/2,col="gray") #add horizontal and vertical lines
  text(rbind(c(15,(resol/2-25)),c((resol-15),(resol/2-25))),c("L","R"),cex=0.6) #add R and L
  
  #print result in PNG file
  png("nirscoords.png",height=resol,width=resol) #define output device as PNG
  par(mfrow=c(1,1),mar=c(1,5,1,1),oma=c(0,0,0,0))  #define graphical parameters
  plot(mr,col=m.colors,axes=F,legend=F,box=F)  #plot activation matrix
  text(unique(usedchannels)[,2:3],unique(usedchannels)[,1], cex=1.2) #add names of used channels
  abline(v=resol/2,h=resol/2,col="gray") #add horizontal and vertical lines
  text(rbind(c(15,(resol/2-25)),c((resol-15),(resol/2-25))),c("L","R"),cex=1.4) #add R and L
  dev.off() #turn device off (saves image)

} #end of plot.nirs.coords

### Function to plot results in 3D using 5 standard views of 128 eeg positions ###

## TO DO ##
## 1. Get good resolution 3D images of 5 standard views (left, right, top, front, back) - Freesurfer?
## 2. Map location of eeg positions onto images
## 3. Filter eeg positions according to view
## 4. Multiply activation matrix by 3D image to obtain a 3D effect on the results (?)

plot.nirs.3d <- function(channels, betas) {
  
} #end of plot.nirs.3d