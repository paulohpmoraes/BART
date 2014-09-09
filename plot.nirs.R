#input
channels <- list(ch1="PO9-PO7", ch2="PO7-O1", ch3="PO7-PO3", ch4="I1-O1", ch5="PO3-O1",
                 ch6="O1-Oz", ch7="PO3-PO2", ch8="POz-Oz", ch9="Oz-Iz", ch10="POz-PO4",
                 ch11="Oz-O2", ch12="PO4-O2", ch13="O2-I2", ch14="PO4-PO8", ch15="O2-PO8",
                 ch16="PO8-PO10")
betas <- c(0.9, 0.6, 0.75, 0.5, 0.70, 0.78, 0.85, 0.3, 0.22, 0.2, 0.3, 0.15, 0.27, 0.22, 0.32, 0.15)

#test
plot.nirs(channels,betas)

## function ##
plot.nirs <- function(channels, betas) {
  
  #libraries
  require(png)
  require(raster)
  require(EBImage)
  
  #load table with eeg coordinates
  res <- 1000 #resolution of image in pixels
  ec <- as.data.frame(read.table("eegcoords.txt",header=T))
  ec[,c(2,3)] <- round(ec[,c(2,3)]*res) #multiply x,y values in ec (between 0 and 1) by res
  
  #load background image with eeg coordinates
  bg.img <- readPNG("eegmap.png",native=F)
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
  m<-t(m[,ncol(m):1])
  m<-gblur(m, sigma=12)
  m<-ifelse(m<0.0001,NaN,m)
  mr<-raster(m,xmn=1,xmx=res,ymn=1,ymx=res)
  
  #plot everything in R
  par(mfrow=c(1,1),mar=c(1,5,1,1),oma=c(0,0,0,0))
  plot(bg.raster,col=bg.colors,axes=F,legend=F,box=F)
  plot(mr,col=m.colors,add=T,axes=F,legend=F,box=F)
  
  #print result in PNG file
  png("nirsmap.png",height=res,width=res)
  par(mfrow=c(1,1),mar=c(1,5,1,1),oma=c(0,0,0,0))
  plot(bg.raster,col=bg.colors,axes=F,legend=F,box=F)
  plot(mr,col=m.colors,add=T,axes=F,legend=F,box=F)
  dev.off()
  
  if(1==0) {
    #plot background image in X11 window. use locator(n) to get mouse coordinates of clicks inside
    #the image (where n=number of coordinates). use to map coordinates of eeg locations.
    x11(display="",5,5)
    par(mfrow=c(1,1),mar=c(0,0,0,0),oma=c(1,1,1,1))
    plot.new(); rasterImage(bg.img,0,0,1,1)
    #locator(1)
  }

}