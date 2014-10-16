read.NIRx <- function(inputFile,save=FALSE,name,id='none') {
    
    # Read data from NIRx data folders
    
    hdrFile   <- paste(inputFile,"hdr",sep=".")
    txtFile   <- paste(inputFile,"config.txt",sep="_")
    wl1File   <- paste(inputFile,"wl1",sep=".")
    wl2File   <- paste(inputFile,"wl2",sep=".")

    # Reads config file (_config.txt)
    if (file.exists(hdrFile)) { # Tests if the input file exists
        connection <- file(hdrFile, encoding="UTF-8")    
        hdrLines  <- readLines(connection, warn=FALSE)
        close(connection)
        
        readValue <- function(value) {
            linha <- 0
            repeat {
                linha <- linha + 1
                if (sub("=.*","",hdrLines[linha]) == value) {
                    return(sub(".*=","",hdrLines[linha]))
                }
                if (linha == length(hdrLines)) { break }
            }
        }
        
        getLine <- function(value) {
            linha <- 0
            repeat {
                linha <- linha + 1
                if (sub("=.*","",hdrLines[linha]) == value) {
                    return(linha)
                }
                if (linha == length(hdrLines)) { break }
            }
        }
        
        filenm     <- as.character(readValue("FileName"))
        filenm     <- substring(filenm,2,nchar(filenm)-1)
        filedate   <- as.character(readValue("Date"))
        filedate   <- substring(filedate,2,nchar(filedate)-1)
        filetime   <- as.character(readValue("Time"))
        filetime   <- substring(filetime,2,nchar(filetime)-1)
        notes      <- as.character(readValue("Notes"))
        notes      <- substring(notes,2,nchar(notes)-1)
        nSrcs      <- as.integer(readValue("Sources"))
        nDets      <- as.integer(readValue("Detectors"))
        nWLs       <- as.integer(readValue("Wavelengths"))
        nTrigs     <- as.integer(readValue("TrigIns"))
        f          <- as.numeric(readValue("SamplingRate"))

        if (nTrigs > 0) {
            event <- matrix(scan(hdrFile,skip=getLine("Events"),nmax=nTrigs*3,what=numeric(),quiet=TRUE),nrow=nTrigs,ncol=3,byrow=TRUE,dimnames=list(c(),c('time','trigger','frame')))
        } else {
            event <- matrix()
        }
        
    }
    else { # If file not found        
        stop("Header file '", hdrFile,"' not found\n", call.=FALSE)
    } # end if file exists
    
    # Reads config file (_config.txt)
    if (file.exists(txtFile)) {
        connection <- file(txtFile, encoding="UTF-8")    
        txtLines  <- readLines(connection, warn=FALSE)
        close(connection)
        linha <- 0
        repeat {
            linha <- linha + 1
            if (sub("=.*","",txtLines[linha]) == "time_point_N") {
                nTF <- as.integer(sub(";","",sub(".*=","",txtLines[linha])))
            }
            if (linha == length(txtLines)) { break }
        }
    } else {
        stop("Config file '", txtFile,"' not found\n", call.=FALSE)
    }
    
    # Reads wavelength 1 file (.wl1)
    if (file.exists(wl1File)) {
        wl1 <- matrix(scan(wl1File,what=numeric(),nmax=nTF*nSrcs*nDets,quiet=TRUE), nrow = nTF, ncol=nSrcs*nDets, byrow = TRUE)
    } else {
        stop("Wavelength 1 file '", wl1File,"' not found\n", call.=FALSE)
    }
    
    # Reads wavelength 2 file (.wl2)
    if (file.exists(wl1File)) {
        wl2 <- matrix(scan(wl2File,what=numeric(),nmax=nTF*nSrcs*nDets,quiet=TRUE), nrow = nTF, ncol=nSrcs*nDets, byrow = TRUE)
    } else {
        stop("Wavelength 2 file '", wl2File,"' not Found\n", call.=FALSE)
    }

    sdMask <- scan(hdrFile,skip=getLine("S-D-Mask"),nmax=nSrcs*nDets,what=integer(),quiet=TRUE)
    nChan <- sum(sdMask) # Counts the number or channels
    sdMask <- as.logical(sdMask)
    wl1 <- wl1[,sdMask] # Keeps only the channels defined
    wl2 <- wl2[,sdMask] # Keeps only the channels defined
    d <- cbind(wl1,wl2)
    dim(sdMask) <- c(nSrcs,nDets)
    channel <- which(t(sdMask),arr.ind=TRUE,useNames=FALSE) # Finds the srcs x dets actives in sdMask matrix
    channel[,c(1,2)] <- channel[,c(2,1)] # Put the columns in the right order: src x det
    dimnames(channel) <- list(c(),c('source','detector'))
    
    frameSize <- 1/f
    t  <- seq(frameSize,nTF,frameSize)
    wl <- c(0,0)
    srcPos <- matrix(rep(0,times=nSrcs),nrow=nSrcs,ncol=3,dimnames=list(c(),c('x','y','z')))
    detPos <- matrix(rep(0,times=nDets),nrow=nDets,ncol=3,dimnames=list(c(),c('x','y','z')))
    
    sD <- list(filenm = filenm,           # File name
               filedate = filedate,       # File date
               filetime = filetime,       # File time
               id     = id,               # Identification
               notes  = notes,            # File notes
               d      = d,                # Data matrix
               t      = t,                # Time points vector
               event  = event,            # Stimulus onsets
               f      = f,                # Frequency of data aquisition
               frameSize = frameSize,     # Length of a frame in seconds
               wl     = wl,          # Wavelengths
               nSrcs  = nSrcs,            # Number of sources
               nDets  = nDets,            # Number of detectors
               nChan  = nChan,            # Number of channels
               nWLs   = nWLs,             # Number of wavelengths
               nTrigs = nTrigs,           # Number of triggers inputs
               nTF    = nTF,              # Number of time frames
               channel = channel,         # Channels
               srcPos = srcPos,           # Sources positions 
               detPos = detPos            # Detectors posisitons
               )

    if (save) {
        if (missing(name)) { name <- filenm }
        name <- paste(name,"rds",sep=".")
        if (!file.exists(name)) {
            saveRDS(sD,file=name,compress="bzip2")
            cat("Data saved as",name,"\n")
        }
        else {
            cat("Warning: File '",name,"' already exists. The data was NOT saved.")
        }
    }

    return(sD)
        
}