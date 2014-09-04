read.nirs <- function(inputFile) {
    
    # Reads .nirs file data
    # For more information on NIRS data format see Homer2 User's Guide
    
    if (missing(inputFile)) { # If no input file, lists all .nirs files and asks for a choice
        
        nirsFiles <- list.files(pattern="\\.nirs$")
        cat("--- Current working directory: ",getwd(),"\n")
        cat("--- Nirs files in the current directory:\n")
        
        for (i in 1:length(nirsFiles)) {
            cat("---  ",i,". ",nirsFiles[i],"\n")
        } # ebd if no input file
        
        i <- readline(prompt="--- Select a file to read (q to quit): ")
        
        if (i == 'q') {
            stop("Function read.nirs() quit by user. No file read.\n", call.=FALSE)
        } # end if quit
        
        inputFile <- nirsFiles[as.integer(i)]
        
    } # end if missing input file
    
    if (file.exists(inputFile)) { # Tests if the input file exists

        # Loads library to read matlab format files, reads matlab format (.nirs) input file and creates sD list    
        if (library("R.matlab",logical.return=TRUE,warn.conflicts=FALSE,quietly=TRUE)) { # Tests if R.matlab package is installed

            nD <- readMat(inputFile)
            detach("package:R.matlab",unload=TRUE) # Unloads R.matlab package to free memory
            
            # Converts the bidimensional data matrix to 3 dimensions (frame,channel,wavelength)
            dd <- as.matrix(nD$d)
            dim(dd) <- c(dim(dd)[1],dim(dd)[2]%/%2,length(nD$SD[[4]]))
            
            # creates sD list with .nirs file data and some new variables
            sD <- list(filenm = as.character(nD$filenm),            # File name
                       d = dd,                                      # Data matrix (frame,channel,wavelength)
                       t = as.numeric(nD$t),                        # Time points vector
                       s = as.integer(nD$s),                        # Time points and condition of stimulus onsets
                       ml = as.matrix(nD$ml),                       # MeasList (see Homer2 user's guide)
                       markers = as.numeric(nD$markers),            # Time of events
                       lambda = as.numeric(nD$SD[[4]]),             # Wavelengths
                       srcPos = as.matrix(nD$SD[[6]]),              # Sources positions 
                       detPos = as.matrix(nD$SD[[7]]),              # Detectors posisitons
                       f = as.numeric(nD$SD[3]),                    # Frequency of data aquisition
                       frameSize = as.numeric(1/nD$SD[[3]]),        # Length of a frame in seconds
                       nSrcs = as.integer(nD$SD[1]),                # Number of sources
                       nDets = as.integer(nD$SD[2]),                # Number of detectors
                       nChan = as.integer(dim(dd)[2]),              # Number of channels
                       nTF = as.integer(length(nD$t)),              # Number of time frames
                       nWls = as.integer(length(nD$SD[[4]])),       # Number of wavelengths
                       nTrigs = as.integer(length(nD$markers))      # Number of triggers inputs
            ) # end sD list 
            
            # Calculates de length of the channels
            chnLen <- rep(NA,sD$nChan)
            
            for (chn in 1:sD$nChan){
                
                chnLen[chn] <- dist(rbind(sD$srcPos[sD$ml[chn,1],],sD$detPos[sD$ml[chn,2],]))
                
            } # end for
            
            sD[["chnLen"]] <- chnLen
            
            return(sD)
            
        } else  { # If R.matlab package not installed
            
            stop('Couldn\'t load R.matlab package. If not intalled use install.packages("R.matlab") to intall it.', call.=FALSE)
        
        } # end if R.matlab
        
    } else { # If file not found
        
        stop('File "',inputFile,'" not found.', call.=FALSE)
        
    } # end if file exists
}