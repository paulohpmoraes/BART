probeInfo.read <- function(inputfile="") {

#
# probeInfo.read() - Reads NIRx probeinfo file data.
#

    if (!require("R.matlab", quietly=TRUE)) {
        install.packages("R.matlab", quiet=TRUE)
    }

    if (inputfile == "") { 
        inputfile <- file.choose() 
    }

    if (!file.exists(inputfile)) {
        stop("File '", inputfile,"' not found.\n", call.=FALSE)
    }

    probeinfo <- readMat(inputfile)

    probeinfoName <<- as.character(probeinfo[[1]][[4]])     # Name of the probeInfo File
    nSrcs         <<- as.integer(probeinfo[[1]][[2]][[2]])  # Number of sources
    nDets         <<- as.integer(probeinfo[[1]][[2]][[3]])  # Number of detectors
    nChan         <<- as.integer(probeinfo[[1]][[2]][[16]]) # Number of channels
    chnPairs      <<- probeinfo[[1]][[2]][[19]]     # Pairs source-detectors that make a channel
    srcsIndex     <<- probeinfo[[1]][[2]][[8]][,2]  # Correspondence between source number and position number
    detsIndex     <<- probeinfo[[1]][[2]][[13]][,2] # Correspondence between detector number and position number
    posLabel      <<- unlist(probeinfo[[1]][[1]][[1]][[1]][[2]],use.names=FALSE)    # Correspondence between 10-10 labels and position number
    srcCoord2d    <<- probeinfo[[1]][[2]][[6]]      # Sources 2D coordinates
    srcCoord3d    <<- probeinfo[[1]][[2]][[7]]      # Sources 3D coordinates
    detCoord2d    <<- probeinfo[[1]][[2]][[11]]     # Detectors 2D coordinates
    detCoord3d    <<- probeinfo[[1]][[2]][[12]]     # Detectors 3D coordinates
    chnCoord2d    <<- probeinfo[[1]][[2]][[17]]     # Channels 2D coordinates
    chnCoord3d    <<- probeinfo[[1]][[2]][[18]]     # Channels 3D coordinates
    posCoord3d    <<- unlist(probeinfo[[1]][[1]][[1]][[1]][[1]]) # 10-10 positions 3D coordinates
    posCoord2d    <<- unlist(probeinfo[[1]][[1]][[1]][[1]][[6]]) # 10-10 positions 2D coordinates

    detach("package:R.matlab")
}