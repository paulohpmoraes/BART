nirs.summary <- function(d) {
    
    # Display a resume of the data file parameters
    cat('Original file         :',d$filenm,'\n')
    cat('Id                    :',d$id,'\n')
    cat('Timestamp             :',d$filedate,'at',d$filetime,'\n')
    cat('Sampling rate         :',d$f,'Hz\n')
    cat('Frame length          :',d$frameSize,'s\n')
    cat('Number of frames      :',d$nTF,' \n')
    aux <- d$nTF * d$frameSize
    cat('Total time            :',aux %/% 3600,'h',aux %/% 60,'m',aux %% 60,'s\n')
    cat('Number of sources     :',d$nSrcs,'\n')
    cat('Number of detectors   :',d$nDets,'\n')
    cat('Number of channels    :',d$nChan,'\n')
    cat('Number of wavelengths :',d$nWLs,'\n')
    cat('Wavelengths           :',d$wl,'\n')
    cat('Number of triggers    :',d$nTrigs,'\n')
    t <- as.factor(d$event[,2])
    cat('           Trigger    :',levels(t),'\n')
    cat('             Count    :',summary(t),'\n')
    cat('Notes :\n',d$notes,'\n',sep='')
}