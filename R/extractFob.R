extractFob <- function(year, followUp, data){
    ## data = fob (or lisa?)
    
    out <- data[data$AR == year, ]
    
    library(eha)
    
    out$fobdate <- as.numeric(toTime(paste(year, "11", "01", sep = "-")))
    weq <- (!is.na(out$deathdate)) & (out$deathdate <= out$fobdate)
    out <- out[!weq, ]    
    out$enter <- out$fobdate - out$birthdate
    
    ## Bug fix 2018-06-18: 

    ##out$event <- as.numeric(!(is.na(out$deathdate)) & (out$deathdate <= out$fobdate + followUp))
    ##out$exit <- out$enter + followUp
    ##out$exit[out$event] <- out$deathdate[out$event] - out$birthdate[out$event]
    event <- !(is.na(out$deathdate)) & (out$deathdate <= out$fobdate + followUp)
    out$exit <- out$enter + followUp
    out$exit[event] <- out$deathdate[event] - out$birthdate[event]
    out$event <- as.numeric(event)
    
    ## End Bug fix
    
    out
}