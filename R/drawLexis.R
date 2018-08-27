drawLexis <- function(enter, exit, event, birthdate, col = "black", add = FALSE, xlim = NULL,
                      ylim = NULL, at = NULL){
    ## x1 : first date, x2 : second date
    ## dur : x2 - x1
    ## y1 : first age
    ## y2 : second age

    x1 <- birthdate + enter
    x2 <- birthdate + exit
    y1 <- enter
    y2 <- exit
    if (is.null(xlim)){
        xlim <- c(min(x1), max(x2))
    }
    if (is.null(ylim)){
        ylim <- c(min(y1), max(y2))
    }
    if (!add){
        if (is.null(at)){
            plot(c(x1[1], x2[1]), c(y1[1], y2[1]), type = "l", ylim = ylim, xlim = xlim,
                 xlab = "Date", ylab = "Age", col = col)
        }else{
            plot(c(x1[1], x2[1]), c(y1[1], y2[1]), type = "l", ylim = ylim, xlim = xlim,
                 xlab = "Date", ylab = "Age", col = col, axes = FALSE)

            axis(1, at = at)
            axis(2)
            box()
        }
    }else{
        lines(c(x1[1], x2[1]), c(y1[1], y2[1]), col = col)
    }

    if (event[1] == 1){
        points(x2[1], y2[1], pch = "+", col = col)
    }else{
        points(x2[1], y2[1],  col = col)
    }
    for(i in 2:length(exit)){
        lines(c(x1[i], x2[i]), c(y1[i], y2[i]), col = col)
        if (event[i] == 1){
            points(x2[i], y2[i], pch = "+", col = col)
        }else{
            points(x2[i], y2[i],  col = col)
        }

    }
}
