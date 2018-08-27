Hgomp <- function(x, coe){
    ## 'coe is output from "gompertz, param = "rate"
    fn <- function(t){
        exp(coe[2]) * exp(t * coe[1])
    }
    n <- length(x)
    res <- numeric(n)
    for (i in 1:n){
        res[i] <- integrate(fn, 0, x[i])$value
    }
    res
}
