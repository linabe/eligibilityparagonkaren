dF <- function(vals, dig = 0, ...){
  out <- formatC(vals, format = "f", digits = dig, big.mark = ",", ...)
  return(out)
}