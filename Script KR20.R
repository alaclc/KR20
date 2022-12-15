KR20 <- function(BD){
  primero <- ncol(BD)/(ncol(BD)-1)
  p <- sapply(BD, mean)
  q <- 1-p
  pxq <- sum(p*q)
  varianza <- var(rowSums(BD))
  segundo <- 1-(pxq/varianza)
  primero*segundo
}