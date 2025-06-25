#' @title Kuder Richardson Formula 20
#' @description it calculates internal consistency coefficient of a scale with binary (0 and 1) items
#' @author Alexis Junior La Cruz
#' @param a Assign the database containing only binary rating items 0 and 1
#' @return internal consistency coefficient
#' @export KR20
#' @examples
#' KR20(df)
#' result <- KR20(df)
KR20 <- function(a){
  first <- ncol(a)/(ncol(a)-1)
  p <- sapply(a,function(x) mean(x))
  q <- 1-sapply(a,function(x) mean(x))
  pxq <- sum(p*q)
  varianza <- var(rowSums(a))
  second <- 1-(pxq/varianza)
  result <- (first*second)
  result2 <- paste("KR20 =", result)
  if(result<0.50){
    print(paste("KR20 =", result, "The items are poorly correlated. The scale has low internal consistency."))
  } else {
    print(paste("KR20 = ", result, "The items of this scale have a good correlation between them. The scale has a good internal consistency"))
  }
}
