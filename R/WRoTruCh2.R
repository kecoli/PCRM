#' Title
#'
#' @param p 
#' @param n 
#'
#' @return
#' @export
#'
#' @examples
consRockeCh2 <- function(p,n)
{
  beta <- c(-6.1357, -1.0078, 0.81564)
  if (p >= 15) {
  a <- c(1, log(p), log(n))
  alpha <- exp(sum(beta * a))
  gamma <- qchisq(1 - alpha, df = p)/p - 1
  gamma <- min(gamma, 1)
  }
  else {
  gamma <- 1
  alpha <- 1e-06
  }
return(list(gamma = gamma, alpha = alpha))
}
#' Title
#'
#' @param tt 
#' @param gamma 
#' @param q 
#'
#' @return
#' @export
#'
#' @examples
WRoTruCh2 <- function(tt,gamma,q = 2)
{
  ss <- (tt - 1)/gamma
  w <- 1 - ss^q
  w[abs(ss) > 1] <- 0
  return(w)
}
