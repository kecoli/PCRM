#' mathWtsEfrontRiskyNew
#'
#' @param returns 
#' @param mu.efront 
#' @param display.wts 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathWtsEfrontRiskyNew <- function (returns, mu.efront, display.wts = T, digits = NULL) 
{
  # This is just a dummy comment 
  V = var(returns)
  mu = apply(returns, 2, mean)
  n.assets = length(mu)
  one = rep(1, nrow(V))
  z1 = solve(V, one)
  a = as.numeric(t(mu) %*% z1)
  cc = as.numeric(t(one) %*% z1)
  z2 = solve(V, mu)
  b = as.numeric(t(mu) %*% z2)
  d = b * cc - a^2
  g1 = as.matrix((b * z1 - a * z2)/d, ncol = 1)
  g2 = as.matrix((cc * z2 - a * z1)/d, ncol = 1)
  n = length(mu.efront)
  a1 = matrix(rep(g1, n), ncol = n)
  a2 = g2 %*% mu.efront
  wts.efront = a1 + a2
  wts.efront = as.data.frame(wts.efront)
  vol.efront = (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
  vol.efront = round(vol.efront,digits)
  out = rbind(vol.efront, mu.efront, wts.efront)
  rowNames = c("VOL", "MU", paste("W", sep = "-", names(returns)))
  row.names(out) = rowNames
  names(out) = paste("Port", sep = "", 1:n)
  if(display.wts) {
    barplot.wts(as.matrix(out), legend.text = T, col = topo.colors(10), 
                ylab = "Wt", xlab = "VOL")
  }
  if(is.null(digits)) {
    out
  }
  else {
    out = sapply(out, FUN = round, digits = digits)
    row.names(out) = rowNames
    out
  }
}
# returns = midcap.ts[,1:10]
# efront = mathEfrontRisky(returns, npoints = 10, display = F)
# mu.efront = efront$mu.efront
# mathWtsEfrontRisky(returns,mu.efront,display.wts = T,digits = 2)
# END OF BAD CODE
