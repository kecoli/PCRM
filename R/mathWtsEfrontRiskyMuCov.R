mathWtsEfrontRiskyMuCov <- function (muRet,volRet,corrRet,mu.efront,efront.only = T,
                                     display.wts = T, npoints = 5, digits = NULL) 
{
  muRet = c(.10,.04,.02)
  volRet = c(.20,.15,.10)
  corrRet = diag(c(1,1,1))
  npoints = 3
  efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 3,display = F)
  library(mpo)  # To make barplot.wts() available to mathWtsEfrontRiskyiMuCov()
  mu.efront = efront$mu
  
  covRet = diag(volRet)%*%corrRet%*%diag(volRet)
  mu = muRet
  V = covRet
  n.assets = length(mu)
  one = rep(1, n.assets)
  z1 = solve(V, one)  # Vinv*one
  a = as.numeric(t(mu) %*% z1) # a = mu*Vinv*one
  cc = as.numeric(t(one) %*% z1) # c = one*Vinv*one
  z2 = solve(V, mu) # Vinv*mu
  b = as.numeric(t(mu) %*% z2) # b = mu*Vinv*mu
  d = b * cc - a^2
  g1 = as.matrix((b * z1 - a * z2)/d, ncol = 1)
  g2 = as.matrix((cc * z2 - a * z1)/d, ncol = 1)
  n = length(mu.efront)
  a1 = matrix(rep(g1, n), ncol = n)
  a2 = g2 %*% mu.efront
  .037*g2[,1]
  wts.efront = a1 + a2
  wts.efront = as.data.frame(wts.efront)
  vol.efront = (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
  out = rbind(vol.efront,mu.efront,wts.efront)
  rowNames = c("PortVol","PortMean", paste("Weight",sep="",1:n.assets)) 
  row.names(out) = rowNames
  names(out) = paste("Port",sep = "",1:npoints)
  if (display.wts) {
    wtsMat = as.matrix(wts.efront)
    dimnames(wtsMat)[[2]] = paste("P",sep = "",1:npoints)
    barplot.wts(as.matrix(wtsMat), legend.text = T, col = topo.colors(10), 
                ylab = "WEIGHTS", xlab = "Portfolio Volatility", bar.ylim = c(-1,2))
  }
  if(is.null(digits)) {out} else
  {out = sapply(out, FUN = round, digits = digits)
    row.names(out) = rowNames
    out}
}

# library(mpo)
# source("mathEfrontRiskyMuCov.R")
muRet = c(.10,.04,.02)
volRet = c(.20,.15,.10)
corrRet = diag(c(1,1,1))
efront = mathEfrontRiskyMuCov(muRet,volRet,corrRet, npoints = 5,display = F)
library(mpo)  # To make barplot.wts() available to mathWtsEfrontRiskyiMuCov()
mu.efront = efront$mu
mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,display.wts = F,digits = 3)
mathWtsEfrontRiskyMuCov(muRet,volRet,corrRet,mu.efront,display.wts = T,digits = 3)
