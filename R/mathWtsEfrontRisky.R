#' mathWtsEfrontRisky
#'
#' @param returns 
#' @param mu.efront 
#' @param bar.ylim 
#' @param display.wts 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathWtsEfrontRisky <-
		function(returns,mu.efront,bar.ylim=c(0,1),display.wts=T,digits=NULL)
{
	V = var(returns)
	mu = apply(returns, 2, mean)
	one = rep(1, nrow(V))
	z1 = solve(V, one)               # z1 = Vinv * 1
	a = as.numeric(t(mu) %*% z1)     # a = mutp * Vinv * 1
	cc = as.numeric(t(one) %*% z1)   # cc = 1tp * Vinv * 1
	z2 = solve(V, mu)                # z2 = Vinv * mu
	b = as.numeric(t(mu) %*% z2)     # b = mutp * Vinv* mu
	d = b * cc - a^2
	g1 = as.matrix((b*z1-a*z2)/d,ncol = 1)
	g2 = as.matrix((cc*z2-a*z1)/d,ncol = 1)
	n = length(mu.efront)
	a1 = matrix(rep(g1,n),ncol=n)
	a2 = g2%*%mu.efront
	wts.efront = a1 + a2
	MU.EFRONT = mu.efront
	VOL.EFRONT = (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
	if(display.wts) {barplot.wts(wts.efront,legend.text = paste("PORT",1:n,sep=""), col = topo.colors(10),ylab = "WEIGHTS",
				xlab = "VOL",bar.ylim = c(-2,6))}
	if(is.null(digits))
	{out = list(mu = MU.EFRONT,vol = VOL.EFRONT,wts = wts.efront)}
	else
	{out = rbind(MU.EFRONT, VOL.EFRONT,wts.efront)
		out = as.data.frame(round(out,digits=digits))
		names(out) = paste("PORT",1:n,sep="")}
	out
}

#' mathWtsEfrontRiskyMuCov
#'
#' @param muRet 
#' @param volRet 
#' @param corrRet 
#' @param mu.efront 
#' @param efront.only 
#' @param display.wts 
#' @param npoints 
#' @param digits 
#'
#' @return
#' @export
#'
#' @examples
mathWtsEfrontRiskyMuCov <- function (muRet,volRet,corrRet,mu.efront,efront.only = T,
		display.wts = T, npoints = 5, digits = NULL) 
{
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
	wts.efront = a1 + a2
	wts.efront = as.data.frame(wts.efront)
	vol.efront = (1/cc + (cc/d) * (mu.efront - a/cc)^2)^0.5
	out = rbind(vol.efront,mu.efront,wts.efront)
	rowNames = c("VOL","MU", paste("Weight",sep="",1:n.assets)) 
	row.names(out) = rowNames
	names(out) = paste("Port",sep = "",1:npoints)
	if (display.wts) {
#		wtsMat = as.matrix(wts.efront)
#		dimnames(wtsMat)[[2]] = paste("P",sep = "",1:npoints)
		barplot.wts(as.matrix(out), legend.text = T, col = topo.colors(10), 
				ylab = "WEIGHTS", xlab = "VOL")
	}
	if(is.null(digits)) {out} else
	{out = sapply(out, FUN = round, digits = digits)
		row.names(out) = rowNames
		out}
}

