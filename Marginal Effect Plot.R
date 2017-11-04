
# ------------------------------------
# Create Marginal Effect Plot Function
# ------------------------------------
meplot <- function(model,var1,var2,ci=.95,xlab=var2,ylab=var1,main="Marginal Effect Plot"){
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[paste(var1,var2,sep=":")]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[paste(var1,var2,sep=":"),paste(var1,var2,sep=":")] + 2*z0*cov[var1,paste(var1,var2,sep=":")])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
       ylim=c(min(lwr),max(upr)),
       xlab = xlab,
       ylab = paste("Marginal Effect of",var1),
       main = main)
  lines(z0, dy.dx, lwd = 3)
  lines(z0, lwr)
  lines(z0, upr)
  abline(h=0,lty=2)
}


# ----------------------------------
# An example with the mtcars dataset
# ----------------------------------

# Estimate a regression model with an interaction term
mod <- lm(mpg ~ hp + wt + hp*wt, data=mtcars)

# Plot it
windows()
meplot(model=mod,var1="hp",var2="wt")

# You can use the par() function to spice things up
windows()
par(family="serif",bty="l",mar=c(5,5.5,2,2))
meplot(model=mod,var1="hp",var2="wt")
