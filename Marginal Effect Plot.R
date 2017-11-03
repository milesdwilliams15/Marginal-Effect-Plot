
# ------------------------------------
# Create Marginal Effect Plot Function
# ------------------------------------
meplot <- function(model,var1,var2,int,z,xlab="Variable z",ylab="Marginal Effect of x",main="Marginal Effect Plot"){
  beta.hat <- coef(model)
  cov <- vcov(model)
  z0 <- seq(min(model$model[,var2],na.rm=T),max(model$model[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[int]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[int,int] + 2*z0*cov[var1,int])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  plot(x=z0, y=dy.dx,type="n",xlim=c(min(z0),max(z0)),
       ylim=c(min(lwr),max(upr)),
       xlab = xlab,
       ylab = ylab,
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
meplot(model=mod,var1="hp",var2="wt",int="hp:wt",
       z=1.96,xlab="Weight",ylab="Marginal Effect of Horsepower on MPG")

# You can use the par() function to spice things up
par(family="serif",bty="l",mar=c(5,5.5,2,2))
meplot(model=mod,var1="hp",var2="wt",int="hp:wt",
       z=1.96,xlab="Weight",ylab="Marginal Effect of Horsepower on MPG")
