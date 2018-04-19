
# New and improved marginal effect plot.
# Has functionality for linear, logit/probit,
# and censored regression models in the censReg
# package. Can accomodate any user specified 
# variance-covariance matrix.

meplot <- function(model,var1,var2,int,vcov,ci=.95,
                   xlab=var2,ylab=paste("Marginal Effect of",var1),
                   main="Marginal Effect Plot",
                   me_lty=1,me_lwd=1,me_col="black",
                   ci_lty=1,ci_lwd=.5,ci_col="black",
                   yint_lty=2,yint_lwd=1,yint_col="black"){
  require(ggplot2)
  alpha <- 1-ci
  z <- qnorm(1-alpha/2)
  beta.hat <- coef(model)
  cov <- vcov
  z0 <- seq(min(model.frame(model)[,var2],na.rm=T),max(model.frame(model)[,var2],na.rm=T),length.out=1000)
  dy.dx <- beta.hat[var1] + beta.hat[int]*z0
  se.dy.dx <- sqrt(cov[var1,var1] + z0^2*cov[nrow(cov),ncol(cov)] + 2*z0*cov[var1,ncol(cov)])
  upr <- dy.dx + z*se.dy.dx
  lwr <- dy.dx - z*se.dy.dx
  ggplot(data=NULL,aes(x=z0, y=dy.dx)) +
    labs(x=xlab,y=ylab,title=main) +
    geom_line(aes(z0, dy.dx),size = me_lwd, 
              linetype = me_lty, 
              color = me_col) +
    geom_line(aes(z0, lwr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    geom_line(aes(z0, upr), size = ci_lwd, 
              linetype = ci_lty, 
              color = ci_col) +
    geom_hline(yintercept=0,linetype=yint_lty,
               size=yint_lwd,
               color=yint_col)
}