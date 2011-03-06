par(mar=c(3,3,2,1), mgp=c(2,.7,0), tck=-.01)

plot.uniform.discrete = function()
{
  xseq = 3:8
  plot(xseq, rep(0.5,length(xseq)), xlim=c(1,10), pch=19, col=2,
      xaxt="n", yaxt="n", xlab="x", ylab="PMF", main="Uniform (discrete)")
  axis(1, at=xseq, labels=c("a", rep("", length(xseq)-2), "b"))
  axis(2, at=0.5, labels=expression(frac(1, n)), las=1)

  for (x in xseq)
    segments(x, 0, x, 0.5, lty=2, col=2)

  dev.print(pdf, "figs/uniform-discrete.pdf")
}

plot.binomial = function()
{
  n = c(40,30,25)
  p = c(0.3, 0.6, 0.9)
  xseq = 1:40
  N = length(n)
  f = function(x,y) dbinom(xseq, x, y)
  matplot(xseq, mapply(f, n, p), type="b", 
      main="Binomial", xlab="x", ylab="PMF", pch=1:N)

  s = function(k) substitute(list(n==i, p==j), list(i=n[k], j=p[k]))
  legend.labels = do.call("expression", lapply(1:N, s))
  legend("topright", legend.labels, bty="n", col=1:N, pch=1:N, lty=1:N)
  dev.print(pdf, "figs/binomial.pdf")
}

plot.geometric = function()
{
  p = c(0.2, 0.5, 0.8)
  xseq = 0:10
  N = length(p)
  f = function(x) dgeom(xseq, x)
  matplot(xseq+1, sapply(p, f), type="b", xlim=range(xseq),
      main="Geometric", xlab="x", ylab="PMF", pch=1:N)

  s = function(k) substitute(p==j, list(j=p[k]))
  legend.labels = do.call("expression", lapply(1:N, s))
  legend("topright", legend.labels, bty="n", col=1:N, pch=1:N, lty=1:N)
  dev.print(pdf, "figs/geometric.pdf")
}

plot.poisson = function()
{
  lambda = c(1,4,10)
  xseq = 0:20
  N = length(lambda)
  f = function(x) dpois(xseq, x)
  matplot(xseq, sapply(lambda, f), type="b",
      main="Poisson", xlab="x", ylab="PMF", pch=1:N)

  s = function(k) substitute(lambda==j, list(j=lambda[k]))
  legend.labels = do.call("expression", lapply(1:N, s))
  legend("topright", legend.labels, bty="n", col=1:N, pch=1:N, lty=1:N)
  dev.print(pdf, "figs/poisson.pdf")
}

# ----------------------------------------------------------------------------#

plot.uniform.continuous = function()
{
  xseq = 3:7
  plot(range(xseq), rep(0.4,2), xlim=c(0,10), ylim=c(0,1), pch=19, col=2,
      xaxt="n", yaxt="n", xlab="x", ylab="PDF", main="Uniform (continuous)")
  axis(1, at=range(xseq), labels=c("a", "b"))
  axis(2, at=0.4, labels=expression(frac(1, b-a)), las=1)

  segments(0, 0, min(xseq), 0, col=2, lwd=2)
  for (x in range(xseq))
    segments(x, 0, x, 0.4, lty=2, col=2)
  segments(max(xseq), 0, 10, 0, col=2, lwd=3)
  segments(min(xseq), 0.4, max(xseq), 0.4, col=2, lwd=2)

  points(range(xseq), rep(0,2), col=2, pch=21, bg="white")

  dev.print(pdf, "figs/uniform-continuous.pdf")
}

plot.normal = function()
{
  mu = c(0,0,0,-2)
  s2 = c(0.2,1,5,0.5)
  xseq = seq(-5,5, by=0.01)
  f = function(x,y) dnorm(xseq, x, y)
  matplot(xseq, mapply(f, mu, sqrt(s2)), type="l", 
      main="Normal", xlab="x", ylab=expression(phi(x)))

  n = length(mu)
  s = function(k) substitute(list(mu==i, sigma^2==j), list(i=mu[k], j=s2[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/normal.pdf")
}

plot.lognormal = function()
{
  mu = c(0,2,0,1/2,1/4,1/8)
  s2 = c(3,2,1,1,1,1)
  xseq = seq(0,3, by=0.01)
  f = function(x,y) dlnorm(xseq, x, y)
  matplot(xseq, mapply(f, mu, sqrt(s2)), type="l", 
      main="Log-normal", xlab="x", ylab="PDF")

  n = length(mu)
  s = function(k) substitute(list(mu==i, sigma^2==j), list(i=mu[k], j=s2[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/lognormal.pdf")
}

plot.student = function()
{
  nu = c(1,2,5,Inf)
  xseq = seq(-5,5, by=0.01)
  f = function(x) dt(xseq, x)
  matplot(xseq, sapply(nu, f), type="l", 
      main=expression(bold("Student\'s") ~ italic(t)), xlab="x", ylab="PDF")

  n = length(nu)
  s = function(k) substitute(nu==i, list(i=nu[k]))
  s.last = quote(nu==infinity)
  legend.labels = do.call("expression", c(lapply(1:(n-1), s), s.last))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/student.pdf")
}

plot.chisquare = function()
{
  k = 1:5
  xseq = seq(0,8, by=0.01)
  f = function(x) dchisq(xseq, x)
  matplot(xseq, sapply(k, f), type="l", ylim=c(0,0.5),
      main=expression(chi^2), xlab="x", ylab="PDF")

  n = length(k)
  s = function(l) substitute(k == i, list(i=k[l]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/chisquare.pdf")
}

plot.f = function()
{
  d1 = c(1,2,5,100,100)
  d2 = c(1,1,2,1,100)
  xseq = seq(0,5, by=0.01)
  f = function(x,y) df(xseq, x, y)
  matplot(xseq, mapply(f, d1, d2), type="l", main="F", xlab="x", ylab="PDF")

  n = length(d1)
  s = function(k) substitute(list(d[1]==i, d[2]==j), list(i=d1[k], j=d2[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/f.pdf")
}

plot.exp = function()
{
  b = c(2, 1, 0.4)
  xseq = seq(0, 5, by=0.01)
  f = function(x) dexp(xseq, x)
  matplot(xseq, sapply(b, f), type="l", 
      main="Exponential", xlab="x", ylab="PDF")

  n = length(b)
  s = function(k) substitute(beta == i, list(i=b[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/exponential.pdf")
}

plot.gamma = function()
{
  a = c(1,2,3,5,9)
  b = c(2,2,2,1,0.5)
  xseq = seq(0,20, by=0.1)
  f = function(x,y) dgamma(xseq, x, 1/y)
  matplot(xseq, mapply(f, a, b), type="l", 
      main="Gamma", xlab="x", ylab="PDF")

  n = length(a)
  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/gamma.pdf")
}

plot.invgamma = function()
{
  require(MCMCpack)

  a = c(1,2,3,3)
  b = c(1,1,1,0.5)
  xseq = seq(0,5, by=0.01)
  f = function(x,y) dinvgamma(xseq, x, y)
  matplot(xseq, mapply(f, a, b), type="l", 
      main="Inverse Gamma", xlab="x", ylab="PDF")

  n = length(a)
  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/invgamma.pdf")
}

plot.dirichlet = function()
{
  require(MCMCpack)

  #  a = c(1,2,3)
  #  xseq = seq(0, 5, by=0.01)
  #  matplot(xseq, ddirichlet(, type="l", 
  #      main="Dirichlet", xlab="x", ylab="PDF")
  #
  #  n = length(a)
  #  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  #  legend.labels = do.call("expression", lapply(1:n, s))
  #  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  #  dev.print(pdf, "figs/dirichlet.pdf")
}

plot.beta = function()
{
  a = c(0.5,5,1,2,2)
  b = c(0.5,1,3,2,5)
  xseq = seq(0,1, by=0.01)
  f = function(x,y) dbeta(xseq, x, y)
  matplot(xseq, mapply(f, a, b), type="l", ylim=c(0,3),
      main="Beta", xlab="x", ylab="PDF")

  n = length(a)
  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("top", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/beta.pdf")
}

plot.weibull = function()
{
  lambda = c(1,1,1,1)
  k = c(0.5,1,1.5,5)
  xseq = seq(0, 2.5, by=0.01)
  f = function(x,y) dweibull(xseq, x, y)
  matplot(xseq, mapply(f, k, lambda), type="l", ylim=c(0,2.5),
      main="Weibull", xlab="x", ylab="PDF")

  n = length(k)
  s = function(l) substitute(list(lambda == i, k == j), 
      list(i=lambda[l], j=k[l]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n, lty=1:n)
  dev.print(pdf, "figs/weibull.pdf")
}

plot.pareto = function()
{
  require(VGAM)

  xm = rep(1,3)
  a = c(1,2,4)
  xseq = seq(0,5, by=0.01)
  f = function(x,y) dpareto(xseq, x, y)
  matplot(xseq, mapply(f, xm, a), type="l", col=2:5,
      main="Pareto", xlab="x", ylab="PDF")

  n = length(a)
  s = function(k) substitute(list(x[m] == i, alpha == j), list(i=xm[k], j=a[k]))
  legend.labels = do.call("expression", lapply(1:n, s))
  legend("topright", legend.labels, bty="n", col=1:n+1, lty=1:n)
  dev.print(pdf, "figs/pareto.pdf")
}

plot.uniform.discrete()
plot.uniform.continuous()
plot.binomial()
plot.geometric()
plot.poisson()
plot.normal()
plot.lognormal()
plot.student()
plot.chisquare()
plot.f()
plot.exp()
plot.gamma()
plot.invgamma()
plot.beta()
plot.weibull()
plot.pareto()
