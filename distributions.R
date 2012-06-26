library(ggplot2)
library(reshape)  # melt
library(grid)     # unit

line_width = 1.3
point_size = 4
theme_set(theme_bw(base_size=20))
theme_update(legend.key=theme_rect(colour="white"),
             legend.key.width=unit(3, "lines"),
             plot.margin=unit(rep(0, 4), "lines"))

# FIXME: is it possible to move this statement into theme_update?
scale_color_discrete = function(...) scale_color_brewer(..., palette="Dark2")

# --------------------------------------------------------------------------- #

# Plots a probability mass function.
#
# Requires: x is a molten data frame and has a column "x" representing the
# abscissa values.
plot.discrete = function(data, name, legend_labels)
{
  ggplot(data) +
    aes(x, value,
        group=variable, color=variable, shape=variable, linetype=variable) + 
    geom_line(size=line_width) + 
    geom_point(size=point_size) + 
    ylab("PMF") +
    scale_color_discrete(name="", labels=legend_labels) +
    scale_shape_discrete(name="", labels=legend_labels) +
    scale_linetype_discrete(name="", labels=legend_labels) +
    opts(title=name,
         legend.justification=c(1, 1),
         legend.position=c(1, 1))
}

plot.uniform.discrete = function()
{
  xseq = 3:8
  ggplot(data.frame(x0=factor(xseq), x1=xseq, y0=0, y1=0.5)) +
    aes(x=x0, y=y1) +
    geom_point(size=point_size) + 
#    geom_segment(aes(x=x1, xend=x1, y=y0, yend=y1), linetype="dashed") +
    opts(title="Uniform (discrete)", panel.grid.minor=theme_blank()) +
    scale_x_discrete(name="x",
                     breaks=xseq,
                     limits=1:10,
                     labels=c("a", rep("", length(xseq)-2), "b")) +
    scale_y_continuous(name="PMF",
                       breaks=0.5,
                       limits=0:1,
                       labels=expression(frac(1, n)))
}

plot.binomial = function()
{
  n = c(40, 30, 25)
  p = c(0.3, 0.6, 0.9)
  xseq = 1:40
  pmf = function(x, y) dbinom(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(pmf, n, p))), id=1)

  s = function(k) substitute(list(n==i, p==j), list(i=n[k], j=p[k]))
  plot.discrete(molten, "Binomial", lapply(1:length(n), s))
}

plot.geometric = function()
{
  p = c(0.2, 0.5, 0.8)
  xseq = 0:10
  pmf = function(x) dgeom(xseq, x)
  molten = melt(cbind(x=xseq+1, data.frame(sapply(p, pmf))), id=1)

  s = function(k) substitute(p==j, list(j=p[k]))
  plot.discrete(molten, "Geometric", lapply(1:length(p), s))
}

plot.poisson = function()
{
  lambda = c(1,4,10)
  xseq = 0:20
  N = length(lambda)
  pmf = function(x) dpois(xseq, x)
  molten = melt(cbind(x=xseq+1, data.frame(sapply(lambda, pmf))), id=1)

  s = function(k) substitute(lambda==j, list(j=lambda[k]))
  plot.discrete(molten, "Poisson", lapply(1:length(lambda), s))
}

# --------------------------------------------------------------------------- #

# Plots a probability distribution function.
#
# Requires: x is a molten data frame and has a column "x" representing the
# abscissa values.
plot.continuous = function(data, name, legend_labels)
{
  ggplot(data) +
    aes(x, value, group=variable, color=variable, linetype=variable) + 
    geom_line(size=line_width) + 
    ylab("PDF") +
    scale_color_discrete(name="", labels=legend_labels) +
    scale_linetype_discrete(name="", labels=legend_labels) +
    opts(title=name,
         legend.justification=c(1, 1),
         legend.position=c(1, 1))
}

plot.uniform.continuous = function()
{
  solid = data.frame(x0=c(1, 3,   8),
                     x1=c(3, 8,  10),
                     y0=c(0, 0.5, 0),
                     y1=c(0, 0.5, 0))
  dashed = data.frame(x0=c(solid[1,2], solid[3,1]),
                      x1=c(solid[1,2], solid[2,2]),
                      y0=c(solid[1,3], solid[3,3]),
                      y1=c(solid[2,3], solid[2,4]))
  filled = data.frame(x=c(solid[2,1], solid[3,1]),
                      y=c(solid[2,3], solid[2,3]))
  hollow = data.frame(x=c(solid[2,1], solid[3,1]),
                      y=c(solid[1,3], solid[3,3]))

  ggplot(solid) +
    geom_segment(aes(x=x0, xend=x1, y=y0, yend=y1), size=line_width) +
    geom_segment(data=dashed,
                 aes(x=x0, xend=x1, y=y0, yend=y1),
                 size=line_width,
                 linetype="dashed") +
    geom_point(data=filled, aes(x=x, y=y), size=point_size) +
    geom_point(data=hollow, aes(x=x, y=y), size=point_size, shape=21,
               fill="white") +
    opts(title="Uniform (continuous)", panel.grid.minor=theme_blank()) +
    scale_x_continuous(name="x",
                       breaks=c(solid[1,2], solid[3,1]),
                       limits=c(solid[1,1], solid[3,2]),
                       labels=c("a", "b")) +
    scale_y_continuous(name="PDF",
                       breaks=solid[2,3],
                       limits=0:1,
                       labels=expression(frac(1, b-a)))
}

plot.normal = function()
{
  mu = c(0,0,0,-2)
  s2 = c(0.2,1,5,0.5)
  xseq = seq(-5,5, by=0.01)
  f = function(x,y) dnorm(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, mu, sqrt(s2)))), id=1)

  s = function(k) substitute(list(mu==i, sigma^2==j), list(i=mu[k], j=s2[k]))
  plot.continuous(molten, "Normal", lapply(1:length(mu), s)) +
    ylab(expression(phi(x)))
}

plot.lognormal = function()
{
  mu = c(0,2,0,1/2,1/4,1/8)
  s2 = c(3,2,1,1,1,1)
  xseq = seq(0,3, by=0.01)
  f = function(x, y) dlnorm(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, mu, sqrt(s2)))), id=1)

  s = function(k) substitute(list(mu==i, sigma^2==j), list(i=mu[k], j=s2[k]))
  plot.continuous(molten, "Log-Normal", lapply(1:length(mu), s))
}

plot.student = function()
{
  nu = c(1,2,5,Inf)
  xseq = seq(-5,5, by=0.01)
  f = function(x) dt(xseq, x)
  molten = melt(cbind(x=xseq, data.frame(sapply(nu, f))), id=1)

  s = function(k) substitute(nu==i, list(i=nu[k]))
  s.last = quote(nu==infinity)
  plot.continuous(molten,
                  expression(bold("Student\'s") ~ italic(t)),
                  c(lapply(1:(length(nu)-1), s), s.last))
}

plot.chisquare = function()
{
  k = 1:5
  xseq = seq(0,8, by=0.01)
  f = function(x) dchisq(xseq, x)
  molten = melt(cbind(x=xseq, data.frame(sapply(k, f))), id=1)

  s = function(l) substitute(k == i, list(i=k[l]))
  plot.continuous(molten, expression(chi^2), lapply(1:length(k), s))
}

plot.f = function()
{
  d1 = c(1,2,5,100,100)
  d2 = c(1,1,2,1,100)
  xseq = seq(0,5, by=0.01)
  f = function(x, y) df(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, d1, d2))), id=1)

  s = function(k) substitute(list(d[1]==i, d[2]==j), list(i=d1[k], j=d2[k]))
  plot.continuous(molten, expression(chi^2), lapply(1:length(d1), s))
}

plot.exp = function()
{
  b = c(2, 1, 0.4)
  xseq = seq(0, 5, by=0.01)
  f = function(x) dexp(xseq, x)
  molten = melt(cbind(x=xseq, data.frame(sapply(b, f))), id=1)

  s = function(k) substitute(beta == i, list(i=b[k]))
  plot.continuous(molten, "Exponential", lapply(1:length(b), s))
}

plot.gamma = function()
{
  a = c(1,2,3,5,9)
  b = c(2,2,2,1,0.5)
  xseq = seq(0, 20, by=0.1)
  f = function(x, y) dgamma(xseq, x, 1/y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, a, b))), id=1)

  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  plot.continuous(molten, "Gamma", lapply(1:length(a), s))
}

plot.invgamma = function()
{
  require(MCMCpack)

  a = c(1,2,3,3)
  b = c(1,1,1,0.5)
  xseq = seq(0, 5, by=0.01)
  f = function(x,y) dinvgamma(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, a, b))), id=1)

  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  plot.continuous(molten, "Inverse Gamma", lapply(1:length(a), s))
}

#plot.dirichlet = function()
#{
#  require(MCMCpack)
#
#  a = list(c(6,2,2), c(3,7,5), c(6,2,6), c(2,3,4))
#  seqs = seq(0, 15, by=0.01) # FIXME: choose right input
#  f = function(v) ddirichlet(cbind(seqs, seqs, seqs), v)
#
#  # TODO
#  mapply(f, a, b)
#
#  s = function(k) substitute(list(alpha == i), list(i=a[k]))
#  labs = lapply(1:length(a), s)
#}

plot.beta = function()
{
  a = c(0.5,5,1,2,2)
  b = c(0.5,1,3,2,5)
  xseq = seq(0, 1, by=0.01)
  f = function(x,y) dbeta(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, a, b))), id=1)

  s = function(k) substitute(list(alpha == i, beta == j), list(i=a[k], j=b[k]))
  plot.continuous(molten, "Beta", lapply(1:length(a), s)) +
    opts(legend.justification=c(0.5, 1), legend.position=c(0.5, 1))
}

plot.weibull = function()
{
  lambda = c(1,1,1,1)
  k = c(0.5,1,1.5,5)
  xseq = seq(0, 2.5, by=0.01)
  f = function(x,y) dweibull(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, k, lambda))), id=1)

  s = function(l) substitute(list(lambda == i, k == j), 
                             list(i=lambda[l], j=k[l]))
  plot.continuous(molten, "Weibull", lapply(1:length(k), s))
}

plot.pareto = function()
{
  require(VGAM)

  xm = rep(1,3)
  a = c(1,2,4)
  xseq = seq(0,5, by=0.01)
  f = function(x,y) dpareto(xseq, x, y)
  molten = melt(cbind(x=xseq, data.frame(mapply(f, xm, a))), id=1)

  s = function(k) substitute(list(x[m] == i, alpha == j), list(i=xm[k], j=a[k]))
  plot.continuous(molten, "Pareto", lapply(1:length(a), s))
}

# --------------------------------------------------------------------------- #

# Saves a ggplot2 to disk.
#
# Requires: f outputs a ggplot2.
store = function(f, name)
{
    pdf(paste(paste("figs", name, sep="/"), "pdf", sep="."))
    print(f())
    dev.off()
}

store(plot.uniform.discrete, "uniform-discrete")
store(plot.uniform.continuous, "uniform-continuous")
store(plot.binomial, "binomial")
store(plot.geometric, "geometric")
store(plot.poisson, "poisson")
store(plot.normal, "normal")
store(plot.lognormal, "lognormal")
store(plot.poisson, "poisson")
store(plot.student, "student")
store(plot.chisquare, "chisquare")
store(plot.f, "f")
store(plot.exp, "exponential")
store(plot.gamma, "gamma")
store(plot.invgamma, "invgamma")
store(plot.beta, "beta")
store(plot.weibull, "weibull")
store(plot.pareto, "pareto")
