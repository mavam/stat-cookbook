# Install needed packages if necessary
needed_packages = c("ggplot2", "reshape2", "grid", "RColorBrewer", "VGAM")
if (length(setdiff(needed_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(needed_packages, rownames(installed.packages())),
                   dependencies=TRUE, repos="http://cran.r-project.org")
}

library(ggplot2)
library(reshape2)
library(grid)
library(RColorBrewer)
library(VGAM) # [dp]pareto

line_width = 1.3
point_size = 4
theme_set(theme_bw(base_size=20))
theme_update(legend.key=element_rect(colour="white"),
             legend.key.width=unit(3, "lines"),
             plot.margin=unit(rep(0, 4), "lines"))

# FIXME: is it possible to move this statement into theme_update?
scale_color_discrete = function(...) scale_color_brewer(..., palette="Dark2")


# --------------------------------------------------------------------------- #

make.dist.fn <- function(mode, dist) {
  if (mode == "cdf")
    eval(parse(text=paste("p", dist, sep="")))
  else if (mode == "pdf" || mode == "pmf")
    eval(parse(text=paste("d", dist, sep="")))
  else
    stop("invalid mode: must be 'cdf' or 'pdf/pmf'")
}

make.data <- function(mode, dist, theta, xseq) {
  dist.fn <- make.dist.fn(mode, dist)
  unary <- function(...) function(x) dist.fn(x, ...)
  data.fns <- apply(theta, 1, function(x) do.call(unary, as.list(t(x))))
  values <- data.frame(sapply(data.fns, function(f) f(xseq)))
  cbind(x=xseq, values)
}

plot.dist <- function(xseq, theta, dist, mode, title, lab.fn) {
  values <- make.data(mode, dist, theta, xseq)
  molten <- melt(values, 1)
  labels <- apply(theta, 1, function(x) do.call(lab.fn, as.list(t(x))))
  p <- ggplot(molten, aes(x=x, y=value, color=variable, linetype=variable)) +
       ggtitle(title) +
       ylab(toupper(mode)) +
       scale_color_discrete(labels=labels) +
       scale_linetype_discrete(labels=labels)

  # We position the legend for CDFs bottom-right and for P[MD]Fs top-right.
  if (mode == "cdf")
    p <- p + theme(legend.title=element_blank(),
                   legend.justification=c(1, 0),
                   legend.position=c(1, 0))
  else
    p <- p + theme(legend.title=element_blank(),
                   legend.justification=c(1, 1),
                   legend.position=c(1, 1))
  p
}

plot.discrete <- function(from, to, ...) {
  xseq <- seq(from, to)
  plot.dist(xseq, ...) +
    geom_line(size=line_width) +
    geom_point(size=point_size)
}

plot.continuous <- function(from, to, ...) {
  xseq <- seq(from, to, by=0.01)
  plot.dist(xseq, ...) +
    geom_line(size=line_width)
}

# --------------------------------------------------------------------------- #

plot.uniform.cdf.discrete <- function() {
  xseq <- 3:7
  x <- melt(as.data.frame(cbind(xseq, ecdf(xseq)(xseq))), 1)
  ggplot(x, aes(x=xseq, y=value)) +
    geom_point(size=point_size) +
    geom_segment(aes(x=xseq, y=value, xend=xseq+1, yend=value)) +
    geom_segment(aes(x=3.1, y=0.2, xend=4, yend=0.2)) +
    geom_segment(aes(x=6, y=0.8, xend=6.9, yend=0.8)) +
    geom_point(aes(x=xseq+1), size=point_size, color="white", shape=19) +
    geom_point(aes(x=xseq+1), size=point_size, shape=1) +
    ggtitle("Uniform (discrete)") +
    labs(x="x", y="CDF") +
    theme(panel.grid.minor=element_blank()) +
    scale_x_continuous(name="x", limits=c(3.1, 6.9), breaks=4:6,
                       labels=c("a", "", "b")) +
    scale_y_continuous(name="CDF", limits=c(0.2, 0.8),
                       breaks=c(0.2, 0.4, 0.6, 0.8),
                       labels=c(0, expression(frac(i, n)),
                                expression(frac(i, n)), 1.0))
}

plot.uniform.cdf.continuous <- function() {
  x <- as.data.frame(rbind(c(0,0,2,0), c(2,0,6,1), c(6,1,8,1)))
  ggplot(x) +
    geom_segment(aes(x=V1, y=V2, xend=V3, yend=V4)) +
    ggtitle("Uniform (continuous)") +
    labs(x="x", y="CDF") +
    theme(panel.grid.minor=element_blank()) +
    scale_x_continuous(breaks=c(2,6), labels=c("a", "b")) +
    scale_y_continuous(limits=0:1, breaks=0:1, labels=0:1)
}

plot.uniform.pmf <- function() {
  xseq <- 3:8
  ggplot(data.frame(x0=factor(xseq), x1=xseq, y0=0, y1=0.5)) +
    aes(x=x0, y=y1) +
    geom_point(size=point_size) +
#    geom_segment(aes(x=x1, xend=x1, y=y0, yend=y1), linetype="dashed") +
    labs(title="Uniform (discrete)") +
    theme(panel.grid.minor=element_blank()) +
    scale_x_discrete(name="x",
                     breaks=xseq,
                     limits=1:10,
                     labels=c("a", rep("", length(xseq)-2), "b")) +
    scale_y_continuous(name="PMF",
                       breaks=0.5,
                       limits=0:1,
                       labels=expression(frac(1, n)))
}

plot.uniform.pdf <- function() {
  solid <- data.frame(x0=c(1, 3,   8),
                      x1=c(3, 8,  10),
                      y0=c(0, 0.5, 0),
                      y1=c(0, 0.5, 0))
  dashed <- data.frame(x0=c(solid[1,2], solid[3,1]),
                       x1=c(solid[1,2], solid[2,2]),
                       y0=c(solid[1,3], solid[3,3]),
                       y1=c(solid[2,3], solid[2,4]))
  filled <- data.frame(x=c(solid[2,1], solid[3,1]),
                       y=c(solid[2,3], solid[2,3]))
  hollow <- data.frame(x=c(solid[2,1], solid[3,1]),
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
    theme(panel.grid.minor=element_blank()) +
    ggtitle("Uniform (continuous)") +
    scale_x_continuous(name="x",
                       breaks=c(solid[1,2], solid[3,1]),
                       limits=c(solid[1,1], solid[3,2]),
                       labels=c("a", "b")) +
    scale_y_continuous(name="PDF",
                       breaks=solid[2,3],
                       limits=0:1,
                       labels=expression(frac(1, b-a)))
}

# --------------------------------------------------------------------------- #

plot.binomial = function(mode, xmin=1, xmax=40,
                         theta=data.frame(n=c(40, 30, 25), p=c(0.3, 0.6, 0.9)),
                         title="Binomial") {
  lab.fn <- function(x, y) substitute(list(n==i, p==j), list(i=x, j=y))
  plot.discrete(xmin, xmax, theta, "binom", mode, title, lab.fn)
}

plot.geometric <- function(mode, xmin=0, xmax=10,
                           theta=data.frame(p=c(0.2, 0.5, 0.8)),
                           title="Geometric") {
  lab.fn <- function(x) substitute(p==i, list(i=x))
  plot.discrete(xmin, xmax, theta, "geom", mode, title, lab.fn)
}

plot.poisson <- function(mode, xmin=0, xmax=20,
                         theta=data.frame(lambda=c(1,4,10)),
                         title="Poisson") {
  lab.fn <- function(x) substitute(lambda==i, list(i=x))
  plot.discrete(xmin, xmax, theta, "pois", mode, title, lab.fn)
}

# --------------------------------------------------------------------------- #

plot.normal <- function(mode, xmin=-5, xmax=5,
                        theta=data.frame(mu=c(0,0,0,-2), s2=c(0.2,1,5,0.5)),
                        title="Normal") {
  lab.fn <- function(x, y) substitute(list(mu==i, sigma^2==j), list(i=x, j=y))
  plot.continuous(xmin, xmax, theta, "norm", mode, title, lab.fn)
}

plot.lognormal <- function(mode, xmin=0, xmax=3,
                          theta=data.frame(mu=c(0,2,0,1/2,1/4,1/8),
                              s2=c(3,2,1,1,1,1)), title="Log-Normal") {
  lab.fn <- function(x, y) substitute(list(mu==i, sigma^2==j), list(i=x, j=y))
  p <- plot.continuous(xmin, xmax, theta, "lnorm", mode, title, lab.fn)
  if (mode == "cdf")
    p <- p + theme(legend.justification=c(0, 1), legend.position=c(0, 1))

  p
}

plot.student <- function(mode, xmin=-5, xmax=5,
                         theta=data.frame(c(1,2,5,Inf)),
                         title=expression(bold("Student\'s") ~ italic(t))) {
  lab.fn <- function(x) {
    if (x == Inf)
      quote(nu==infinity)
    else
      substitute(nu==i, list(i=x))
  }

  plot.continuous(xmin, xmax, theta, "t", mode, title, lab.fn)
}

plot.chisquare <- function(mode, xmin=0, xmax=8,
                           theta=data.frame(1:5),
                           title=expression(chi^2)) {
  lab.fn <- function(x) substitute(k==i, list(i=x))
  plot.continuous(xmin, xmax, theta, "chisq", mode, title, lab.fn)
}

plot.f <- function(mode, xmin=0, xmax=5,
                   theta=data.frame(d1=c(1,2,5,100,100), d2=c(1,1,2,1,100)),
                   title="F") {
  lab.fn <- function(x, y) substitute(list(d[1]==i, d[2]==j), list(i=x, j=y))
  plot.continuous(xmin, xmax, theta, "f", mode, title, lab.fn)
}

plot.exp <- function(mode, xmin=0, xmax=5,
                     theta=data.frame(c(2,1,0.4)),
                     title="Exponential") {
  lab.fn <- function(x) substitute(beta==i, list(i=1/x))
  plot.continuous(xmin, xmax, theta, "exp", mode, title, lab.fn)
}

plot.gamma <- function(mode, xmin=0, xmax=20,
                       theta=data.frame(a=c(1,2,3,5,9), b=c(0.5,0.5,0.5,1,2)),
                       title="Gamma") {
  lab.fn <- function(x, y) substitute(list(alpha==i, beta==j), list(i=x, j=y))
  plot.continuous(xmin, xmax, theta, "gamma", mode, title, lab.fn)
}

dinvgamma <- function(x, shape = 1, rate = 1, scale = 1/rate, log = FALSE) {
  logval <- shape * log(rate) - lgamma(shape) - (shape+1) * log(x) - rate/x
  if (log)
    logval
  else
    exp(logval)
}

pinvgamma <- function(q, shape = 1, rate = 1, scale = 1/rate,
                      lower.tail = TRUE, log.p = FALSE) {
  pgamma(1 / q, shape, rate, scale, !lower.tail, log.p)
}

plot.invgamma <- function(mode, xmin=0, xmax=5,
                          theta=data.frame(a=c(1,2,3,3), b=c(1,1,1,0.5)),
                          title="Inverse Gamma") {
  lab.fn <- function(x, y) substitute(list(alpha==i, beta==j), list(i=x, j=y))
  plot.continuous(xmin, xmax, theta, "invgamma", mode, title, lab.fn)
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

plot.beta <- function(mode, xmin=0, xmax=1,
                       theta=data.frame(a=c(0.5,5,1,2,2), b=c(0.5,1,3,2,5)),
                       title="Beta") {
  lab.fn <- function(x, y) substitute(list(alpha==i, beta==j), list(i=x, j=y))
  p <- plot.continuous(xmin, xmax, theta, "beta", mode, title, lab.fn)

  if (mode == "cdf")
    p <- p + theme(legend.justification=c(0, 1), legend.position=c(0, 1))
  else
    p <- p + theme(legend.justification=c(0.5, 1), legend.position=c(0.5, 1))

  p
}

plot.weibull <- function(mode, xmin=0, xmax=2.5,
                         theta=data.frame(lambda=c(1,1,1,1), k=c(0.5,1,1.5,5)),
                         title="Weibull") {
  lab.fn <- function(x, y) substitute(list(lambda==i, k==j), list(i=x, j=y))
  plot.continuous(xmin, xmax, theta, "weibull", mode, title, lab.fn)
}

plot.pareto <- function(mode, xmin=0.8, xmax=2.5,
                        theta=data.frame(xm=c(1,1,1), a=c(1,2,4)),
                        title="Pareto") {
  lab.fn <- function(x, y) substitute(list(x[m]==i, k==j), list(i=x, j=y))
  plot.continuous(xmin, xmax, theta, "pareto", mode, title, lab.fn)
}

# --------------------------------------------------------------------------- #

store <- function(name, p) {
  ggsave(paste(name, "pdf", sep="."), p)
}

store("uniform-pmf", plot.uniform.pmf())
store("uniform-pdf", plot.uniform.pdf())
store("uniform-cdf-discrete", plot.uniform.cdf.discrete())
store("uniform-cdf-continuous", plot.uniform.cdf.continuous())

store("binomial-pmf", plot.binomial("pmf"))
store("binomial-cdf", plot.binomial("cdf"))
store("geometric-pmf", plot.geometric("pmf"))
store("geometric-cdf", plot.geometric("cdf"))
store("poisson-pmf", plot.poisson("pmf"))
store("poisson-cdf", plot.poisson("cdf"))

store("normal-pdf", plot.normal("pdf"))
store("normal-cdf", plot.normal("cdf"))
store("lognormal-pdf", plot.lognormal("pdf") + ylim(0,1))
store("lognormal-cdf", plot.lognormal("cdf"))
store("student-pdf", plot.student("pdf"))
store("student-cdf", plot.student("cdf"))
store("chisquare-pdf", plot.chisquare("pdf") + ylim(0,1))
store("chisquare-cdf", plot.chisquare("cdf"))
store("f-pdf", plot.f("pdf"))
store("f-cdf", plot.f("cdf"))
store("exponential-pdf", plot.exp("pdf"))
store("exponential-cdf", plot.exp("cdf"))
store("gamma-pdf", plot.gamma("pdf"))
store("gamma-cdf", plot.gamma("cdf"))
store("invgamma-pdf", plot.invgamma("pdf"))
store("invgamma-cdf", plot.invgamma("cdf"))
store("beta-pdf", plot.beta("pdf"))
store("beta-cdf", plot.beta("cdf"))
store("weibull-pdf", plot.weibull("pdf"))
store("weibull-cdf", plot.weibull("cdf"))
store("pareto-pdf", plot.pareto("pdf"))
store("pareto-cdf", plot.pareto("cdf"))
