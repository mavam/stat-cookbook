PLOTS := beta \
				 binomial \
				 chisquare \
				 exponential \
				 f \
				 gamma \
				 geometric \
				 invgamma \
				 lognormal \
				 normal \
				 pareto \
				 poisson \
				 student \
				 uniform-continuous \
				 uniform-discrete \
				 weibull

.PHONY: all clean

all:
	R --vanilla < distributions.R

clean:
	-rm -f $(PLOTS:=.pdf)
