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

# We'll take the first plot as indicator for all plots until the R script
# allows for more fine-grained plot generation.
all: beta.pdf

%.pdf:
	R --vanilla < distributions.R

clean:
	-rm -f $(PLOTS:=.pdf)
