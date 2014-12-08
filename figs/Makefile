.PHONY: all clean

# We'll take the first plot as indicator for all plots until the R script
# allows for more fine-grained plot generation.
all: beta-cdf.pdf

%.pdf:
	R --vanilla < distributions.R

clean:
	-rm -f *-*.pdf
