all: index.haml
	@haml $< > index.html
