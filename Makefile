PANDOC = pandoc
IFORMAT = markdown
OFORMAT = slidy
FLAGS =
TEMPLATE = page.tmpl
STYLE = style.css

# Watch for changes to the source file
SOURCE_FILES=$(wildcard *.hs)

.PHONY: clean

%.html: %.md $(SOURCE_FILES)
	$(PANDOC) -c -s --mathjax -f $(IFORMAT) -t $(OFORMAT) $(FLAGS) --css $(STYLE) -o $@ $< 

%.pdf: %.md $(SOURCE_FILES)
	$(PANDOC) -c -s --mathjax -f $(IFORMAT) -t beamer $(FLAGS) -o $@ $< 

all: slideshow.html

clean:
	-rm -f diagrams/*.aux
	-rm -f diagrams/*.log
	-rm -f diagrams/*.pdf
	-rm -f diagrams/*.toc
	-rm -f diagrams/*.svg
	-rm -f diagrams/*.png
