MAIN := formation_chapter
FORMAT := pdf

all: $(MAIN).$(FORMAT)

clean:
	rm -rf $(MAIN)_cache

wordcount:
	ps2ascii $(MAIN).pdf | wc -w

$(MAIN).$(FORMAT): $(MAIN).rmd
	Rscript -e 'rmarkdown::render("$<")'
	# Rscript -e 'bookdown::render_book("$<")'
