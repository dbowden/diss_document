MAIN := formation_chapter
FORMAT := pdf

all: $(MAIN).$(FORMAT)

clean:
	rm -rf $(MAIN)_cache

wordcount:
	ps2ascii $(MAIN).pdf | wc -w

view:
	open -a Skim.app $(MAIN).pdf

$(MAIN).$(FORMAT): $(MAIN).Rmd
	Rscript -e 'rmarkdown::render("$<")'
	#Rscript -e 'bookdown::render_book("$<")'
