all: README

README: README.Rmd
	R -e "devtools::build_readme()"

clean:
	rm README.md
