all: README

README: README.Rmd
	R -e "devtools::build_readme()"

run_app:
	Rscript sample_app.R

clean:
	rm README.md
