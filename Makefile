# Prepare Package for Release
#
# System requirements:
#   pdfcrop - http://pdfcrop.sourceforge.net/
#   inkscape - https://inkscape.org/
#   svgcleaner - https://github.com/RazrFalcon/svgcleaner
#   pandoc - https://pandoc.org/
#   phantomjs - https://phantomjs.org/
#   optipng - http://optipng.sourceforge.net/

SHELL := bash
.ONESHELL:
.SHELLFLAGS := -eu -o pipefail -c
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: docs install check clean
.PHONY: all

docs:
	R -q -e 'pkgload::load_all()'
	R -q -e 'roxygen2::roxygenize()'
	R -q -e 'pkgbuild::clean_dll()'
.PHONY: docs

build:
	cd ..
	R CMD build --no-build-vignettes $(PKGSRC)
.PHONY: build

install: build
	cd ..
	R CMD INSTALL --build $(PKGNAME)_$(PKGVERS).tar.gz
.PHONY: install

check:
	cd ..
	R CMD check --no-build-vignettes --as-cran $(PKGNAME)_$(PKGVERS).tar.gz
.PHONY: check

clean:
	cd ..
	rm -f -r $(PKGNAME).Rcheck/
	rm -f sysdata.rda
	rm -f -r figures
.PHONY: clean

datasets:
	cd data-raw
	Rscript build-datasets.R
	[ -f sysdata.rda ] && mv -f sysdata.rda ../R/
.PHONY: datasets

tables: install
	cd data-raw
	Rscript render-tables.R
	rm -r ../man/figures
	mv -f figures ../man/
.PHONY: tables

readme:
	R -q -e 'rmarkdown::render('\''README.Rmd'\'')'
.PHONY: readme
