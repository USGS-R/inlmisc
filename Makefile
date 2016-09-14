# Prerequisites:
#
#   Windows
#     install R https://cran.r-project.org/bin/windows/base/
#     install Rtools https://cran.r-project.org/bin/windows/Rtools/
#     install MikTeX http://www.miktex.org/
#     edit system PATH for command line access
#   Mac OS X
#     install R https://cran.r-project.org/bin/macosx/
#     install XCode https://itunes.apple.com/us/app/xcode/id497799835
#     install MacTeX http://www.tug.org/mactex/
#   Linux (Debian/Ubuntu)
#     sudo apt-get install r-base r-base-dev texlive-full
#     sudo apt-get build-dep r-base-core
#
#   install R packages
#     pkgs <- c("devtools", "roxygen2", "knitr")
#     install.packages(pkgs, repos="http://cran.rstudio.com")
#
# Targets: run using 'make <target>'
#
#   'all'    builds package documentation in './man' using roxygen2 package
#            builds package vignettes in './inst/doc' using knitr package
#            builds and checks source package
#            installs package locally
#   'build'  builds package tarball and binary files in '.'
#   'clean'  removes NAMESPACE file
#            removes documentation files in './man'
#            removes vignette files in './inst/doc'
#            removes package tarball and binary files in '.'
#
PKGDIR  := .
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)

all: document install vignettes check

document:
	R -q -e 'devtools::document(pkg='\''$(PKGDIR)'\'')'

vignettes:
	R -q -e 'devtools::build_vignettes(pkg='\''$(PKGDIR)'\'')'

install:
	R -q -e 'devtools::install(pkg='\''$(PKGDIR)'\'')'

check:
	R -q -e 'devtools::check(pkg='\''$(PKGDIR)'\'')';\
	R -q -e 'devtools::install(pkg='\''$(PKGDIR)'\'')'

build:
	R -q -e 'devtools::build(path='\''$(PKGDIR)'\'')';\
	R -q -e 'devtools::build(path='\''$(PKGDIR)'\'', binary=TRUE)'

clean:
	$(RM) NAMESPACE;\
	$(RM) -r $(PKGDIR)/man;\
	$(RM) -r $(PKGDIR)/inst/doc;\
	$(RM) $(PKGDIR)/$(PKGNAME)_*.tar.gz;\
	$(RM) $(PKGDIR)/$(PKGNAME)_*.zip

.PHONY: all document vignettes install check build clean
