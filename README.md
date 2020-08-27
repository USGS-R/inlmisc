inlmisc
=======

[![Travis Build
Status](https://travis-ci.org/USGS-R/inlmisc.svg?branch=master)](https://travis-ci.org/USGS-R/inlmisc)
[![CRAN
Version](https://www.r-pkg.org/badges/version/inlmisc)](https://CRAN.R-project.org/package=inlmisc)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/inlmisc?color=brightgreen)](https://CRAN.R-project.org/package=inlmisc)
[![Dependencies](https://tinyverse.netlify.com/badge/inlmisc)](https://CRAN.R-project.org/package=inlmisc)
[![Coverage
Status](https://coveralls.io/repos/github/USGS-R/inlmisc/badge.svg?branch=master)](https://coveralls.io/github/USGS-R/inlmisc?branch=master)
[![USGS
Category](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

Description
-----------

The [R](https://www.r-project.org/) package **inlmisc** is a collection
of functions for creating high-level graphics, performing raster-based
analysis, processing
[MODFLOW](https://www.usgs.gov/mission-areas/water-resources/science/modflow-and-related-programs "USGS's modular hydrologic model")-based
models, selecting subsets using a genetic algorithm, creating
interactive web maps, accessing color palettes, etc. Used to support
packages and scripts written by researchers at the U.S. Geological
Survey (USGS) Idaho National Laboratory (INL) [Project
Office](https://www.usgs.gov/centers/id-water/science/idaho-national-laboratory-project-office "USGS INL Project Office").

Installation
------------

The current release is available on
[CRAN](https://CRAN.R-project.org/package=inlmisc "The Comprehensive R Archive Network"),
which you can install using the following command:

``` r
install.packages("inlmisc", dependencies = TRUE)
```

A recent version of [Pandoc](https://pandoc.org/installing.html) (&gt;=
1.12.3) is also required to run particular examples in the help
documentation—[RStudio](https://rstudio.com/products/rstudio/) includes
this so you do not need to download Pandoc if working in a RStudio IDE.
The **webshot** package is imported from and has as a dependency the
external program [PhantomJS](https://phantomjs.org/), which may be
installed using the command:

``` r
webshot::webshotinstall_phantomjs()
```

For creating word clouds, the [OptiPNG](http://optipng.sourceforge.net/)
program is recommended and needs to be accessible through the command
window. Finally, to install the development version of **inlmisc**, you
need to clone the repository and build from source, or run:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("USGS-R/inlmisc", dependencies = TRUE)
```

Usage
-----

Examples are given in the package help pages. To access this
documentation, run:

``` r
library("inlmisc")
help(package = "inlmisc")
```

Author
------

Jason C. Fisher (ORCID iD
[0000-0001-9032-8912](http://orcid.org/0000-0001-9032-8912))

Point of Contact
----------------

Jason C. Fisher (<jfisher@usgs.gov>)

Suggested Citation
------------------

To cite **inlmisc** in publications, please use:

Fisher, J.C., 2020, inlmisc—Miscellaneous functions for the U.S.
Geological Survey Idaho National Laboratory Project Office: U.S.
Geological Survey software release, R package, Reston, Va.

Contributing
------------

We welcome your contributions and suggestions for how to make these
materials more useful to the community. Please feel free to comment on
the [issue tracker](https://github.com/USGS-R/inlmisc/issues) or open a
[pull request](https://github.com/USGS-R/inlmisc/pull/new/master) to
contribute.

Code of Conduct
---------------

All contributions to- and interactions surrounding- this project will
abide by the [USGS Code of Scientific
Conduct](https://www2.usgs.gov/fsp/fsp_code_of_scientific_conduct.asp).

Disclaimer
----------

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.

License Information
-------------------

This software is a product of the U.S. Geological Survey (USGS), an
agency of the United States Department of Interior, which is part of the
U.S. Government.

#### Cost

This software is freely distributed. There is no fee to download and
(or) use this software.

#### License

Users do not need a license or permission from the USGS to use this
software. Users can download and install as many copies of the software
as they need. See Exceptions below.

#### Public domain

As a work of the United States Government, this USGS product is in the
[public domain](http://www.usa.gov/publicdomain/label/1.0/) within the
United States. You can copy, modify, distribute, and perform the work,
even for commercial purposes, all without asking permission.
Additionally, USGS waives copyright and related rights in the work
worldwide through [CC0 1.0 Universal Public Domain
Dedication](https://creativecommons.org/publicdomain/zero/1.0/).

#### Exceptions

This project may have numerous dependencies from other open-source
software projects. The re-use and distribution of those software
packages may be subject to the licenses of each of those dependencies.

Support
-------

The Idaho National Laboratory Project Office of the USGS supports the
development and maintenance of **inlmisc**. Resources are available
primarily for maintenance and responding to user questions. Priorities
on the development of new features are determined by the inlmisc
development team.

Disclaimer
----------

This software has been approved for release by the U.S. Geological
Survey (USGS). Although the software has been subjected to rigorous
review, the USGS reserves the right to update the software as needed
pursuant to further analysis and review. No warranty, expressed or
implied, is made by the USGS or the U.S. Government as to the
functionality of the software and related material nor shall the fact of
release constitute any such warranty. Furthermore, the software is
released on condition that neither the USGS nor the U.S. Government
shall be held liable for any damages resulting from its authorized or
unauthorized use.
