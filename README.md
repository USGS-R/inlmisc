# inlmisc

[![Build
status](https://ci.appveyor.com/api/projects/status/pvcq1jsgabqx61ah?svg=true)](https://ci.appveyor.com/project/jfisher-usgs/inlmisc)
[![CRAN
Version](https://www.r-pkg.org/badges/version/inlmisc)](https://CRAN.R-project.org/package=inlmisc)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/inlmisc?color=brightgreen)](https://CRAN.R-project.org/package=inlmisc)
[![Dependencies](https://tinyverse.netlify.com/badge/inlmisc)](https://CRAN.R-project.org/package=inlmisc)
[![Coverage
Status](https://coveralls.io/repos/github/USGS-R/inlmisc/badge.svg?branch=main)](https://coveralls.io/github/USGS-R/inlmisc?branch=main)
[![USGS
Category](https://img.shields.io/badge/USGS-Support-yellow.svg)](https://owi.usgs.gov/R/packages.html#support)

## Description

The [R](https://www.r-project.org/) package **inlmisc** is a collection
of functions for creating high-level graphics, performing raster-based
analysis, processing
[MODFLOW](https://www.usgs.gov/mission-areas/water-resources/science/modflow-and-related-programs "USGS's modular hydrologic model")-based
models, selecting subsets using a genetic algorithm, creating
interactive web maps, accessing color palettes, etc. Used to support
packages and scripts written by researchers at the U.S. Geological
Survey (USGS) Idaho National Laboratory (INL) [Project
Office](https://www.usgs.gov/centers/idaho-water-science-center/science/idaho-national-laboratory-project-office "USGS INL Project Office").

## Installation

The current release is available on
[CRAN](https://CRAN.R-project.org/package=inlmisc "The Comprehensive R Archive Network"),
which you can install using the following command:

``` r
install.packages("inlmisc", dependencies = TRUE)
```

A recent version of [Pandoc](https://pandoc.org/installing.html) (\>=
1.12.3) is also required to run particular examples in the help
documentation—[RStudio](https://www.rstudio.com/products/rstudio/)
includes this so you do not need to download Pandoc if working in a
RStudio IDE. The **webshot** package is imported from and has as a
dependency the external program [PhantomJS](https://phantomjs.org/),
which may be installed using the command:

``` r
webshot::install_phantomjs()
```

For creating word clouds, the [OptiPNG](http://optipng.sourceforge.net/)
program is recommended and needs to be accessible through the command
window. Finally, to install the development version of **inlmisc**, you
need to clone the repository and build from source, or run:

``` r
if (!requireNamespace("remotes")) install.packages("remotes")
remotes::install_github("USGS-R/inlmisc@develop", dependencies = TRUE)
```

## Usage

Examples are given in the package help pages. To access this
documentation, run:

``` r
library("inlmisc")
help(package = "inlmisc")
```

## Author

Jason C. Fisher (ORCID iD
[0000-0001-9032-8912](https://orcid.org/0000-0001-9032-8912))

## Point of Contact

Jason C. Fisher (<jfisher@usgs.gov>)

## Suggested Citation

To cite **inlmisc** in publications, please use:

Fisher, J.C., 2020, inlmisc—Miscellaneous functions for the U.S.
Geological Survey Idaho National Laboratory Project Office: U.S.
Geological Survey software release, R package, Reston, Va.

## Contributing

We welcome your contributions and suggestions for how to make these
materials more useful to the community. Please feel free to comment on
the [issue tracker](https://github.com/USGS-R/inlmisc/issues) or open a
[merge request](https://github.com/USGS-R/inlmisc/pull/new/main) to
contribute.

## Code of Conduct

All contributions to- and interactions surrounding- this project will
abide by the [USGS Code of Scientific
Conduct](https://www.usgs.gov/office-of-science-quality-and-integrity/fundamental-science-practices).

<!-- Embedded References -->

## Disclaimer

This software is preliminary or provisional and is subject to revision.
It is being provided to meet the need for timely best science. The
software has not received final approval by the U.S. Geological Survey
(USGS). No warranty, expressed or implied, is made by the USGS or the
U.S. Government as to the functionality of the software and related
material nor shall the fact of release constitute any such warranty. The
software is provided on the condition that neither the USGS nor the U.S.
Government shall be held liable for any damages resulting from the
authorized or unauthorized use of the software.

Any use of trade, product, or firm names is for descriptive purposes
only and does not imply endorsement by the U.S. Government.

## License

Unless otherwise noted, this project is in the public domain in the
United States because it contains materials that originally came from
the United States Geological Survey, an agency of the United States
Department of Interior. For more information, see the official USGS
copyright policy at
[copyrights-and-credits](https://www.usgs.gov/information-policies-and-instructions/copyrights-and-credits).

Additionally, we waive copyright and related rights in the work
worldwide through the CC0 1.0 Universal public domain dedication.

#### CC0 1.0 Universal Summary

This is a human-readable summary of the [Legal Code (read the full
text)](https://creativecommons.org/publicdomain/zero/1.0/legalcode).

##### No Copyright

The person who associated a work with this deed has dedicated the work
to the public domain by waiving all of his or her rights to the work
worldwide under copyright law, including all related and neighboring
rights, to the extent allowed by law.

You can copy, modify, distribute and perform the work, even for
commercial purposes, all without asking permission.

##### Other Information

In no way are the patent or trademark rights of any person affected by
CC0, nor are the rights that other persons may have in the work or in
how the work is used, such as publicity or privacy rights.

Unless expressly stated otherwise, the person who associated a work with
this deed makes no warranties about the work, and disclaims liability
for all uses of the work, to the fullest extent permitted by applicable
law. When using or citing the work, you should not imply endorsement by
the author or the affirmer.

<!-- Embedded References -->

## Support

The Idaho National Laboratory Project Office of the USGS supports the
development and maintenance of **inlpubs**. Resources are available
primarily for maintenance and responding to user questions. Priorities
on the development of new features are determined by the development
team.
