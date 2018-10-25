# Internal Datasets

Set working directory and read code from the *internal-datasets.R* file.

```r
setwd(".")  # set path to current folder
source("internal-datasets.R")
```

Place color palettes in the `schemes` object and save as an
internal package dataset in the *../../R/sysdata.rda* file.

```r
MakeSysdata()
```

Note that the source files for some palettes are downloaded from their
[repository](https://github.com/GenericMappingTools/gmt/tree/master/share/cpt)
and placed in the *cpt* folder.

Rebuild the **inlmisc** package to reflect changes in the `schemes` object.

Create a color palette summary table and save in the *../../man/figures/table.(pdf|svg)* files.

```r
MakeTable()
```

Note that this command requires access to [inkscape](https://inkscape.org/)
from the command line.
