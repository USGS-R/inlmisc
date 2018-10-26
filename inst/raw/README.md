# Internal Datasets

Set working directory to current folder and read R code from the
*internal-datasets.R* file.

```r
setwd("<path/to/current/folder>")
source("internal-datasets.R")
```

Place color palettes in the `schemes` object and save as an
internal package dataset in the *../../R/sysdata.rda* file.

```r
MakeSysdata()
```

Note that the source files for some color schemes are downloaded from the
[GMC repository](https://github.com/GenericMappingTools/gmt/tree/master/share/cpt)
and placed in the *cpt* folder.

Rebuild the **inlmisc** package to reflect changes in the `schemes` object.

Create a summary table for color schemes and save graphics in the
*../../man/figures/table.(pdf|svg)* files
(requires access to [inkscape](https://inkscape.org/) and
[svgcleaner](https://github.com/RazrFalcon/svgcleaner) from the command line).

```r
MakeTable()
```

Edit the command used to insert the standalone PDF version of the table
into the R help documentation by manually changing the width of the page layout.
Open the *../../R/GetColors.R* file and specify the width in centimeters.

```
\if{latex}{\figure{table.pdf}{options: width=<layout width>cm}}
```

Note that a PDF viewer is currently used to determine the layout width.
