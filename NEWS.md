# inlmisc 0.5.4

- Explicitly set `stringsAsFactors` option in all calls to `data.frame` and `as.data.frame`.

# inlmisc 0.5.3

- Fix inkscape command in `render-tables.R` script.

- Fix bug in `SetPolygons` that resulted in error.

- Fix invalid URL's.

# inlmisc 0.5.2

- Fixed invalid URL's.

# inlmisc 0.5.1

- Import **wordcloud2** and **webshot** packages, and suggest **png** package. The **webshot** package has as a dependency the external program [PhantomJS](https://phantomjs.org/).

- Add `CreateWordCloud`, used to create a word cloud from a frequency table of words, and plot to a PNG file.

# inlmisc 0.5.0

- Add top-level *README.Rmd* file.

- Remove *LICENSE.note* file from package, and replace witha repo-only *LICENSE.md* file.

- Add *DISCLAIMER.md* file.

- In LaTeX preamble, include new solution for printing fancy header on all pages.

- In `AddInsetMap`, add `feature` argument, used to add one or more spatial features to the inset map.

# inlmisc 0.4.9

- In LaTeX preamble, omit group separators in the decimal part when using **siunitx** package.

- In `GetColors`, type change from diverging to sequential for color schemes: `"berlin"`, `"broc"`, `"cork"`, `"lisbon"`, `"oleron"`, `"tofino"`, and `"vik"`. Results from CPTs not actually having a zero entry.

- In `PlotMap`, fix bug resulting from change in `raster::crop` that returns an error when the subset is empty, was previously returning `NULL`.

- In `FindOptimalSubset`, stop setting state of random number generator when `seed` argument is `NULL`.

# inlmisc 0.4.8

- In `FindOptimalSubset`, add `numIslands` argument, used to specify the number of islands, was previously dependent on the `parallel` argument; change default value of `elitism` argument from 0 to 5-percent of the island population.

- Add *misc/latex-packages.txt*: contains a list of required LaTeX packages that are not included in the default installation of [TinyTeX](https://yihui.org/tinytex/); use the `inlmisc:::InstallLatexPackages()` command to install these packages into TinyTeX.

- In LaTeX preamble, replace **xcolor** package with **color**, eliminates an annoying warning when compiling document.

- In `GetColors`, add `"turbo"` color scheme.

- In `SetHinge`, non-backward compatible change to allocating colors on each side of hinge, is only perceivable when the number of colors is small (`n < 10`).

- Rename `PrintHelpPages` to `PrintPackageHelp`. Preserve empty lines in the Examples section of help documentation; fix parsing bug associated with identifying code blocks; tidy main header for each help topic; add `internal` argument, used to print help pages flagged with keyword `internal`; replace `hr` argument with `sep`; add `title_to_name` argument, used to replace the help-topic "title" with its "name"; add `notrun` argument, used to remove `## Not run` comments; include internal links by default.

- In `Grid2Polygons`, add `check_validity` argument, used to check the validity of polygons.

- Fix bugs in *raw/build-datasets.R* and *raw/render-tables.R* files that incorrectly called `IsColor` function.

- Change package dependency from R >= 3.4.0 to R >= 3.5.0 because serialized objects in serialize/load version 3 cannot be read in older versions of R.

# inlmisc 0.4.7

- In `SetPolygons`, set `checkValidity` argument to 2 and suppress warnings.

# inlmisc 0.4.6

- Add `"usgs_article"` template for R Markdown documents. This template is experimental and subject to change in the future.

- Set `checkValidity` argument to 2 (check and try to buffer by zero distance to repair) in many of the **rgeos** function calls.

- In `AddColorKey`, remove border line around color blocks.

- In `Grid2Polygons`, improved performance; change polygon cropping function from `raster::crop` to `SetPolygons`.

- In `RecreateLibrary`, remove `github` option that allowed package installation from GitHub repositories.

- Add `PrintHelpPages`, used to print the HTML code associated with help pages of a loaded R package.

- In `BuildVignettes`, rearrange order of arguments and add `doc` argument, used to specify the directory to write vignette output files.

- In `GetColors`, add `"bpy"` (blue-pink-yellow) color scheme.

- In `AddPoints`, change interval to reflect labels, that is, open at the left and closed at right.

- Add `AddIntervals`, used to add vertical interval symbols to plots. Replaces use of `graphics::arrows` in `PlotGraph`, and avoids the warning message resulting from an arrow length less than 1/1000 inch.

- In *misc/preamble.tex*: reduce hyphenation and prevent writing into margin.

- In `PlotGraph`: improve handling of cases where range of `y` values is zero; extend y-axis limits to prevent symbols from being drawn on an axis limit; change margin line for the main title, axis title, and axis labels; decrease size of upper margin by 0.2 lines; add `add.grid` argument, determines whether to draw a rectangular grid.

- In `PrintFigure`, allow heading on single figure.

- In `AddSearchButton`, fix bug that prevented popup from opening when `openPopup` argument is true.

# inlmisc 0.4.5

- In `PlotGraph`, change alignment of `main` title from center to left-hand side of plot.

- In `ToScientific`, add `zero` argument, used to substitute a string for values of zero.

- In `PrintTable`: allow `colheadings` argument to be a data table, used to make column headings with spanners; separate multi-table pages using `\clearpage`.

- In *misc/preamble.tex*: set 'pdfa' option to true, tries to avoid violations of PDF/A in code generated by **hyperref** package; add **multirow**, **footmisc**, and **siunitx** packages; ragged right justification of captions.

- Add **alphahull**, **maptools**, and **remotes** to suggested packages.

- Remove **httr** and **devtools** from suggested packages.

- In `GetRegionOfInterest`: replace `obj` argument with `x` and `y`; add `alpha` and `width` arguments, used to compute alpha-shape and expand the region of interest, respectively.

# inlmisc 0.4.4

- Add `BuildVignettes`, used to build package vignettes.

- In `SummariseBudget`, improve memory management.

- Change package imports by adding **data.table** and removing **dplyr**.

- Add `SetHinge`, used to specify a hinge location in a color palette.

- In `PrintTable`: `d` argument can be of class 'matrix'; and a default caption is no longer added when `title` and `headnotes` arguments are not specified.

- Tidy help documentation

- Add `GetRegionOfInterest`, used to calculate the region of interest from spatial points.

- In `FindOptimalSubset`: improve matrix construction of `suggestions`; change calculation of number of bits in a number.

- In `AddColorKey`, rearrange order of arguments.

- In `PlotCrossSection`: add `bend.label` argument, used to place labels at top of the bend-in-section lines; change default value of `bg.col` argument from `"#FFFFFFD8"` to `NULL`, and for `scale.loc` argument from `"bottom"` to `NULL`; make font/line color darker for section breaks and features.

- Rename `GetTolColors` to `GetColors`. Add additional color schemes; replace `start` and `end` arguments with `stops`, a vector of length 2; change `blind` argument option from `"monochromacy"` to `"monochrome"`; return a variant of the `GetColors` function when argument `n` is unspecified.

- For leaflet-search plugin, change version from `2.8.0` to `2.9.6` and tidy wrapper functions.

- In `CreateWebMap`, add `service` argument for specifying the mapping service to use for accessing base-map tiles.

- Change required R version from `>= 3.2.0` to `>= 3.4.0`.

# inlmisc 0.4.3

- Add `AddNorthArrow`, was previously defined as an internal function.

- In `AddScaleBar`: non-backward compatible argument changes. Remove `offset` argument and replace with `...`, see help documentation for details.

- Make **dichromat** a suggested package.

- Remove **viridisLite** from suggested packages.

- Add `GetInsetLocation`, used to determine the location for a inset graphics in the main plot region.

- In `AddPoints`: add `bty`, `draw.legend`, and `draw.points` arguments.

- In `PlotGraph`: add `xpd` argument, determines whether point and (or) line symbols are clipped to the plot region; change default color scheme; change contents of `fill` argument (non-backward compatible) and add a `fillcolor` argument; provide `main` argument a default value of `NULL`.

- In `FindOptimalSubset`, add `monitor` argument to check on status of GA run.

- In `GetTolColors`: add color schemes; change default scheme from `"bright"` to `"smooth rainbow"`; add `start`, `end`, `bias`, `reverse`, `blind`, and `gray` arguments.

# inlmisc 0.4.2

- In `GetTolColors`, revise color schemes based of issue 3.0 of technical note.

- In `PlotGraph`, improve placement of tick marks.

- Remove **colorspace** from suggested packages and replace with **viridisLite**.

- In `ToScientific`, better identify whether to print numeric values in fixed or scientific notation.

- In `AddScaleBar`, non-backward compatible argument changes and formatting improvements.

- In `PlotGraph`: fix bug that resulted in an error when `type = "n"`; and add `main` argument for placing a title at the top of the plot.

- Make **gstat** and **roxygen2** suggested packages.

- Remove **httr**, **maps**, and **maptools** from suggested packages.

- In *misc/preamble.tex*: tidy code; include fix for spacing after number in List of Figures and List of Tables;  remove **helvet** package; and add **float** package, provides the H float modifier option.

- In `AddInsetMap`: add an optional `e` argument, a numeric vector describing the extent of the smaller axis-aligned rectangle; add "center" option for `loc` argument; and add `bty` argument, determines if a box is to be drawn around the inset map.

- Update *leaflet-search.min.js* and *leaflet-search.min.css* files to version 2.8.0.

- Add `PrintFigure`, used to print the LaTeX code associated with a figure.

# inlmisc 0.4.0

- Add `GetTolColors`, used to access color palettes by Paul Tol.

- Avoid importing all functions from a package.

- Remove `AddCertificate`, no longer needed with new version of R (3.4.3).

- Add `PrintTable`, used to print the LaTeX code associated with a `data.frame` object.

- In `PlotMap`, remove option to map points.

- In `ReadCodeChunks`, remove dependency on *knitr-intro.Rmd* file in example.

- In `AddColorKey`, replace `scientific` argument with `scipen`, see `getOption("scipen")` command.

- In `AddColorKey`, add `log` argument to specify axis to be logarithmic.

- In `AddColorKey`, use *m x 10^n* format for tick labels written in scientific notation.

- In `RecreateLibrary`, add `parallel` argument to install packages from source using parallel processes.

- In `ToScientific`, add `big.mark` argument with default value of `","`.

- In `ToScientific`, fix bug that formatted `0` as `NA` when `type = "plotmath"`.

- In `CreateWebMap`, pass `...` arguments to `leaflet::leaflet` function,
  these arguments were previously passed to the `leaflet::leafletOptions` function.

- In `CreateWebMap`, add `maps` argument to specify which base maps to include.

- In `CreateWebMap`, remove coordinates and zoom level information from top of map.

- Add `AddHomeButton`, `AddClusterButton`, and `AddSearchButton`, and `AddLegend`, used to add additional web map elements.

- In `FindOptimalSubset`, allow integer chromosomes to be specified for the `suggestions` argument.

- Add additional argument checks using **checkmate** package.

# inlmisc 0.3.5

- In `PlotMap`, fix bug introduced by previous fix of raster range calculation.

- Made `AddCertificate` an internal function.

- Add argument checks using **checkmate** package.

# inlmisc 0.3.4

- In `PlotMap`, fix bug in range calculation of raster values by removing `finite = TRUE`.

- In `PlotMap`, fix extent of background image, rivers, lakes, and roads using `par("usr")`.

- Add `FindOptimalSubset`, used to identify an optimal subset using a genetic algorithm.

# inlmisc 0.3.3

- In `AddPoints`: add option to scale symbol size to radius;
  revise calculation of symbol size and legend labels.

- In `PlotMap`, let `r` argument accept any object that can be converted to a `RasterLayer` class.

- In LaTeX preamble: add **verbatimbox** package; add maketitle format.

- In `PlotMap`, add `simplify` argument, used to convert raster to polygons prior to plotting.

- In `Grid2Polygons`: allow `grd` argument to be an object of class `SpatialPixelsDataFrame` or `Raster*`; transform coordinate reference system (CRS) of `ply` argument to match the CRS of the `grd` argument; and add `zlim` argument, a limit on the attribute variable.

- In `ToScientific`, add arguments `scipen`, `delimiter`, and `...`; and rename `lab.type` argument to `type`. Code in place for backwards compatibility.

# inlmisc 0.3.2

- In `RecreateLibrary` and `SavePackageDetails`, add support for *gzip* file compression.

- In `SavePackageDetails`, fix bug that results in an 'invalid cross-device link' error on some flavor's of R.

# inlmisc 0.3.1

- Rename `SavePackageNames` to `SavePackageDetails`.

- Add `AddCertificate`, used to add a X.509 certificate to your CA bundle.

- Make **httr** a suggested package.

- In `RecreateLibrary`, add `local` argument, file paths of files containing builds of packages.

- In `AddGradientLegend`, add `n` argument, the desired number of tick-marks to be drawn.

# inlmisc 0.3.0

- Add `Grid2Polygons` from the **Grid2Polygons** package.

- Total revision of `RecreateLibrary` and `SavePackageNames`.

- In `ToScientific`, improve default value for `digits` argument.

- Add `AddGradientLegend`, used to add a continuous color gradient legend strip to a plot.

# inlmisc 0.2.7

- Add `RecreateLibrary`, used to recreate an existing library on a new installation of R.

- Add `FormatPval`, used to format *p*-values.

- Add **bm** and **makecell** packages to LaTeX preamble.

- Changes in `PlotGraph`: Add option for plotting interval censored data by specifying `type = "i"`. Select box-and-whisker plot using `type = "w"`, was previously `type = "box"`. Default for `seq.date.by` argument changed from `"year"` to `NULL`.

- Add `CreateWebMap`, creates a Leaflet map widget using [The National Map](https://www.usgs.gov/programs/national-geospatial-program/national-map) services.

- Tidy help documentation for functions.

# inlmisc 0.2.6

- Add `endian` argument to `ReadModflowBinary`. Argument describes the endian-ness (or byte-order) of the binary file and is required for calls to the `readBin` function. Thanks to Professor Brian Ripley for identifying this issue.

# inlmisc 0.2.5

- In `SummariseBudget`, the `desc` argument no longer needs to be specified. If missing, all data types are summarized.

- In `SummariseBudget`, remove default for `desc` argument and add `id` argument, the auxiliary variable name.

- In `ReadModflowBinary`, enable cell-by-cell budget files to be read when they are not in compact form.

- Change legend format for binned point data from `[#, )` to `># to #`.

- Fix bug that resulted in error when plotting a single point location using the `AddPoints` function.

- Add [AppVeyor](https://www.appveyor.com/) to package.

# inlmisc 0.2.4

- Fix bug that resulted in incorrect dimensions for saved graphics.

# inlmisc 0.2.3

- In `PlotMap`, `PlotCrossSection`, and `AddColorKey`, dynamically adjust vertical plot margins based on the number of lines in the user specified labels.

- In `AddPoints`, remove `draw.legend` argument and rename `pos` argument to `legend.pos`. A `NULL` value for `legend.pos` will prevent the legend from being drawn.

- Change name from `AddBubbles` to `AddPoints`.

- In `PlotMap` and `PlotCrossSection`, fix layout so color key dimensions don't change on resize.

- In `PlotMap` function, add `file.close` argument. A logical that indicates if the graphics device driver should be shut down when the function exits.

- In `AddBubbles`, allow `z` argument to be an object of class `factor`.

- In `AddBubbles`, account for aspect ratio when constructing legend.

- In `AddBubbles`, rename `bg.pos` argument to `bg`, and set `bg.neg = NULL`. The `bg` argument may now be used to specify circle colors for all `z` values, not just positive values. See help documentation for details.

- In `AddBubbles`, specifying a single numeric value for the `inches` arguments results in a fixed radius being used for all circle symbols.

- In `PlotMap`, account for z-axis limits prior to removing rows and columns having all missing values.

- In `PlotMap`, change default for `scale.loc` argument from `"leftbottom"` to `NULL`; the scale bar is no longer drawn by default.

- In `PlotMap`, change `asp` argument from 1 to NULL. Defaults to 1 when data is projected, otherwise, a calculated value based on axes limits is used.

- Add `POSIXct2Character`, used to convert objects from `POSIXct` to `character` class.

- Update URL links to HTTP Secure

- Generalize CITATION file

# inlmisc 0.2.2

- Change argument name in `ReadModflowBinary` function from `f` to `path`.

- Add `ReadCodeChunk`, reads **knitr** code chunks into the current session.

- Change NEWS file to markdown format.

- Remove *misc* folder from *.Rbuildignore* file, adds the *misc/preamble.tex* file back to the package build.

# inlmisc 0.2.1

- Fix invalid URLs.

# inlmisc 0.2.0

- Add `file` argument to `PlotMap` and `PlotCrossSection`. Specifying this argument will start a graphics device driver for producing either a PDF or PNG file.

- Width and height of graphics device is no longer calculated when drawing to screen.

# inlmisc 0.1.2

- Explain the USGS and INL acronyms from the Title in the DESCRIPTION file.

- Fix invalid URLs.

# inlmisc 0.1.0

- First version of **inlmisc**
