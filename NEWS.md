# inlmisc 0.3.4.9000

- ...

# inlmisc 0.3.4

- In `PlotMap` function, fix bug in range calculation of raster values by removing `finite = TRUE`.

- In `PlotMap` function, fix extent of background image, rivers, lakes, and roads using `par("usr")`.

- Add `FindOptimalSubset` function, used to identify an optimal subset using a genetic algorithm.

# inlmisc 0.3.3

- In `AddPoints` function: add option to scale symbol size to radius;
  revise calculation of symbol size and legend labels.

- In `PlotMap` function, let `r` argument accept any object that can be converted to a `RasterLayer` class.

- In LaTeX preamble: add **verbatimbox** package; add maketitle format.

- In `PlotMap` function, add `simplify` argument, used to convert raster to polygons prior to plotting.

- In `Grid2Polygons` function: allow `grd` argument to be an object of class `SpatialPixelsDataFrame` or `Raster*`;
  transform coordinate reference system (CRS) of `ply` argument to match the CRS of the `grd` argument; and
  add `zlim` argument, a limit on the attribute variable.

- In `ToScientific` function, add arguments `scipen`, `delimiter`, and `...`; and rename `lab.type` argument to `type`.
  Code in place for backwards compatibility.

# inlmisc 0.3.2

- In `RecreateLibrary` and `SavePackageDetails` functions, add support for *gzip* file compression.

- In `SavePackageDetails` function, fix bug that results in an 'invalid cross-device link' error on some flavor's of R.

# inlmisc 0.3.1

- Rename `SavePackageNames` function to `SavePackageDetails`.

- Add `AddCertificate` function, used to add a X.509 certificate to your CA bundle.

- Make **httr** a suggested package.

- In `RecreateLibrary` function, add `local` argument, file paths of files containing builds of packages.

- In `AddGradientLegend` function, add `n` argument, the desired number of tick-marks to be drawn.

# inlmisc 0.3.0

- Add `Grid2Polygons` function from the **Grid2Polygons** package.

- Total revision of the `RecreateLibrary` and `SavePackageNames` functions.

- In `ToScientific` function, improve default value for `digits` argument.

- Add `AddGradientLegend` function, used to add a continuous color gradient legend strip to a plot.

# inlmisc 0.2.7

- Add `RecreateLibrary` function, used to recreate an existing library on a new installation of R.

- Add `FormatPval` function, used to format *p*-values.

- Add **bm** and **makecell** packages to LaTeX preamble.

- Changes in `PlotGraph` function inlclude:
  Add option for plotting interval censored data by specifying `type = "i"`.
  Select box-and-whisker plot using `type = "w"`, was previously `type = "box"`.
  Default for `seq.date.by` argument changed from `"year"` to `NULL`.

- Add `CreateWebMap` function, creates a Leaflet map widget using [The National Map](https://nationalmap.gov/) services.

- Tidy help documentation for functions.

# inlmisc 0.2.6

- Add `endian` argument to `ReadModflowBinary` function.
  Argument describes the endian-ness (or byte-order) of the binary file and is required for calls to the `readBin` function.
  Thanks to Professor Brian Ripley for identifying this issue.

# inlmisc 0.2.5

- In `SummariseBudget` function, the `desc` argument no longer needs to be specified.
  If missing, all data types are summarized.

- In `SummariseBudget` function, remove default for `desc` argument and add `id` argument,
  the auxiliary variable name.

- In `ReadModflowBinary` function, enable cell-by-cell budget files to be read when they are not in compact form.

- Change legend format for binned point data from `[#, )` to `># to #`.

- Fix bug that resulted in error when plotting a single point location using the `AddPoints` function.

- Add [AppVeyor](https://www.appveyor.com/) to package.

# inlmisc 0.2.4

- Fix bug that resulted in incorrect dimensions for saved graphics.

# inlmisc 0.2.3

- In `PlotMap`, `PlotCrossSection`, and `AddColorKey` functions,
  dynamically adjust vertical plot margins based on the number of lines in the user specified labels.

- In `AddPoints` function, remove `draw.legend` argument and rename `pos` argument to `legend.pos`.
  A `NULL` value for `legend.pos` will prevent the legend from being drawn.

- Change function name from `AddBubbles` to `AddPoints`.

- In `PlotMap` and `PlotCrossSection` functions, fix layout so color key dimensions don't change on resize.

- In `PlotMap` function, add `file.close` argument.
  A logical that idicates if the graphics device driver should be shut down when the function exits.

- In `AddBubbles` function, allow `z` argument to be an object of class `factor`.

- In `AddBubbles` function, account for aspect ratio when constructing legend.

- In `AddBubbles` function, rename `bg.pos` argument to `bg`, and set `bg.neg = NULL`.
  The `bg` argument may now be used to specify circle colors for all `z` values, not just positive values.
  See help documentation for details.

- In `AddBubbles` function, specifying a single numeric value for the `inches` arguments results in
  a fixed radius being used for all circle symbols.

- In `PlotMap` function, account for z-axis limits prior to removing rows and columns having all missing values.

- In `PlotMap` function, change default for `scale.loc` argument from `"leftbottom"` to `NULL`;
  the scale bar is no longer drawn by default.

- In `PlotMap` function, change `asp` argument from 1 to NULL.
  Defaults to 1 when data is projected, otherwise, a calculated value based on axes limits is used.

- Add function `POSIXct2Character`, used to convert objects from `POSIXct` to `character` class.

- Update URL links to HTTP Secure

- Generalize CITATION file

# inlmisc 0.2.2

- Change argument name in `ReadModflowBinary` function from `f` to `path`.

- Add `ReadCodeChunk` function, reads **knitr** code chunks into the current session.

- Change NEWS file to markdown format.

- Remove "misc" folder from ".Rbuildignore", adds the "misc/preamble.tex" file back to the package build.

# inlmisc 0.2.1

- Fix invalid URLs.

# inlmisc 0.2.0

- Add `file` argument to `PlotMap` and `PlotCrossSection` functions.
  Specifying this argument will start a graphics device driver for producing either a PDF or PNG file.

- Width and height of graphics device is no longer calculated when drawing to screen.

# inlmisc 0.1.2

- Explain the USGS and INL acronyms from the Title in the DESCRIPTION file.

- Fix invalid URLs.

# inlmisc 0.1.0

- First version of **inlmisc**
