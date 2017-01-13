# inlmisc 0.2.2.9000

- Add `fixed.radius` argument in `AddBubbles` function; enables a fixed radius to be used for all circle symbols.

- In `PlotMap` function, account for *z*-axis limits prior to removing rows and columns having all missing values.

- Change default for `scale.loc` argument in `PlotMap` function from `"leftbottom"` to `NULL`;
  the scale bar is no longer drawn by default.

- Change `asp` argument in `PlotMap` function from 1 to NULL.
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
