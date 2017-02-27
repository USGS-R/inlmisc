# inlmisc 0.2.4.9000

- Legend format for binned point data has changed from `[#, )` to `># to #`.

- Fix bug that resulted in an error when plotting a single point location using the `AddPoints` function.

- Add [AppVeyor](https://www.appveyor.com/) to package.

# inlmisc 0.2.4

- Fix bug that was resulting in incorrect dimensions for saved graphics.

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
