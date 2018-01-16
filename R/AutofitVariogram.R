#' Autofit Variogram Model
#'
#' This function automatically fits a theoretical variogram model to input data.
#' Automatic fitting is performed using the \code{\link[gstat]{fit.variogram}} function.
#' And initial values for the sill, range, nugget, and model type are estimated using a
#' modified version of the \code{\link[automap]{autofitVariogram}} function.
#' Many options are available for customization of the bins in the empirical variogram.
#'
#' @param formula 'formula'.
#'   Formula that defines the dependent variable as a linear model of independent variables.
#' @param input_data 'SpatialPointsDataFrame'.
#'   Input data
#' @param model 'character'.
#'   Vector of model types that will be examined when automatically fitting a
#'   theoretical variogram model to the empirical variogram data.
#'   The different model types include:
#'   \code{"Sph"}, spherical;
#'   \code{"Exp"}, exponential;
#'   \code{"Gau"}, Gaussian;
#'   \code{"Ste"}, Matern with Michael Stein's parameterization.
#' @param kappa 'numeric'.
#'   Vector of smoothing parameter(s) of the Matern model.
#' @param fix.values 'numeric'.
#'   A vector of length 3 containing user specified fixed values for nugget, range, and sill.
#'   Specifying value as \code{NA} indicates that the value is not fixed.
#' @param verbose 'logical'.
#'   Specify as true to print additional output describing the fitting process.
#' @param GLS.model 'variogramModel'.
#'   If specified, a Generalized Least Squares (GLS) empirical variogram is calculated.
#' @param start_vals 'numeric'.
#'   Starting values for variogram fitting, see \code{fix.values} argument for format.
#' @param bin_method 'character'.
#'   Binning method, either \code{"automap"} (default), \code{"gstat"}, \code{"equal_width"}, or \code{"equal_count"}.
#'   See \sQuote{Details} section for method descriptions.
#' @param merge.small.bins 'logical'.
#'   Whether to check if there are bins with less than 5 observations.
#'   If true, the first two bins are merged and the check is repeated.
#'   This is repeated until all bins have more than \code{min.np.bin} observations.
#' @param min.np.bin 'integer'.
#'   Minimum number of observations that every bin should contain.
#' @param num.bins 'integer'.
#'   Initial number of bins
#' @param init.width 'numeric'.
#'   Initial bin width
#' @param ...
#'   Parameters passed to the \code{\link[gstat]{variogram}} function when calculating the empirical variogram.
#'
#' @details
#'   The different binning methods include:
#'   \describe{
#'     \item{\code{"automap"}}{is the default binning method used by the \code{\link[automap]{autofitVariogram}} function.
#'       The distance intervals that define the bins are equal to 2, 4, 6, \ldots{}, 100 percent
#'       of about 1/3 the diagonal of the box spanning the observation locations.}
#'     \item{\code{"gstat"}}{is the default binning method used by the \code{\link[gstat]{variogram}} function.}
#'     \item{\code{"equal_width"}}{resizes the bins so that each contains the minimum number of observations.}
#'     \item{\code{"equal_count"}}{defines bins so that each contains the same number of observations.}
#'   }
#'
#' @return Returns a 'autofitVariogram' object with the following components:
#'   \describe{
#'     \item{\code{exp_var}}{empirical variogram}
#'     \item{\code{var_model}}{theoretical variogram model}
#'     \item{\code{sserr}}{sums of squares between the empirical variogram and the fitted theoretical variogram model.}
#'   }
#'
#' @note
#'   Function source code was modified from the \code{autofitVariogram} function in the
#'   \href{https://CRAN.R-project.org/package=automap}{automap} package (version 1.0-14, license
#'   \href{https://cran.r-project.org/web/licenses/GPL-2}{GPL-2}) |
#'   \href{https://cran.r-project.org/web/licenses/GPL-3}{GPL-3}).
#'   And source code for the \code{"equal_width"} and \code{"equal_count"} binning methods was modified from the
#'   \href{http://hydroecology.net/know-your-variograms/}{know-your-variograms} blog post,
#'   by M.C. Koohafkan, accessed on Jan. 14, 2018.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords models
#'
#' @export
#'
#' @examples
#' data(meuse, package = "sp")
#' sp::coordinates(meuse) <- ~x+y
#'
#' v1 <- AutofitVariogram(zinc~1, meuse, verbose = TRUE)
#' automap:::plot.autofitVariogram(v1)
#'
#' v2 <- AutofitVariogram(zinc~1, meuse, bin_method = "equal_width",
#'                        num.bins = 20, verbose = TRUE)
#' automap:::plot.autofitVariogram(v2)
#'
#' v3 <- AutofitVariogram(zinc~1, meuse, bin_method = "equal_width",
#'                        init.width = 200, verbose = TRUE)
#' automap:::plot.autofitVariogram(v3)
#'
#' v4 <- AutofitVariogram(zinc~1, meuse, bin_method = "equal_count",
#'                        min.np.bin = 800, verbose = TRUE)
#' automap:::plot.autofitVariogram(v4)
#'

AutofitVariogram <- function(formula, input_data, model=c("Sph", "Exp", "Gau", "Ste"),
                             kappa=c(0.05, seq(0.2, 2, 0.1), 5, 10), fix.values=c(NA, NA, NA),
                             verbose=FALSE, GLS.model=NA, start_vals=c(NA, NA, NA),
                             bin_method=c("automap", "gstat", "equal_width", "equal_count"),
                             merge.small.bins=TRUE, min.np.bin=5, num.bins=NA, init.width=NA,
                             ...) {

  model <- match.arg(model, several.ok=TRUE)
  bin_method <- match.arg(bin_method)

  # check for anisotropy parameters
  if ("alpha" %in% names(list(...)))
    warning("Anisotropic variogram model fitting not supported, ",
            "see the documentation of autofitVariogram for more details.")

  # create boundaries
  longlat <- !sp::is.projected(input_data)
  if (is.na(longlat)) longlat <- FALSE
  diagonal <- sp::spDists(t(sp::bbox(input_data)), longlat=longlat)[1, 2]

  # equal-width bins (approximately, variogram does its own binning too)
  if (bin_method == "equal_width") {
    if (verbose) cat("Using equal-width bins...\n")
    if ("width" %in% names(list(...)))
      stop("Cannot pass width when 'equal.width.bins' is TRUE. ",
           "Supply 'init.width' instead.", call.=FALSE)
    # replace diagonal with cutoff
    if ("cutoff" %in% names(list(...))) diagonal <- list(...)[["cutoff"]]
    # user must supply either bin width or number of bins
    if (is.na(init.width)) {
      if (is.na(num.bins))
        stop("When 'equal.width.bins' is TRUE, ",
             "user must also supply either 'init.width' or 'num.bins'.", call.=FALSE)
      width <- diagonal / num.bins
      if (verbose) cat("Initial width not provided. Calculating using num.bins.\n")
    } else {
      width <- init.width
      if (verbose) cat("Initial width provided.\n")
    }
    # get the empirical variogram
    if (methods::is(GLS.model, "variogramModel")) {
      empirical_variogram <- gstat::variogram(g, width=width, ...)
    } else {
      empirical_variogram <- gstat::variogram(formula, input_data, width=width, ...)
    }
    # merge small bins if requested
    if (merge.small.bins) {
      if (verbose) cat("Checking if any bins have less than", min.np.bin,
                       "points, merging bins when necessary...\n")
      iter <- 0
      maxiter <- 1000
      while (TRUE) {
        if (!any(empirical_variogram$np < min.np.bin)) break
        width <- width * 1.1  # increase width by 10 percent and try again
        if (methods::is(GLS.model, "variogramModel")) {
          empirical_variogram <- gstat::variogram(g, width=width, ...)
        } else {
          empirical_variogram <- gstat::variogram(formula, input_data, width=width, ...)
        }
        iter <- iter + 1
        if (iter > maxiter) {
          cat("Maximum number of interations reached.",
              "Try decreasing 'min.np.bin' or 'init.width'.\n\n")
          break
        }
      }
    }

  # equal observation count bins
  } else if (bin_method == "equal_count") {
    if (verbose) cat("Using bins of equal observation counts...\n")
    if ("boundaries" %in% names(list(...)))
      stop("Can not pass boundaries when 'equal.np.bins' is TRUE. ",
           "Pass 'num.bins' or 'min.np.bin' instead.", call.=FALSE)
    # replace diagonal with cutoff, if provided
    if ("cutoff" %in% names(list(...))) diagonal <- list(...)[["cutoff"]]
    # get a sorted list of distances
    dists <- sort(sp::spDists(input_data))
    # apply the cutoff
    dists <- dists[dists < diagonal & dists > 0]
    # split the data into bins based on number of observations
    if (is.na(num.bins)) {
      # compute number of bins based on the minimum number of observations per bin
      num.bins <- floor(0.5 * length(dists) / min.np.bin)
      if (verbose)
        cat("num.bins not supplied. Setting 'num.bins' equal to", num.bins,
            "based on min.np.bin.\n")
    }
    cat("Checking bins, decreasing num.bins if necessary... \n")
    while (TRUE) {
      # compute interval based on the number of bins
      interval <- length(dists) / num.bins
      # define boundaries
      boundaries <- rep(NA, num.bins)
      for (i in seq_len(num.bins)) {
        boundaries[i] <- dists[round(i * interval)]
      }
      if (length(boundaries == length(unique(boundaries)))) break
      # reduce number of bins
      num.bins <- num.bins - 1
    }
    if (methods::is(GLS.model, "variogramModel")) {
      if (verbose) cat("Calculating GLS sample variogram.\n")
      g <- gstat::gstat(NULL, "bla", formula, input_data, model=GLS.model, set=list(gls=1))
      empirical_variogram <- gstat::variogram(g, boundaries=boundaries, ...)
    } else {
      empirical_variogram <- gstat::variogram(formula, input_data, boundaries=boundaries, ...)
    }

  # compute boundaries using automap method
  } else if (bin_method == "automap") {
    if (verbose) cat("Boundaries as defined by automap::autofitVariogram...\n\n")
    boundaries <- c(2, 4, 6, 9, 12, 15, 25, 35, 50, 65, 80, 100) *
                    diagonal * 0.35 / 100  # boundaries for the bins
    # specifiy a variogram model in GLS.model the generelised least squares sample variogram is constructed
    if (methods::is(GLS.model, "variogramModel")) {
      if (verbose) cat("Calculating GLS sample variogram.\n")
      g <- gstat::gstat(NULL, "bla", formula, input_data, model=GLS.model, set=list(gls=1))
      empirical_variogram <- gstat::variogram(g, boundaries=boundaries, ...)
    } else {
      empirical_variogram <- gstat::variogram(formula, input_data, boundaries=boundaries, ...)
    }
    if (merge.small.bins) {
      if (verbose) cat("Checking if any bins have less than 5 points, merging bins when necessary...\n\n")
      while (TRUE) {
        if (length(empirical_variogram$np[empirical_variogram$np < min.np.bin]) == 0 |
            length(boundaries) == 1) break
        boundaries <- boundaries[2:length(boundaries)]
        if (methods::is(GLS.model, "variogramModel")) {
          empirical_variogram <- gstat::variogram(g, boundaries=boundaries, ...)
        } else {
          empirical_variogram <- gstat::variogram(formula, input_data,boundaries=boundaries, ...)
        }
      }
    }

  # compute boundaries using gstat method
  } else if (bin_method == "gstat") {
    if (verbose) cat("Boundaries as defined by gstat::variogram...\n\n")

    if (methods::is(GLS.model, "variogramModel")) {
      if (verbose) cat("Calculating GLS sample variogram\n")
      g <- gstat::gstat(NULL, "bla", formula, input_data, model=GLS.model, set=list(gls=1))
      empirical_variogram=gstat::variogram(g, ...)
    } else {
      empirical_variogram <- gstat::variogram(formula, input_data, ...)
    }
  }

  # set initial values
  if (is.na(start_vals[1])) {  # nugget
    initial_nugget <- min(empirical_variogram$gamma)
  } else {
    initial_nugget <- start_vals[1]
  }
  if (is.na(start_vals[2])) {  # range
    initial_range <- 0.1 * diagonal  # 0.10 times the length of the central axis through the area
  } else {
    initial_range <- start_vals[2]
  }
  if (is.na(start_vals[3])) {  # sill
    initial_sill <- mean(c(max(empirical_variogram$gamma),
                           stats::median(empirical_variogram$gamma)))
  } else {
    initial_sill <- start_vals[3]
  }

  # determine what should be automatically fitted and what should be fixed
  if (is.na(fix.values[1])) {  # nugget
    fit_nugget <- TRUE
  } else {
    fit_nugget <- FALSE
    initial_nugget <- fix.values[1]
  }
  if (is.na(fix.values[2])) {  # range
    fit_range <- TRUE
  } else {
    fit_range <- FALSE
    initial_range <- fix.values[2]
  }
  if (is.na(fix.values[3])) {  # partial sill
    fit_sill <- TRUE
  } else {
    fit_sill <- FALSE
    initial_sill <- fix.values[3]
  }

  GetModel <- function(psill, model, range, kappa, nugget, fit_range, fit_sill,
                       fit_nugget, verbose) {
    debug.level <- if (verbose) 1 else 0
    if (model == "Pow") {
      warning("Using the power model is at your own risk, read the docs of autofitVariogram for more details.")
      if (is.na(start_vals[1])) nugget <- 0
      if (is.na(start_vals[2])) range  <- 1  # if a power mode, range == 1 is a better start value
      if (is.na(start_vals[3])) sill   <- 1
    }
    obj <- try(gstat::fit.variogram(empirical_variogram,
                                    model=gstat::vgm(psill=psill, model=model, range=range, nugget=nugget, kappa=kappa),
                                    fit.ranges=c(fit_range),
                                    fit.sills=c(fit_nugget, fit_sill),
                                    debug.level=0), TRUE)
    if (inherits(obj, "try-error")) {
      warning("An error has occured during variogram fitting. Used:\n",
              "\tnugget:\t", nugget,
              "\n\tmodel:\t", model,
              "\n\tpsill:\t", psill,
              "\n\trange:\t", range,
              "\n\tkappa:\t", ifelse(kappa == 0, NA, kappa),
              "\n  as initial guess. This particular variogram fit is not taken into account. \nGstat error:\n", obj)
      return(NULL)
    } else {
      return(obj)
    }
  }

  # automatically test different models, the one with the smallest sums-of-squares is chosen
  test_models <- model
  SSerr_list <- c()
  vgm_list <- list()
  counter <- 1

  for (m in test_models) {
    if (m != "Mat" && m != "Ste") {  # matern and not stein
      model_fit <- GetModel(initial_sill - initial_nugget, m, initial_range,
                            kappa=0, initial_nugget, fit_range, fit_sill,
                            fit_nugget, verbose=verbose)
      if (!is.null(model_fit)) {  # skip models that failed
        vgm_list[[counter]] <- model_fit
        SSerr_list <- c(SSerr_list, attr(model_fit, "SSErr"))
      }
      counter <- counter + 1
    } else {  # loop over kappa values
      for (k in kappa) {
        model_fit <- GetModel(initial_sill - initial_nugget, m, initial_range,
                              k, initial_nugget, fit_range, fit_sill,
                              fit_nugget, verbose=verbose)
        if (!is.null(model_fit)) {
          vgm_list[[counter]] <- model_fit
          SSerr_list <- c(SSerr_list, attr(model_fit, "SSErr"))
        }
        counter <- counter + 1
      }
    }
  }

  # check for negative values in sill or range coming from fit.variogram
  # and NULL values in vgm_list, and remove those with a warning
  strange_entries <- sapply(vgm_list, function(v) any(c(v$psill, v$range) < 0) | is.null(v))
  if (any(strange_entries)) {
    if (verbose) {
      print(vgm_list[strange_entries])
      cat("^^^ ABOVE MODELS WERE REMOVED ^^^\n\n")
    }
    warning("Some models where removed for being either NULL or having a negative sill/range/nugget, ",
            "\n\tset verbose == TRUE for more information.")
    SSerr_list <- SSerr_list[!strange_entries]
    vgm_list <- vgm_list[!strange_entries]
  }

  if (verbose) {
    cat("Selected:\n")
    print(vgm_list[[which.min(SSerr_list)]])
    cat("\nTested models, best first:\n")
    tested <- data.frame("Tested models"=sapply(vgm_list, function(x) as.character(x[2, 1])),
                         kappa=sapply(vgm_list, function(x) as.character(x[2, 4])),
                         "SSerror"=SSerr_list)
    tested <- tested[order(tested$SSerror), ]
    print(tested)
  }

  result <- list(exp_var=empirical_variogram,
                 var_model=vgm_list[[which.min(SSerr_list)]],
                 sserr=min(SSerr_list))
  class(result) <- c("autofitVariogram", "list")

  return(result)
}
