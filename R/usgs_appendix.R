#' USGS Appendix Format
#'
#' Format for creating a U.S. Geological Survey appendix.
#'
#' @inheritParams rmarkdown::pdf_document
#' @param ... Arguments to \code{rmarkdown::pdf_document}
#'
#' @return R Markdown output format to pass to \code{\link[rmarkdown:render]{render}}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' rmarkdown::draft("Appendix.Rmd", template = "usgs_appendix", package = "inlmisc")
#' }
#'

usgs_appendix <- function(...) {

  rmarkdown::pandoc_available(version="2.2", error=TRUE)

  f <- "rmarkdown/templates/usgs_appendix/resources/template.tex"
  template <- system.file(f, package="inlmisc")
  if (template == "")
    stop("Couldn't find template file resources/template.tex", call.=FALSE)

  base <- rmarkdown::pdf_document(..., template=template, highlight=NULL)

  base$inherits <- "pdf_document"

  # render generates a tex file,
  # post-processing hook generates appropriate wrapper.tex, and
  # pandoc builds pdf from wrapper.tex

  base$pandoc$to <- "latex"
  base$pandoc$ext <- ".tex"

  base$post_processor <- function(metadata, utf8_input, output_file, clean, verbose) {
    filename <- basename(output_file)

    # underscores in the filename will be problematic in \input{filename};
    # pandoc will escape underscores but it should not,
    # i.e., should be \input{foo_bar} instead of \input{foo\_bar}
    if (filename != (filename2 <- gsub("_", "-", filename))) {
      file.rename(filename, filename2)
      filename <- filename2
    }

    wrapper_metadata <- list("preamble" = metadata$preamble,
                             "filename" = tools::file_path_sans_ext(filename))

    f <- "rmarkdown/templates/usgs_appendix/resources/wrapper.tex"
    wrapper_template <- system.file(f, package="inlmisc")
    if (wrapper_template == "")
      stop("Couldn't find template file resources/wrapper.tex", call.=FALSE)

    wrapper_output <- file.path(getwd(), "wrapper.tex")

    # render pandoc template
    f <- tempfile(fileext=".md"); on.exit(unlink(f))
    cat("---\n", file=f)
    cat(yaml::as.yaml(wrapper_metadata), file=f, append=TRUE)
    cat("---\n", file=f, append=TRUE)
    cat("\n", file=f, append=TRUE)
    rmarkdown::pandoc_convert(f, to="markdown", output=wrapper_output,
                              options=paste0("--template=", wrapper_template),
                              verbose=verbose)

    tinytex::latexmk("wrapper.tex", engine=base$pandoc$latex_engine, clean=clean)
  }

  # mostly copied from knitr::render_sweave

  base$knitr$opts_chunk$comment <- "#>"

  HookChunk <- function(x, options) {
    FUN <- utils::getFromNamespace("output_asis", "knitr")
    if (FUN(x, options)) return(x)
    paste0("```{=latex}\n\\begin{Schunk}\n", x, "\\end{Schunk}\n```")
  }

  HookInput <- function(x, options) {
    FUN <- utils::getFromNamespace("hilight_source", "knitr")
    paste(c("\\begin{Sinput}", FUN(x, "sweave", options), "\\end{Sinput}", ""),
          collapse="\n")
  }

  HookOutput <- function(x, options) {
    paste0("\\begin{Soutput}\n", x, "\\end{Soutput}\n")
  }

  FUN <- utils::getFromNamespace("merge_list", "knitr")
  base$knitr$knit_hooks <- FUN(list(base$knitr$knit_hooks),
                               list("chunk"   = HookChunk,
                                    "source"  = HookInput,
                                    "output"  = HookOutput,
                                    "message" = HookOutput,
                                    "warning" = HookOutput,
                                    "plot"    = knitr::hook_plot_tex))

  base
}
