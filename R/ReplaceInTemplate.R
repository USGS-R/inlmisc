#' Replace Values in a Template Text
#'
#' This function replaces keys within special markups in a template text with specified values.
#' Pieces of \R code can be put into the markups of the template text,
#' and are evaluated during the replacement.
#'
#' @param text 'character' vector.
#'   Template text
#' @param replacement 'list'.
#'   Values to replace in \code{text}.
#'
#' @details Keys are enclosed into markups of the form \code{$(KEY)} and \code{@\{CODE\}}.
#'
#' @return Returns a vector of character strings after key replacement.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @seealso \code{\link{SummariseBudget}}
#'
#' @keywords IO
#'
#' @references This code was derived from the
#'   \href{https://CRAN.R-project.org/package=sensitivity}{sensitivity}\code{::template.replace} function,
#'   accessed on Feb 6, 2015.
#'
#' @export
#'
#' @examples
#' text <- c("Hello $(name)!", "$(a) + $(b) = @{$(a) + $(b)}",
#'           "pi = @{format(pi, digits = 5)}")
#' cat(text, sep = "\n")
#' replacement <- list(name = "world", a = 1, b = 2)
#' cat(ReplaceInTemplate(text, replacement), sep = "\n")
#'

ReplaceInTemplate <- function(text, replacement=list()) {

  checkmate::assertCharacter(text, any.missing=FALSE, min.len=1)
  checkmate::assertList(replacement)

  for (i in seq_along(text)) {
    for (j in names(replacement)) {
      pattern <- sub("KEY", j, "\\$\\(KEY\\)", perl=TRUE)
      text[i] <- gsub(pattern, paste(replacement[[j]]), text[i], perl=TRUE)
    }

    repeat {
      reg <- regexpr("@\\{.+?\\}", text[i], perl=TRUE)
      if (reg == -1) break

      match.first <- as.integer(reg)
      match.last <- match.first + attr(reg, "match.length") - 1
      match.text <- substr(text[i], match.first + 2, match.last - 1)

      val.match.text <- eval(parse(text=match.text))

      line.begin <- substr(text[i], 1, match.first - 1)
      line.middle <- paste(val.match.text)
      line.end <- substr(text[i], match.last + 1, nchar(text[i]))
      text[i] <- paste0(line.begin, line.middle, line.end)
    }
  }

  text
}
