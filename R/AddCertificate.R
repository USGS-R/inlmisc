#' Add X.509 Certificate
#'
#' This function adds a \href{https://en.wikipedia.org/wiki/X.509}{X.509}
#' certificate to your certificate authority (CA) bundle.
#' The X.509 certificate is used to authenticate clients and servers.
#' And the CA bundle is a file that contains root and intermediate certificates.
#'
#' @param file 'character'.
#'   Path of file containing the X.509 certificate.
#'   Its default is the path to the U.S. Department of Interior (DOI) certificate file.
#'   To access this file you must be affiliated with the DOI.
#' @param header 'character'.
#'   Header line(s) to identify the certificate (optional);
#'   specify as \code{NULL} to exclude this metadata.
#'
#' @note This function must be used on Windows and requires access to the \pkg{httr} package.
#'
#' @author J.C. Fisher, U.S. Geological Survey, Idaho Water Science Center
#'
#' @keywords utilities
#'
#' @export
#'
#' @examples
#' \dontrun{
#' AddCertificate()
#' }
#'

AddCertificate <- function(file="http://sslhelp.doi.net/docs/DOIRootCA2.cer",
                           header="DOI Root CA 2") {

  if (.Platform$OS.type != "windows")
    stop("Only implemented on Windows", call.=FALSE)

  if (!requireNamespace("httr", quietly=TRUE))
    stop("Requires access to the 'httr' package.", call.=FALSE)

  if (!file.exists(file) || httr::http_error(file))
    stop("Invalid certificate file or access denied.", call.=FALSE)

  text <- readLines(file)

  env <- Sys.getenv("CURL_CA_BUNDLE")
  if (env == "")
    certificates <- system.file("cacert.pem", package="openssl", mustWork=TRUE)
  else
    certificates <- env
  if (!file.exists(certificates))
    stop("Can not locate certificates bundle.", call.=FALSE)

  if (all(text %in% readLines(certificates))) {
    message("Certificate already appended")
  } else {
    if (!is.null(header))
      header <- c(header, paste(rep("=", nchar(header)), collapse=""))
    text <- c("", header, text)
    cat(text, file=certificates, sep="\n", append=TRUE)
    message("Certificate added to the CA bundle:\n ",
            normalizePath(certificates))
  }

  invisible(NULL)
}
