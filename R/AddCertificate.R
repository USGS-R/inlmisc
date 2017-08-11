#' Add X.509 Certificate
#'
#' This function adds a \href{https://en.wikipedia.org/wiki/X.509}{X.509}
#' certificate to your certificate authority (CA) bundle.
#' The X.509 certificate is used to authenticate clients and servers.
#' And the CA bundle is a file that contains root and intermediate certificates.
#'
#' @param file 'character'.
#'   Path of file containing the X.509 certificate.
#' @param header 'character'.
#'   Header line to identify the certificate (optional).
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
#' # Add the U.S. Department of Interior (DOI) certificate:
#' \dontrun{
#' AddCertificate(file = "http://sslhelp.doi.net/docs/DOIRootCA2.cer",
#'                header = "DOI Root CA 2")
#' }
#'

AddCertificate <- function(file, header=NULL) {

  if (.Platform$OS.type != "windows")
    stop("Only implemented on Windows operating system.", call.=FALSE)

  if (!requireNamespace("httr", quietly=TRUE))
    stop("Requires access to the 'httr' package.", call.=FALSE)

  if (!file.exists(file) || httr::http_error(file))
    stop("Invalid certificate or access denied.", call.=FALSE)

  certificate <- readLines(file)

  env <- Sys.getenv("CURL_CA_BUNDLE")
  if (env == "")
    bundle <- system.file("cacert.pem", package="openssl", mustWork=TRUE)
  else
    bundle <- env
  if (!file.exists(bundle))
    stop("Can not locate certificates bundle.", call.=FALSE)

  if (all(certificate %in% readLines(bundle))) {
    message("Certificate already added to CA bundle.")
  } else {
    if (!is.null(header))
      header <- c(header, paste(rep("=", nchar(header)), collapse=""))
    certificate <- c("", header, certificate)
    cat(certificate, file=bundle, sep="\n", append=TRUE)
    message("Certificate added to the CA bundle:\n ",
            normalizePath(bundle))
  }

  invisible(NULL)
}
