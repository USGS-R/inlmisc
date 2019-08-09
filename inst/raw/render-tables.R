#!/usr/bin/env Rscript

.PlotBox <- function(file, color, width, height=width) {
  checkmate::assertPathForOutput(file, overwrite=TRUE)
  stopifnot(inlmisc::IsColor(color))
  checkmate::assertNumber(width,  lower=0, finite=TRUE)
  checkmate::assertNumber(height, lower=0, finite=TRUE)

  grDevices::postscript(file, width=width, height=height,
                        horizontal=FALSE, paper="special")
  graphics::par(mar=c(0, 0, 0, 0))
  graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=NULL,
                         xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                         xlab="", ylab="")
  graphics::rect(0, 0, 1, 1, col=color, border=NA, lwd=0.5)
  graphics::box(lwd=0.25, col="#D3D3D3")
  dev.off()

  invisible()
}


.Clean <- function() {
  pattern <- "^g[1-4]_[0-9]{3}(\\.eps|-eps-converted-to\\.pdf)$"
  unlink(list.files(pattern=pattern))
  unlink(sprintf("table.%s", c("tex", "pdf", "svg", "log", "aux", "dvi")))
  invisible()
}


dir.create("figures", showWarnings=FALSE)

cite <- as.factor(vapply(inlmisc:::schemes, function(x) x$cite, ""))

for (no in seq_along(levels(cite))) {

  idxs <- which(cite == levels(cite)[no])

  m <- do.call("rbind", lapply(idxs, function(i) {

    s <- inlmisc:::schemes[[i]]

    w <- 100; h <- 10

    n <- ifelse(is.finite(s$nmax), s$nmax, w - 1)
    pal <- inlmisc::GetColors(n, scheme=names(inlmisc:::schemes)[i])

    fmt <- "%s_%03d.eps"
    f1 <- sprintf(fmt, "g1", i)
    f2 <- sprintf(fmt, "g2", i)
    f3 <- sprintf(fmt, "g3", i)
    f4 <- sprintf(fmt, "g4", i)

    fmt <- "\\adjustimage{width=%spx, height=%spx, valign=m}{%s}"
    g1 <- sprintf(fmt, w, h, f1)
    g2 <- sprintf(fmt, h, h, f2)
    g3 <- sprintf(fmt, h, h, f3)
    g4 <- sprintf(fmt, h, h, f4)

    px_to_in <- 1 / 72

    grDevices::postscript(f1, onefile=FALSE,
                          width=w * px_to_in, height=h * px_to_in,
                          horizontal=FALSE, pointsize=10, paper="special")
    inlmisc:::plot.inlpal(pal, label=FALSE)
    dev.off()

    if (is.null(s$nan))  g2 <- "--" else .PlotBox(f2, s$nan,  h * px_to_in)
    if (is.null(s$back)) g3 <- "--" else .PlotBox(f3, s$back, h * px_to_in)
    if (is.null(s$fore)) g4 <- "--" else .PlotBox(f4, s$fore, h * px_to_in)

    nmax <- s$nmax
    nmax[nmax == Inf] <- "$\\infty$"
    if (!is.null(s$gray))
      nmax <- sprintf("%s (%s)", nmax, length(s$gray))

    c("Type"    = s$type,
      "Scheme"  = names(inlmisc:::schemes)[i],
      "Palette" = g1,
      "\\hspace*{0.3ex} Max n \\hspace{0.3ex}" = nmax,
      "\\hspace*{0.3ex} N \\hspace{0.3ex}" = g2,
      "\\hspace*{0.3ex} B \\hspace{0.3ex}" = g3,
      "\\hspace*{0.3ex} F \\hspace{0.3ex}" = g4)
  }))
  m[duplicated(m[, "Type"]), "Type"] <- ""

  src <- levels(cite)[no]
  if (src == "Paul Tol (2018)") {
    title <- sprintf("Schemes by %s with permission granted to distribute in Oct 2018.", src)
  } else if (src == "Thomas Dewez (2004)") {
    title <- sprintf("Schemes by %s with permission granted to distribute in Oct 2018.", src)
  } else if (src == "Wessel and others (2013)") {
    title <- sprintf("Schemes collected by %s and released under an open license.", src)
  } else if (src == "unknown") {
    title <- "Scheme by unknown author; discovered on gnuplot-info by Edzer Pebesma."
  } else {
    title <- "ADD ATTRIBUTION"
  }

  sink("table.tex")
  cat("\\documentclass[varwidth=\\maxdimen, border=0pt]{standalone}",
      "\\usepackage[T1]{fontenc}",
      "\\usepackage{mathptmx}",
      "\\usepackage{amsfonts}",
      "\\usepackage{textcomp}",
      "\\renewcommand{\\sfdefault}{lmss}",
      "\\renewcommand{\\ttdefault}{lmtt}",
      "\\usepackage{booktabs}",
      "\\usepackage{makecell}",
      "\\usepackage{adjustbox}",
      "\\usepackage[skip=2pt, labelsep=period, labelfont=bf]{caption}",
      sprintf("\\setcounter{table}{%d}", no - 1L),
      "\\begin{document}", sep="\n")
  inlmisc::PrintTable(m, align=c("p{1.8cm}", "p{2.5cm}", "c", "c", "c", "c", "c"),
                      title=title)
  cat("\\end{document}\n")
  sink()

  tools::texi2pdf("table.tex", clean=TRUE)

  arg <- c("--margins 1", "--clip", "table.pdf", "table.pdf")
  system2("pdfcrop", args=arg, stdout=FALSE, stderr=FALSE)

  arg <- c("--without-gui", "--file=table.pdf", "--export-plain-svg=table.svg")
  system2("inkscape", args=arg, stdout=FALSE, stderr=FALSE)

  gs_cmd <- Sys.getenv("R_GSCMD", tools::find_gs_cmd())
  tools::compactPDF("table.pdf", qpdf="", gs_cmd=gs_cmd, gs_quality="printer")
  system2("svgcleaner", args=c("table.svg", "table.svg"), stdout=FALSE, stderr=FALSE)

  from <- c("table.pdf", "table.svg")
  to <- file.path("figures", sprintf("table%02d.%s", no, tools::file_ext(from)))
  stopifnot(file.copy(from, to, overwrite=TRUE))

  .Clean()
}
