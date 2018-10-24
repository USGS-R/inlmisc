MakeFigures<- function() {

  load("../../R/sysdata.rda")

  m <- do.call("rbind", lapply(seq_along(schemes), function(i) {

    s <- schemes[[i]]

    n <- ifelse(is.finite(s$nmax), s$nmax, 255)
    pal <- inlmisc::GetColors(n, scheme=names(schemes)[i])

    w <- 100; h <- 10

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

    grDevices::postscript(f1, width=w / 72, height=h / 72, horizontal=FALSE, paper="special")
    inlmisc:::plot.inlcol(pal, label=FALSE)
    dev.off()

    if (is.null(s$nan))  g2 <- "--" else .PlotBox(f2, s$nan,  h / 72)
    if (is.null(s$back)) g3 <- "--" else .PlotBox(f3, s$back, h / 72)
    if (is.null(s$fore)) g4 <- "--" else .PlotBox(f4, s$fore, h / 72)

    nmax <- s$nmax
    nmax[nmax == Inf] <- "$\\infty$"
    if (!is.null(s$gray))
      nmax <- sprintf("%s (%s)", nmax, length(s$gray))

    c("Type"    = s$type,
      "Scheme"  = names(schemes)[i],
      "Palette" = g1,
      "Max n"   = nmax,
      "N"       = g2,
      "B"       = g3,
      "F"       = g4)
  }))
  m[duplicated(m[, "Type"]), "Type"] <- ""

  cite <- as.factor(vapply(schemes, function(x) x$cite, ""))
  m[, "Scheme"] <- sprintf("\\footnotemark[%d] %s", as.integer(cite), m[, "Scheme"])
  txt <- levels(cite)
  txt[grep("^Paul T", txt)] <- "Paul Tol (2018) granted permission to use"
  txt[grep("^Thomas", txt)] <- "Thomas Dewez (2004) granted permission to use"
  txt[grep("^Wessel", txt)] <- "Wessel and others (2013) released under the GNU Lesser General Public License v3 or later"

  x <- sprintf("\\footnotemark[%d] %s", seq_along(txt), txt)
  footnotes <- paste(x, collapse="\\\\")

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
      "\\begin{document}", sep="\n")
  inlmisc::PrintTable(m, align=c("l", "l", "c", "c", "c", "c", "c"),
                      footnotes=footnotes)
  cat("\\end{document}\n")
  sink()

  tools::texi2pdf("table.tex", clean=TRUE)

  args <- c("--without-gui",
            "--file=table.pdf",
            "--export-plain-svg=table.svg")
  system2("inkscape", args=args, stdout=FALSE, stderr=FALSE, invisible=TRUE)

  if (dir.exists("../../man"))
    dir.create(path <- "../../man/figures/", showWarnings=FALSE)
  stopifnot(file.copy(c("table.pdf", "table.svg"), path, overwrite=TRUE))

  width <- dim(rsvg::rsvg("table.svg"))[2]
  fmt <- c("#'   \\if{html}{\\figure{table.svg}{options: width=%d alt=\"Table: schemes\"}}",
           "#'   \\if{latex}{\\figure{table.pdf}{options: width=%.2fcm}}")
  cat(sprintf(fmt[1], width), sprintf(fmt[2], width * 0.0264583333), sep="\n")

  unlink(list.files(pattern="^g[1-4]_[0-9]{3}\\.eps$"))
  unlink(sprintf("table.%s", c("tex", "pdf", "svg")))

  invisible()
}


.PlotBox <- function(file, color, width, height=width) {
  checkmate::assertPathForOutput(file, overwrite=TRUE)
  stopifnot(inlmisc:::.IsColor(color))
  checkmate::assertNumber(width,  lower=0, finite=TRUE)
  checkmate::assertNumber(height, lower=0, finite=TRUE)

  grDevices::postscript(file, width=width, height=height,
                        horizontal=FALSE, paper="special")
  graphics::par(mar=c(0, 0, 0, 0))
  graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=NULL,
                         xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                         xlab="", ylab="")
  graphics::rect(0, 0, 1, 1, col=color, border=NA, lwd=0.5)
  graphics::box(lwd=0.5, col="#D3D3D3")
  dev.off()

  invisible()
}
