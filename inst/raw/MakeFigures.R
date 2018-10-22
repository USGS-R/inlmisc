MakeFigures<- function() {

  load("../../R/sysdata.rda")

  m <- do.call("rbind", lapply(seq_along(schemes), function(i) {

    s <- schemes[[i]]

    n <- ifelse(is.finite(s$nmax), s$nmax, 255)
    pal <- inlmisc::GetColors(n, scheme=names(schemes)[i])

    f1 <- sprintf("%s_%03d.eps", "g1", i)
    f2 <- sprintf("%s_%03d.eps", "g2", i)
    w <- 100; h <- 10
    g1 <- sprintf("\\adjustimage{width=%spx, height=%spx, valign=m}{%s}", w, h, f1)
    g2 <- sprintf("\\adjustimage{width=%spx, height=%spx, valign=m}{%s}", h, h, f2)

    grDevices::postscript(f1, width=w / 72, height=h / 72, horizontal=FALSE, paper="special")
    inlmisc:::plot.inlcol(pal, label=FALSE)
    dev.off()

    if (is.null(s$nan)) {
      g2 <- ""
    } else {
      grDevices::postscript(f2, width=h / 72, height=h / 72, horizontal=FALSE, paper="special")
      graphics::par(mar=c(0, 0, 0, 0))
      graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=NULL,
                             xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                             xlab="", ylab="")
      graphics::rect(0, 0, 1, 1, col=s$nan, border=NA, lwd=0.5)
      graphics::box(lwd=0.5, col="#D3D3D3")
      dev.off()
    }

    n <- format(ifelse(is.finite(s$nmax), s$nmax, "--"))
    if (!is.null(s$gray)) n <- sprintf("%s (%s)", n, length(s$gray))

    c("Type"    = s$type,
      "Scheme"  = names(schemes)[i],
      "Palette" = g1,
      "Max n"   = n,
      "NaN"     = g2,
      "Source"  = s$cite)
  }))
  m[duplicated(m[, "Type"]), "Type"] <- ""

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
  inlmisc::PrintTable(m, align=c("l", "l", "c", "c", "c", "l"))
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

  unlink(list.files(pattern="^g[1-2]_[0-9]{3}\\.eps$"))
  unlink(sprintf("table.%s", c("tex", "pdf", "svg")))

  invisible()
}
