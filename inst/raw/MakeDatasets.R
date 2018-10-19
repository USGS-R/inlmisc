MakeDatasets <- function() {

  schemes <- list()

  schemes[["bright"]] <- list(
    color = c("blue"   = "#4477AA",
              "red"    = "#EE6677",
              "green"  = "#228833",
              "yellow" = "#CCBB44",
              "cyan"   = "#66CCEE",
              "purple" = "#AA3377",
              "grey"   = "#BBBBBB"),
    gray  = c("yellow", "red", "green"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~5",
    nmax  = 7
  )

  schemes[["high-contrast"]] <- list(
    color = c("blue"   = "#004488",
              "yellow" = "#DDAA33",
              "red"    = "#BB5566",
              "black"  = "#000000",
              "white"  = "#FFFFFF"),
    gray  = c("white", "yellow", "red", "blue", "black"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~5",
    nmax  = 5
  )

  schemes[["vibrant"]] <- list(
    color = c("orange"  = "#EE7733",
              "blue"    = "#0077BB",
              "cyan"    = "#33BBEE",
              "magenta" = "#EE3377",
              "red"     = "#CC3311",
              "teal"    = "#009988",
              "grey"    = "#BBBBBB"),
    gray  = c("grey", "orange", "magenta", "blue"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~5",
    nmax  = 7
  )

  schemes[["muted"]] <- list(
    color = c("rose"   = "#CC6677",
              "indigo" = "#332288",
              "sand"   = "#DDCC77",
              "green"  = "#117733",
              "cyan"   = "#88CCEE",
              "wine"   = "#882255",
              "teal"   = "#44AA99",
              "olive"  = "#999933",
              "purple" = "#AA4499"),
    gray  = c("sand", "teal", "purple", "green", "indigo"),
    bad   = c("pale grey" = "#DDDDDD"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~5",
    nmax  = 9
  )

  schemes[["pale"]] <- list(
    color = c("pale blue"   = "#BBCCEE",
              "pale cyan"   = "#CCEEFF",
              "pale green"  = "#CCDDAA",
              "pale yellow" = "#EEEEBB",
              "pale red"    = "#FFCCCC",
              "pale grey"   = "#DDDDDD"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~5",
    nmax  = 6
  )

  schemes[["dark"]] <- list(
    color = c("dark blue"   = "#222255",
              "dark cyan"   = "#225555",
              "dark green"  = "#225522",
              "dark yellow" = "#666633",
              "dark red"    = "#663333",
              "dark grey"   = "#555555"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~5",
    nmax  = 6
  )

  schemes[["light"]] <- list(
    color = c("light blue"   = "#77AADD",
              "orange"       = "#EE8866",
              "light yellow" = "#EEDD88",
              "pink"         = "#FFAABB",
              "light cyan"   = "#99DDFF",
              "mint"         = "#44BB99",
              "pear"         = "#BBCC33",
              "olive"        = "#AAAA00",
              "pale grey"    = "#DDDDDD"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~6",
    nmax  = 9
  )

  schemes[["ground cover"]] <- list(
    color = c("water"                       = "#5566AA",
              "evergreen needleleaf forest" = "#117733",
              "deciduous needleleaf forest" = "#44AA66",
              "mixed forest"                = "#55AA22",
              "evergreen broadleaf forest"  = "#668822",
              "deciduous broadleaf forest"  = "#99BB55",
              "woodland"                    = "#558877",
              "wooded grassland"            = "#88BBAA",
              "grassland"                   = "#AADDCC",
              "cropland"                    = "#44AA88",
              "closed shrubland"            = "#DDCC66",
              "open shrubland"              = "#FFDD44",
              "bare ground"                 = "#FFEE88",
              "urban and built"             = "#BB0011"),
    type  = "Qualitative",
    cite  = "Tol, 2018, p.~19",
    nmax  = 14
  )

  schemes[["sunset"]] <- list(
    color = c("#364B9A",
              "#4A7BB7",
              "#6EA6CD",
              "#98CAE1",
              "#C2E4EF",
              "#EAECCC",
              "#FEDA8B",
              "#FDB366",
              "#F67E4B",
              "#DD3D2D",
              "#A50026"),
    bad   = "#FFFFFF",
    type  = "Diverging",
    cite  = "Tol, 2018, p.~9",
    nmax  = Inf
  )

  schemes[["BuRd"]] <- list(
    color = c("#2166AC",
              "#4393C3",
              "#92C5DE",
              "#D1E5F0",
              "#F7F7F7",
              "#FDDBC7",
              "#F4A582",
              "#D6604D",
              "#B2182B"),
    bad   = "#FFEE99",
    type  = "Diverging",
    cite  = "Tol, 2018, p.~9",
    nmax  = Inf
  )

  schemes[["PRGn"]] <- list(
    color = c("#762A83",
              "#9970AB",
              "#C2A5CF",
              "#E7D4E8",
              "#F7F7F7",
              "#D9F0D3",
              "#ACD39E",
              "#5AAE61",
              "#1B7837"),
    bad   = "#FFEE99",
    type  = "Diverging",
    cite  = "Tol, 2018, p.~9",
    nmax  = Inf
  )

  schemes[["YlOrBr"]] <- list(
    color = c("#FFFFE5",
              "#FFF7BC",
              "#FEE391",
              "#FEC44F",
              "#FB9A29",
              "#EC7014",
              "#CC4C02",
              "#993404",
              "#662506"),
    bad   = "#888888",
    type  = "Sequential",
    cite  = "Tol, 2018, p.~11",
    nmax  = Inf
  )

  schemes[["iridescent"]] <- list(
    color = c("#FEFBE9",
              "#FCF7D5",
              "#F5F3C1",
              "#EAF0B5",
              "#DDECBF",
              "#D0E7CA",
              "#C2E3D2",
              "#B5DDD8",
              "#A8D8DC",
              "#9BD2E1",
              "#8DCBE4",
              "#81C4E7",
              "#7BBCE7",
              "#7EB2E4",
              "#88A5DD",
              "#9398D2",
              "#9B8AC4",
              "#9D7DB2",
              "#9A709E",
              "#906388",
              "#805770",
              "#684957",
              "#46353A"),
    bad   = "#999999",
    type  = "Sequential",
    cite  = "Tol, 2018, p.~11",
    nmax  = Inf
  )

  schemes[["discrete rainbow"]] <- list(
    color = c("1"  = "#E8ECFB",
              "2"  = "#D9CCE3",
              "3"  = "#D1BBD7",
              "4"  = "#CAACCB",
              "5"  = "#BA8DB4",
              "6"  = "#AE76A3",
              "7"  = "#AA6F9E",
              "8"  = "#994F88",
              "9"  = "#882E72",
              "10" = "#1965B0",
              "11" = "#437DBF",
              "12" = "#5289C7",
              "13" = "#6195CF",
              "14" = "#7BAFDE",
              "15" = "#4EB265",
              "16" = "#90C987",
              "17" = "#CAE0AB",
              "18" = "#F7F056",
              "19" = "#F7CB45",
              "20" = "#F6C141",
              "21" = "#F4A736",
              "22" = "#F1932D",
              "23" = "#EE8026",
              "24" = "#E8601C",
              "25" = "#E65518",
              "26" = "#DC050C",
              "27" = "#A5170E",
              "28" = "#72190E",
              "29" = "#42150A"),
    bad   = "#777777",
    type  = "Sequential",
    cite  = "Tol, 2018, p.~12--13",
    nmax  = 23
  )

  schemes[["smooth rainbow"]] <- list(
    color = c("#E8ECFB",
              "#DDD8EF",
              "#D1C1E1",
              "#C3A8D1",
              "#B58FC2",
              "#A778B4",
              "#9B62A7",
              "#8C4E99",
              "#6F4C9B",
              "#6059A9",
              "#5568B8",
              "#4E79C5",
              "#4D8AC6",
              "#4E96BC",
              "#549EB3",
              "#59A5A9",
              "#60AB9E",
              "#69B190",
              "#77B77D",
              "#8CBC68",
              "#A6BE54",
              "#BEBC48",
              "#D1B541",
              "#DDAA3C",
              "#E49C39",
              "#E78C35",
              "#E67932",
              "#E4632D",
              "#DF4828",
              "#DA2222",
              "#B8221E",
              "#95211B",
              "#721E17",
              "#521A13"),
    bad   = "#666666",
    type  = "Sequential",
    cite  = "Tol, 2018, p.~12",
    nmax  = Inf
  )

  schemes[["DEM print"]] <- list(
    color = c("#336600",
              "#81C31F",
              "#FFFFCC",
              "#F4BD45",
              "#66330C",
              "#663300",
              "#FFFFFF"),
    type  = "Sequential",
    cite  = "Dewez, 2004",
    nmax  = Inf
  )

  schemes[["DEM screen"]] <- list(
    color = c("#008435",
              "#33CC00",
              "#F4F071",
              "#F4BD45",
              "#99642B",
              "#FFFFFF"),
    type  = "Sequential",
    cite  = "Dewez, 2004",
    nmax  = Inf
  )

  schemes[["DEM poster"]] <- list(
    color = c("#006147",
              "#107A2F",
              "#E8D77D",
              "#A14300",
              "#9E0000",
              "#6E6E6E",
              "#FFFFFF",
              "#FFFFFF"),
    type  = "Sequential",
    cite  = "Dewez, 2004",
    nmax  = Inf
  )

  schemes <- schemes[order(vapply(schemes, function(x) x$type, ""), names(schemes))]

  invisible(lapply(schemes, function(x) {
    pattern <- "^#(\\d|[a-f]){6}$"
    checkmate::assertCharacter(x$color, pattern=pattern, ignore.case=TRUE,
                               any.missing=FALSE, min.len=1)
    stopifnot(all(inlmisc:::.IsColor(x$color)))
    checkmate::assertCharacter(x$gray, any.missing=FALSE, min.len=1,
                               unique=TRUE, null.ok=TRUE)
    checkmate::assertCharacter(x$bad, pattern=pattern, ignore.case=TRUE, null.ok=TRUE)
    stopifnot(all(inlmisc:::.IsColor(x$bad, null.ok=TRUE)))
    checkmate::assertSubset(x$type, c("Qualitative", "Diverging", "Sequential"))
    checkmate::assertString(x$cite)
    checkmate::assertNumber(x$nmax)
  }))

  m <- do.call("rbind", lapply(seq_along(schemes), function(i) {
    scheme <- schemes[[i]]

    n <- ifelse(is.finite(scheme$nmax), scheme$nmax, 255)
    pal <- inlmisc::GetColors(n, scheme=names(schemes)[i])

    f1 <- sprintf("%s_%03d.eps", "g1", i)
    f2 <- sprintf("%s_%03d.eps", "g2", i)
    w <- 100; h <- 10
    g1 <- sprintf("\\adjustimage{width=%spx, height=%spx, valign=m}{%s}", w, h, f1)
    g2 <- sprintf("\\adjustimage{width=%spx, height=%spx, valign=m}{%s}", h, h, f2)

    grDevices::postscript(f1, width=w / 72, height=h / 72, horizontal=FALSE,, paper="special")
    inlmisc:::plot.inlcol(pal, label=FALSE)
    dev.off()

    if (is.null(scheme$bad)) {
      g2 <- ""
    } else {
      grDevices::postscript(f2, width=h / 72, height=h / 72, horizontal=FALSE, paper="special")
      graphics::par(mar=c(0, 0, 0, 0))
      graphics::plot.default(NA, type="n", xlim=c(0, 1), ylim=c(0, 1), main=NULL,
                             xaxs="i", yaxs="i", bty="n", xaxt="n", yaxt="n",
                             xlab="", ylab="")
      graphics::rect(0, 0, 1, 1, col=scheme$bad, border=NA, lwd=0.5)
      graphics::box(lwd=0.5, col="#D3D3D3")
      dev.off()
    }

    n <- format(ifelse(is.finite(scheme$nmax), scheme$nmax, "--"))
    if (!is.null(scheme$gray)) n <- sprintf("%s (%s)", n, length(scheme$gray))

    c("Type"    = scheme$type,
      "Scheme"  = names(schemes)[i],
      "Palette" = g1,
      "Max n"   = n,
      "Bad"     = g2,
      "Source"  = scheme$cite)
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

  unlink(list.files(pattern="^g[1-2]_[0-9]{3}\\.eps$"))
  unlink(sprintf("table.%s", c("tex", "pdf", "svg")))

  save(schemes, file="../../R/sysdata.rda")

  invisible()
}
