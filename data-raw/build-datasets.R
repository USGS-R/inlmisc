#!/usr/bin/env Rscript

.ReadCpt <- function(file, cite=NULL, type="Sequential") {

  checkmate::assertString(file)
  checkmate::assertString(cite, null.ok=TRUE)
  type <- match.arg(type, c("Sequential", "Diverging", "Qualitative"))

  line <- readLines(file)
  line <- line[-grep("^(#$|#-+)", line)]

  nm <- c("COLOR_MODEL", "RANGE", "HINGE", "CYCLIC")
  option <- lapply(nm, function(opt) {
    idx <- grep(sprintf("^#[ \t]%s", opt), line)
    if (length(idx) == 0) return(NULL)
    x <- tail(strsplit(line[idx], "[ \t]")[[1]], 1)
    line <<- line[-idx]
    if (opt == "COLOR_MODEL") x <- toupper(x)
    if (opt == "RANGE")       x <- as.numeric(strsplit(x, "/")[[1]])
    if (opt == "HINGE")       x <- as.numeric(x)
    if (opt == "CYCLIC")      x <- TRUE
    x
  })
  names(option) <- nm

  if (option$COLOR_MODEL != "RGB") return(NULL)

  color <- lapply(c("N"="N", "B"="B", "F"="F"), function(key) {
    idx <- grep(sprintf("^%s[ |\t]", key), line)
    if (length(idx) == 0) return(NULL)
    x <- strsplit(line[idx], "[ \t]")[[1]]
    x <- tail(x, 1)
    line <<- line[-idx]
    .Cpt2Hex(x)
  })

  idx <- grep("^#[ \t]", line)
  note <- strwrap(substring(line[idx], 3), width=.Machine$integer.max)
  line <- line[-idx]

  m <- do.call("rbind", lapply(line, function(x) {
    x <- strsplit(x, "[ \t]")[[1]]
    x <- x[x != ""]
    if (length(x) == 4) {
      elem <- c(x[1], .Cpt2Hex(x[2]), x[3], .Cpt2Hex(x[4]))
    } else if (length(x) == 8) {
      elem <- c(x[1], .Cpt2Hex(x[2:4]), x[5], .Cpt2Hex(x[6:8]))
    } else {
      return(NULL)
    }
    elem
  }))

  if (is.null(m)) return(NULL)

  for (i in seq_len(nrow(m) - 1))
    if (!identical(m[i, 4], m[i + 1, 2]))
      return(NULL)

  d <- as.data.frame(rbind(m[, 1:2], m[nrow(m), 3:4]), stringsAsFactors=FALSE)
  names(d) <- c("value", "color")
  d$value <- as.numeric(d$value)
  if (is.numeric(option$RANGE)) {
    if (is.numeric(option$HINGE)) {
      x <- c(-1, 0, 1)
      y <- c(option$RANGE[1], option$HINGE, option$RANGE[2])
      d$value <- stats::approx(x, y, xout=d$value)
    } else {
      d$value <- d$value * diff(option$RANGE) + option$RANGE[1]
    }
  }

  l <- list("data" = d,
            "type" = type,
            "cite" = cite,
            "nmax" = Inf,
            "nan"  = color$N,
            "back" = color$B,
            "fore" = color$F,
            "note" = note)
   l[vapply(l, is.null, FALSE)] <- NULL
   l
}


.Cpt2Hex <- function(x) {
  checkmate::assertVector(x, strict=TRUE, any.missing=FALSE, min.len=1, max.len=3)
  if (length(x) == 1) {
    if (grepl("/", x))
      x <- as.integer(strsplit(x, "/")[[1]])
    else
      x <- t(grDevices::col2rgb(x))[1, ]
  }
  grDevices::rgb(x[1], x[2], x[3], maxColorValue=255)
}


.GetCptGmt <- function() {

  # Generic Mapping Tools (GMT)
  cite <- "Wessel and others (2013)"

  # code adapted from stackoverflow answer by lukeA, accessed October 27, 2018
  # at https://stackoverflow.com/questions/25485216
  host  <- "api.github.com"
  owner <- "GenericMappingTools"
  repo  <- "gmt"
  fmt <- "https://%s/repos/%s/%s/git/trees/master?recursive=1"
  path <- sprintf(fmt, host, owner, repo)
  info <- httr::GET(sprintf(fmt, host, owner, repo))
  httr::stop_for_status(info)
  tree <- unlist(lapply(httr::content(info)$tree, "[", "path"), use.names=FALSE)
  path <- grep("share/cpt/", tree, value=TRUE, fixed=TRUE)
  host <- "raw.githubusercontent.com"
  file <- sprintf("https://%s/%s/%s/master/%s", host, owner, repo, path)

  destfile <- file.path(getwd(), "cpt", basename(file))
  for (i in seq_along(file)) {
    utils::download.file(file[i], destfile[i], quiet=TRUE)
  }

  nm <- tools::file_path_sans_ext(basename(file))
  type <- rep("Sequential", length(nm))
  div <- c("polar",
           "red2green",
           "roma",
           "split")
  type[nm %in% div] <- "Diverging"

  cpt <- lapply(seq_along(destfile), function(i) {
    .ReadCpt(destfile[i], cite=cite, type=type[i])
  })
  names(cpt) <- nm

  is <- !vapply(cpt, is.null, FALSE)
  cpt <- cpt[is]
  unlink(destfile[!is])

  cpt
}


.CheckScheme <- function(x) {
  checkmate::assertDataFrame(x$data, any.missing=FALSE,
                             min.rows=2, min.cols=1, null.ok=TRUE)
  pattern <- "^#(\\d|[a-f]){6}$"
  checkmate::assertCharacter(x$data$color, pattern=pattern,
                             ignore.case=TRUE, null.ok=TRUE)
  stopifnot(all(inlmisc::IsColor(x$data$color)))

  checkmate::qassert(x$data$name, c("0", "S", "X(0,)"))
  checkmate::qassert(x$gray, c("0", "S", "X(0,)"))
  checkmate::assertSubset(x$gray, x$data$name)

  checkmate::assertNumeric(x$data$value, finite=TRUE, unique=TRUE,
                           sorted=TRUE, null.ok=TRUE)

  checkmate::assertSubset(x$type, c("Qualitative", "Diverging", "Sequential"))
  checkmate::assertString(x$cite)
  checkmate::assertNumber(x$nmax)

  checkmate::assertCharacter(x$back, pattern=pattern, ignore.case=TRUE, null.ok=TRUE)
  checkmate::assertCharacter(x$fore, pattern=pattern, ignore.case=TRUE, null.ok=TRUE)
  checkmate::assertCharacter(x$nan,  pattern=pattern, ignore.case=TRUE, null.ok=TRUE)
  stopifnot(inlmisc::IsColor(x$back, null.ok=TRUE))
  stopifnot(inlmisc::IsColor(x$fore, null.ok=TRUE))
  stopifnot(inlmisc::IsColor(x$nan,  null.ok=TRUE))

  checkmate::assertCharacter(x$note, null.ok=TRUE)

  invisible()
}


options(stringsAsFactors=FALSE)
dir.create(file.path(getwd(), "cpt"), showWarnings=FALSE)

schemes <- .GetCptGmt()

# Thomas Dewez (SRON) granted permission to add his color schemes on October 17, 2018.

schemes[["DEM screen"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  value, color
                      0, #008435
                    100, #33CC00
                    200, #F4F071
                    400, #F4BD45
                    600, #99642B
                    800, #FFFFFF
                  "),
  type = "Sequential",
  cite = "Thomas Dewez (2004)",
  nmax = Inf,
  back = "#FFFFFF",
  fore = "#008435",
  nan  = "#008435"
)

schemes[["DEM print"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  value, color
                      0, #336600
                    100, #81C31F
                    200, #FFFFCC
                    400, #F4BD45
                    500, #66330C
                    600, #663300
                    800, #FFFFFF
                  "),
  type = "Sequential",
  cite = "Thomas Dewez (2004)",
  nmax = Inf,
  back = "#336600",
  fore = "#FFFFFF",
  nan  = "#336600"
)

schemes[["DEM poster"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  value, color
                      0, #006147
                     50, #107A2F
                    500, #E8D77D
                   1200, #A14300
                   1700, #9E0000
                   2800, #6E6E6E
                   4000, #FFFFFF
                   4900, #FFFFFF
                  "),
  type = "Sequential",
  cite = "Thomas Dewez (2004)",
  nmax = Inf,
  back = "#99CCFF",
  fore = "#99CCFF",
  nan  = "#99CCFF"
)

# Paul Tol (SRON) granted permission to add his color schemes on October 17, 2018.

schemes[["bright"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,   color
                  blue,   #4477AA
                  red,    #EE6677
                  green,  #228833
                  yellow, #CCBB44
                  cyan,   #66CCEE
                  purple, #AA3377
                  grey,   #BBBBBB
                  "),
  gray = c("yellow", "red", "green"),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 7
)

schemes[["high-contrast"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,   color
                  blue,   #004488
                  yellow, #DDAA33
                  red,    #BB5566
                  black,  #000000
                  white,  #FFFFFF
                  "),
  gray = c("white", "yellow", "red", "blue", "black"),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 5
)

schemes[["vibrant"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,    color
                  orange,  #EE7733
                  blue,    #0077BB
                  cyan,    #33BBEE
                  magenta, #EE3377
                  red,     #CC3311
                  teal,    #009988
                  grey,    #BBBBBB
                  "),
  gray = c("grey", "orange", "magenta", "blue"),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 7
)

schemes[["muted"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,   color
                  rose,   #CC6677
                  indigo, #332288
                  sand,   #DDCC77
                  green,  #117733
                  cyan,   #88CCEE
                  wine,   #882255
                  teal,   #44AA99
                  olive,  #999933
                  purple, #AA4499
                  "),
  gray = c("sand", "teal", "purple", "green", "indigo"),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 9,
  nas  = "#DDDDDD"
)

schemes[["pale"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,        color
                  pale blue,   #BBCCEE
                  pale cyan,   #CCEEFF
                  pale green,  #CCDDAA
                  pale yellow, #EEEEBB
                  pale red,    #FFCCCC
                  pale grey,   #DDDDDD
                  "),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 6
)

schemes[["dark"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,        color
                  dark blue,   #222255
                  dark cyan,   #225555
                  dark green,  #225522
                  dark yellow, #666633
                  dark red,    #663333
                  dark grey,   #555555
                  "),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 6
)

schemes[["light"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,         color
                  light blue,   #77AADD
                  orange,       #EE8866
                  light yellow, #EEDD88
                  pink,         #FFAABB
                  light cyan,   #99DDFF
                  mint,         #44BB99
                  pear,         #BBCC33
                  olive,        #AAAA00
                  pale grey,    #DDDDDD
                  "),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 9
)

schemes[["ground cover"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name,                        color
                  water,                       #5566AA
                  evergreen needleleaf forest, #117733
                  deciduous needleleaf forest, #44AA66
                  mixed forest,                #55AA22
                  evergreen broadleaf forest,  #668822
                  deciduous broadleaf forest,  #99BB55
                  woodland,                    #558877
                  wooded grassland,            #88BBAA
                  grassland,                   #AADDCC
                  cropland,                    #44AA88
                  closed shrubland,            #DDCC66
                  open shrubland,              #FFDD44
                  bare ground,                 #FFEE88
                  urban and built,             #BB0011
                  "),
  type = "Qualitative",
  cite = "Paul Tol (2018)",
  nmax = 14
)

schemes[["sunset"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  color
                  #364B9A
                  #4A7BB7
                  #6EA6CD
                  #98CAE1
                  #C2E4EF
                  #EAECCC
                  #FEDA8B
                  #FDB366
                  #F67E4B
                  #DD3D2D
                  #A50026
                  "),
  type = "Diverging",
  cite = "Paul Tol (2018)",
  nmax = Inf,
  nan  = "#FFFFFF"
)

schemes[["BuRd"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  color
                  #2166AC
                  #4393C3
                  #92C5DE
                  #D1E5F0
                  #F7F7F7
                  #FDDBC7
                  #F4A582
                  #D6604D
                  #B2182B
                  "),
  type = "Diverging",
  cite = "Paul Tol (2018)",
  nmax = Inf,
  nan  = "#FFEE99"
)

schemes[["PRGn"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  color
                  #762A83
                  #9970AB
                  #C2A5CF
                  #E7D4E8
                  #F7F7F7
                  #D9F0D3
                  #ACD39E
                  #5AAE61
                  #1B7837
                  "),
  type = "Diverging",
  cite = "Paul Tol (2018)",
  nmax = Inf,
  nan  = "#FFEE99"
)

schemes[["YlOrBr"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  color
                  #FFFFE5
                  #FFF7BC
                  #FEE391
                  #FEC44F
                  #FB9A29
                  #EC7014
                  #CC4C02
                  #993404
                  #662506
                  "),
  type = "Sequential",
  cite = "Paul Tol (2018)",
  nmax = Inf,
  nan  = "#888888"
)

schemes[["iridescent"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  color
                  #FEFBE9
                  #FCF7D5
                  #F5F3C1
                  #EAF0B5
                  #DDECBF
                  #D0E7CA
                  #C2E3D2
                  #B5DDD8
                  #A8D8DC
                  #9BD2E1
                  #8DCBE4
                  #81C4E7
                  #7BBCE7
                  #7EB2E4
                  #88A5DD
                  #9398D2
                  #9B8AC4
                  #9D7DB2
                  #9A709E
                  #906388
                  #805770
                  #684957
                  #46353A
                  "),
  type = "Sequential",
  cite = "Paul Tol (2018)",
  nmax = Inf,
  nan  = "#999999"
)

schemes[["smooth rainbow"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  color
                  #E8ECFB
                  #DDD8EF
                  #D1C1E1
                  #C3A8D1
                  #B58FC2
                  #A778B4
                  #9B62A7
                  #8C4E99
                  #6F4C9B
                  #6059A9
                  #5568B8
                  #4E79C5
                  #4D8AC6
                  #4E96BC
                  #549EB3
                  #59A5A9
                  #60AB9E
                  #69B190
                  #77B77D
                  #8CBC68
                  #A6BE54
                  #BEBC48
                  #D1B541
                  #DDAA3C
                  #E49C39
                  #E78C35
                  #E67932
                  #E4632D
                  #DF4828
                  #DA2222
                  #B8221E
                  #95211B
                  #721E17
                  #521A13
                  "),
  type = "Sequential",
  cite = "Paul Tol (2018)",
  nmax = Inf,
  nan  = "#666666"
)

schemes[["discrete rainbow"]] <- list(
  data = read.csv(strip.white=TRUE, text="
                  name, color
                     1, #E8ECFB
                     2, #D9CCE3
                     3, #D1BBD7
                     4, #CAACCB
                     5, #BA8DB4
                     6, #AE76A3
                     7, #AA6F9E
                     8, #994F88
                     9, #882E72
                    10, #1965B0
                    11, #437DBF
                    12, #5289C7
                    13, #6195CF
                    14, #7BAFDE
                    15, #4EB265
                    16, #90C987
                    17, #CAE0AB
                    18, #F7F056
                    19, #F7CB45
                    20, #F6C141
                    21, #F4A736
                    22, #F1932D
                    23, #EE8026
                    24, #E8601C
                    25, #E65518
                    26, #DC050C
                    27, #A5170E
                    28, #72190E
                    29, #42150A
                  "),
  type = "Sequential",
  cite = "Paul Tol (2018)",
  nmax = 23,
  nan  = "#777777"
)

discrete_rainbow_indexes <- list(
  c(10),
  c(10, 26),
  c(10, 18, 26),
  c(10, 15, 18, 26),
  c(10, 14, 15, 18, 26),
  c(10, 14, 15, 17, 18, 26),
  c( 9, 10, 14, 15, 17, 18, 26),
  c( 9, 10, 14, 15, 17, 18, 23, 26),
  c( 9, 10, 14, 15, 17, 18, 23, 26, 28),
  c( 9, 10, 14, 15, 17, 18, 21, 24, 26, 28),
  c( 9, 10, 12, 14, 15, 17, 18, 21, 24, 26, 28),
  c( 3,  6,  9, 10, 12, 14, 15, 17, 18, 21, 24, 26),
  c( 3,  6,  9, 10, 12, 14, 15, 16, 17, 18, 21, 24, 26),
  c( 3,  6,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26),
  c( 3,  6,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28),
  c( 3,  5,  7,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28),
  c( 3,  5,  7,  8,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 28),
  c( 3,  5,  7,  8,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28),
  c( 2,  4,  5,  7,  8,  9, 10, 12, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28),
  c( 2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 20, 22, 24, 26, 27, 28),
  c( 2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28),
  c( 2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29),
  c( 1,  2,  4,  5,  7,  8,  9, 10, 11, 13, 14, 15, 16, 17, 18, 19, 21, 23, 25, 26, 27, 28, 29)
)

# Unknown author on gnuplot-info

schemes[["bpy"]] <- list(
  type = "Sequential",
  cite = "unknown",
  nmax = Inf
)

# Anton Mikhailov (Google)

schemes[["turbo"]] <- list(
  type = "Sequential",
  cite = "Anton Mikhailov (2019)",
  nmax = Inf
)

# data from turbo colormap look-up table,
# copyright 2019 Google LLC, Apache-2.0 license,
# accessed August 21, 2019
# at https://gist.github.com/mikhailov-work/ee72ba4191942acecc03fe6da94fc73f
turbo_colormap_data <- rbind(c(0.18995, 0.07176, 0.23217),
                             c(0.19483, 0.08339, 0.26149),
                             c(0.19956, 0.09498, 0.29024),
                             c(0.20415, 0.10652, 0.31844),
                             c(0.20860, 0.11802, 0.34607),
                             c(0.21291, 0.12947, 0.37314),
                             c(0.21708, 0.14087, 0.39964),
                             c(0.22111, 0.15223, 0.42558),
                             c(0.22500, 0.16354, 0.45096),
                             c(0.22875, 0.17481, 0.47578),
                             c(0.23236, 0.18603, 0.50004),
                             c(0.23582, 0.19720, 0.52373),
                             c(0.23915, 0.20833, 0.54686),
                             c(0.24234, 0.21941, 0.56942),
                             c(0.24539, 0.23044, 0.59142),
                             c(0.24830, 0.24143, 0.61286),
                             c(0.25107, 0.25237, 0.63374),
                             c(0.25369, 0.26327, 0.65406),
                             c(0.25618, 0.27412, 0.67381),
                             c(0.25853, 0.28492, 0.69300),
                             c(0.26074, 0.29568, 0.71162),
                             c(0.26280, 0.30639, 0.72968),
                             c(0.26473, 0.31706, 0.74718),
                             c(0.26652, 0.32768, 0.76412),
                             c(0.26816, 0.33825, 0.78050),
                             c(0.26967, 0.34878, 0.79631),
                             c(0.27103, 0.35926, 0.81156),
                             c(0.27226, 0.36970, 0.82624),
                             c(0.27334, 0.38008, 0.84037),
                             c(0.27429, 0.39043, 0.85393),
                             c(0.27509, 0.40072, 0.86692),
                             c(0.27576, 0.41097, 0.87936),
                             c(0.27628, 0.42118, 0.89123),
                             c(0.27667, 0.43134, 0.90254),
                             c(0.27691, 0.44145, 0.91328),
                             c(0.27701, 0.45152, 0.92347),
                             c(0.27698, 0.46153, 0.93309),
                             c(0.27680, 0.47151, 0.94214),
                             c(0.27648, 0.48144, 0.95064),
                             c(0.27603, 0.49132, 0.95857),
                             c(0.27543, 0.50115, 0.96594),
                             c(0.27469, 0.51094, 0.97275),
                             c(0.27381, 0.52069, 0.97899),
                             c(0.27273, 0.53040, 0.98461),
                             c(0.27106, 0.54015, 0.98930),
                             c(0.26878, 0.54995, 0.99303),
                             c(0.26592, 0.55979, 0.99583),
                             c(0.26252, 0.56967, 0.99773),
                             c(0.25862, 0.57958, 0.99876),
                             c(0.25425, 0.58950, 0.99896),
                             c(0.24946, 0.59943, 0.99835),
                             c(0.24427, 0.60937, 0.99697),
                             c(0.23874, 0.61931, 0.99485),
                             c(0.23288, 0.62923, 0.99202),
                             c(0.22676, 0.63913, 0.98851),
                             c(0.22039, 0.64901, 0.98436),
                             c(0.21382, 0.65886, 0.97959),
                             c(0.20708, 0.66866, 0.97423),
                             c(0.20021, 0.67842, 0.96833),
                             c(0.19326, 0.68812, 0.96190),
                             c(0.18625, 0.69775, 0.95498),
                             c(0.17923, 0.70732, 0.94761),
                             c(0.17223, 0.71680, 0.93981),
                             c(0.16529, 0.72620, 0.93161),
                             c(0.15844, 0.73551, 0.92305),
                             c(0.15173, 0.74472, 0.91416),
                             c(0.14519, 0.75381, 0.90496),
                             c(0.13886, 0.76279, 0.89550),
                             c(0.13278, 0.77165, 0.88580),
                             c(0.12698, 0.78037, 0.87590),
                             c(0.12151, 0.78896, 0.86581),
                             c(0.11639, 0.79740, 0.85559),
                             c(0.11167, 0.80569, 0.84525),
                             c(0.10738, 0.81381, 0.83484),
                             c(0.10357, 0.82177, 0.82437),
                             c(0.10026, 0.82955, 0.81389),
                             c(0.09750, 0.83714, 0.80342),
                             c(0.09532, 0.84455, 0.79299),
                             c(0.09377, 0.85175, 0.78264),
                             c(0.09287, 0.85875, 0.77240),
                             c(0.09267, 0.86554, 0.76230),
                             c(0.09320, 0.87211, 0.75237),
                             c(0.09451, 0.87844, 0.74265),
                             c(0.09662, 0.88454, 0.73316),
                             c(0.09958, 0.89040, 0.72393),
                             c(0.10342, 0.89600, 0.71500),
                             c(0.10815, 0.90142, 0.70599),
                             c(0.11374, 0.90673, 0.69651),
                             c(0.12014, 0.91193, 0.68660),
                             c(0.12733, 0.91701, 0.67627),
                             c(0.13526, 0.92197, 0.66556),
                             c(0.14391, 0.92680, 0.65448),
                             c(0.15323, 0.93151, 0.64308),
                             c(0.16319, 0.93609, 0.63137),
                             c(0.17377, 0.94053, 0.61938),
                             c(0.18491, 0.94484, 0.60713),
                             c(0.19659, 0.94901, 0.59466),
                             c(0.20877, 0.95304, 0.58199),
                             c(0.22142, 0.95692, 0.56914),
                             c(0.23449, 0.96065, 0.55614),
                             c(0.24797, 0.96423, 0.54303),
                             c(0.26180, 0.96765, 0.52981),
                             c(0.27597, 0.97092, 0.51653),
                             c(0.29042, 0.97403, 0.50321),
                             c(0.30513, 0.97697, 0.48987),
                             c(0.32006, 0.97974, 0.47654),
                             c(0.33517, 0.98234, 0.46325),
                             c(0.35043, 0.98477, 0.45002),
                             c(0.36581, 0.98702, 0.43688),
                             c(0.38127, 0.98909, 0.42386),
                             c(0.39678, 0.99098, 0.41098),
                             c(0.41229, 0.99268, 0.39826),
                             c(0.42778, 0.99419, 0.38575),
                             c(0.44321, 0.99551, 0.37345),
                             c(0.45854, 0.99663, 0.36140),
                             c(0.47375, 0.99755, 0.34963),
                             c(0.48879, 0.99828, 0.33816),
                             c(0.50362, 0.99879, 0.32701),
                             c(0.51822, 0.99910, 0.31622),
                             c(0.53255, 0.99919, 0.30581),
                             c(0.54658, 0.99907, 0.29581),
                             c(0.56026, 0.99873, 0.28623),
                             c(0.57357, 0.99817, 0.27712),
                             c(0.58646, 0.99739, 0.26849),
                             c(0.59891, 0.99638, 0.26038),
                             c(0.61088, 0.99514, 0.25280),
                             c(0.62233, 0.99366, 0.24579),
                             c(0.63323, 0.99195, 0.23937),
                             c(0.64362, 0.98999, 0.23356),
                             c(0.65394, 0.98775, 0.22835),
                             c(0.66428, 0.98524, 0.22370),
                             c(0.67462, 0.98246, 0.21960),
                             c(0.68494, 0.97941, 0.21602),
                             c(0.69525, 0.97610, 0.21294),
                             c(0.70553, 0.97255, 0.21032),
                             c(0.71577, 0.96875, 0.20815),
                             c(0.72596, 0.96470, 0.20640),
                             c(0.73610, 0.96043, 0.20504),
                             c(0.74617, 0.95593, 0.20406),
                             c(0.75617, 0.95121, 0.20343),
                             c(0.76608, 0.94627, 0.20311),
                             c(0.77591, 0.94113, 0.20310),
                             c(0.78563, 0.93579, 0.20336),
                             c(0.79524, 0.93025, 0.20386),
                             c(0.80473, 0.92452, 0.20459),
                             c(0.81410, 0.91861, 0.20552),
                             c(0.82333, 0.91253, 0.20663),
                             c(0.83241, 0.90627, 0.20788),
                             c(0.84133, 0.89986, 0.20926),
                             c(0.85010, 0.89328, 0.21074),
                             c(0.85868, 0.88655, 0.21230),
                             c(0.86709, 0.87968, 0.21391),
                             c(0.87530, 0.87267, 0.21555),
                             c(0.88331, 0.86553, 0.21719),
                             c(0.89112, 0.85826, 0.21880),
                             c(0.89870, 0.85087, 0.22038),
                             c(0.90605, 0.84337, 0.22188),
                             c(0.91317, 0.83576, 0.22328),
                             c(0.92004, 0.82806, 0.22456),
                             c(0.92666, 0.82025, 0.22570),
                             c(0.93301, 0.81236, 0.22667),
                             c(0.93909, 0.80439, 0.22744),
                             c(0.94489, 0.79634, 0.22800),
                             c(0.95039, 0.78823, 0.22831),
                             c(0.95560, 0.78005, 0.22836),
                             c(0.96049, 0.77181, 0.22811),
                             c(0.96507, 0.76352, 0.22754),
                             c(0.96931, 0.75519, 0.22663),
                             c(0.97323, 0.74682, 0.22536),
                             c(0.97679, 0.73842, 0.22369),
                             c(0.98000, 0.73000, 0.22161),
                             c(0.98289, 0.72140, 0.21918),
                             c(0.98549, 0.71250, 0.21650),
                             c(0.98781, 0.70330, 0.21358),
                             c(0.98986, 0.69382, 0.21043),
                             c(0.99163, 0.68408, 0.20706),
                             c(0.99314, 0.67408, 0.20348),
                             c(0.99438, 0.66386, 0.19971),
                             c(0.99535, 0.65341, 0.19577),
                             c(0.99607, 0.64277, 0.19165),
                             c(0.99654, 0.63193, 0.18738),
                             c(0.99675, 0.62093, 0.18297),
                             c(0.99672, 0.60977, 0.17842),
                             c(0.99644, 0.59846, 0.17376),
                             c(0.99593, 0.58703, 0.16899),
                             c(0.99517, 0.57549, 0.16412),
                             c(0.99419, 0.56386, 0.15918),
                             c(0.99297, 0.55214, 0.15417),
                             c(0.99153, 0.54036, 0.14910),
                             c(0.98987, 0.52854, 0.14398),
                             c(0.98799, 0.51667, 0.13883),
                             c(0.98590, 0.50479, 0.13367),
                             c(0.98360, 0.49291, 0.12849),
                             c(0.98108, 0.48104, 0.12332),
                             c(0.97837, 0.46920, 0.11817),
                             c(0.97545, 0.45740, 0.11305),
                             c(0.97234, 0.44565, 0.10797),
                             c(0.96904, 0.43399, 0.10294),
                             c(0.96555, 0.42241, 0.09798),
                             c(0.96187, 0.41093, 0.09310),
                             c(0.95801, 0.39958, 0.08831),
                             c(0.95398, 0.38836, 0.08362),
                             c(0.94977, 0.37729, 0.07905),
                             c(0.94538, 0.36638, 0.07461),
                             c(0.94084, 0.35566, 0.07031),
                             c(0.93612, 0.34513, 0.06616),
                             c(0.93125, 0.33482, 0.06218),
                             c(0.92623, 0.32473, 0.05837),
                             c(0.92105, 0.31489, 0.05475),
                             c(0.91572, 0.30530, 0.05134),
                             c(0.91024, 0.29599, 0.04814),
                             c(0.90463, 0.28696, 0.04516),
                             c(0.89888, 0.27824, 0.04243),
                             c(0.89298, 0.26981, 0.03993),
                             c(0.88691, 0.26152, 0.03753),
                             c(0.88066, 0.25334, 0.03521),
                             c(0.87422, 0.24526, 0.03297),
                             c(0.86760, 0.23730, 0.03082),
                             c(0.86079, 0.22945, 0.02875),
                             c(0.85380, 0.22170, 0.02677),
                             c(0.84662, 0.21407, 0.02487),
                             c(0.83926, 0.20654, 0.02305),
                             c(0.83172, 0.19912, 0.02131),
                             c(0.82399, 0.19182, 0.01966),
                             c(0.81608, 0.18462, 0.01809),
                             c(0.80799, 0.17753, 0.01660),
                             c(0.79971, 0.17055, 0.01520),
                             c(0.79125, 0.16368, 0.01387),
                             c(0.78260, 0.15693, 0.01264),
                             c(0.77377, 0.15028, 0.01148),
                             c(0.76476, 0.14374, 0.01041),
                             c(0.75556, 0.13731, 0.00942),
                             c(0.74617, 0.13098, 0.00851),
                             c(0.73661, 0.12477, 0.00769),
                             c(0.72686, 0.11867, 0.00695),
                             c(0.71692, 0.11268, 0.00629),
                             c(0.70680, 0.10680, 0.00571),
                             c(0.69650, 0.10102, 0.00522),
                             c(0.68602, 0.09536, 0.00481),
                             c(0.67535, 0.08980, 0.00449),
                             c(0.66449, 0.08436, 0.00424),
                             c(0.65345, 0.07902, 0.00408),
                             c(0.64223, 0.07380, 0.00401),
                             c(0.63082, 0.06868, 0.00401),
                             c(0.61923, 0.06367, 0.00410),
                             c(0.60746, 0.05878, 0.00427),
                             c(0.59550, 0.05399, 0.00453),
                             c(0.58336, 0.04931, 0.00486),
                             c(0.57103, 0.04474, 0.00529),
                             c(0.55852, 0.04028, 0.00579),
                             c(0.54583, 0.03593, 0.00638),
                             c(0.53295, 0.03169, 0.00705),
                             c(0.51989, 0.02756, 0.00780),
                             c(0.50664, 0.02354, 0.00863),
                             c(0.49321, 0.01963, 0.00955),
                             c(0.47960, 0.01583, 0.01055))
colnames(turbo_colormap_data) <- c("red", "green", "blue")

schemes <- schemes[order(vapply(schemes, function(x) x$type, ""), names(schemes))]
invisible(lapply(schemes, .CheckScheme))
save(schemes, discrete_rainbow_indexes, turbo_colormap_data, file="sysdata.rda")
tools::resaveRdaFiles(getwd(), compress="auto")
