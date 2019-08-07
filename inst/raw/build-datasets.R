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
  div <- c("berlin",
           "broc",
           "cork",
           "lisbon",
           "oleron",
           "polar",
           "red2green",
           "roma",
           "split",
           "tofino",
           "vik")
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

# unknown author on gnuplot-info

schemes[["bpy"]] <- list(
  type = "Sequential",
  cite = "unknown",
  nmax = Inf
)

schemes <- schemes[order(vapply(schemes, function(x) x$type, ""), names(schemes))]
invisible(lapply(schemes, .CheckScheme))
save(schemes, file="sysdata.rda")

tools::resaveRdaFiles(getwd(), compress="auto")
