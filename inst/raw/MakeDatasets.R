MakeDatasets <- function() {

  options(stringsAsFactors=FALSE)

  schemes <- .GetGMTCpt(c("abyss",
                          "bathy",
                          "copper",
                          "cubhelix",
                          "dem1",
                          "dem2",
                          "dem3",
                          "dem4",
                          "drywet",
                          "elevation",
                          "gebco",
                          "globe",
                          "gray",
                          "haxby",
                          "hot",
                          "ibcso",
                          "jet",
                          "ocean",
                          "relief",
                          "seafloor"))

  schemes[["bright"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #4477AA, blue
                    #EE6677, red
                    #228833, green
                    #CCBB44, yellow
                    #66CCEE, cyan
                    #AA3377, purple
                    #BBBBBB, grey
                    "),
    gray = c("yellow", "red", "green"),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 7
  )

  schemes[["high-contrast"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #004488, blue
                    #DDAA33, yellow
                    #BB5566, red
                    #000000, black
                    #FFFFFF, white
                    "),
    gray = c("white", "yellow", "red", "blue", "black"),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 5
  )

  schemes[["vibrant"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #EE7733, orange
                    #0077BB, blue
                    #33BBEE, cyan
                    #EE3377, magenta
                    #CC3311, red
                    #009988, teal
                    #BBBBBB, grey
                    "),
    gray = c("grey", "orange", "magenta", "blue"),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 7
  )

  schemes[["muted"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #CC6677, rose
                    #332288, indigo
                    #DDCC77, sand
                    #117733, green
                    #88CCEE, cyan
                    #882255, wine
                    #44AA99, teal
                    #999933, olive
                    #AA4499, purple
                    "),
    gray = c("sand", "teal", "purple", "green", "indigo"),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 9,
    nas  = "#DDDDDD"
  )

  schemes[["pale"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #BBCCEE, pale blue
                    #CCEEFF, pale cyan
                    #CCDDAA, pale green
                    #EEEEBB, pale yellow
                    #FFCCCC, pale red
                    #DDDDDD, pale grey
                    "),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 6
  )

  schemes[["dark"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #222255, dark blue
                    #225555, dark cyan
                    #225522, dark green
                    #666633, dark yellow
                    #663333, dark red
                    #555555, dark grey
                    "),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 6
  )



  schemes[["light"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #77AADD, light blue
                    #EE8866, orange
                    #EEDD88, light yellow
                    #FFAABB, pink
                    #99DDFF, light cyan
                    #44BB99, mint
                    #BBCC33, pear
                    #AAAA00, olive
                    #DDDDDD, pale grey
                    "),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
    nmax = 9
  )

  schemes[["ground cover"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #5566AA, water
                    #117733, evergreen needleleaf forest
                    #44AA66, deciduous needleleaf forest
                    #55AA22, mixed forest
                    #668822, evergreen broadleaf forest
                    #99BB55, deciduous broadleaf forest
                    #558877, woodland
                    #88BBAA, wooded grassland
                    #AADDCC, grassland
                    #44AA88, cropland
                    #DDCC66, closed shrubland
                    #FFDD44, open shrubland
                    #FFEE88, bare ground
                    #BB0011, urban and built
                    "),
    type = "Qualitative",
    cite = "Paul Tol, 2018",
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
    cite = "Paul Tol, 2018",
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
    cite = "Paul Tol, 2018",
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
    cite = "Paul Tol, 2018",
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
    cite = "Paul Tol, 2018",
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
    cite = "Paul Tol, 2018",
    nmax = Inf,
    nan  = "#999999"
  )

  schemes[["discrete rainbow"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #E8ECFB, 1
                    #D9CCE3, 2
                    #D1BBD7, 3
                    #CAACCB, 4
                    #BA8DB4, 5
                    #AE76A3, 6
                    #AA6F9E, 7
                    #994F88, 8
                    #882E72, 9
                    #1965B0, 10
                    #437DBF, 11
                    #5289C7, 12
                    #6195CF, 13
                    #7BAFDE, 14
                    #4EB265, 15
                    #90C987, 16
                    #CAE0AB, 17
                    #F7F056, 18
                    #F7CB45, 19
                    #F6C141, 20
                    #F4A736, 21
                    #F1932D, 22
                    #EE8026, 23
                    #E8601C, 24
                    #E65518, 25
                    #DC050C, 26
                    #A5170E, 27
                    #72190E, 28
                    #42150A, 29
                    "),
    type = "Sequential",
    cite = "Paul Tol, 2018",
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
    cite = "Paul Tol, 2018",
    nmax = Inf,
    nan  = "#666666"
  )

  schemes[["DEM print"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #336600, 0
                    #81C31F, 100
                    #FFFFCC, 200
                    #F4BD45, 400
                    #66330C, 500
                    #663300, 600
                    #FFFFFF, 800
                    "),
    type = "Sequential",
    cite = "Thomas Dewez, 2004",
    nmax = Inf,
    back = "#336600",
    fore = "#FFFFFF",
    nan  = "#336600"
  )

  schemes[["DEM screen"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #008435, 0
                    #33CC00, 100
                    #F4F071, 200
                    #F4BD45, 400
                    #99642B, 600
                    #FFFFFF, 800
                    "),
    type = "Sequential",
    cite = "Thomas Dewez, 2004",
    nmax = Inf,
    back = "#FFFFFF",
    fore = "#008435",
    nan  = "#008435"
  )

  schemes[["DEM poster"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #006147, 0
                    #107A2F, 50
                    #E8D77D, 500
                    #A14300, 1200
                    #9E0000, 1700
                    #6E6E6E, 2800
                    #FFFFFF, 4000
                    #FFFFFF, 4900
                    "),
    type = "Sequential",
    cite = "Thomas Dewez, 2004",
    nmax = Inf,
    back = "#99CCFF",
    fore = "#99CCFF",
    nan  = "#99CCFF"
  )

  schemes <- schemes[order(vapply(schemes, function(x) x$type, ""), names(schemes))]
  invisible(lapply(schemes, .CheckScheme))
  if (dir.exists("../../R")) save(schemes, file="../../R/sysdata.rda")

  invisible(schemes)
}


.ReadCpt <- function(file, cite=NULL, type=c("Sequential", "Diverging", "Qualitative")) {
  checkmate::assertString(file)
  checkmate::assertString(cite, null.ok=TRUE)
  type <- match.arg(type)

  line <- readLines(file)
  line <- line[-grep("^(#$|#-+)", line)]

  nm <- c("N", "B", "F")
  color <- lapply(nm, function(key) {
    idx <- grep(sprintf("^%s[ |\t]", key), line)
    if (length(idx) == 0) return(NULL)
    x <- strsplit(line[idx], "[ \t]")[[1]]
    x <- tail(x, 1)
    line <<- line[-idx]
    .Col2Hex(x)
  })
  names(color) <- nm

  nm <- c("COLOR_MODEL", "RANGE", "HINGE", "CYCLIC")
  option <- lapply(nm, function(opt) {
    idx <- grep(sprintf("^#[ \t]%s", opt), line)
    if (length(idx) == 0) return(NULL)
    x <- tail(strsplit(line[idx], "[ \t]")[[1]], 1)
    line <<- line[-idx]
    if (opt == "RANGE") x <- as.numeric(strsplit(x, "/")[[1]])
    x
  })
  names(option) <- nm

  idx <- grep("^#[ \t]", line)
  note <- strwrap(substring(line[idx], 3), width=.Machine$integer.max)
  line <- line[-idx]

  m <- do.call("rbind", lapply(line, function(x) {
    elem <- strsplit(x, "\t")[[1]]
    elem <- elem[elem != ""]
    elem[2] <- .Col2Hex(elem[2])
    elem[4] <- .Col2Hex(elem[4])
    elem
  }))
  d <- as.data.frame(rbind(m[, 1:2], m[nrow(m), 3:4]), stringsAsFactors=FALSE)
  names(d) <- c("value", "color")
  d$value <- as.numeric(d$value)
  if (is.numeric(option$RANGE))
    d$value <- seq(option$RANGE[1], option$RANGE[2], length.out=nrow(d))

  l <- list(data = d,
            type = type,
            cite = cite,
            nmax = Inf,
            nan  = color$N,
            back = color$B,
            fore = color$F,
            note = note)
   l[vapply(l, is.null, FALSE)] <- NULL
   l
}


.Col2Hex <- function(x) {
  checkmate::assertString(x, na.ok=FALSE)
  if (grepl("^[0-9]{1,3}/[0-9]{1,3}/[0-9]{1,3}$", x))
    val <- as.integer(strsplit(x, "/")[[1]])
  else
    val <- t(grDevices::col2rgb(x))[1, ]
  grDevices::rgb(val[1], val[2], val[3], maxColorValue=255)
}


.GetGMTCpt <- function(x, ...) {
  checkmate::assertCharacter(x, any.missing=FALSE, min.len=1, unique=TRUE)

  host <- "raw.githubusercontent.com"
  path <- "GenericMappingTools/gmt/master/share/cpt"
  file <- sprintf("https://%s/%s/%s.cpt", host, path, x)

  is <- !vapply(file, RCurl::url.exists, FALSE)
  if (any(is)) stop("URL responds with error:\n", paste(file[is], collapse="\n"))

  destdir <- file.path(getwd(), "cpt")
  dir.create(destdir, showWarnings=FALSE)

  destfile <- file.path(destdir, basename(file))
  for (i in seq_along(file)) utils::download.file(file[i], destfile[i], quiet=TRUE)

  cpt <- lapply(destfile, .ReadCpt, cite="Wessel and others, 2013", ...)
  names(cpt) <- paste("GMT", x)
  cpt
}


.CheckScheme <- function(x) {
  checkmate::assertDataFrame(x$data, any.missing=FALSE, min.rows=2, min.cols=1)

  pattern <- "^#(\\d|[a-f]){6}$"
  checkmate::assertCharacter(x$data$color, pattern=pattern, ignore.case=TRUE)
  stopifnot(all(inlmisc:::.IsColor(x$data$color)))

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
  stopifnot(inlmisc:::.IsColor(x$back, null.ok=TRUE))
  stopifnot(inlmisc:::.IsColor(x$fore, null.ok=TRUE))
  stopifnot(inlmisc:::.IsColor(x$nan,  null.ok=TRUE))

  checkmate::assertCharacter(x$note, null.ok=TRUE)

  invisible()
}