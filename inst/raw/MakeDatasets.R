MakeDatasets <- function() {

  options(stringsAsFactors=FALSE)

  schemes <- list()

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
    cite = "Tol, 2018, p.~5",
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
    cite = "Tol, 2018, p.~5",
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
    cite = "Tol, 2018, p.~5",
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
    cite = "Tol, 2018, p.~5",
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
    cite = "Tol, 2018, p.~5",
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
    cite = "Tol, 2018, p.~5",
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
    cite = "Tol, 2018, p.~6",
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
    cite = "Tol, 2018, p.~19",
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
    cite = "Tol, 2018, p.~9",
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
    cite = "Tol, 2018, p.~9",
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
    cite = "Tol, 2018, p.~9",
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
    cite = "Tol, 2018, p.~11",
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
    cite = "Tol, 2018, p.~11",
    nmax = Inf,
    nan  = "#999999"
  )

  schemes[["discrete rainbow"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   name
                    #E8ECFB,  1
                    #D9CCE3,  2
                    #D1BBD7,  3
                    #CAACCB,  4
                    #BA8DB4,  5
                    #AE76A3,  6
                    #AA6F9E,  7
                    #994F88,  8
                    #882E72,  9
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
    cite = "Tol, 2018, p.~12--13",
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
    cite = "Tol, 2018, p.~12",
    nmax = Inf,
    nan  = "#666666"
  )

  schemes[["DEM print"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #336600,   0
                    #81C31F, 100
                    #FFFFCC, 200
                    #F4BD45, 400
                    #66330C, 500
                    #663300, 600
                    #FFFFFF, 800
                    "),
    type = "Sequential",
    cite = "Dewez, 2004",
    nmax = Inf,
    back = "#336600",
    fore = "#FFFFFF",
    nan  = "#336600"
  )

  schemes[["DEM screen"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #008435,   0
                    #33CC00, 100
                    #F4F071, 200
                    #F4BD45, 400
                    #99642B, 600
                    #FFFFFF, 800
                    "),
    type  = "Sequential",
    cite  = "Dewez, 2004",
    nmax  = Inf,
    back = "#FFFFFF",
    fore = "#008435",
    nan  = "#008435"
  )

  schemes[["DEM poster"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #006147,    0
                    #107A2F,   50
                    #E8D77D,  500
                    #A14300, 1200
                    #9E0000, 1700
                    #6E6E6E, 2800
                    #FFFFFF, 4000
                    #FFFFFF, 4900
                    "),
    type  = "Sequential",
    cite  = "Dewez, 2004",
    nmax  = Inf,
    back = "#99CCFF",
    fore = "#99CCFF",
    nan  = "#99CCFF"
  )

  schemes[["GMT abyss"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #000000, -8000
                    #141E35, -7000
                    #263C6A, -6000
                    #2E5085, -5000
                    #3563A0, -4000
                    #4897D3, -3000
                    #5AB9E9, -2000
                    #8DD2EF, -1000
                    #F5FFFF,     0
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#000000",
    fore = "#FFFFFF",
    nan  = "#BEBEBE",
    note = strwrap("Color table for bathymetry modeled after IBCSO
                    at depth but turning lighter towards sea level
                    Designed by P. Wessel, SOEST")
  )

  schemes[["GMT bathy"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #000000, -8000
                    #1F284F, -7000
                    #263C6A, -6000
                    #3563A0, -4000
                    #4897D3, -3500
                    #66CDAA, -2500
                    #8DD2EF, -1000
                    #F5FFFF,     0
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#000000",
    fore = "#F5FFFF",
    nan  = "#BEBEBE",
    note = strwrap("Color table for bathymetry modeled after IBCSO
                    at depth but going through an aquamarine patch
                    between 2009-3000, then into lightblue to white
                    Designed by P. Wessel, SOEST")
  )

  schemes[["GMT copper"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #000000, 0
                    #FF9F66, 0.796875
                    #FFC880, 1
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    note = strwrap("Simulates the COPPER colormap in MATLAB")
  )

  schemes[["GMT cubhelix"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color
                    #000000
                    #010001
                    #030103
                    #040104
                    #060206
                    #080208
                    #090309
                    #0A040B
                    #0C040D
                    #0D050F
                    #0E0611
                    #0F0613
                    #110715
                    #120817
                    #130919
                    #140A1B
                    #140B1D
                    #150B1F
                    #160C21
                    #170D23
                    #170E25
                    #180F27
                    #181129
                    #19122B
                    #19132D
                    #19142F
                    #1A1530
                    #1A1632
                    #1A1834
                    #1A1936
                    #1A1A38
                    #1A1C39
                    #1A1D3B
                    #1A1F3C
                    #1A203E
                    #1A223F
                    #1A2341
                    #192542
                    #192643
                    #192845
                    #192946
                    #182B47
                    #182D48
                    #182E49
                    #17304A
                    #17324A
                    #17344B
                    #17354C
                    #16374C
                    #16394D
                    #163A4D
                    #153C4D
                    #153E4E
                    #15404E
                    #15424E
                    #15434E
                    #15454E
                    #14474E
                    #14494E
                    #144A4D
                    #154C4D
                    #154E4D
                    #154F4C
                    #15514C
                    #15534B
                    #16544B
                    #16564A
                    #165849
                    #175949
                    #175B48
                    #185C47
                    #195E46
                    #1A5F45
                    #1B6144
                    #1B6243
                    #1C6342
                    #1E6542
                    #1F6641
                    #206740
                    #21683F
                    #236A3D
                    #246B3C
                    #266C3B
                    #276D3A
                    #296E3A
                    #2B6F39
                    #2D7038
                    #2F7137
                    #317236
                    #337235
                    #357334
                    #377433
                    #397433
                    #3C7532
                    #3E7631
                    #417631
                    #437730
                    #467730
                    #48782F
                    #4B782F
                    #4E782F
                    #51792E
                    #53792E
                    #56792E
                    #59792E
                    #5C7A2E
                    #5F7A2F
                    #627A2F
                    #657A2F
                    #687A30
                    #6B7A30
                    #6E7A31
                    #717A32
                    #747A32
                    #787A33
                    #7B7A34
                    #7E7A35
                    #817A37
                    #847A38
                    #877A39
                    #8A793B
                    #8D793C
                    #90793E
                    #937940
                    #967941
                    #997943
                    #9B7945
                    #9E7947
                    #A1794A
                    #A4784C
                    #A6784E
                    #A97851
                    #AB7853
                    #AE7856
                    #B07858
                    #B2785B
                    #B5785E
                    #B77860
                    #B97863
                    #BB7966
                    #BD7969
                    #BF796C
                    #C1796F
                    #C27972
                    #C47A75
                    #C67A78
                    #C77A7C
                    #C97B7F
                    #CA7B82
                    #CB7C85
                    #CC7C88
                    #CD7D8C
                    #CE7D8F
                    #CF7E92
                    #D07F95
                    #D17F99
                    #D1809C
                    #D2819F
                    #D382A2
                    #D383A5
                    #D383A9
                    #D484AC
                    #D485AF
                    #D487B2
                    #D488B5
                    #D489B8
                    #D48ABA
                    #D48BBD
                    #D48CC0
                    #D38EC3
                    #D38FC5
                    #D390C8
                    #D292CB
                    #D293CD
                    #D295CF
                    #D196D2
                    #D098D4
                    #D09AD6
                    #CF9BD8
                    #CF9DDA
                    #CE9EDC
                    #CDA0DE
                    #CDA2E0
                    #CCA4E2
                    #CBA5E3
                    #CBA7E5
                    #CAA9E6
                    #C9ABE7
                    #C9ACE9
                    #C8AEEA
                    #C7B0EB
                    #C7B2EC
                    #C6B4ED
                    #C5B6EE
                    #C5B7EF
                    #C4B9EF
                    #C4BBF0
                    #C3BDF1
                    #C3BFF1
                    #C2C1F2
                    #C2C2F2
                    #C2C4F2
                    #C1C6F3
                    #C1C8F3
                    #C1CAF3
                    #C1CBF3
                    #C1CDF3
                    #C1CFF3
                    #C1D0F3
                    #C1D2F3
                    #C1D4F3
                    #C1D5F3
                    #C2D7F2
                    #C2D8F2
                    #C3DAF2
                    #C3DBF2
                    #C4DDF1
                    #C4DEF1
                    #C5E0F1
                    #C6E1F1
                    #C7E2F0
                    #C8E4F0
                    #C8E5F0
                    #CAE6EF
                    #CBE7EF
                    #CCE8EF
                    #CDE9EF
                    #CEEBEF
                    #D0ECEE
                    #D1EDEE
                    #D2EEEE
                    #D4EFEE
                    #D5F0EE
                    #D7F0EE
                    #D9F1EE
                    #DAF2EE
                    #DCF3EF
                    #DEF4EF
                    #DFF4EF
                    #E1F5F0
                    #E3F6F0
                    #E5F7F0
                    #E7F7F1
                    #E8F8F2
                    #EAF8F2
                    #ECF9F3
                    #EEFAF4
                    #F0FAF5
                    #F2FBF6
                    #F4FBF7
                    #F5FCF8
                    #F7FCF9
                    #F9FDFA
                    #FBFDFC
                    #FDFEFD
                    #FFFFFF
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#000000",
    fore = "#FFFFFF",
    nan  = "#FF0000",
    note = strwrap("Dave Green, Cambridge
                    Designed for intensities
                    From ch05m151010.cpt at cpt city")
  )

  schemes[["GMT dem1"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #336600,   0
                    #81C31F, 100
                    #FFFFCC, 200
                    #F4BD45, 400
                    #66330C, 500
                    #663300, 600
                    #FFFFFF, 800
                    "),
    type  = "Sequential",
    cite  = "Wessel and others, 2013",
    nmax  = Inf,
    back = "#336600",
    fore = "#FFFFFF",
    nan  = "#336600",
    note = strwrap("Color table for topography, via cpt-city
                    Designed by: Thomas Dewez for printing
                    Modified by P Wessel to avoid discontinuities")
  )

  schemes[["GMT dem2"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #006147,    0
                    #107A2F,   50
                    #E8D77D,  500
                    #A14300, 1200
                    #643219, 1700
                    #6E6E6E, 2800
                    #FFFFFF, 4000
                    #FFFFFF, 4900
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#BEBEBE",
    fore = "#FFFFFF",
    nan  = "#000000",
    note = "Paul Wessel, modified from DEM_poster at cpt city to yield brown"
  )

  schemes[["GMT dem3"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #3CB371,    0
                    #DEB887, 1000
                    #CD6600, 2000
                    #8B4513, 3000
                    #EED5B7, 4000
                    #EEEEE0, 5000
                    #FFFAFA, 6000
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#000000",
    fore = "#FFFFFF",
    nan  = "#BEBEBE",
    note = strwrap("Color table for topography
                    Designed by P. Wessel, SOEST")
  )

  schemes[["GMT dem4"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #B0F2D8,    0
                    #B1F3D6,   10
                    #B2F5D2,   20
                    #B2F5CC,   30
                    #B3F5C6,   40
                    #B2F6C0,   50
                    #B2F7BC,   60
                    #B4F9B7,   70
                    #B7F9B4,   80
                    #BCFAB4,   90
                    #C2FAB4,  100
                    #C8FBB4,  110
                    #CEFCB4,  120
                    #D4FDB3,  130
                    #DCFDB4,  140
                    #E3FEB4,  150
                    #EAFFB4,  160
                    #F1FFB5,  170
                    #F8FFB5,  180
                    #FDFFB4,  190
                    #FBFCAE,  200
                    #F1F7A4,  210
                    #E5F399,  220
                    #D8EE8E,  230
                    #C8E782,  240
                    #BAE177,  250
                    #ACDB6D,  260
                    #9CD663,  270
                    #8ECF5A,  280
                    #7EC953,  290
                    #6FC44B,  300
                    #5FBE43,  310
                    #4EB83A,  320
                    #3FB233,  330
                    #32AD2D,  340
                    #27A72B,  350
                    #1EA22C,  360
                    #189C2F,  370
                    #129632,  380
                    #0E9035,  390
                    #098B39,  400
                    #07863D,  410
                    #0C8440,  420
                    #188440,  430
                    #28863E,  440
                    #358A3D,  450
                    #418E3C,  460
                    #4D903C,  470
                    #589439,  480
                    #649637,  490
                    #6F9835,  500
                    #799C33,  510
                    #829E31,  520
                    #8BA22F,  530
                    #95A42C,  540
                    #9EA629,  550
                    #A8A827,  560
                    #B2AC24,  570
                    #BDAF22,  580
                    #C7B21E,  590
                    #D1B31C,  600
                    #DDB518,  610
                    #E7B614,  620
                    #F1B80E,  630
                    #F9B808,  640
                    #FBB204,  650
                    #F7A802,  660
                    #F19D02,  670
                    #EB9202,  680
                    #E58602,  690
                    #DF7B02,  700
                    #DB7002,  710
                    #D66702,  720
                    #D05D02,  730
                    #CA5502,  740
                    #C24B02,  750
                    #BC4302,  760
                    #B63B02,  770
                    #B03202,  780
                    #AB2B02,  790
                    #A52402,  800
                    #9F1E02,  810
                    #991702,  820
                    #941201,  830
                    #8F0E01,  840
                    #890800,  850
                    #840500,  860
                    #7E0400,  870
                    #7B0802,  880
                    #780D02,  890
                    #771002,  900
                    #761204,  910
                    #761404,  920
                    #761504,  930
                    #751604,  940
                    #751805,  950
                    #731A06,  960
                    #731D06,  970
                    #711F07,  980
                    #702108,  990
                    #6F2308, 1000
                    #6F2408, 1010
                    #6E2609, 1020
                    #6D280A, 1030
                    #6D280A, 1040
                    #6D2B0A, 1050
                    #6C2D0B, 1060
                    #6B2D0C, 1070
                    #6B2F0C, 1080
                    #6C310E, 1090
                    #6F3512, 1100
                    #723A17, 1110
                    #753F1C, 1120
                    #774320, 1130
                    #7A4725, 1140
                    #7E4B2C, 1150
                    #825033, 1160
                    #855639, 1170
                    #895B40, 1180
                    #8C6146, 1190
                    #8E664D, 1200
                    #926B55, 1210
                    #95705B, 1220
                    #987561, 1230
                    #9A7B69, 1240
                    #9E8371, 1250
                    #A08979, 1260
                    #A28F84, 1270
                    #A5958D, 1280
                    #A89C95, 1290
                    #A9A29E, 1300
                    #ACA9A6, 1310
                    #AEAEAD, 1320
                    #B0B0B0, 1330
                    #B4B4B4, 1340
                    #B7B7B7, 1350
                    #BABABA, 1360
                    #BEBEBE, 1370
                    #C2C2C2, 1380
                    #C6C6C6, 1390
                    #CACACA, 1400
                    #CECECE, 1410
                    #D2D0D2, 1420
                    #D7D4D7, 1430
                    #DBD9DB, 1440
                    #DDDBDD, 1450
                    #E0DEE0, 1460
                    #E4E2E4, 1470
                    #E8E6E8, 1480
                    #ECEAEC, 1490
                    #EEECEE, 1500
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#000000",
    fore = "#EBE9EB",
    nan  = "#FF00FF",
    note = strwrap("Originally wiki-schwarzwald-cont.cpt continuous version extracted from
                    http://commons.wikimedia.org/wiki/File:Schwarzwald-topographie.png
                    Author: wikipedia users W-j-s, Jide
                    License: Creative Commons Attribution-Share Alike 3.0 Unported
                    J.J. Green 2012")
  )

  schemes[["GMT drywet"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #90682D,  0
                    #FFD56B,  2
                    #C1FF91,  4
                    #36FFFC,  6
                    #0D81FF,  8
                    #2901C4, 10
                    #093779, 12
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    note = strwrap("Dry to Wet color table
                    Created by Ed Maurer, University of Washington")
  )

  schemes[["GMT elevation"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #6C9D89,    0
                    #7AAE97,   50
                    #87BBA0,  200
                    #AAC6AC,  600
                    #DAD0B9, 1000
                    #DBBEA9, 2000
                    #DBC6BB, 3000
                    #DDD8D5, 4000
                    #E5E4E5, 5000
                    #F6F6F6, 6000
                    #FEFFFE, 7000
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#000000",
    fore = "#FFFFFF",
    nan  = "#BEBEBE",
    note = strwrap("Washed out color table for topography, via cpt-city
                    Designed by: Tom Patterson
                    Modified by P Wessel to avoid discontinuities")
  )

  schemes[["GMT gebco"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #00F0FF, -7000
                    #23FFFF, -6000
                    #5AFFFF, -5000
                    #8CFFE6, -4000
                    #A5FFD7, -3000
                    #C3FFD7, -2000
                    #D2FFD7, -1000
                    #E6FFF0,  -500
                    #EBFFFF,  -200
                    #EBFFFF,     0
                    "),
    type = "Sequential",
    cite = "Wessel and others, 2013",
    nmax = Inf,
    back = "#FFFFFF",
    fore = "#000000",
    nan  = "#808080",
    note = strwrap("Bathymetry color table approximating the GEBCO charts.
                    Designed by Andrew Goodwillie, Lamont-Doherty Earth Observatory")
  )


  schemes[["GMT globe"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color,   value
                    #9900FF, -10000
                    #9900FF,  -9500
                    #9900FF,  -9000
                    #8811FF,  -8500
                    #7722FF,  -8000
                    #6633FF,  -7500
                    #5544FF,  -7000
                    #4455FF,  -6500
                    #3366FF,  -6000
                    #2277FF,  -5500
                    #1188FF,  -5000
                    #0099FF,  -4500
                    #1BA4FF,  -4000
                    #36AFFF,  -3500
                    #51BAFF,  -3000
                    #6CC5FF,  -2500
                    #86D0FF,  -2000
                    #A1DBFF,  -1500
                    #BCE6FF,  -1000
                    #D7F1FF,   -500
                    #F1FCFF,   -200
                    #336600,      0
                    #33CC66,    100
                    #BBE492,    200
                    #FFDCB9,    500
                    #F3CA89,   1000
                    #E6B858,   1500
                    #D9A627,   2000
                    #A89A1F,   2500
                    #A49019,   3000
                    #A28613,   3500
                    #9F7B0D,   4000
                    #9C7107,   4500
                    #996600,   5000
                    #A25959,   5500
                    #B27676,   6000
                    #B79393,   6500
                    #C2B0B0,   7000
                    #CCCCCC,   7500
                    #E5E5E5,   8000
                    #F2F2F2,   8500
                    #FFFFFF,   9000
                    #FFFFFF,   9500
                    #FFFFFF,  10000
                    "),
    type  = "Diverging",
    cite  = "Wessel and others, 2013",
    nmax  = Inf,
    back = "#FFFFFF",
    fore = "#000000",
    nan  = "#808080",
    note = strwrap("Color table using in global relief maps
                    Bathymetry colours manually redefined for blue-shade effect and
                    new topography colour scheme for use with Generic Mapping Tools.
                    Designed by Designed by Lester M. Anderson ( arctica1963@gmail.com )")
  )

  schemes[["GMT gray"]] <- list(
    data = read.csv(strip.white=TRUE, text="
                    color
                    #000000
                    #FFFFFF
                    "),
    type  = "Sequential",
    cite  = "Wessel and others, 2013",
    nmax  = Inf,
    note = strwrap("Plain linear gray color table.")
  )

  schemes <- schemes[order(vapply(schemes, function(x) x$type, ""), names(schemes))]

  invisible(lapply(schemes, function(x) {
    checkmate::assertDataFrame(x$data, any.missing=FALSE, min.rows=2, min.cols=1)

    pattern <- "^#(\\d|[a-f]){6}$"
    checkmate::assertCharacter(x$data$color, pattern=pattern, ignore.case=TRUE)
    stopifnot(all(inlmisc:::.IsColor(x$data$color)))

    checkmate::assertNumeric(x$data$value, finite=TRUE, unique=TRUE, sorted=TRUE, null.ok=TRUE)

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
  }))

  if (dir.exists("../../R")) save(schemes, file="../../R/sysdata.rda")

  invisible()
}
