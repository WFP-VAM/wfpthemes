##########################################################
## Creates discrete color palettes for vizualization of categorical values
##########################################################

main_blue <- "#007DBC"
main_light_blue <- "#36B5C5"
main_navy <- "#1A4262"
main_green <- "#008868"
main_light_green <- "#00B485"
main_red <- "#982B56"
main_light_red <- "#EF404C"
main_grey <- "#999999"
main_light_grey <- "#E6E6E6"
main_orange <- "#F47847"
main_brown <- "#B79F8D"
main_tan <- "#ECDFBB"
main_black <- "#000000"
main_white <- "#FFFFFF"

## creates blue color palette object
pal_blue_wfp <- c(
  "blue1" = colorspace::lighten(main_blue, 0.75),
  "blue2" = colorspace::lighten(main_blue, 0.50),
  "blue3" = colorspace::lighten(main_blue, 0.25),
  "blue4" = main_blue,
  "blue5" = colorspace::darken(main_blue, 0.25),
  "blue6" = colorspace::darken(main_blue, 0.50),
  "blue7" = colorspace::darken(main_blue, 0.75)
)

## creates navy color palette object
pal_navy_wfp <- c(
  "navy1" = colorspace::lighten(main_navy, 0.75),
  "navy2" = colorspace::lighten(main_navy, 0.50),
  "navy3" = colorspace::lighten(main_navy, 0.25),
  "navy4" = main_navy,
  "navy5" = colorspace::darken(main_navy, 0.25),
  "navy6" = colorspace::darken(main_navy, 0.50),
  "navy7" = colorspace::darken(main_navy, 0.75)
)

## creates green color palette object
pal_green_wfp <- c(
  "green1" = colorspace::lighten(main_green, 0.75),
  "green2" = colorspace::lighten(main_green, 0.50),
  "green3" = colorspace::lighten(main_green, 0.25),
  "green4" = main_green,
  "green5" = colorspace::darken(main_green, 0.25),
  "green6" = colorspace::darken(main_green, 0.50),
  "green7" = colorspace::darken(main_green, 0.75)
)

## creates red color palette object
pal_red_wfp <- c(
  "red1" = colorspace::lighten(main_red, 0.75),
  "red2" = colorspace::lighten(main_red, 0.50),
  "red3" = colorspace::lighten(main_red, 0.25),
  "red4" = main_red,
  "red5" = colorspace::darken(main_red, 0.25),
  "red6" = colorspace::darken(main_red, 0.50),
  "red7" = colorspace::darken(main_red, 0.75)
)

## creates grey color palette object
pal_grey_wfp <- c(
  "grey1" = colorspace::lighten(main_grey, 0.75),
  "grey2" = colorspace::lighten(main_grey, 0.50),
  "grey3" = colorspace::lighten(main_grey, 0.25),
  "grey4" = main_grey,
  "grey5" = colorspace::darken(main_grey, 0.25),
  "grey6" = colorspace::darken(main_grey, 0.50),
  "grey7" = colorspace::darken(main_grey, 0.75)
)

## creates orange color palette object
pal_orange_wfp <- c(
  "orange1" = colorspace::lighten(main_orange, 0.75),
  "orange2" = colorspace::lighten(main_orange, 0.50),
  "orange3" = colorspace::lighten(main_orange, 0.25),
  "orange4" = main_orange,
  "orange5" = colorspace::darken(main_orange, 0.25),
  "orange6" = colorspace::darken(main_orange, 0.50),
  "orange7" = colorspace::darken(main_orange, 0.75)
)



## creates color palette object aligned with WFP corporate guidance
pal_main <- c(main_blue, main_light_blue, main_navy, main_grey, main_green, main_light_green, main_orange, main_red, main_brown, main_light_red, main_tan)

## creates color palette object aligned with ipc guidance
# https://www.ipcinfo.org/fileadmin/user_upload/ipcinfo/docs/IPC_Acute_Food_Insecurity_Mapping_Guidelines.pdf

pal_ipc <- c("#CDFACD","#FAE61E","#E67800","#C80000", "#640000")

## creates color palette object aligned with ipc guidance
# https://docs.wfp.org/api/documents/WFP-0000134704/download/?_ga=2.91951514.145906710.1681992709-313551633.1680773110
pal_stoplight_3pt <- c("#27AE60","#F1C40F","#C0392B")

## creates color palette object aligned with ipc guidance
pal_stoplight_4pt <- c("#92D050","#FFFF00","#FFC000","#FF0000")

##add another palette for FCSN based on guidance below
##https://documents.wfp.org/stellent/groups/public/documents/manual_guide_proced/wfp277333.pdf?_ga=2.245902989.80447050.1684258173-449667123.1675176625
#pal_FCSN <-  c("#92D050","#E46C0A","#C00000")


## creates object containing palette name, potential palette applications, and min/max number of categories palette can accommodate
wfpcolors <- tibble::tibble(name = c("pal_wfp_main",
                                     "pal_ipc",
                                     "pal_stoplight_3pt",
                                     "pal_stoplight_4pt",
                                     "pal_blue",
                                     "pal_navy",
                                     "pal_green",
                                     "pal_red",
                                     "pal_orange",
                                     "pal_grey",
                                     "pal_blue_red",
                                     "pal_navy_red"),
                            type = c(rep("qualitative", 4),
                                     rep("sequential", 6),
                                     rep("diverging", 2)),
                            min_n = 1,
                            max_n = c(c(10, 5, 3, 4),
                                      rep(7, 6),
                                      rep(9, 2)),
                            n1 = list(pal_main[1],
                                      pal_ipc[1],
                                      pal_stoplight_3pt[1],
                                      pal_stoplight_4pt[1],
                                      pal_blue_wfp[4],
                                      pal_navy_wfp[5],
                                      pal_green_wfp[4],
                                      pal_red_wfp[4],
                                      pal_orange_wfp[3],
                                      pal_grey_wfp[4],
                                      NULL, NULL),
                            n2 = list(pal_main[1:2],
                                      pal_ipc[1:2],
                                      pal_stoplight_3pt[1:2],
                                      pal_stoplight_4pt[1:2],
                                      c(pal_blue_wfp[2], pal_blue_wfp[6]),
                                      c(pal_navy_wfp[2], pal_navy_wfp[6]),
                                      c(pal_green_wfp[2], pal_green_wfp[6]),
                                      c(pal_red_wfp[2], pal_red_wfp[6]),
                                      c(pal_orange_wfp[2], pal_orange_wfp[6]),
                                      c(pal_grey_wfp[2], pal_grey_wfp[6]),
                                      c(pal_blue_wfp[2], pal_red_wfp[6]),
                                      c(pal_navy_wfp[2], pal_red_wfp[6])),
                            n3 = list(pal_main[1:3],
                                      pal_ipc[1:3],
                                      pal_stoplight_3pt[1:3],
                                      pal_stoplight_4pt[1:3],
                                      pal_blue_wfp[3:5],
                                      pal_navy_wfp[3:5],
                                      pal_green_wfp[3:5],
                                      pal_red_wfp[3:5],
                                      pal_orange_wfp[3:5],
                                      pal_grey_wfp[3:5],
                                      c(pal_blue_wfp[4], pal_grey_wfp[1], pal_red_wfp[4]),
                                      c(pal_navy_wfp[4], pal_grey_wfp[1], pal_red_wfp[4])),
                            n4 = list(pal_main[1:4],
                                      pal_ipc[1:4],
                                      NULL,
                                      pal_stoplight_4pt[1:4],
                                      pal_blue_wfp[2:5],
                                      pal_navy_wfp[2:5],
                                      pal_green_wfp[2:5],
                                      pal_red_wfp[2:5],
                                      pal_orange_wfp[2:5],
                                      pal_grey_wfp[2:5],
                                      c(pal_blue_wfp[4], pal_blue_wfp[2],
                                        pal_red_wfp[2], pal_red_wfp[4]),
                                      c(pal_navy_wfp[5], pal_navy_wfp[3],
                                        pal_red_wfp[2], pal_red_wfp[4])),
                            n5 = list(pal_main[1:5],
                                      pal_ipc[1:5],
                                      NULL,
                                      NULL,
                                      pal_blue_wfp[2:6],
                                      pal_navy_wfp[2:6],
                                      pal_green_wfp[2:6],
                                      pal_red_wfp[2:6],
                                      pal_orange_wfp[2:6],
                                      pal_grey_wfp[2:6],
                                      c(pal_blue_wfp[5], pal_blue_wfp[2],
                                        pal_grey_wfp[1],
                                        pal_red_wfp[2], pal_red_wfp[5]),
                                      c(pal_navy_wfp[5], pal_navy_wfp[3],
                                        pal_grey_wfp[1],
                                        pal_red_wfp[3], pal_red_wfp[5])),
                            n6 = list(pal_main[1:6],
                                      NULL,
                                      NULL,
                                      NULL,
                                      pal_blue_wfp[1:6],
                                      pal_navy_wfp[1:6],
                                      pal_green_wfp[1:6],
                                      pal_red_wfp[1:6],
                                      pal_orange_wfp[1:6],
                                      pal_grey_wfp[1:6],
                                      c(pal_blue_wfp[4:2],  pal_red_wfp[2:4]),
                                      c(pal_navy_wfp[5:3], pal_red_wfp[2:4])),
                            n7 = list(pal_main[1:7],
                                      NULL,
                                      NULL,
                                      NULL,
                                      pal_blue_wfp[1:7],
                                      pal_navy_wfp[1:7],
                                      pal_green_wfp[1:7],
                                      pal_red_wfp[1:7],
                                      pal_orange_wfp[1:7],
                                      pal_grey_wfp[1:7],
                                      c(pal_blue_wfp[4:2],  pal_grey_wfp[1], pal_red_wfp[2:4]),
                                      c(pal_navy_wfp[5:3], pal_grey_wfp[1], pal_red_wfp[2:4])),
                            n8 = list(pal_main[1:8],
                                      NULL,NULL, NULL,
                                      NULL, NULL, NULL,
                                      NULL, NULL, NULL,
                                      c(pal_blue_wfp[5:2], pal_red_wfp[2:5]),
                                      c(pal_navy_wfp[5:2], pal_red_wfp[2:5])),
                            n9 = list(pal_main[1:9],
                                      NULL, NULL, NULL,
                                      NULL, NULL, NULL,
                                      NULL, NULL, NULL,
                                      c(pal_blue_wfp[5:2], pal_grey_wfp[1], pal_red_wfp[2:5]),
                                      c(pal_navy_wfp[5:2], pal_grey_wfp[1], pal_red_wfp[2:5])),
                            n10 = list(pal_main,
                                       NULL, NULL, NULL,
                                       NULL, NULL, NULL,
                                       NULL, NULL, NULL,
                                       NULL, NULL))


wfpcolors <- as.data.frame(wfpcolors)


##########################################################
## Creates continuous color palettes for vizualization of continuous values
##########################################################

scale_colour_wfp_a <- function(..., type = "sequential",
                               palette = 1,
                               direction = 1,
                               na.value = "#E9E9E9", guide = "colourbar") {

  pal <- wfp_pal_scale(type = type,
                       palette = palette,
                       direction = direction)(256)

  continuous_scale("colour",
                   "wfp_continuous",
                   gradient_n_pal(pal),
                   na.value = na.value,
                   guide = guide,
                   ...)
}


scale_colour_wfp_b <- function(..., type = "qualitative",
                               palette = 1,
                               direction = 1,
                               nmax = NULL,
                               order = NULL,
                               na.value = "#E9E9E9") {

  pal <- wfp_pal_scale(type = type,
                       palette = palette,
                       nmax = nmax,
                       order = order,
                       direction = direction)

  discrete_scale("colour",
                 "wfp_discrete",
                 pal,
                 na.value = na.value,
                 ...)
}



##########################################################
## Creates continuous color palettes for vizualization of continuous values
##########################################################

scale_fill_wfp_a <- function(..., type = "sequential",
                             palette = 1,
                             direction = 1,
                             na.value = "#E9E9E9",
                             guide = "colourbar") {

  pal <- wfp_pal_scale(type = type,
                       palette = palette,
                       direction = direction)(256)

  continuous_scale("fill",
                   "wfp_continuous",
                   gradient_n_pal(pal),
                   na.value = na.value,
                   guide = guide,
                   ...)
}


scale_fill_wfp_b <- function(..., type = "qualitative",
                             palette = 1,
                             direction = 1,
                             nmax = NULL,
                             order = NULL,
                             na.value	= "#E9E9E9") {

  pal <- wfp_pal_scale(type = type,
                       palette = palette,
                       nmax = nmax,
                       order = order,
                       direction = direction)

  discrete_scale("fill",
                 "wfp_discrete",
                 pal,
                 na.value = na.value,
                 ...)
}



##########################################################
## Creates a series of functions to validate the palette name and ensure the the palette aligns with data type and number of categories
##########################################################

wfp_pal_scale <- function(type = "qualitative",
                          nmax = NULL, order = NULL,
                          palette = 1, direction = 1) {
  pal <- wfp_pal_name(palette, type)

  function(n) {
    if (is.null(nmax) | type != "qualitative")
      nmax <- n
    if (is.null(order) | type != "qualitative")
      order <- 1:n

    if (n > nmax) {
      warning("Insufficient values in scale_{color|fill}_wfp_d. ", n, " needed but only ",
              nmax, " provided.", call. = FALSE)
    }

    # If less than 3 colors are requested, brewer.pal will return a 3-color palette and
    # give a warning. This warning isn't useful, so suppress it.
    # If the palette has k colors and >k colors are requested, brewer.pal will
    # return a k-color palette and give a warning. This warning is useful, so
    # don't suppress it.

    if (nmax < 3) {
      pal <- suppressWarnings(wfp_pal(nmax, pal))
    } else {
      pal <- wfp_pal(nmax, pal)
    }

    # In both cases ensure we have n items
    pal <- pal[order]

    if (direction == -1)
      pal = rev(pal)

    unname(pal)
  }
}


##########################################################
## Creates function to validate whether palette name is valid otherwise replaces with pal_blue
##########################################################

wfp_pal_name <- function(palette, type) {
  if (is.character(palette)) {
    if (!palette %in% wfpcolors$name) {
      warning("Unknown palette ", palette)
      palette = "pal_blue"
    }
    return(palette)
  }
  type <- match.arg(type, unique(wfpcolors$type))
  wfpcolors$name[wfpcolors$type == type][palette]
}


##########################################################
## Creates function to validate whether palette has enough categories to allow for vizualization
##########################################################

wfp_pal <- function(n = NULL, name, ...){
  if (!(name %in% wfpcolors$name)){
    stop(paste(name, "is not a valid palette name\n"),
         call. = FALSE)
  }
  selected_metadata <- wfpcolors[wfpcolors$name == name, ]
  min_n <- selected_metadata$min_n
  max_n <- selected_metadata$max_n
  type <- selected_metadata$type
  if (is.null(n)) {
    n <- max_n
  }
  proper_n <- n
  if (proper_n > max_n) {
    proper_n <- max_n
  }
  if (!(n %in% min_n:max_n) && type == "qualitative") {
    warning(paste("Number of colors (n) in the", name,
                  "palette should be between", min_n, "and", max_n,
                  "\n"), call. = FALSE)
  }
  if (n < min_n) {
    warning(paste("Number of colors (n) in the", name,
                  "palette should be between", min_n, "and",
                  max_n, "\n"), call. = FALSE)
    proper_n <- min_n
    n <- min_n
  }
  coln <- paste0("n", proper_n)
  colors <- wfpcolors[wfpcolors$name == name, ][[coln]][[1]]
  if (n > 2) {
    colors <- grDevices::colorRampPalette(colors, ...)(n = n)
  }
  colors
}


##########################################################
## Creates a function that displays the color palettes
##########################################################

display_wfp_all <- function(n = NULL, type = "all") {
  wfpcolors <- wfpcolors[nrow(wfpcolors):1, ]
  if (any(type == "all")) {
    selected_type <- wfpcolors
  } else if (any(type %in% c("qualitative", "sequential", "diverging"))) {
    selected_type <- wfpcolors[wfpcolors$type %in% type, ]
  } else {
    stop(paste(type, "is not a valid name for a color type\n"))
  }
  selected_metadata <- wfpcolors[wfpcolors$name %in% selected_type$name, ]

  n_colors <- nrow(selected_metadata)

  if (is.null(n)) {
    my_n <- selected_metadata$max_n
  } else{
    my_n <- rep(n, n_colors)
  }

  selected_colors <- vector("list", n_colors)

  ylim <- c(0, n_colors)
  oldpar <- par(mgp = c(2, 0.25, 0))
  on.exit(par(oldpar))
  max_my_n <- max(my_n)
  plot(1, 1, xlim = c(-1.5, max_my_n), ylim = ylim,
       type = "n", axes = FALSE, bty = "n", xlab = "", ylab = "")

  for(i in seq_len(n_colors)) {
    one_color <- wfp_pal(n = my_n[i],
                         name = selected_metadata$name[i])
    rect(xleft = 0:(my_n[i] - 1),
         ybottom = i - 1,
         xright = 1:my_n[i],
         ytop = i - 0.2,
         col = one_color,
         border = "white")
    text(-0.1, i - 0.6,
         labels = selected_metadata$name[i],
         xpd = TRUE,
         adj = 1)
  }
}
