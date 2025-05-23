#########################################################
# Creates discrete color palettes for visualization of categorical values
##########################################################

main_blue <- "#007DBC"
main_light_blue <- "#008EB2"
main_navy <- "#002F5A"
main_red <- "#E3002B"
main_light_red <- "#ECE1B1"
main_ivory <- "#ECE1B1"
main_orange <- "#E67536"
main_purple <- "#810054"
main_tan <- "#ECDFBB"
main_black <- "#000000"
main_white <- "#FFFFFF"

## creates blue color palette object
blue_sequential_wfp <- c(
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

## creates red color palette object
red_sequential_wfp <- c(
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
  "grey1" = colorspace::lighten(main_ivory, 0.75),
  "grey2" = colorspace::lighten(main_ivory, 0.50),
  "grey3" = colorspace::lighten(main_ivory, 0.25),
  "grey4" = main_ivory,
  "grey5" = colorspace::darken(main_ivory, 0.25),
  "grey6" = colorspace::darken(main_ivory, 0.50),
  "grey7" = colorspace::darken(main_ivory, 0.75)
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

## creates color palette object aligned with ipc guidance
# https://www.ipcinfo.org/fileadmin/user_upload/ipcinfo/docs/IPC_Acute_Food_Insecurity_Mapping_Guidelines.pdf

ipc_palette <- c("#CDFACD","#FAE61E","#E67800","#C80000", "#640000")

## creates color palette object aligned with indicator compendium guidance
cari_palette <-  c("#FFD7D7","#ff6e6e","#ff0000","#820000")

## creates color palette object aligned with food security guidance
food_security_blue_3cat <- c("#B4CFED","#007DBC","#002F5A")  # Update colors as needed

## creates color palette object aligned with food security guidance
food_security_blue_4cat <- c("#B4CFED","#73A5D4","#007DBC","#002F5A")  # Update colors as needed

## creates new red food security palette with 3 points
food_security_red_3cat <- c("#ECE1B1","#E67536","#E3002B")  # Update colors as needed

## creates new red food security palette with 4 points
food_security_red_4cat <- c("#ECE1B1","#E6B068","#E67536","#E3002B")  # Update colors as needed

## creates new CARI area palette
cari_area_palette <- c("#FFFFD4","#EAE297","#D7C55A","#C7A600")  # Update colors as needed

## creates color palette object aligned with WFP corporate guidance
pal_main <- c(main_blue, main_light_blue, main_navy, main_ivory, main_orange, main_red, main_purple, main_light_red, main_tan)


## creates object containing palette name, potential palette applications, and min/max number of categories palette can accommodate

# Define the main color palette information table
# This table maps palette names to their properties and color values for different sizes
wfpcolors <- tibble::tibble(
  # Palette names - includes standard WFP palettes and specialized indicator palettes
  name = c(
    # Main WFP corporate palette
    "wfp_main_8cat",
    # Food security classification palettes
    "ipc_palette",
    # Renamed from stoplight to foodsec per roadmap
    "food_security_blue_3cat", 
    "food_security_blue_4cat",
    # Added new food security red palettes per roadmap
    "food_security_red_3cat",
    "food_security_red_4cat",
    # Consolidated Approach for Reporting Indicators palettes
    "cari_palette",
    "cari_area_palette", # Added per roadmap
    # Single-color sequential palettes
    "blue_sequential",
    "red_sequential",
    # Diverging palettes
    "blue_red_divergent",
    "dark_blue_red_divergent"
  ),
  
  # Palette types: qualitative (categorical), sequential (continuous), or diverging
  type = c(
    rep("qualitative", 8), # First 8 palettes are qualitative (removed 2)
    rep("sequential", 2),  # Next 2 are sequential
    rep("diverging", 2)    # Last 2 are diverging
  ),
  
  # Minimum number of colors for each palette
  min_n = rep(1, 12),      # Reduced from 14 to 12
  
  # Maximum number of colors for each palette
  max_n = c(
    7, 5, 3, 4, 3, 4, 4, 4,  # Max colors for qualitative palettes (removed 2)
    rep(7, 2),               # Max colors for sequential palettes
    rep(9, 2)                # Max colors for diverging palettes
  ),
  
  # Single color palettes (n=1)
  n1 = list(
    pal_main[1],           # Main WFP blue
    ipc_palette[1],            # IPC minimal/none
    food_security_blue_3cat[1], # First color
    food_security_blue_4cat[1], # First color
    food_security_red_3cat[1],  # First color
    food_security_red_4cat[1],  # First color
    # Removed pal_fcsn and pal_lcs
    cari_palette[1],           # CARI
    cari_area_palette[1],      # CARI area
    blue_sequential_wfp[4],       # Medium blue
    red_sequential_wfp[4],        # Medium red
    NULL, NULL             # Diverging palettes need at least 2 colors
  ),
  
  # Two-color palettes (n=2)
  n2 = list(
    pal_main[1:2],                         # Main blue, light blue
    ipc_palette[1:2],                          # IPC 1-2
    food_security_blue_3cat[1:2],             # First 2 colors
    food_security_blue_4cat[1:2],             # First 2 colors
    food_security_red_3cat[1:2],              # First 2 colors
    food_security_red_4cat[1:2],              # First 2 colors
    # Removed pal_fcsn and pal_lcs
    cari_palette[1:2],                         # CARI
    cari_area_palette[1:2],                    # CARI area
    c(blue_sequential_wfp[2], blue_sequential_wfp[6]),   # Light and dark blue
    c(red_sequential_wfp[2], red_sequential_wfp[6]),     # Light and dark red
    c(blue_sequential_wfp[2], red_sequential_wfp[6]),    # Light blue to dark red
    c(pal_navy_wfp[2], red_sequential_wfp[6])     # Light navy to dark red
  ),
  
  # Three-color palettes (n=3)
  n3 = list(
    pal_main[1:3],                         # Main blue, light blue, navy
    ipc_palette[1:3],                          # IPC 1-3
    food_security_blue_3cat,                  # Full 3-color palette
    food_security_blue_4cat[1:3],             # First 3 colors
    food_security_red_3cat,                   # Full 3-color palette
    food_security_red_4cat[1:3],              # First 3 colors
    # Removed pal_fcsn and pal_lcs
    cari_palette[1:3],                         # CARI
    cari_area_palette[1:3],                    # CARI area
    blue_sequential_wfp[3:5],                     # Medium-light to medium-dark blue
    red_sequential_wfp[3:5],                      # Medium-light to medium-dark red
    c(blue_sequential_wfp[4], pal_grey_wfp[1], red_sequential_wfp[4]), # Blue-neutral-red
    c(pal_navy_wfp[4], pal_grey_wfp[1], red_sequential_wfp[4])  # Navy-neutral-red
  ),
  
  # Four-color palettes (n=4)
  n4 = list(
    pal_main[1:4],                         # Main blue, light blue, navy, grey
    ipc_palette[1:4],                          # IPC 1-4
    NULL,                                  # foodsec_blue_3pt doesn't have 4 colors
    food_security_blue_4cat,                  # Full 4-color palette
    NULL,                                  # foodsec_red_3pt doesn't have 4 colors
    food_security_red_4cat,                   # Full 4-color palette
    # Removed pal_fcsn and pal_lcs
    cari_palette,                              # Full 4-color palette
    cari_area_palette,                         # Full 4-color palette
    blue_sequential_wfp[2:5],                     # Light to dark blue range
    red_sequential_wfp[2:5],                      # Light to dark red range
    c(blue_sequential_wfp[4], blue_sequential_wfp[2], red_sequential_wfp[2], red_sequential_wfp[4]), # Blue to red
    c(pal_navy_wfp[5], pal_navy_wfp[3], red_sequential_wfp[2], red_sequential_wfp[4])  # Navy to red
  ),

  # Five-color palettes (n=5)
  n5 = list(
    pal_main[1:5],                         # First 5 colors of main palette
    ipc_palette[1:5],                          # IPC 1-5 (full IPC scale)
    NULL,                                  # foodsec_blue_3pt doesn't have 5 colors
    NULL,                                  # foodsec_blue_4pt doesn't have 5 colors
    NULL,                                  # foodsec_red_3pt doesn't have 5 colors
    NULL,                                  # foodsec_red_4pt doesn't have 5 colors
    # Removed pal_fcsn and pal_lcs
    NULL,                                  # CARI doesn't have 5 colors
    NULL,                                  # CARI area doesn't have 5 colors
    blue_sequential_wfp[2:6],                     # Light to very dark blue range
    red_sequential_wfp[2:6],                      # Light to very dark red range
    # 5-color diverging blue-grey-red
    c(blue_sequential_wfp[5], blue_sequential_wfp[2], pal_grey_wfp[1], red_sequential_wfp[2], red_sequential_wfp[5]),
    # 5-color diverging navy-grey-red
    c(pal_navy_wfp[5], pal_navy_wfp[3], pal_grey_wfp[1], red_sequential_wfp[3], red_sequential_wfp[5])
  ),
  
  # Six-color palettes (n=6)
  n6 = list(
    pal_main[1:6],                         # First 6 colors of main palette
    NULL,                                  # IPC doesn't have 6 colors
    NULL,                                  # foodsec_blue_3pt doesn't have 6 colors
    NULL,                                  # foodsec_blue_4pt doesn't have 6 colors
    NULL,                                  # foodsec_red_3pt doesn't have 6 colors
    NULL,                                  # foodsec_red_4pt doesn't have 6 colors
    # Removed pal_fcsn and pal_lcs
    NULL,                                  # CARI doesn't have 6 colors
    NULL,                                  # CARI area doesn't have 6 colors
    blue_sequential_wfp[1:6],                     # Very light to very dark blue range
    red_sequential_wfp[1:6],                      # Very light to very dark red range
    c(blue_sequential_wfp[4:2], red_sequential_wfp[2:4]), # 6-color diverging blue-red
    c(pal_navy_wfp[5:3], red_sequential_wfp[2:4])  # 6-color diverging navy-red
  ),
  
  # Seven-color palettes (n=7)
  n7 = list(
    pal_main[1:7],                         # First 7 colors of main palette (now max)
    NULL,                                  # IPC doesn't have 7 colors
    NULL,                                  # foodsec_blue_3pt doesn't have 7 colors
    NULL,                                  # foodsec_blue_4pt doesn't have 7 colors
    NULL,                                  # foodsec_red_3pt doesn't have 7 colors
    NULL,                                  # foodsec_red_4pt doesn't have 7 colors
    # Removed pal_fcsn and pal_lcs
    NULL,                                  # CARI doesn't have 7 colors
    NULL,                                  # CARI area doesn't have 7 colors
    blue_sequential_wfp[1:7],                     # Full blue range (very light to very dark)
    red_sequential_wfp[1:7],                      # Full red range
    # 7-color diverging blue-grey-red
    c(blue_sequential_wfp[4:2], pal_grey_wfp[1], red_sequential_wfp[2:4]),
    # 7-color diverging navy-grey-red
    c(pal_navy_wfp[5:3], pal_grey_wfp[1], red_sequential_wfp[2:4])
  ),
  
  # Eight-color palettes (n=8)
  n8 = list(
    NULL,                                  # Main palette reduced to 7 colors
    NULL, NULL, NULL,                      # IPC, foodsec_blue_3pt, foodsec_blue_4pt don't have 8 colors
    NULL, NULL,                            # foodsec_red_3pt, foodsec_red_4pt don't have 8 colors
    # Removed pal_fcsn and pal_lcs
    NULL, NULL,                            # CARI, CARI area don't have 8 colors
    NULL, NULL,                            # Sequential palettes don't have 8 colors
    c(blue_sequential_wfp[5:2], red_sequential_wfp[2:5]), # 8-color diverging blue-red
    c(pal_navy_wfp[5:2], red_sequential_wfp[2:5])  # 8-color diverging navy-red
  ),
  
  # Nine-color palettes (n=9)
  n9 = list(
    NULL,                                  # Main palette reduced to 7 colors
    NULL, NULL, NULL,                      # IPC, foodsec_blue_3pt, foodsec_blue_4pt don't have 9 colors
    NULL, NULL,                            # foodsec_red_3pt, foodsec_red_4pt don't have 9 colors
    # Removed pal_fcsn and pal_lcs
    NULL, NULL,                            # CARI, CARI area don't have 9 colors
    NULL, NULL,                            # Sequential palettes don't have 9 colors
    # 9-color diverging blue-grey-red
    c(blue_sequential_wfp[5:2], pal_grey_wfp[1], red_sequential_wfp[2:5]),
    # 9-color diverging navy-grey-red
    c(pal_navy_wfp[5:2], pal_grey_wfp[1], red_sequential_wfp[2:5])
  ),
  
  # Ten-color palettes (n=10)
  n10 = list(
    NULL,                                  # Main palette reduced to 7 colors
    NULL, NULL, NULL,                      # IPC, foodsec_blue_3pt, foodsec_blue_4pt don't have 10 colors
    NULL, NULL,                            # foodsec_red_3pt, foodsec_red_4pt don't have 10 colors
    # Removed pal_fcsn and pal_lcs
    NULL, NULL,                            # CARI, CARI area don't have 10 colors
    NULL, NULL,                            # Sequential palettes don't have 10 colors
    NULL, NULL                             # Diverging palettes don't have 10 colors
  )
)


##########################################################
## Creates continuous color palettes for vizualization of continuous values
##########################################################


#' scale_colour_wfp_a
#'
#' Creates a continuous color scale for ggplot2 visualizations using WFP color palettes.
#'
#' @param ... Additional arguments passed to `continuous_scale`.
#' @param type Character. Type of palette to use ("sequential", "qualitative", etc.). Default is "sequential".
#' @param palette Numeric or Character. Palette index or name. Default is 1.
#' @param direction Numeric. Direction of the palette (1 for normal, -1 for reversed). Default is 1.
#' @param na.value Character. Color for missing values. Default is "#E9E9E9".
#' @param guide Character. Type of legend guide ("colourbar" or "legend"). Default is "colourbar".
#'
#' @return A ggplot2 continuous color scale.
#' @export
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


#' scale_colour_wfp_b
#'
#' Creates a discrete color scale for ggplot2 visualizations.
#'
#' @param type Character. Type of palette to use ("qualitative", etc.). Default is "qualitative".
#' @param palette Numeric or Character. Palette index or name. Default is 1.
#' @param direction Numeric. Direction of the palette (1 for normal, -1 for reversed). Default is 1.
#' @param nmax Numeric. Maximum number of colors in the palette. Default is NULL.
#' @param order Numeric. Order of colors in the palette. Default is NULL.
#' @param na.value Character. Color for missing values. Default is "#E9E9E9".
#' @param ... Additional arguments passed to `discrete_scale`.
#'
#' @return A ggplot2 discrete color scale.
#' @export
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

#' scale_fill_wfp_a
#'
#' Creates a continuous fill scale for ggplot2 visualizations.
#'
#' @param type Character. Type of palette to use ("sequential", etc.). Default is "sequential".
#' @param palette Numeric or Character. Palette index or name. Default is 1.
#' @param direction Numeric. Direction of the palette (1 for normal, -1 for reversed). Default is 1.
#' @param na.value Character. Color for missing values. Default is "#E9E9E9".
#' @param guide Character. Type of legend guide ("colourbar" or "legend"). Default is "colourbar".
#' @param ... Additional arguments passed to `continuous_scale`.
#'
#' @return A ggplot2 continuous fill scale.
#' @export
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


#' scale_fill_wfp_b
#'
#' Creates a discrete fill scale for ggplot2 visualizations.
#'
#' @param type Character. Type of palette to use ("qualitative", etc.). Default is "qualitative".
#' @param palette Numeric or Character. Palette index or name. Default is 1.
#' @param direction Numeric. Direction of the palette (1 for normal, -1 for reversed). Default is 1.
#' @param nmax Numeric. Maximum number of colors in the palette. Default is NULL.
#' @param order Numeric. Order of colors in the palette. Default is NULL.
#' @param na.value Character. Color for missing values. Default is "#E9E9E9".
#' @param ... Additional arguments passed to `discrete_scale`.
#'
#' @return A ggplot2 discrete fill scale.
#' @export
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

#' wfp_pal_scale
#'
#' Creates a palette function for ggplot2 scales.
#'
#' @param type Character. Type of palette to use ("qualitative", "sequential", etc.). Default is "qualitative".
#' @param nmax Numeric. Maximum number of colors in the palette. Default is NULL.
#' @param order Numeric. Order of colors in the palette. Default is NULL.
#' @param palette Numeric or Character. Palette index or name. Default is 1.
#' @param direction Numeric. Direction of the palette (1 for normal, -1 for reversed). Default is 1.
#'
#' @return A function that generates a palette for ggplot2 scales.
#' @export
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
## Creates function to validate whether palette name is valid otherwise replaces with blue_sequential
##########################################################

#' wfp_pal_name
#'
#' Validates the palette name and replaces invalid names with "blue_sequential".
#'
#' @param palette Character. Palette name to validate.
#' @param type Character. Type of palette ("qualitative", "sequential", etc.).
#'
#' @return A valid palette name.
#' @examples
#' wfp_pal_name("invalid_palette", "qualitative")
#' @export
wfp_pal_name <- function(palette, type) {
  if (is.character(palette)) {
    if (!palette %in% wfpcolors$name) {
      warning("Unknown palette ", palette)
      palette = "blue_sequential"
    }
    return(palette)
  }
  type <- match.arg(type, unique(wfpcolors$type))
  wfpcolors$name[wfpcolors$type == type][palette]
}


##########################################################
## Creates function to validate whether palette has enough categories to allow for vizualization
##########################################################

#' wfp_pal
#'
#' Validates the number of colors in a palette and generates the palette.
#'
#' @param n Numeric. Number of colors to generate. Default is NULL.
#' @param name Character. Name of the palette.
#' @param ... Additional arguments passed to `colorRampPalette`.
#'
#' @return A vector of colors.
#' @examples
#' wfp_pal(5, "blue_sequential")
#' @export
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

#' display_wfp_all
#'
#' Displays all WFP color palettes.
#'
#' @param n Numeric. Number of colors to display for each palette. Default is NULL.
#' @param type Character. Type of palettes to display ("all", "qualitative", "sequential", "diverging"). Default is "all".
#'
#' @return A plot displaying the palettes.
#' @examples
#' display_wfp_all()
#' @export
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