#########################################################
## Creates discrete color palettes for vizualization of categorical values
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

pal_ipc <- c("#CDFACD","#FAE61E","#E67800","#C80000", "#640000")


## creates color palette object aligned with indicator compendium guidance
pal_fcsn <-  c("#92D050","#E46C0A","#C00000")

## creates color palette object aligned with indicator compendium guidance
pal_lcs <-  c("#F1ECE8","#D5B868","#F37847","#C00000")

## creates color palette object aligned with indicator compendium guidance
pal_cari <-  c("#FFD7D7","#ff6e6e","#ff0000","#820000")

## creates color palette object aligned with food security guidance
# Renamed from pal_stoplight_3pt to pal_foodsec_blue_3pt
pal_foodsec_blue_3pt <- c("#B4CFED","#007DBC","#002F5A")  # Update colors as needed

## creates color palette object aligned with food security guidance
# Renamed from pal_stoplight_4pt to pal_foodsec_blue_4pt
pal_foodsec_blue_4pt <- c("#B4CFED","#73A5D4","#007DBC","#002F5A")  # Update colors as needed

## creates new red food security palette with 3 points
pal_foodsec_red_3pt <- c("#ECE1B1","#E67536","#E3002B")  # Update colors as needed

## creates new red food security palette with 4 points
pal_foodsec_red_4pt <- c("#ECE1B1","#E6B068","#E67536","#E3002B")  # Update colors as needed

## creates new CARI area palette
pal_cari_area <- c("#FFFFD4","#EAE297","#D7C55A","#C7A600")  # Update colors as needed


## creates color palette object aligned with WFP corporate guidance

pal_main <- c(main_blue, main_light_blue, main_navy, main_ivory, main_orange, main_red, main_purple, main_light_red, main_tan)



## creates object containing palette name, potential palette applications, and min/max number of categories palette can accommodate

# Define the main color palette information table
# This table maps palette names to their properties and color values for different sizes
# Define the main color palette information table
# This table maps palette names to their properties and color values for different sizes
wfpcolors <- tibble::tibble(
  # Palette names - includes standard WFP palettes and specialized indicator palettes
  name = c(
    # Main WFP corporate palette (reduced to 7 colors per roadmap)
    "pal_wfp_main",
    # Food security classification palettes
    "pal_ipc",
    # Renamed from stoplight to foodsec per roadmap
    "pal_foodsec_blue_3pt", 
    "pal_foodsec_blue_4pt",
    # Added new food security red palettes per roadmap
    "pal_foodsec_red_3pt",
    "pal_foodsec_red_4pt",
    # Food consumption score palette
    "pal_fcsn",
    # Livelihood coping strategies palette
    "pal_lcs",
    # Consolidated Approach for Reporting Indicators palettes
    "pal_cari",
    "pal_cari_area", # Added per roadmap
    # Single-color sequential palettes
    "pal_blue",
    # Removed per roadmap: "pal_navy", "pal_grey", "pal_orange"
    "pal_red",
    # Diverging palettes
    "pal_blue_red",
    "pal_navy_red"
  ),
  
  # Palette types: qualitative (categorical), sequential (continuous), or diverging
  type = c(
    rep("qualitative", 10), # First 10 palettes are qualitative (added 3 new ones)
    rep("sequential", 2),   # Next 2 are sequential (removed 3 per roadmap)
    rep("diverging", 2)     # Last 2 are diverging
  ),
  
  # Minimum number of colors for each palette
  min_n = rep(1, 14),
  
  # Maximum number of colors for each palette
  max_n = c(
    7, 5, 3, 4, 3, 4, 3, 4, 4, 4,  # Max colors for qualitative palettes (reduced main to 7)
    rep(7, 2),                     # Max colors for sequential palettes (reduced from 5)
    rep(9, 2)                      # Max colors for diverging palettes
  ),
  
  # Define new color codes for the renamed/new palettes
  # These are placeholders - you'll need to replace with actual color codes
  n1 = list(
    pal_main[1],           # Main WFP blue
    pal_ipc[1],            # IPC minimal/none
    "#92D050",             # foodsec_blue_3pt (placeholder - update with new color)
    "#92D050",             # foodsec_blue_4pt (placeholder - update with new color)
    "#FF0000",             # foodsec_red_3pt (placeholder - update with new color)
    "#FF0000",             # foodsec_red_4pt (placeholder - update with new color)
    "#92D050",             # FCSN (update to match foodsec_blue_3pt)
    "#F1ECE8",             # LCS (update to match foodsec_blue_3pt)
    pal_cari[1],           # CARI
    pal_cari[1],           # CARI area (new)
    pal_blue_wfp[4],       # Medium blue
    pal_red_wfp[4],        # Medium red
    NULL, NULL             # Diverging palettes need at least 2 colors
  ),
  
  # Two-color palettes (n=2)
  n2 = list(
    pal_main[1:2],                         # Main blue, light blue
    pal_ipc[1:2],                          # IPC 1-2
    c("#92D050", "#FFFF00"),               # foodsec_blue_3pt (placeholder - update)
    c("#92D050", "#FFFF00"),               # foodsec_blue_4pt (placeholder - update)
    c("#FF0000", "#FFC000"),               # foodsec_red_3pt (placeholder - update)
    c("#FF0000", "#FFC000"),               # foodsec_red_4pt (placeholder - update)
    c("#92D050", "#E46C0A"),               # FCSN (update to match foodsec_blue_3pt)
    c("#F1ECE8", "#D5B868"),               # LCS (update to match foodsec_blue_3pt)
    pal_cari[1:2],                         # CARI
    c(pal_cari[1], pal_cari[2]),           # CARI area (new)
    c(pal_blue_wfp[2], pal_blue_wfp[6]),   # Light and dark blue
    c(pal_red_wfp[2], pal_red_wfp[6]),     # Light and dark red
    c(pal_blue_wfp[2], pal_red_wfp[6]),    # Light blue to dark red
    c(pal_navy_wfp[2], pal_red_wfp[6])     # Light navy to dark red
  ),
  
  # Three-color palettes (n=3)
  n3 = list(
    pal_main[1:3],                         # Main blue, light blue, navy
    pal_ipc[1:3],                          # IPC 1-3
    c("#92D050", "#FFFF00", "#FF0000"),    # foodsec_blue_3pt (placeholder - update)
    c("#92D050", "#FFFF00", "#FFC000"),    # foodsec_blue_4pt (placeholder - update)
    c("#FF0000", "#FFC000", "#92D050"),    # foodsec_red_3pt (placeholder - update)
    c("#FF0000", "#FFC000", "#FFFF00"),    # foodsec_red_4pt (placeholder - update)
    c("#92D050", "#E46C0A", "#C00000"),    # FCSN (update to match foodsec_blue_3pt)
    c("#F1ECE8", "#D5B868", "#F37847"),    # LCS (update to match foodsec_blue_3pt)
    pal_cari[1:3],                         # CARI
    c(pal_cari[1], pal_cari[2], pal_cari[3]), # CARI area (new)
    pal_blue_wfp[3:5],                     # Medium-light to medium-dark blue
    pal_red_wfp[3:5],                      # Medium-light to medium-dark red
    c(pal_blue_wfp[4], pal_grey_wfp[1], pal_red_wfp[4]), # Blue-neutral-red
    c(pal_navy_wfp[4], pal_grey_wfp[1], pal_red_wfp[4])  # Navy-neutral-red
  ),
  
  # Four-color palettes (n=4)
  n4 = list(
    pal_main[1:4],                         # Main blue, light blue, navy, grey
    pal_ipc[1:4],                          # IPC 1-4
    NULL,                                  # foodsec_blue_3pt doesn't have 4 colors
    c("#92D050", "#FFFF00", "#FFC000", "#FF0000"), # foodsec_blue_4pt (placeholder - update)
    NULL,                                  # foodsec_red_3pt doesn't have 4 colors
    c("#FF0000", "#FFC000", "#FFFF00", "#92D050"), # foodsec_red_4pt (placeholder - update)
    NULL,                                  # FCSN doesn't have 4 colors
    c("#F1ECE8", "#D5B868", "#F37847", "#C00000"), # LCS (update to match foodsec_blue_4pt)
    pal_cari[1:4],                         # CARI
    c(pal_cari[1], pal_cari[2], pal_cari[3], pal_cari[4]), # CARI area (new)
    pal_blue_wfp[2:5],                     # Light to dark blue range
    pal_red_wfp[2:5],                      # Light to dark red range
    c(pal_blue_wfp[4], pal_blue_wfp[2], pal_red_wfp[2], pal_red_wfp[4]), # Blue to red
    c(pal_navy_wfp[5], pal_navy_wfp[3], pal_red_wfp[2], pal_red_wfp[4])  # Navy to red
  ),

  # Five-color palettes (n=5)
  n5 = list(
    pal_main[1:5],                         # First 5 colors of main palette
    pal_ipc[1:5],                          # IPC 1-5 (full IPC scale)
    NULL,                                  # foodsec_blue_3pt doesn't have 5 colors
    NULL,                                  # foodsec_blue_4pt doesn't have 5 colors
    NULL,                                  # foodsec_red_3pt doesn't have 5 colors
    NULL,                                  # foodsec_red_4pt doesn't have 5 colors
    NULL,                                  # FCSN doesn't have 5 colors
    NULL,                                  # LCS doesn't have 5 colors
    NULL,                                  # CARI doesn't have 5 colors
    NULL,                                  # CARI area doesn't have 5 colors
    pal_blue_wfp[2:6],                     # Light to very dark blue range
    pal_red_wfp[2:6],                      # Light to very dark red range
    # 5-color diverging blue-grey-red
    c(pal_blue_wfp[5], pal_blue_wfp[2], pal_grey_wfp[1], pal_red_wfp[2], pal_red_wfp[5]),
    # 5-color diverging navy-grey-red
    c(pal_navy_wfp[5], pal_navy_wfp[3], pal_grey_wfp[1], pal_red_wfp[3], pal_red_wfp[5])
  ),
  
  # Six-color palettes (n=6)
  n6 = list(
    pal_main[1:6],                         # First 6 colors of main palette
    NULL,                                  # IPC doesn't have 6 colors
    NULL,                                  # foodsec_blue_3pt doesn't have 6 colors
    NULL,                                  # foodsec_blue_4pt doesn't have 6 colors
    NULL,                                  # foodsec_red_3pt doesn't have 6 colors
    NULL,                                  # foodsec_red_4pt doesn't have 6 colors
    NULL,                                  # FCSN doesn't have 6 colors
    NULL,                                  # LCS doesn't have 6 colors
    NULL,                                  # CARI doesn't have 6 colors
    NULL,                                  # CARI area doesn't have 6 colors
    pal_blue_wfp[1:6],                     # Very light to very dark blue range
    pal_red_wfp[1:6],                      # Very light to very dark red range
    c(pal_blue_wfp[4:2], pal_red_wfp[2:4]), # 6-color diverging blue-red
    c(pal_navy_wfp[5:3], pal_red_wfp[2:4])  # 6-color diverging navy-red
  ),
  
  # Seven-color palettes (n=7)
  n7 = list(
    pal_main[1:7],                         # First 7 colors of main palette (now max)
    NULL,                                  # IPC doesn't have 7 colors
    NULL,                                  # foodsec_blue_3pt doesn't have 7 colors
    NULL,                                  # foodsec_blue_4pt doesn't have 7 colors
    NULL,                                  # foodsec_red_3pt doesn't have 7 colors
    NULL,                                  # foodsec_red_4pt doesn't have 7 colors
    NULL,                                  # FCSN doesn't have 7 colors
    NULL,                                  # LCS doesn't have 7 colors
    NULL,                                  # CARI doesn't have 7 colors
    NULL,                                  # CARI area doesn't have 7 colors
    pal_blue_wfp[1:7],                     # Full blue range (very light to very dark)
    pal_red_wfp[1:7],                      # Full red range
    # 7-color diverging blue-grey-red
    c(pal_blue_wfp[4:2], pal_grey_wfp[1], pal_red_wfp[2:4]),
    # 7-color diverging navy-grey-red
    c(pal_navy_wfp[5:3], pal_grey_wfp[1], pal_red_wfp[2:4])
  ),
  
  # Eight-color palettes (n=8)
  n8 = list(
    NULL,                                  # Main palette reduced to 7 colors
    NULL, NULL, NULL,                      # IPC, foodsec_blue_3pt, foodsec_blue_4pt don't have 8 colors
    NULL, NULL, NULL,                      # foodsec_red_3pt, foodsec_red_4pt, FCSN don't have 8 colors
    NULL, NULL, NULL,                      # LCS, CARI, CARI area don't have 8 colors
    NULL, NULL,                            # Sequential palettes don't have 8 colors
    c(pal_blue_wfp[5:2], pal_red_wfp[2:5]), # 8-color diverging blue-red
    c(pal_navy_wfp[5:2], pal_red_wfp[2:5])  # 8-color diverging navy-red
  ),
  
  # Nine-color palettes (n=9)
  n9 = list(
    NULL,                                  # Main palette reduced to 7 colors
    NULL, NULL, NULL,                      # IPC, foodsec_blue_3pt, foodsec_blue_4pt don't have 9 colors
    NULL, NULL, NULL,                      # foodsec_red_3pt, foodsec_red_4pt, FCSN don't have 9 colors
    NULL, NULL, NULL,                      # LCS, CARI, CARI area don't have 9 colors
    NULL, NULL,                            # Sequential palettes don't have 9 colors
    # 9-color diverging blue-grey-red
    c(pal_blue_wfp[5:2], pal_grey_wfp[1], pal_red_wfp[2:5]),
    # 9-color diverging navy-grey-red
    c(pal_navy_wfp[5:2], pal_grey_wfp[1], pal_red_wfp[2:5])
  ),
  
  # Ten-color palettes (n=10)
  n10 = list(
    NULL,                                  # Main palette reduced to 7 colors
    NULL, NULL, NULL,                      # IPC, foodsec_blue_3pt, foodsec_blue_4pt don't have 10 colors
    NULL, NULL, NULL,                      # foodsec_red_3pt, foodsec_red_4pt, FCSN don't have 10 colors
    NULL, NULL, NULL,                      # LCS, CARI, CARI area don't have 10 colors
    NULL, NULL,                            # Sequential palettes don't have 10 colors
    NULL, NULL                             # Diverging palettes don't have 10 colors
  )
)

# Convert tibble to data frame for compatibility
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