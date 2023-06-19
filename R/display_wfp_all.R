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
