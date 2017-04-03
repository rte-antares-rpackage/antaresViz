#' Color palette for categorical variables
#' 
#' @param x factor or character vector.
#' @param levels levels present in the data
#' @param colors vector of colors. If its length is less than the number of 
#'   levels, then the colors are recycled.
#' 
#' @return 
#' A vector of colors with same length as x. This vector has attributes "levels"
#' and "pal" containing respectively levels and color palette.
#' 
#' @noRd
#'
catColorPal <- function(x, levels = NULL, colors = NULL, naCol = "#EEEEEE", ...) {
  
  if (is.null(levels)) {
    if (is.factor(x)) levels <- levels(x)
    else levels <- unique(x)
  }
  
  if (is.null(colors)) colors <- DEFAULT_CAT_COLORS
  
  colors <- rep(colors, length.out = length(levels))
  names(colors) <- levels
  
  res <- unname(colors[as.character(x)])
  
  res[is.na(res)] <- naCol
  
  attr(res, "levels") <- levels 
  attr(res, "pal") <- colors
  
  res
}