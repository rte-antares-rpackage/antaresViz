# Copyright © 2016 RTE Réseau de transport d’électricité

#' Function that maps numeric variable to colors
#'
#' @param x       
#'   numeric vector
#' @param breaks      
#'   Either a single number representing the approximate number of colors to 
#'   use in the color scale or a vector of numbers indicating how to cut data.
#' @param domain 
#'   range of possible values x could take
#' @param trim 
#' @param negCol  
#'   color of the most negative values
#' @param zeroCol 
#'   color of zero value
#' @param posCol  
#'   color of the most positive value
#' @param zeroTol 
#'   values in [-zeroTol, zeroTol] are considered to be equal to 0, then
#'   the color used is zeroCol
#' @param colors
#'   Vector of colors. If it is set and if user manually sets break points, then
#'   these colors are used instead of the colors defined by parameters negCol,
#'   zeroCol and posCol.
#'   
#' @return
#' A vector of colors with same length than x. 
#' 
#' @noRd
#' 
continuousColorPal <- function(x, breaks = 5, domain = NULL,
                               negCol = "#FF0000", zeroCol = "#FFFFFF", posCol = "#0000FF",
                               naCol = "#EEEEEE", zeroTol = NULL, colors = NULL, ...) {
  
  if (is.null(domain)) domain <- range(x, na.rm = TRUE)
  if (domain[1] == domain[2]) domain <- domain[1] + c(-1, 1)
  if (is.null(zeroTol)) {
    zeroTol <- signif(diff(domain) * 0.02, 1)
  }
  
  if (length(breaks) == 1) {
    # Automatically choose approximatelly 'length(breaks)' break points
    autobreaks <- TRUE
    breaks <- pretty(domain, breaks)
    
    if (zeroTol > 0) {
      breaks <- breaks[breaks != 0]
      breaks <- sort(c(breaks, -zeroTol, + zeroTol))
    }
    
    # Replace the first and last break by the bounds of the domain
    breaks[1] <- domain[1]
    breaks[length(breaks)] <- domain[2]
    
    breaks <- breaks[breaks >= domain[1] & breaks <= domain[2]]
  } else {
    autobreaks <- FALSE
    # If breaks do not enclose the domain, we add domain bounds as break points
    if (min(breaks) > domain[1]) breaks <- c(domain[1], breaks)
    if (max(breaks) < domain[2]) breaks <- c(breaks, domain[2])
  }
  
  breaks <- sort(unique(breaks))
  # Ensure that extreme values are contained in the first or the last interval.
  breaks2 <- breaks
  breaks2[1] <- -Inf
  breaks2[length(breaks2)] <- Inf
  
  if (!is.null(colors) & !autobreaks) {
    pal <- colors
  } else {
    # Choose colors for positive and negative values
    if (all(breaks <= 0)) {
      if (max(domain) >= -zeroTol) {
        pal <- colorRampPalette(c(negCol, zeroCol), space = "Lab")(length(breaks) - 1)
      } else {
        pal <- colorRampPalette(c(negCol, zeroCol), space = "Lab")(length(breaks))
        pal <- pal[-length(pal)]
      }
    } else if (all(breaks >= 0)) {
      if (min(domain) <= zeroTol) {
        pal <- colorRampPalette(c(zeroCol, posCol), space = "Lab")(length(breaks) - 1)
      } else {
        pal <- colorRampPalette(c(zeroCol, posCol), space = "Lab")(length(breaks))
        pal <- pal[-1]
      }
    } else {
      negPal <- colorRampPalette(c(negCol, zeroCol), space = "Lab")(sum(breaks <= 0))
      posPal <- colorRampPalette(c(zeroCol, posCol), space = "Lab")(sum(breaks >= 0))
      
      # Create the color palette
      # We include the zero color only if some interval contains 0
      changeSign <- any(diff(sign(breaks)) == 2)
      if (changeSign) {
        pal <- c(negPal[-length(negPal)], zeroCol, posPal[-1])
      } else {
        pal <- c(negPal[-length(negPal)], posPal[-1])
      }
    }
  }
  
  
  # Map values to colors
  colId <- as.numeric(cut(x, breaks2))
  
  # NA values and values outside the domain are mapped to the naCol
  res <- pal[colId]
  res[is.na(res) | x < domain[1] | x > domain[2]] <- naCol
  
  attr(res, "breaks") <- breaks 
  attr(res, "pal") <- pal
  
  res
}