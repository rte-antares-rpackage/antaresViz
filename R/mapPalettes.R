#' Function that maps numeric variable to colors
#'
#' @param x       
#'   numeric vector
#' @param n      
#'   approximate number of colors to use in the color scale
#' @param domain 
#'   range of possible values x could take
#' @param trim 
#' @param negCol  
#'   color of the most negative values
#' @param zeroCol 
#'   color of zero value
#' @param posCol  
#'   color of the most positive value
#' @param alpha   
#'   controls the transparency of the color. 1 is perfectly opaque and 0
#'   is perfectly transparent
#' @param zeroTol 
#'   values in [-zeroTol, zeroTol] are considered to be equal to 0, then
#'   the color used is zeroCol
#'
#' @return
#' A vector of colors with same length than x. 
#' 
#' @noRd
#' 
continuousColorPal <- function(x, n = 5, domain = range(x, na.rm = TRUE),
                               negCol = "#FF0000", zeroCol = "#FFFFFF", posCol = "#0000FF",
                               naCol = "#EEEEEE", zeroTol = NULL) {
  if (is.null(zeroTol)) {
    zeroTol <- signif(diff(domain) * 0.01, 1)
  }
  
  breaks <- pretty(domain, n)
  breaks <- breaks[!breaks == 0]
  breaks <- sort(c(breaks, -zeroTol, + zeroTol))
  
  breaks2 <- breaks
  breaks2[1] <- -Inf
  breaks2[length(breaks2)] <- Inf
  
  negPal <- colorRampPalette(c(negCol, zeroCol))(sum(breaks2 < 0))
  posPal <- colorRampPalette(c(zeroCol, posCol))(sum(breaks2 > 0))
  
  pal <- c(negPal[-length(negPal)], zeroCol, posPal[-1])
  colId <- as.numeric(cut(x, breaks2))
  
  res <- pal[colId]
  res[is.na(res)] <- naCol
  
  breaks[1] <- min(x, na.rm = TRUE)
  breaks[length(breaks)] <- max(x, na.rm = TRUE)
  if (any(breaks < min(x, na.rm = TRUE))) {
    pal <- pal[-(1:sum(breaks < min(x, na.rm = TRUE)))]
    breaks <- breaks[breaks >= min(x, na.rm = TRUE)]
  }
  attr(res, "breaks") <- breaks 
  attr(res, "pal") <- pal
  res
}