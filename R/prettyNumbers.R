#' Private function that rounds a number and add a suffix (KMBT) of it is too
#' large
#' 
#' @noRd
prettyNumbers <- function(x) {
  if (!is.numeric(x)) return(x)
  
  sapply(x, function(number) {
    absVal <- abs(number)
    sign <- ifelse(number < 0, "-", "")
    scale <- ""
    
    if( absVal < 1000 ) {
      scale <- ''
    } else if( absVal < 1000000 ) {
      scale <- 'K'
      absVal <- absVal/1000
      
    } else if( absVal < 1000000000 ) {
      scale <- 'M'
      absVal <- absVal/1000000
      
    } else if( absVal < 1000000000000 ) {
      scale <- 'B'
      absVal <- absVal/1000000000
      
    } else if( absVal < 1000000000000000 ) {
      scale <- 'T'
      absVal <- absVal/1000000000000
    }
    absVal <- round(absVal, 2)
    
    paste0(sign, absVal, scale)
  })
}